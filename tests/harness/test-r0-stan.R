box::use(
  R / harness / r0[r0_stan_data],
  R / harness / info_set[info_set, read_inputs]
)

R0_STAN <- "polling_and_fundamentals_kjordaemi_r0.stan"
PRODUCTION_STAN <- "Stan/polling_and_fundamentals_kjordaemi.stan"
# WHY: R0 is production's model at this commit plus the hooks (Decision 5 keeps it uncentred); production has since centred delta.
R0_PARENT_COMMIT <- "5d22031"
H1_DECLARATIONS <- c(
  "vector<lower = 0, upper = 1>[P - 1] mu_gamma_scale;",
  "array[D] int<lower = 1, upper = P> n_active_d;",
  "array[D, P] int<lower = 0, upper = P> active_index_d;"
)

#' R0's parent, production's Stan file at `R0_PARENT_COMMIT`, read with `git show` into
#' `results/harness/stan_parent/` (gitignored) and rewritten only when its bytes differ.
#' @return the file's path
parent_stan <- function() {
  withr::local_envvar(c(GIT_DIR = NA, GIT_WORK_TREE = NA, GIT_INDEX_FILE = NA))
  shown <- withr::local_tempfile(fileext = ".stan")
  spec <- paste0(R0_PARENT_COMMIT, ":", PRODUCTION_STAN)
  if (system2("git", c("-C", shQuote(root()), "show", shQuote(spec)), stdout = shown, stderr = FALSE) != 0) {
    stop("git show ", spec, " failed")
  }
  bytes <- readBin(shown, "raw", file.size(shown))
  path <- root("results", "harness", "stan_parent", basename(PRODUCTION_STAN))
  if (!file.exists(path) || !identical(readBin(path, "raw", file.size(path)), bytes)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes, path)
  }
  path
}

#' A Stan file as one string marked "bytes", so that positions and comparisons are bytewise.
stan_bytes <- function(path) {
  x <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(x) <- "bytes"
  x
}

#' The text before `transformed data {`, from there up to `generated quantities {`, and the
#' rest; an error unless each marker occurs once.
stan_blocks <- function(x) {
  at <- function(marker) {
    i <- gregexpr(marker, x, fixed = TRUE, useBytes = TRUE)[[1]]
    if (length(i) != 1 || i[1] < 1) stop("`", marker, "` is not in the program exactly once")
    i[1]
  }
  td <- at("transformed data {")
  gq <- at("generated quantities {")
  list(data = substr(x, 1, td - 1), middle = substr(x, td, gq - 1), gq = substr(x, gq, nchar(x, type = "bytes")))
}

#' Stan text without comments or whitespace.
bare <- function(x) {
  x <- gsub("(?s)/\\*.*?\\*/", "", x, perl = TRUE, useBytes = TRUE)
  x <- gsub("//[^\n]*", "", x, useBytes = TRUE)
  gsub("[[:space:]]+", "", x, useBytes = TRUE)
}

#' For each row r of `y`, whether it is 0 outside the columns `index[r, 1:n[r]]`.
zero_outside <- function(y, index, n) {
  vapply(seq_len(nrow(y)), \(r) all(y[r, -index[r, seq_len(n[r])]] == 0), logical(1))
}

#' For each row r of `y`, "" if it is positive on exactly the columns `index[r, 1:n[r]]` and
#' 0 elsewhere, else how it breaks that.
support_breaks <- function(y, index, n) {
  vapply(seq_len(nrow(y)), \(r) {
    cols <- index[r, seq_len(n[r])]
    out <- setdiff(seq_len(ncol(y)), cols)
    at_zero <- cols[y[r, cols] == 0]
    filled <- out[y[r, out] != 0]
    paste(c(
      if (length(at_zero)) paste0("active column(s) ", paste(at_zero, collapse = ","), " at zero"),
      if (length(filled)) paste0("column(s) ", paste(filled, collapse = ","), " outside the active set nonzero")
    ), collapse = "; ")
  }, character(1))
}

#' The rows `support_breaks()` flags, as "level r: how; ...".
described <- function(breaks) {
  bad <- which(breaks != "")
  if (length(bad)) paste0("level ", bad, ": ", breaks[bad], collapse = "; ") else "every level exact"
}

#' A fit's draws of `variables`, each an array with the draw first.
draws_of <- function(fit, variables) {
  rv <- posterior::as_draws_rvars(fit$draws(variables))
  lapply(stats::setNames(variables, variables), \(v) unname(posterior::draws_of(rv[[v]])))
}

#' Softmax of the sum-to-zero vector whose last P - 1 entries are `e`.
closed_softmax <- function(e) {
  eta <- c(-sum(e), e)
  exp(eta - max(eta)) / sum(exp(eta - max(eta)))
}

#' R0's data at the 2024 eve in one JSON that both models read, both models compiled with
#' their methods, R0 fitted for 10 draws and its parent (`parent_stan()`) for 1 + 10.
fit_both <- function() {
  x <- r0_stan_data(info_set(as.Date("2024-11-30"), as.Date("2024-11-29"), read_inputs()))
  dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  json <- file.path(dir, "data.json")
  cmdstanr::write_stan_json(x$data, json)
  # WHY: compiled afresh: cmdstanr builds the log_prob methods only when it compiles, never from a cached executable.
  compile <- function(path) {
    cmdstanr::cmdstan_model(path, dir = dir, compile_model_methods = TRUE, quiet = TRUE)
  }
  run <- function(model, warmup, sampling) {
    model$sample(
      data = json, chains = 1, iter_warmup = warmup, iter_sampling = sampling, seed = 1, init = 0,
      max_treedepth = 6, sig_figs = 18, output_dir = dir, refresh = 0, show_messages = FALSE,
      show_exceptions = FALSE, diagnostics = NULL
    )
  }
  list(data = x$data, r0 = run(compile(root("Stan", R0_STAN)), 20, 10), parent = run(compile(parent_stan()), 1, 10))
}

fixture <- new.env()
fits <- function() {
  # WHY: one compile and fit serves every slow test, and a failed one is not retried per test.
  if (is.null(fixture$value)) fixture$value <- tryCatch(fit_both(), error = identity)
  if (inherits(fixture$value, "error")) stop(fixture$value)
  fixture$value
}

test_that("the r0 file differs from its parent, production at 5d22031, only in the data block and generated quantities", {
  expect_true(file.exists(root("Stan", R0_STAN)))
  parent <- stan_blocks(stan_bytes(parent_stan()))
  r0 <- stan_blocks(stan_bytes(root("Stan", R0_STAN)))
  expect_identical(r0$middle, parent$middle)
  m <- bare(parent$data)
  expect_true(endsWith(m, "}"))
  expect_identical(bare(r0$data), paste0(substr(m, 1, nchar(m) - 1), bare(paste(H1_DECLARATIONS, collapse = "\n")), "}"))
})

test_that("the r0 log density is its parent's", {
  skip_unless_slow()
  skip_without_snapshot()
  f <- fits()
  u <- posterior::as_draws_matrix(f$r0$unconstrain_draws())
  expect_equal(nrow(u), 10)
  for (i in seq_len(nrow(u))) {
    ui <- as.numeric(u[i, ])
    expect_equal(f$parent$log_prob(ui), f$r0$log_prob(ui), tolerance = 1e-10)
    expect_equal(f$parent$grad_log_prob(ui), f$r0$grad_log_prob(ui), tolerance = 1e-10)
  }
})

test_that("the r0 hooks compute what they claim", {
  skip_unless_slow()
  skip_without_snapshot()
  f <- fits()
  d <- f$data
  v <- draws_of(f$r0, c(
    "mu_gamma", "mu_gamma_weight", "mu_gamma_pred", "beta", "delta_pred", "pi_eday", "pi_kd",
    "y_rep_k", "y_rep_national", "y_pred_constituency"
  ))
  expect_equal(nrow(v$mu_gamma), 10)
  eday <- d$D + d$pred_y_time_diff
  for (i in seq_len(nrow(v$mu_gamma))) {
    mg <- v$mu_gamma[i, ]
    expect_equal(v$mu_gamma_pred[i, ], v$mu_gamma_weight[i, 1] * d$mu_gamma_scale * c(mg, -sum(mg)), tolerance = 1e-12)
    expect_equal(v$pi_eday[i, ], closed_softmax(v$beta[i, , eday] + v$mu_gamma_pred[i, ]), tolerance = 1e-10)
    for (k in seq_len(d$K)) {
      e <- v$beta[i, , eday] + v$delta_pred[i, , k] + v$mu_gamma_pred[i, ]
      expect_equal(v$pi_kd[i, k, ], closed_softmax(e), tolerance = 1e-10)
    }
    y_k <- v$y_rep_k[i, , ]
    expect_true(all(zero_outside(y_k, d$party_index_k, d$n_parties_k)))
    expect_equal(rowSums(y_k), unname(rowSums(d$y_k)))
    y_n <- v$y_rep_national[i, , ]
    breaks <- support_breaks(y_n[seq_len(d$D), ], d$active_index_d, d$n_active_d)
    # WHY: positivity is safe: R0's smallest active count is 901 of n_pred 196,292, with draws from all P parties.
    expect_true(all(breaks == ""), info = described(breaks))
    expect_equal(rowSums(y_n), rep(d$n_pred, eday))
    expect_equal(rowSums(v$y_pred_constituency[i, , ]), as.numeric(d$n_pred_k))
  }
})

test_that("the gather checks catch the parent's positional slicing", {
  skip_unless_slow()
  skip_without_snapshot()
  f <- fits()
  d <- f$data
  v <- draws_of(f$parent, c("y_rep_k", "y_rep_national"))
  n <- dim(v$y_rep_k)[1]
  expect_equal(n, 10)
  # WHY: a draw can leave every dropped column at zero by chance, so the kjördæmi check needs only one draw to fail.
  leaks <- vapply(seq_len(n), \(i) !all(zero_outside(v$y_rep_k[i, , ], d$party_index_k, d$n_parties_k)), logical(1))
  expect_true(any(leaks))
  for (i in seq_len(n)) {
    breaks <- support_breaks(v$y_rep_national[i, seq_len(d$D), ], d$active_index_d, d$n_active_d)
    expect_false(all(breaks == ""), info = described(breaks))
    # WHY: the parent slices one column too few at a level whose active set skips a party, never one too many.
    expect_true(all(grepl("^active column\\(s\\) [0-9,]+ at zero$", breaks[breaks != ""])), info = described(breaks))
    expect_equal(rowSums(v$y_rep_national[i, , ]), rep(d$n_pred, d$D + d$pred_y_time_diff))
  }
})
