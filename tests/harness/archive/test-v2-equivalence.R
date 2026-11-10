# The P-step equivalence check (R/harness/v2_equivalence.R). Its CSV writer is tested with the
# file loaded as a module, so its script part does not run, and its refusals by running the
# script on command lines it stops on before anything is compiled. The check itself runs as a
# subprocess, into a temporary CSV, never the committed one: slow, and named by the step whose
# form it checks.
box::use(
  R / harness / constants[POLL_PARTIES],
  R / harness / v2_equivalence[
    EQUIVALENCE_SUMMARY, check_reasons, control_verdicts, gq_points_compared, provisional_row, write_equivalence_row
  ],
  R / harness / v2_reference[M_e]
)

# The modules' namespaces, for the delta map's internals and the reference's delta walk.
eq <- environment(check_reasons)
ref <- environment(M_e)

P1_CONFIG <- "harness/configs/steps/v2s-p1.json"
P2_CONFIG <- "harness/configs/steps/v2s-p2.json"

# The script's output lines, with attribute `status` when it exits non-zero.
run_equivalence <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/v2_equivalence.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

shown <- function(out) paste(out, collapse = "\n")

# A summary row with every EQUIVALENCE_SUMMARY column, these values set.
summary_row <- function(...) {
  row <- as.list(stats::setNames(rep("x", length(EQUIVALENCE_SUMMARY)), EQUIVALENCE_SUMMARY))
  utils::modifyList(row, list(...))
}

test_that("the CSV writer creates the file, replaces a row of the same E, F and order in place, and appends others", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "FAIL"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), EQUIVALENCE_SUMMARY)
  expect_identical(x$verdict, "FAIL")
  write_equivalence_row(path, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "reversed", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", reasons = "rerun"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25", "2024-11-30"))
  expect_identical(x$order, c("default", "default", "reversed"))
  expect_identical(x$verdict, c("PASS", "PASS", "PASS"))
  expect_identical(x$reasons, c("rerun", "x", "x"))
  expect_length(list.files(dirname(path), pattern = "^equivalence-"), 0)
})

test_that("the CSV writer keeps the file's columns first and fills a column a row lacks", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, list(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", a = 1))
  write_equivalence_row(path, list(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "FAIL", b = 2))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), c("E", "F", "order", "verdict", "a", "b"))
  expect_identical(x$a, c("1", ""))
  expect_identical(x$b, c("", "2"))
  expect_error(write_equivalence_row(path, list(E = "2024-11-30", verdict = "PASS")), "E, F, order and verdict")
})

test_that("a provisional row replaces an earlier verdict of its cell and order, and the check's own row replaces it", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, provisional_row("2024-11-30", "2024-11-29", "default", "walk", P1_CONFIG, "sha"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), EQUIVALENCE_SUMMARY)
  expect_identical(x$verdict, c("FAIL", "PASS"))
  expect_identical(x$reasons, c("incomplete", "x"))
  expect_identical(x$stan_sha256, c("sha", "x"))
  expect_identical(x$n_posterior, c("", "x"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", reasons = ""))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25"))
  expect_identical(x$verdict, c("PASS", "PASS"))
  expect_identical(x$reasons, c("", "x"))
})

# check_reasons() at a passing check's values, these changed. Unless given, gq_ran is what
# main() takes it to be: gq_points_compared() of kept_n and main_kind above 0.
reasons_at <- function(..., main_kind = "posterior") {
  args <- utils::modifyList(list(
    form = "walk", const_range = 1e-14, tol = 1e-12,
    controls = c(ref_walk = 1, new_anchor = 1, new_bridge = 1, new_forward = 1),
    gq_max_rel = 1e-15, path_err = 1e-12, one_sided = 0L, posterior_dropped = FALSE,
    kept_n = c(posterior = 30L, `off-posterior` = 15L)
  ), list(...))
  if (is.null(args$gq_ran)) args$gq_ran <- gq_points_compared(args$kept_n, main_kind) > 0L
  do.call(check_reasons, args)
}

test_that("check_reasons() gives none at a passing check's values, and each failed condition in order", {
  expect_identical(reasons_at(), character(0))
  expect_identical(reasons_at(const_range = 1e-11), "constant range 1e-11 is not below tol 1e-12")
  expect_identical(reasons_at(gq_max_rel = 1e-6), "generated quantities differ by 1e-06 relative")
  expect_identical(reasons_at(one_sided = 2L), "2 points finite on one side only")
  expect_identical(
    reasons_at(const_range = Inf, path_err = 1e-7, posterior_dropped = TRUE),
    c("constant range Inf is not below tol 1e-12", "path error 1e-07", "a posterior point was dropped")
  )
  expect_identical(
    reasons_at(
      const_range = 1, controls = c(ref_walk = 1, new_anchor = NA, new_bridge = 1e-13, new_forward = 1),
      gq_max_rel = 1e-3, path_err = 1, one_sided = 3L, posterior_dropped = TRUE,
      kept_n = c(posterior = 30L, `off-posterior` = 4L)
    ),
    c(
      "constant range 1 is not below tol 1e-12", "a negative control passes: new_bridge",
      "a negative control is not exercised: new_anchor", "generated quantities differ by 0.001 relative",
      "path error 1", "3 points finite on one side only", "a posterior point was dropped",
      "fewer than 5 kept points of a kind"
    )
  )
})

test_that("check_reasons() fails a walk check on a negative control that is not exercised, naming its term", {
  controls <- c(ref_walk = 1, new_anchor = NA, new_bridge = 1, new_forward = 1)
  expect_identical(
    control_verdicts(controls, 1e-12),
    c(ref_walk = "fails", new_anchor = "not exercised", new_bridge = "fails", new_forward = "fails")
  )
  expect_identical(reasons_at(controls = controls), "a negative control is not exercised: new_anchor")
  expect_identical(
    reasons_at(controls = c(ref_walk = 1, new_anchor = NA, new_bridge = 1e-13, new_forward = NA)),
    c("a negative control passes: new_bridge", "a negative control is not exercised: new_anchor, new_forward")
  )
  # WHY: only a walk check fails on a control that is not exercised; under the delta form only new_delta_absent can be constant over the points, on a cell where no closed election lacks a party.
  expect_identical(reasons_at(form = "delta", controls = c(ref_delta = 1, ref_unused = NA, new_delta_absent = 1)), character(0))
  expect_identical(
    reasons_at(form = "delta", controls = c(ref_delta = 1, ref_unused = 1e-13, new_delta_absent = NA)),
    "a negative control passes: ref_unused"
  )
})

test_that("the summary has one control column per log-Jacobian term of each form, before the reasons", {
  controls <- paste0("control_", c("ref_walk", "new_anchor", "new_bridge", "new_forward", "ref_delta", "ref_unused", "new_delta_absent"))
  expect_identical(EQUIVALENCE_SUMMARY[EQUIVALENCE_SUMMARY %in% controls], controls)
  expect_identical(utils::tail(EQUIVALENCE_SUMMARY, length(controls) + 1), c(controls, "reasons"))
})

test_that("check_reasons() fails on fewer than 5 kept points of the main kind and judges no generated quantities then", {
  expect_identical(reasons_at(kept_n = c(posterior = 4L, `off-posterior` = 15L), gq_max_rel = NA_real_), "fewer than 5 kept points of a kind")
  expect_identical(reasons_at(main_kind = "prior", kept_n = c(prior = 4L), gq_max_rel = NA_real_), "fewer than 5 kept points of a kind")
  # WHY: the control for the two above: with 5 kept points the same NA difference is judged.
  expect_identical(reasons_at(kept_n = c(posterior = 5L, `off-posterior` = 15L), gq_max_rel = NA_real_), "generated quantities differ by NA relative")
  expect_identical(reasons_at(kept_n = c(posterior = 30L, `off-posterior` = 4L)), "fewer than 5 kept points of a kind")
})

test_that("gq_points_compared() is the one gate on the GQ comparison, and a check that compared none writes gq_points 0", {
  expect_identical(gq_points_compared(c(posterior = 30L, `off-posterior` = 15L), "posterior"), 30L)
  expect_identical(gq_points_compared(c(posterior = 5L, `off-posterior` = 15L), "posterior"), 5L)
  expect_identical(gq_points_compared(c(posterior = 4L, `off-posterior` = 15L), "posterior"), 0L)
  expect_identical(gq_points_compared(c(posterior = 0L, `off-posterior` = 0L, prior = 20L), "prior"), 20L)
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, summary_row(
    E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "FAIL",
    gq_points = gq_points_compared(c(posterior = 4L, `off-posterior` = 15L, prior = 0L), "posterior"),
    gq_max_rel = NA_real_, reasons = "fewer than 5 kept points of a kind"
  ))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(x$gq_points, "0")
  expect_identical(x$gq_max_rel, "")
})

# A small layout for the delta form's map, with the real cells' cases: K = 4, P = 5, three
# closed elections and an open epoch, each epoch with its own weights and each election's
# parties listed out of order. Party 4 has results at elections 1 and 3 but not at 2; party
# 5 enters in epoch 2, has no results at its entry election and has them at 3.
delta_toy <- function() {
  K <- 4L
  P <- 5L
  S <- list(c(3L, 1L, 4L, 2L), c(2L, 3L, 1L), c(5L, 1L, 4L, 3L, 2L))
  y <- array(0, c(3L, K, P))
  withr::with_seed(31, for (e in 1:3) y[e, , S[[e]]] <- sample(200:9000, K * length(S[[e]]), replace = TRUE))
  list(
    K = K, P = P, n_closed = 3L, n_epoch = 4L, idx_res = t(vapply(S, \(s) c(s, rep(0L, P - length(s))), integer(P))),
    n_rep_res = lengths(S), y_res = y,
    w = rbind(c(0.1, 0.2, 0.3, 0.4), c(0.25, 0.25, 0.3, 0.2), c(0.4, 0.3, 0.2, 0.1), c(0.15, 0.35, 0.3, 0.2)),
    entry_epoch = c(0L, 0L, 0L, 0L, 2L), delta_walk = 1L, delta_form = 1L, delta_years = c(0, 1, 4, 3), delta_years_bar = 4
  )
}

# The offset of closed election e on `d`, the coordinates of A_e log(y_e + 1/2) C_S, with
# explicit matrices: A_e = I - 1 w_e', C_S = I - 11'/|S_e| over S_e's columns, and party p of
# S_e (p > 1) at coordinate p - 1 of rows 2:K; 0 in the other coordinates.
offset_by_hand <- function(d, e) {
  S <- d$idx_res[e, seq_len(d$n_rep_res[e])]
  A <- diag(d$K) - matrix(1, d$K, 1) %*% d$w[e, , drop = FALSE]
  Z <- A %*% log(d$y_res[e, , S] + 0.5) %*% (diag(length(S)) - 1 / length(S))
  off <- matrix(0, d$K - 1, d$P - 1)
  for (j in seq_along(S)) if (S[j] > 1) off[, S[j] - 1] <- Z[-1, j]
  off
}

# Stan/forecast_v2.stan's delta at the closed elections from delta_coord, with base chol()
# for its QR factors and offset_by_hand() for its offset: at election e, with s_e the step's
# scales, M the coordinates of M_e(m_e) and D = delta_e[2:K, 2:P], the absent columns D_B =
# M_B + L_Psi W_B L_BB' and the present ones D_Pr = off_Pr + R - rowSums(D_B) / (|Pr| + 1);
# then delta_e[k, 1] = -sum D[k - 1, ] and row 1 from the weights.
centred_delta_by_hand <- function(coord, s_delta0, s_zeta, d) {
  K <- d$K
  P <- d$P
  lower_chol <- function(X) t(chol(X))
  out <- array(0, c(d$n_closed, K, P))
  for (e in seq_len(d$n_closed)) {
    S <- d$idx_res[e, seq_len(d$n_rep_res[e])]
    pr <- sort(S[S > 1]) - 1L
    ab <- setdiff(seq_len(P - 1), pr)
    enter <- d$entry_epoch == e
    s <- if (e == 1) s_delta0 else ifelse(enter, s_delta0, sqrt(d$delta_years[e] / d$delta_years_bar) * s_zeta)
    M <- matrix(0, K - 1, P - 1)
    if (e > 1) {
      prev <- out[e - 1, , ]
      m <- prev - rowMeans(prev[, d$idx_res[e - 1, seq_len(d$n_rep_res[e - 1])], drop = FALSE])
      m[, enter] <- 0
      M <- M_e(m, d$w[e, ])[-1, -1]
    }
    A <- diag(K) - matrix(1, K, 1) %*% d$w[e, , drop = FALSE]
    Tp <- ((diag(P) - 1 / P) %*% diag(s))[-1, ]
    W <- coord[e, , ]
    D <- matrix(0, K - 1, P - 1)
    if (length(ab)) {
      D[, ab] <- M[, ab] + lower_chol(tcrossprod(A)[-1, -1]) %*% W[, ab, drop = FALSE] %*% t(lower_chol(tcrossprod(Tp[ab, , drop = FALSE])))
    }
    D[, pr] <- offset_by_hand(d, e)[, pr] + W[, pr] - rowSums(D[, ab, drop = FALSE]) / (length(pr) + 1)
    de <- matrix(0, K, P)
    de[-1, -1] <- D
    de[-1, 1] <- -rowSums(D)
    de[1, ] <- -colSums(d$w[e, -1] * de[-1, , drop = FALSE]) / d$w[e, 1]
    out[e, , ] <- de
  }
  out
}

test_that("delta_layout() gives each closed election's present and absent coordinates, and its offset over S_e's columns", {
  # WHY: the toy's elections 1 and 2 lack parties, so an offset centred over all P parties, with
  # log(0 + 1/2) in the missing ones, differs from one centred over S_e; so do one without the
  # + 1/2 and one with another epoch's weights.
  d <- delta_toy()
  lay <- eq$delta_layout(d)
  expect_identical(lay$present, list(1:3, 1:2, 1:4))
  expect_identical(lay$absent, list(4L, 3:4, integer(0)))
  for (e in 1:3) {
    expect_identical(lay$S[[e]], d$idx_res[e, seq_len(d$n_rep_res[e])])
    expect_lt(max(abs(lay$off[[e]] - offset_by_hand(d, e))), 1e-12, label = paste("offset", e))
    expect_true(all(lay$off[[e]][, lay$absent[[e]]] == 0), label = paste("absent offset", e))
  }
})

test_that("to_centred() inverts the centred delta: the reference walk's delta, mapped to delta_coord and built back as the Stan model builds it, is itself", {
  # WHY: the toy's absent blocks are 1 and 2 parties wide, one of them an entrant at its own
  # entry election and S changes between elections, so a shift by rowSums(D_B) / |Pr|, an
  # untransposed L_BB, the mean taken over S_e rather than S_{e-1} or s_zeta as an entrant's
  # scale each leave delta_coord that builds back to another delta.
  d <- delta_toy()
  withr::with_seed(32, {
    zeta <- array(stats::rnorm(d$n_epoch * d$K * d$P), c(d$n_epoch, d$K, d$P))
    s_delta0 <- stats::runif(d$P, 0.2, 1.5)
    s_zeta <- stats::runif(d$P, 0.05, 0.6)
  })
  delta <- ref$delta_epochs(zeta, s_delta0, s_zeta, d)
  coord <- eq$to_centred(delta, s_delta0, s_zeta, d, eq$delta_layout(d))
  expect_identical(dim(coord), c(3L, d$K - 1L, d$P - 1L))
  built <- centred_delta_by_hand(coord, s_delta0, s_zeta, d)
  expect_lt(max(abs(built - delta[1:3, , , drop = FALSE])), 1e-10)
})

test_that("the script refuses a bad command line before it compiles anything, and writes no CSV", {
  local_no_cmdstan()
  out <- withr::local_tempfile(fileext = ".csv")
  missing_dir <- file.path(withr::local_tempdir(), "no-such-dir", "out.csv")
  cell <- "--cell=2024-11-30,2024-11-29"
  cases <- list(
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell), "--out is required"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", out), "--n=ten"), "--n must be a whole number"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", out), "--bogus=1"), "unknown argument --bogus=1"),
    list(c("--config=configs/v2s-p1.json", "--form=walk", cell, paste0("--out=", out)), "--config must be a file under harness/configs/"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30", paste0("--out=", out)), "--cell must be <E>,<F>"),
    list(c(paste0("--config=", P1_CONFIG), "--form=centred", cell, paste0("--out=", out)), "--form must be walk, delta or both"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", missing_dir)), "--out must be a file in an existing directory"),
    list(c(paste0("--config=", P1_CONFIG), "--form=delta", cell, paste0("--out=", out)), "the config's delta_form is 0"),
    list(c(paste0("--config=", P2_CONFIG), "--form=both", cell, paste0("--out=", out)), "--form=both is not built: P1 kept walk_form = 0"),
    list(c("--config=harness/configs/steps/v2s-m0.json", "--form=walk", cell, paste0("--out=", out)), "the config's walk_form is 0"),
    list(c(paste0("--config=", P2_CONFIG), "--form=walk", cell, paste0("--out=", out)), "the config's walk_form is 0"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2017-10-28,2017-10-27", paste0("--out=", out), "--n-off=5"), "a 2017 cell takes prior points only")
  )
  for (case in cases) {
    res <- run_equivalence(case[[1]])
    expect_identical(attr(res, "status"), 1L, label = shown(case[[1]]))
    expect_match(shown(res), paste0("v2_equivalence: ", case[[2]]), fixed = TRUE)
  }
  expect_false(file.exists(out))
  expect_false(file.exists(missing_dir))
})

test_that("a check that stops after its command line is accepted leaves a provisional FAIL row, not an earlier verdict", {
  skip_without_snapshot()
  local_no_cmdstan()
  out <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(out, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS"))
  write_equivalence_row(out, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  # WHY: with no CmdStan the check stops at its first CmdStan call, the compile, after the provisional row and before any point.
  res <- run_equivalence(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30,2024-11-29", paste0("--out=", out)))
  expect_identical(attr(res, "status"), 1L)
  expect_false(any(grepl("^EQUIV ", res)), label = shown(res))
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25"))
  expect_identical(x$verdict, c("FAIL", "PASS"))
  expect_identical(x$reasons, c("incomplete", "x"))
  expect_identical(x$stan_sha256[1], digest::digest(file = root("Stan", "forecast_v2.stan"), algo = "sha256"))
})

test_that("v2s-p1: the anchored walk is the reference walk on 2021 f180", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2021-09-25,2021-03-29", "--n=10", "--n-off=5", paste0("--out=", out)
  ))
  expect_null(attr(res, "status"), label = shown(res))
  expect_match(res, "^EQUIV PASS", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(nrow(x), 1L)
  expect_identical(unlist(x[1, c("E", "F", "order", "verdict")], use.names = FALSE), c("2021-09-25", "2021-03-29", "default", "PASS"))
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_prior")], use.names = FALSE), c("10", "5", "0"))
  # WHY: each log-Jacobian term must be exercised on this cell, so that its negative control is decisive.
  expect_identical(unlist(x[1, paste0("control_", c("ref_walk", "new_anchor", "new_bridge", "new_forward"))], use.names = FALSE), rep("fails", 4))
})

test_that("v2s-p1: the check fails when the bridge's conditional scale is the step's own", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  # The bridge's conditional sd sqrt(v_t (cv_b - cv_t) / R) replaced by sqrt(v_t): a valid model,
  # but not the reference walk, so the constant moves with the walk scales between points.
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  bridge <- "sqrt(v[t] * (cv[a] - cv[t]) / R)"
  hit <- grepl(bridge, stan, fixed = TRUE)
  expect_identical(sum(hit), 1L)
  mutant <- file.path(withr::local_tempdir("v2-equivalence-mutant-"), "forecast_v2.stan")
  writeLines(sub(bridge, "sqrt(v[t])", stan, fixed = TRUE), mutant)
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2021-09-25,2021-03-29", "--n=10", "--n-off=5",
    paste0("--out=", out), paste0("--stan=", mutant)
  ))
  expect_identical(attr(res, "status"), 1L, label = shown(res))
  expect_match(res, "^EQUIV FAIL", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(x$verdict, "FAIL")
  # WHY: the FAIL must come from the density check alone: the path, the generated quantities and the points are as asked.
  expect_match(x$reasons, "^constant range [^;]* is not below tol [^;]*$")
  expect_lt(as.numeric(x$path_err), 1e-8)
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_rejected", "n_outside", "n_one_sided")], use.names = FALSE), c("10", "5", "0", "0", "0"))
})

test_that("v2s-p1: the anchored walk is the reference walk on 2024 eve under the reversed orders", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30,2024-11-29", "--n=10", "--n-off=5", "--reverse",
    paste0("--out=", out)
  ))
  expect_null(attr(res, "status"), label = shown(res))
  expect_match(res, "^EQUIV PASS", all = FALSE)
  # WHY: under the reversed orders the last poll party (Annad) is the coordinate party, not the first, so every present and absent set changes.
  expect_match(res, "order reversed", all = FALSE)
  expect_true(paste("coordinate party:", POLL_PARTIES[length(POLL_PARTIES)]) %in% res, label = shown(res))
  expect_false(paste("coordinate party:", POLL_PARTIES[1]) %in% res)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(unlist(x[1, c("E", "F", "order", "verdict")], use.names = FALSE), c("2024-11-30", "2024-11-29", "reversed", "PASS"))
})

# Step v2s-p2, delta held centred at the closed elections (delta_form = 1): the same model as
# v2s-m4's non-centred delta walk.
test_that("v2s-p2: the centred delta is the delta walk on 2024 eve", {
  skip_unless_slow()
  skip_unless_step("v2s-p2")
  skip_without_snapshot()
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P2_CONFIG), "--form=delta", "--cell=2024-11-30,2024-11-29", "--n=10", "--n-off=5", paste0("--out=", out)
  ))
  expect_null(attr(res, "status"), label = shown(res))
  expect_match(res, "^EQUIV PASS", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(nrow(x), 1L)
  expect_identical(unlist(x[1, c("E", "F", "order", "verdict", "form")], use.names = FALSE), c("2024-11-30", "2024-11-29", "default", "PASS", "delta"))
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_prior", "n_rejected", "n_outside", "n_one_sided")], use.names = FALSE), c("10", "5", "0", "0", "0", "0"))
  # WHY: 2024 eve has absent parties at its first two closed elections and entrants at its second and third, so each term varies and its negative control is decisive.
  expect_identical(unlist(x[1, paste0("control_", c("ref_delta", "ref_unused", "new_delta_absent"))], use.names = FALSE), rep("fails", 3))
  expect_identical(unlist(x[1, paste0("control_", c("ref_walk", "new_anchor", "new_bridge", "new_forward"))], use.names = FALSE), rep("", 4))
})

test_that("v2s-p2: the check fails when the present block's Kronecker factors are swapped", {
  skip_unless_slow()
  skip_unless_step("v2s-p2")
  skip_without_snapshot()
  # The present block's conditional covariance (L_PrPr L_PrPr') x Psi on the column-major
  # to_vector(D_Pr) has the factor kron(L_PrPr, L_Psi); swapped, it is a valid density of the
  # same size, but not the delta walk's, so the constant moves between points.
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  factor <- "kron(L_PrPr, L_Psi[e])"
  hit <- grepl(factor, stan, fixed = TRUE)
  expect_identical(sum(hit), 1L)
  mutant <- file.path(withr::local_tempdir("v2-equivalence-mutant-"), "forecast_v2.stan")
  writeLines(sub(factor, "kron(L_Psi[e], L_PrPr)", stan, fixed = TRUE), mutant)
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P2_CONFIG), "--form=delta", "--cell=2024-11-30,2024-11-29", "--n=10", "--n-off=5",
    paste0("--out=", out), paste0("--stan=", mutant)
  ))
  expect_identical(attr(res, "status"), 1L, label = shown(res))
  expect_match(res, "^EQUIV FAIL", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(x$verdict, "FAIL")
  # WHY: the FAIL must come from the density check alone, not an error: the path, the generated quantities and the points are as asked.
  expect_match(x$reasons, "^constant range [^;]* is not below tol [^;]*$")
  expect_lt(as.numeric(x$path_err), 1e-8)
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_rejected", "n_outside", "n_one_sided")], use.names = FALSE), c("10", "5", "0", "0", "0"))
})
