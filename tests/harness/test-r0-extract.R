box::use(
  R / harness / r0[r0_extract, r0_stan_data, read_r0_config, R0_PARAM_VARIABLES, R0_EXTRACT_VARIABLES],
  R / harness / arms[ARMS, arm]
)

K <- 2L
P <- 3L
EDAY <- 5L
N_PRED <- 1000
META <- list(
  party_order = c("Annað", "X", "Y"),
  kjordaemi_levels = c("Suður", "Norðvestur"),
  eday_index = EDAY,
  n_pred = N_PRED
)
SCALARS <- c(
  "mu_log_sigma", "tau_log_sigma", "tau_stjornarslit", "mu_phi", "sigma_phi", "beta_lag_f",
  "beta_inc_years_f", "beta_vnv_f", "beta_growth_f", "phi_f_inv", "tau_f", "mu_gamma_weight"
)
MATRICES <- c("L_Omega", "delta_pred")
DRAW <- seq_len(16)

# Element names of a parameter in Stan's (column-major) order.
elements <- function(v) {
  if (v %in% SCALARS) v else if (v %in% MATRICES) paste0(v, c("[1,1]", "[2,1]", "[1,2]")) else paste0(v, c("[1]", "[2]"))
}
PARAM_COLUMNS <- unlist(lapply(R0_PARAM_VARIABLES, elements))

# A draws_df of 4 chains x 4 iterations: y_pred_constituency[k,p] = 1000k + p in every draw;
# rows EDAY - 1 and EDAY of y_rep_national; pi_eday; and each parameter element, each
# varying by draw. The columns come in reverse Stan order, so a label taken by position is
# wrong.
synthetic_draws <- function(drop = character(0), change = list()) {
  cols <- list()
  for (p in seq_len(P)) for (k in seq_len(K)) cols[[sprintf("y_pred_constituency[%d,%d]", k, p)]] <- rep(1000 * k + p, 16)
  for (p in seq_len(P)) for (d in c(EDAY - 1L, EDAY)) cols[[sprintf("y_rep_national[%d,%d]", d, p)]] <- 100 * d + 10 * DRAW + p
  for (p in seq_len(P)) cols[[sprintf("pi_eday[%d]", p)]] <- (DRAW + p) / 100
  for (i in seq_along(PARAM_COLUMNS)) cols[[PARAM_COLUMNS[i]]] <- i + DRAW / 1000
  cols[names(change)] <- change
  cols <- rev(cols[setdiff(names(cols), drop)])
  x <- array(unlist(cols, use.names = FALSE), dim = c(4, 4, length(cols)), dimnames = list(NULL, NULL, names(cols)))
  posterior::as_draws_df(posterior::as_draws_array(x))
}

extracted <- function(draws = synthetic_draws(), meta = META, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  paths <- r0_extract(draws, meta, dir)
  list(dir = dir, paths = paths, read = lapply(paths, arrow::read_parquet))
}

classes <- function(df) vapply(df, \(x) class(x)[1], character(1))

test_that("R0_PARAM_VARIABLES is the plan's list and the extraction reads it with the forecast variables", {
  expect_identical(R0_PARAM_VARIABLES, c(
    "mu_gamma", "sigma_gamma", "sigma_delta", "mu_log_sigma", "tau_log_sigma", "tau_stjornarslit",
    "log_phi", "mu_phi", "sigma_phi", "beta_lag_f", "beta_inc_years_f", "beta_vnv_f", "beta_growth_f",
    "phi_f_inv", "L_Omega", "sigma", "tau_f", "mu_pred", "mu_gamma_weight", "mu_gamma_pred", "delta_pred"
  ))
  expect_identical(R0_EXTRACT_VARIABLES, c("y_pred_constituency", "y_rep_national", "pi_eday", R0_PARAM_VARIABLES))
})

test_that("r0_extract labels kjördæmi and parties by the Stan indices, never by column position", {
  x <- extracted()
  kd <- x$read$eday_kjordaemi
  expect_equal(nrow(kd), 16 * K * P)
  ny <- kd[kd$kjordaemi == "Norðvestur" & kd$flokkur == "Y", ]
  expect_identical(sort(ny$.draw), seq_len(16L))
  expect_true(all(ny$votes == 2003L))
  k <- match(kd$kjordaemi, META$kjordaemi_levels)
  p <- match(kd$flokkur, META$party_order)
  expect_false(anyNA(k) || anyNA(p))
  expect_identical(kd$votes, as.integer(1000 * k + p))
  expect_identical(nrow(dplyr::distinct(kd, .draw, kjordaemi, flokkur)), nrow(kd))
})

test_that("the four parquets have exactly the listed columns and types, and .draw runs 1..16", {
  x <- extracted()
  expect_named(x$paths, c("eday_kjordaemi", "eday_national", "latent_national", "param_draws"))
  expect_identical(unname(x$paths), file.path(x$dir, paste0(names(x$paths), ".parquet")))
  ids <- c(.chain = "integer", .iteration = "integer", .draw = "integer")
  expect_identical(classes(x$read$eday_kjordaemi), c(ids, kjordaemi = "character", flokkur = "character", votes = "integer"))
  expect_identical(classes(x$read$eday_national), c(ids, flokkur = "character", share = "numeric"))
  expect_identical(classes(x$read$latent_national), c(ids, flokkur = "character", share = "numeric"))
  expect_identical(names(x$read$param_draws), c(names(ids), PARAM_COLUMNS))
  expect_identical(classes(x$read$param_draws), c(ids, stats::setNames(rep("numeric", length(PARAM_COLUMNS)), PARAM_COLUMNS)))
  for (f in names(x$read)) {
    expect_identical(sort(unique(x$read[[f]]$.draw)), seq_len(16L), label = f)
    expect_identical(sort(unique(x$read[[f]]$.chain)), seq_len(4L), label = f)
    expect_identical(sort(unique(x$read[[f]]$.iteration)), seq_len(4L), label = f)
  }
})

test_that("eday_national is y_rep_national[eday_index, p] / n_pred and latent_national is pi_eday", {
  x <- extracted()
  nat <- x$read$eday_national
  expect_equal(nrow(nat), 16 * P)
  p <- match(nat$flokkur, META$party_order)
  expect_false(anyNA(p))
  expect_identical(nat$share, (100 * EDAY + 10 * nat$.draw + p) / N_PRED)
  lat <- x$read$latent_national
  expect_equal(nrow(lat), 16 * P)
  expect_identical(lat$share, (lat$.draw + match(lat$flokkur, META$party_order)) / 100)
  expect_identical(nat$.chain, as.integer((nat$.draw - 1L) %/% 4L + 1L))
  expect_identical(nat$.iteration, as.integer((nat$.draw - 1L) %% 4L + 1L))
})

test_that("param_draws holds every element of R0_PARAM_VARIABLES, in Stan order", {
  x <- extracted()
  pd <- x$read$param_draws
  expect_identical(pd$.draw, seq_len(16L))
  for (i in seq_along(PARAM_COLUMNS)) expect_identical(pd[[PARAM_COLUMNS[i]]], i + DRAW / 1000, label = PARAM_COLUMNS[i])
})

test_that("r0_extract stops on a missing cell, a non-whole count, a missing eday row or a missing parameter", {
  dir <- withr::local_tempdir()
  expect_error(r0_extract(synthetic_draws(drop = "y_pred_constituency[2,1]"), META, dir), "y_pred_constituency")
  expect_error(r0_extract(synthetic_draws(change = list(`y_pred_constituency[1,2]` = rep(1.5, 16))), META, dir), "whole")
  expect_error(r0_extract(synthetic_draws(drop = sprintf("y_rep_national[%d,2]", EDAY)), META, dir), "y_rep_national")
  expect_error(r0_extract(synthetic_draws(drop = "tau_f"), META, dir), "tau_f")
  expect_error(r0_extract(synthetic_draws(), META[c("party_order", "kjordaemi_levels", "n_pred")], dir), "eday_index")
  expect_length(list.files(dir), 0)
})

test_that("arm() returns an arm of ARMS and refuses an unknown one", {
  expect_named(ARMS, "r0")
  a <- arm("r0")
  expect_identical(a, ARMS$r0)
  expect_identical(a$stan_file, "Stan/polling_and_fundamentals_kjordaemi_r0.stan")
  expect_identical(a$config, "harness/configs/r0.json")
  expect_true(file.exists(root(a$stan_file)))
  expect_true(file.exists(root(a$config)))
  expect_identical(a$build, r0_stan_data)
  expect_identical(a$read_config, read_r0_config)
  expect_identical(a$extract, r0_extract)
  expect_identical(a$variables, R0_EXTRACT_VARIABLES)
  expect_error(arm("r1"), "unknown arm: r1")
  expect_error(arm(c("r0", "r0")), "unknown arm")
})
