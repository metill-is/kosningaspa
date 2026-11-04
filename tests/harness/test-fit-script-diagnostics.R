FIT_STAN <- "Stan/polling_and_fundamentals_kjordaemi.stan"

# The fit script's code without comments, one deparsed line per element.
fit_script_code <- function() {
  exprs <- parse(root(FIT_SCRIPTS[["fit_script"]]), encoding = "UTF-8", keep.source = FALSE)
  unlist(lapply(exprs, deparse, width.cutoff = 500L))
}

test_that("the fit script reads only variables the model writes", {
  cmdstan <- tryCatch(suppressWarnings(cmdstanr::cmdstan_path()), error = \(e) "")
  if (!nzchar(cmdstan)) skip("no CmdStan: stanc lists the model's variables")
  info <- cmdstanr::cmdstan_model(root(FIT_STAN), compile = FALSE)$variables()
  written <- unlist(lapply(info[c("parameters", "transformed_parameters", "generated_quantities")], names))
  calls <- unlist(regmatches(
    fit_script_code(),
    gregexpr("fit\\$(summary|draws)\\((c\\([^)]*\\)|\"[^\"]*\")", fit_script_code())
  ))
  read <- unique(unlist(regmatches(calls, gregexpr("(?<=\")[A-Za-z_][A-Za-z0-9_]*(?=\")", calls, perl = TRUE))))
  expect_gt(length(read), 10)
  expect_equal(setdiff(read, written), character(0))
})

test_that("the fit script's house-effect plot gives the industry bias a row for each party but Annað", {
  for (pkg in c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "ggplot2", "scales", "posterior")) {
    withr::local_package(pkg)
  }
  parties <- c("Other", "A", "B", "C")
  P <- length(parties)
  H <- 3
  withr::local_seed(1)
  # WHY: the plot's ggh4x::guide_axis_truncated() is deprecated; that is not this test's subject.
  withr::local_options(lifecycle_verbosity = "quiet")
  cells <- expand.grid(p = seq_len(P - 1), h = seq_len(H))
  draws <- cbind(
    matrix(rnorm(20 * nrow(cells)), 20, dimnames = list(NULL, sprintf("gamma[%d,%d]", cells$p, cells$h))),
    matrix(rnorm(20 * (P - 2)), 20, dimnames = list(NULL, sprintf("mu_gamma[%d]", seq_len(P - 2))))
  ) |>
    posterior::as_draws_df()
  fit <- list(
    summary = \(v) posterior::summarise_draws(posterior::subset_draws(draws, variable = v), "mean", posterior::quantile2),
    draws = \(v) posterior::subset_draws(draws, variable = v)
  )
  block <- Find(
    \(e) any(grepl("fit$summary(\"gamma\")", deparse(e, width.cutoff = 500L), fixed = TRUE)),
    parse(root(FIT_SCRIPTS[["fit_script"]]), encoding = "UTF-8", keep.source = FALSE)
  )
  env <- list2env(list(
    fit = fit,
    stan_data = list(y_n = matrix(0L, 1, P, dimnames = list(NULL, parties)), P = P),
    polling_data = tibble::tibble(fyrirtaeki = factor(c("Kosning", "Gallup", "Maskina")))
  ))
  eval(block, env)
  samtals <- dplyr::filter(env$p$data, fyrirtaeki == "Samtals")
  mu <- posterior::as_draws_matrix(posterior::subset_draws(draws, variable = "mu_gamma"))
  expect_identical(as.character(samtals$flokkur), parties[-1])
  expect_equal(samtals$mean, c(colMeans(mu), mean(-rowSums(mu))), ignore_attr = TRUE)
})
