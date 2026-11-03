FIT_SCRIPTS <- c(
  fit_script = "R/fit_polling_and_fundamentals_kjordaemi_model.R",
  fit_headless = "R/sampler_check/fit_headless.R"
)

is_assign <- function(e, targets) {
  is.call(e) && identical(e[[1]], as.name("<-")) && paste(deparse(e[[2]]), collapse = "") %in% targets
}

# The statements that build stan_data: the fit script's top level, build_stan_data()'s body
# in fit_headless.R.
stan_data_statements <- function(path) {
  exprs <- as.list(parse(root(path), encoding = "UTF-8", keep.source = FALSE))
  builder <- Find(\(e) is_assign(e, "build_stan_data"), exprs)
  if (is.null(builder)) exprs else as.list(builder[[3]][[3]])[-1]
}

# Both scripts are interactive or need live data, so run only their box::use() calls and
# the statements from the first assignment to one of `from` through the last to one of `to`,
# in an environment holding `...`.
run_block <- function(path, from, to, ...) {
  withr::local_package("dplyr")
  exprs <- stan_data_statements(path)
  starts <- which(vapply(exprs, is_assign, logical(1), from))
  ends <- which(vapply(exprs, is_assign, logical(1), to))
  uses <- Filter(\(e) is.call(e) && identical(e[[1]], quote(box::use)), exprs)
  env <- list2env(list(...))
  for (e in c(uses, exprs[min(starts):max(ends)])) eval(e, env)
  env
}

# A script's stan_data$n_pred_k, given a kjordaemi factor with `levels`.
n_pred_for <- function(path, levels) {
  env <- run_block(
    path,
    from = c("n_pred_thjodskra", "kjorsokn", "kjorskra", "stan_data$n_pred_k"),
    to = "stan_data$n_pred_k",
    constituency_data = tibble::tibble(kjordaemi = factor(levels, levels = levels)),
    stan_data = list(n_pred_k = seq_along(levels))
  )
  stats::setNames(env$stan_data$n_pred_k, levels)
}

# Kjörskrá × expected turnout for 2024, by name.
EXPECTED <- c(
  "Reykjavík Norður" = 37418L, "Reykjavík Suður" = 37622L, "Suðvestur" = 64190L,
  "Norðvestur" = 18325L, "Norðaustur" = 25141L, "Suður" = 32303L
)
PRODUCTION <- c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suður")
SHUFFLED <- c("Suður", "Norðvestur", "Reykjavík Suður", "Suðvestur", "Norðaustur", "Reykjavík Norður")
UNKNOWN <- c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suðurland")

for (script in names(FIT_SCRIPTS)) {
  path <- FIT_SCRIPTS[[script]]

  test_that(paste(script, "gives each kjördæmi its own expected valid votes in production's level order"), {
    expect_identical(n_pred_for(path, PRODUCTION), EXPECTED[PRODUCTION])
  })

  test_that(paste(script, "matches the expected valid votes to the levels by name, whatever their order"), {
    expect_identical(n_pred_for(path, SHUFFLED), EXPECTED[SHUFFLED])
  })

  test_that(paste(script, "stops on a kjördæmi level the table does not name"), {
    expect_error(n_pred_for(path, UNKNOWN), "kjordaemi levels")
  })
}

test_that("n_pred_k_2024() reads the 2024 table by name, and stops on a level it does not name", {
  box::use(R / stan_data[n_pred_k_2024])
  expect_identical(stats::setNames(n_pred_k_2024(SHUFFLED), SHUFFLED), EXPECTED[SHUFFLED])
  expect_error(n_pred_k_2024(UNKNOWN), "kjordaemi levels")
})
