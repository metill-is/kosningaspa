FIT_SCRIPT <- "R/fit_polling_and_fundamentals_kjordaemi_model.R"

# The fit script is interactive, so run only its top-level expressions from the first
# assignment to n_pred_thjodskra through the last, given a kjordaemi factor with `levels`.
n_pred_for <- function(levels) {
  withr::local_package("dplyr")
  exprs <- parse(root(FIT_SCRIPT), encoding = "UTF-8", keep.source = FALSE)
  assigns <- which(vapply(exprs, \(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("n_pred_thjodskra"))
  }, logical(1)))
  env <- new.env()
  env$constituency_data <- tibble::tibble(kjordaemi = factor(levels, levels = levels))
  env$stan_data <- list(n_pred_k = seq_along(levels))
  for (e in exprs[min(assigns):max(assigns)]) eval(e, env)
  stats::setNames(env$n_pred_thjodskra, levels)
}

# Kjörskrá × expected turnout for 2024, by name.
EXPECTED <- c(
  "Reykjavík Norður" = 37418L, "Reykjavík Suður" = 37622L, "Suðvestur" = 64190L,
  "Norðvestur" = 18325L, "Norðaustur" = 25141L, "Suður" = 32303L
)

test_that("each kjördæmi gets its own expected valid votes in production's level order", {
  production <- c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suður")
  expect_identical(n_pred_for(production), EXPECTED[production])
})

test_that("the expected valid votes follow the levels by name, whatever their order", {
  shuffled <- c("Suður", "Norðvestur", "Reykjavík Suður", "Suðvestur", "Norðaustur", "Reykjavík Norður")
  expect_identical(n_pred_for(shuffled), EXPECTED[shuffled])
})

test_that("a kjördæmi level the table does not name stops the script", {
  expect_error(
    n_pred_for(c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suðurland")),
    "constituency_data's kjordaemi levels"
  )
})
