# Kjörskrá × expected turnout for 2024, by name.
EXPECTED <- c(
  "Reykjavík Norður" = 37418L, "Reykjavík Suður" = 37622L, "Suðvestur" = 64190L,
  "Norðvestur" = 18325L, "Norðaustur" = 25141L, "Suður" = 32303L
)
PRODUCTION <- c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suður")
SHUFFLED <- c("Suður", "Norðvestur", "Reykjavík Suður", "Suðvestur", "Norðaustur", "Reykjavík Norður")
UNKNOWN <- c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suðurland")

test_that("n_pred_k_2024() reads the 2024 table by name, and stops on a level it does not name", {
  box::use(R / stan_data[n_pred_k_2024])
  expect_identical(stats::setNames(n_pred_k_2024(PRODUCTION), PRODUCTION), EXPECTED[PRODUCTION])
  expect_identical(stats::setNames(n_pred_k_2024(SHUFFLED), SHUFFLED), EXPECTED[SHUFFLED])
  expect_error(n_pred_k_2024(UNKNOWN), "kjordaemi levels")
})
