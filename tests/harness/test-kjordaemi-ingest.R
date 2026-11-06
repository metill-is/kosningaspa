box::use(
  R / stan_data[kjordaemi_full_polls],
  R / data[tidy_constituency_sheet]
)

test_that("kjordaemi_full_polls keeps a poll only if it covers every kjördæmi and has a same-house national poll", {
  polls <- tibble::tribble(
    ~date, ~fyrirtaeki, ~kjordaemi,
    "2024-01-10", "Gallup", "North",
    "2024-01-10", "Gallup", "South",
    "2024-02-10", "Gallup", "North",
    "2024-02-10", "Gallup", "South",
    "2024-03-10", "Gallup", "North",
    "2024-04-10", "Gallup", "North",
    "2024-04-10", "Gallup", "South"
  )
  kd <- tidyr::expand_grid(polls, flokkur = c("A", "B")) |>
    dplyr::mutate(date = as.Date(date), n = as.numeric(seq_along(date)))
  # 10 Jan is kept; 10 Feb has no national poll, 10 Mar covers one kjördæmi, and the
  # national poll on 10 Apr is another house's.
  nat <- tibble::tibble(
    date = as.Date(c("2024-01-10", "2024-03-10", "2024-04-10")),
    fyrirtaeki = c("Gallup", "Gallup", "Maskína")
  )
  expect_identical(kjordaemi_full_polls(kd, nat, K = 2L), kd[kd$date == as.Date("2024-01-10"), ])
  expect_equal(nrow(kjordaemi_full_polls(kd, nat)), 0)
})

test_that("tidy_constituency_sheet drops blank kjördæmi, keeps a printed 0 and relabels the sentence-case parties", {
  sheet <- tibble::tibble(
    `Ár` = 2024, `Mánuður` = 11, Dagur = 7, `Fyrirtæki` = "Gallup",
    Flokkur = c("Flokkur fólksins", "Vinstri græn", "Annað"),
    `Reykjavík Norður` = c(10.5, 0, 3), `Suður` = NA
  )
  # WHY: fct_relevel() warns about the house and party levels this small sheet lacks.
  out <- suppressWarnings(tidy_constituency_sheet(sheet))
  expect_named(out, c("date", "fyrirtaeki", "flokkur", "kjordaemi", "n"))
  expect_equal(out$date, rep(as.Date("2024-11-07"), 3))
  expect_equal(out$kjordaemi, rep("Reykjavík Norður", 3))
  expect_equal(as.character(out$flokkur), c("Flokkur Fólksins", "Vinstri Græn", "Annað"))
  expect_equal(levels(out$flokkur)[1], "Annað")
  expect_equal(out$n, c(10.5, 0, 3))
})
