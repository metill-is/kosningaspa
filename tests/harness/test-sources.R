wk <- function(y) {
  readr::read_csv(root("harness", "sources", sprintf("wikipedia_%d_poll_fieldwork.csv", y)),
    show_col_types = FALSE
  )
}
test_that("fieldwork tables share one shape and have ordered dates", {
  for (y in c(2017, 2021, 2024)) {
    w <- wk(y)
    expect_named(w, c("fieldwork", "house", "start", "end", "note", "n", "D", "S", "B", "V", "P", "C", "F", "M", "J"))
    expect_true(all(w$start <= w$end, na.rm = TRUE))
    expect_false(anyNA(w$end))
  }
  expect_lte(min(wk(2017)$end), as.Date("2017-08-15"))
  expect_lte(min(wk(2021)$end), as.Date("2021-01-28"))
  expect_lte(min(wk(2024)$end), as.Date("2024-04-04"))
})
test_that("every Wikipedia house is mapped", {
  m <- readr::read_csv(root("harness", "sources", "wikipedia_house_map.csv"), show_col_types = FALSE)
  houses <- unique(unlist(lapply(c(2017, 2021, 2024), \(y) wk(y)$house)))
  expect_true(all(houses %in% m$wikipedia_house))
})
test_that("corrections name a fieldwork end after the midpoint", {
  cr <- readr::read_csv(root("harness", "sources", "fieldwork_corrections.csv"), show_col_types = FALSE)
  expect_true(all(cr$end >= cr$date))
  expect_false(anyNA(cr$source))
})
test_that("source rows and the house map name snapshot keys", {
  skip_without_snapshot()
  keys <- dplyr::bind_rows(
    national = readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE),
    kjordaemi = readr::read_csv(snapshot_file("constituency_data"), show_col_types = FALSE),
    .id = "level"
  ) |>
    dplyr::distinct(level, date, fyrirtaeki)
  for (f in c("fieldwork_corrections.csv", "poll_publication.csv")) {
    rows <- readr::read_csv(root("harness", "sources", f),
      col_types = readr::cols(date = readr::col_date(), .default = readr::col_character())
    )
    expect_equal(nrow(dplyr::anti_join(rows, keys, by = c("level", "date", "fyrirtaeki"))), 0, info = f)
  }
  m <- readr::read_csv(root("harness", "sources", "wikipedia_house_map.csv"), show_col_types = FALSE)
  expect_true(all(stats::na.omit(m$fyrirtaeki) %in% setdiff(keys$fyrirtaeki, "Kosning")))
})
