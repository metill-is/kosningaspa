box::use(
  R / data[get_election_data],
  R / harness / truth[read_truth]
)

# Each election's official valid votes, from the committed Hagstofa results.
official_totals <- function() {
  read_truth() |>
    dplyr::filter(election < as.Date("2024-01-01")) |>
    dplyr::summarise(total = sum(votes), .by = election) |>
    dplyr::rename(date = election)
}

# The national shares get_election_data() hard-codes, Annað as the remainder.
hard_coded_shares <- function() {
  shares <- tibble::tribble(
    ~flokkur, ~`2021-09-25`, ~`2017-10-28`, ~`2016-10-29`,
    "Samfylkingin", 0.0993, 0.1205, 0.057,
    "Sjálfstæðisflokkurinn", 0.2439, 0.2525, 0.29,
    "Miðflokkurinn", 0.0545, 0.1087, 0,
    "Framsóknarflokkurinn", 0.1727, 0.1071, 0.115,
    "Vinstri Græn", 0.1257, 0.1689, 0.159,
    "Flokkur Fólksins", 0.0885, 0.0688, 0.0354,
    "Viðreisn", 0.0833, 0.0669, 0.105,
    "Píratar", 0.0863, 0.092, 0.145,
    "Sósíalistaflokkurinn", 0.041, 0, 0
  ) |>
    tidyr::pivot_longer(-flokkur, names_to = "date", values_to = "p") |>
    dplyr::mutate(date = as.Date(date))
  dplyr::bind_rows(shares, dplyr::summarise(shares, flokkur = "Annað", p = 1 - sum(p), .by = date))
}

expect_election_anchors <- function(kos) {
  j <- kos |>
    dplyr::inner_join(official_totals(), by = "date") |>
    dplyr::inner_join(hard_coded_shares(), by = c("date", "flokkur"))
  expect_equal(nrow(j), 30)
  sums <- dplyr::summarise(j, n = sum(n), total = dplyr::first(total), .by = date)
  expect_lte(max(abs(sums$n - sums$total)), 1)
  rows <- paste(j$date, j$flokkur)
  expect_equal(stats::setNames(j$n / j$total, rows), stats::setNames(j$p, rows))
}

test_that("get_election_data() gives each election its own valid-vote total", {
  expect_election_anchors(dplyr::select(get_election_data(), date, flokkur, n))
})

test_that("data/polling_data.csv's Kosning rows carry each election's own total", {
  path <- root("data", "polling_data.csv")
  if (!file.exists(path)) skip("no data/polling_data.csv")
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki == "Kosning") |>
    dplyr::select(date, flokkur, n) |>
    expect_election_anchors()
})
