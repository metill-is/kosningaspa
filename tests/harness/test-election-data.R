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

# A data/polling_data.csv as update_polling_data() wrote it before the fix: the 30 Kosning
# rows' shares times the three totals recycled row by row, among a few poll rows. A row
# the recycling gave its own total holds the very double get_election_data() gives now.
write_old_csv <- function(path) {
  kos <- get_election_data()
  total <- unname(c("2021-09-25" = 199730, "2017-10-28" = 196259, "2016-10-29" = 189648)[as.character(kos$date)])
  given <- rep_len(c(199730, 196259, 189648), nrow(kos))
  kos$n <- ifelse(given == total, kos$n, kos$n / total * given)
  polls <- tibble::tribble(
    ~date, ~fyrirtaeki, ~flokkur, ~n, ~p, ~lokadagur,
    "2016-10-20", "Gallup", "Píratar", 141.3, 0.1413, "2016-10-19",
    "2017-10-27", "Félagsvísindastofnun", "Miðflokkurinn", 1 / 3, 0.0997, "2017-10-26",
    "2021-10-15", "Maskína", "Annað", 23.456789012345, NA, NA
  ) |>
    dplyr::mutate(date = as.Date(date), lokadagur = as.Date(lokadagur))
  dplyr::bind_rows(dplyr::mutate(kos, p = NA_real_, lokadagur = as.Date(NA)), polls) |>
    dplyr::select(date, fyrirtaeki, flokkur, n, p, lokadagur) |>
    dplyr::arrange(date, fyrirtaeki != "Kosning", flokkur) |>
    readr::write_csv(path)
}

run_correction <- function(path) {
  out <- withr::with_dir(root(), system2(
    file.path(R.home("bin"), "Rscript"), c("R/correct_election_anchors.R", shQuote(path)),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  ))
  # WHY: report the script's own error, not a later assertion on its output
  expect_null(attr(out, "status"), label = paste(out, collapse = "\n"))
  out
}

test_that("correct_election_anchors.R gives each Kosning row its own total and leaves every other line alone", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_old_csv(path)
  before <- readLines(path, encoding = "UTF-8")
  run_correction(path)
  after <- readLines(path, encoding = "UTF-8")
  kos <- grepl("^[^,]*,Kosning,", before)
  expect_length(after, length(before))
  expect_identical(after[!kos], before[!kos])
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki == "Kosning") |>
    dplyr::select(date, flokkur, n) |>
    expect_election_anchors()
})

test_that("correct_election_anchors.R writes get_election_data()'s n, as write_csv() prints it", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_old_csv(path)
  run_correction(path)
  rows <- grep("^[^,]*,Kosning,", readLines(path, encoding = "UTF-8"), value = TRUE)
  fields <- strsplit(rows, ",", fixed = TRUE)
  written <- stats::setNames(vapply(fields, `[`, "", 4), vapply(fields, \(f) paste(f[1], f[3]), ""))
  want <- get_election_data()
  printed <- strsplit(readr::format_csv(data.frame(n = want$n), col_names = FALSE), "\n", fixed = TRUE)[[1]]
  printed <- stats::setNames(printed, paste(want$date, want$flokkur))
  expect_identical(written, printed[names(written)])
})

test_that("a second run of correct_election_anchors.R changes nothing", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_old_csv(path)
  run_correction(path)
  once <- readBin(path, "raw", file.size(path))
  inode <- fs::file_info(path)$inode
  out <- run_correction(path)
  expect_match(out, "corrected 0 of 30 Kosning rows", all = FALSE)
  expect_identical(readBin(path, "raw", file.size(path)), once)
  # nothing to correct: the file is left in place, not replaced by a rename
  expect_identical(fs::file_info(path)$inode, inode)
})

test_that("data/polling_data.csv's Kosning rows carry each election's own total", {
  skip_unless_live_data()
  path <- root("data", "polling_data.csv")
  if (!file.exists(path)) skip("no data/polling_data.csv")
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki == "Kosning") |>
    dplyr::select(date, flokkur, n) |>
    expect_election_anchors()
})
