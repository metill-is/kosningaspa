box::use(
  R / harness / truth[build_truth, cell_truth, fold_truth, prev_national_shares, read_seats_2021, read_truth],
  R / harness / inputs[snapshot_file],
  R / election_utils[calculate_seats, layout_for]
)
tr <- function() build_truth()

test_that("totals match the official valid votes and 63 seats", {
  skip_without_snapshot()
  t <- tr()
  tot <- t |> dplyr::summarise(v = sum(votes), s = sum(seats), .by = election)
  expect_equal(tot$v, c(189648, 196259, 199730, 212470))
  expect_equal(tot$s, rep(63, 4))
})

test_that("Hagstofa sheets equal the constituency Kosning rows cell by cell", {
  skip_without_snapshot()
  kos <- readr::read_csv(snapshot_file("constituency_data"), show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki == "Kosning") |>
    dplyr::transmute(election = date, kjordaemi, flokkur, n)
  j <- dplyr::full_join(
    fold_truth(tr()) |> dplyr::filter(election < as.Date("2024-01-01")), kos,
    by = c("election", "kjordaemi", "flokkur")
  )
  expect_equal(nrow(j), 180)
  expect_equal(j$votes, j$n)
})

test_that("2024 equals official_2024_kjordaemi.csv", {
  skip_without_snapshot()
  o <- readr::read_csv(root("harness", "sources", "hagstofa", "official_2024_kjordaemi.csv"), show_col_types = FALSE)
  j <- dplyr::inner_join(
    fold_truth(tr()) |> dplyr::filter(election == as.Date("2024-11-30")), o,
    by = c("kjordaemi", "flokkur"), suffix = c("", "_o")
  )
  expect_equal(nrow(j), 60)
  expect_equal(j$votes, j$votes_o)
  expect_equal(j$seats, j$seats_official)
})

test_that("the allocator reproduces every official seat, 2016-2024, on unfolded lists", {
  skip_without_snapshot()
  t <- tr()
  # WHY: for() drops the Date class, so iterate over ISO strings
  for (e in as.character(unique(t$election))) {
    te <- t[t$election == as.Date(e), ]
    s <- calculate_seats(dplyr::transmute(te, flokkur = list_letter, kjordaemi, votes), layout_for(e))
    got <- dplyr::inner_join(s, dplyr::transmute(te, flokkur = list_letter, kjordaemi, official = seats),
      by = c("flokkur", "kjordaemi")
    )
    expect_equal(got$seats, got$official, label = paste("seats", e))
    tot <- dplyr::summarise(te, s = sum(seats), .by = kjordaemi) |> dplyr::arrange(kjordaemi)
    lay <- dplyr::mutate(layout_for(e), s = n_seats + n_jofnun) |> dplyr::arrange(kjordaemi)
    expect_equal(tot$s, lay$s, label = paste("kjördæmi totals", e)) # 2016/17 use the 2013–2021 layout
  }
  expect_equal(sum(t$seats[t$election == as.Date("2016-10-29") & t$list_letter == "A"]), 4)
})

test_that("2021 adjustment seats land where Alþingi's results page places them", {
  skip_without_snapshot()
  te <- dplyr::filter(tr(), election == as.Date("2021-09-25"))
  s <- calculate_seats(dplyr::transmute(te, flokkur = list_letter, kjordaemi, votes), layout_for("2021-09-25"))
  off <- read_seats_2021()
  got <- dplyr::left_join(
    dplyr::transmute(s, list_letter = flokkur, kjordaemi, seats, adjustment = seats - kjordaemi_seats),
    dplyr::select(off, list_letter, kjordaemi, official = seats, adjustment_seats),
    by = c("list_letter", "kjordaemi")
  ) |>
    tidyr::replace_na(list(official = 0L, adjustment_seats = 0L))
  expect_equal(got$seats, got$official)
  expect_equal(got$adjustment, got$adjustment_seats)
  # the page's own national totals and adjustment-seat summary
  expect_equal(c(tapply(off$seats, off$list_letter, sum)), c(B = 13, C = 5, D = 16, F = 6, M = 3, P = 6, S = 6, V = 8))
  expect_equal(c(tapply(off$adjustment_seats, off$list_letter, sum)), c(B = 0, C = 2, D = 0, F = 0, M = 1, P = 3, S = 1, V = 2))
})

test_that("lists that did not run keep a zero row", {
  skip_without_snapshot()
  t <- tr()
  expect_true(all(t$votes[!t$ran] == 0))
  expect_true(any(!t$ran))
  expect_equal(nrow(dplyr::count(t, election, list_letter) |> dplyr::filter(n != 6)), 0)
})

test_that("previous national shares feed the shock scaling", {
  skip_without_snapshot()
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  p16 <- prev_national_shares(as.Date("2016-10-29"), tr(), f)
  expect_equal(unname(p16["Annað"]), (11.9 + 8.3) / 100.2)
  expect_equal(unname(p16["Sjálfstæðisflokkurinn"]), 26.7 / 100.2)
  expect_true(all(is.na(p16[c("Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn")])))
  expect_false(is.na(prev_national_shares(as.Date("2024-11-30"), tr(), f)["Sósíalistaflokkurinn"]))
  # R2: no Sósíalistar list in 2017, so NA although the folded rows carry a 0
  expect_true(is.na(prev_national_shares(as.Date("2021-09-25"), tr(), f)["Sósíalistaflokkurinn"]))
})

test_that("read_truth reads the committed table with its column types", {
  t <- read_truth()
  expect_named(t, c("election", "kjordaemi", "list_letter", "list_name", "flokkur", "votes", "seats", "ran", "source"))
  expect_s3_class(t$election, "Date")
  expect_type(t$votes, "integer")
  expect_type(t$seats, "integer")
  expect_type(t$ran, "logical")
  expect_equal(sort(unique(t$election)), ELECTIONS)
  expect_false(anyNA(t))
})

test_that("read_truth returns what build_truth builds", {
  skip_without_snapshot()
  expect_identical(read_truth(), tr())
})

test_that("cell_truth gives the national shares, the kjördæmi shares and the seats at E", {
  ct <- cell_truth("2024-11-30", POLL_PARTIES)
  expect_named(ct, c("nat", "kd", "seats"))
  expect_named(ct$nat, POLL_PARTIES)
  expect_equal(sum(ct$nat), 1)
  expect_named(ct$seats, POLL_PARTIES)
  expect_type(ct$seats, "integer")
  expect_equal(sum(ct$seats), 63L)
  expect_named(ct$kd, c("kjordaemi", "flokkur", "votes", "share"))
  expect_equal(nrow(ct$kd), 60)
  expect_equal(sum(ct$kd$votes), 212470)
  expect_equal(unname(c(tapply(ct$kd$share, ct$kd$kjordaemi, sum))), rep(1, 6))
  # WHY: a fold of KOS02018 made before the harness, so it checks cell_truth()'s own fold (harness/truth/README.md)
  o <- readr::read_csv(root("harness", "sources", "hagstofa", "official_2024_kjordaemi.csv"), show_col_types = FALSE)
  v <- tapply(o$votes, o$flokkur, sum)
  expect_equal(ct$nat, c(v[POLL_PARTIES] / sum(v)), ignore_attr = "dimnames")
  expect_equal(unname(ct$seats), unname(c(tapply(o$seats_official, o$flokkur, sum))[POLL_PARTIES]))
  j <- dplyr::inner_join(ct$kd, o, by = c("kjordaemi", "flokkur"), suffix = c("", "_o"))
  expect_equal(nrow(j), 60)
  expect_equal(j$votes, j$votes_o)
})

test_that("cell_truth names its vectors in the run's party order", {
  ct <- cell_truth("2024-11-30", POLL_PARTIES)
  r <- cell_truth("2024-11-30", rev(POLL_PARTIES))
  expect_identical(r$nat, ct$nat[rev(POLL_PARTIES)])
  expect_identical(r$seats, ct$seats[rev(POLL_PARTIES)])
})

test_that("cell_truth refuses a result with votes for a party the run does not forecast", {
  expect_error(
    cell_truth("2024-11-30", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn")),
    "truth has votes for parties the run does not forecast: Sósíalistaflokkurinn",
    fixed = TRUE
  )
  expect_error(cell_truth("2024-11-30", c(POLL_PARTIES, "Björt framtíð")), "not poll parties: Björt framtíð")
  expect_error(cell_truth("2013-04-27", POLL_PARTIES), "no results for 2013-04-27")
})

test_that("a nine-party run is scored in 2017, when Sósíalistaflokkurinn had no list", {
  ct <- cell_truth("2017-10-28", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))
  expect_length(ct$nat, 9)
  expect_equal(sum(ct$nat), 1)
  expect_equal(sum(ct$seats), 63L)
  expect_equal(nrow(ct$kd), 54)
  expect_equal(sum(ct$kd$votes), 196259)
})
