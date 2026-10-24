box::use(R/harness/info_set[info_set, validate_inputs, read_inputs])
box::use(R/harness/info_set[apply_n_corrections, cell_polls, read_n_corrections])
box::use(R/harness/info_set[apply_fundamentals_corrections, read_fundamentals_corrections])
box::use(R/harness/info_set[read_economy_release])
box::use(R/harness/availability[poll_availability])
inp <- if (snapshot_present()) read_inputs()
cell <- function(E, F) info_set(as.Date(E), as.Date(F), inp)

test_that("every design cell builds and validates", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) expect_no_error(validate_inputs(info_set(TARGETS$E[i], TARGETS$F[i], inp)))
})
test_that("2017 has nine parties; Gallup 15 Nov is out on 16 Nov and in on the eve", {
  skip_without_snapshot()
  expect_false("Sósíalistaflokkurinn" %in% cell("2017-10-28", "2017-10-14")$parties)
  g <- function(x) any(x$date == as.Date("2024-11-15") & x$fyrirtaeki == "Gallup")
  a <- cell("2024-11-30", "2024-11-16"); b <- cell("2024-11-30", "2024-11-29")
  expect_false(g(a$polls_nat) || g(a$polls_kd)); expect_true(g(b$polls_nat) && g(b$polls_kd))
})
test_that("a poll available exactly on F is admitted", {
  skip_without_snapshot()
  F <- as.Date("2024-11-22")
  expect_true(any(cell("2024-11-30", F)$polls_kd$date == as.Date("2024-11-15")))
  expect_false(any(cell("2024-11-30", F - 1)$polls_kd$date == as.Date("2024-11-15")))
})
test_that("planted leaks are refused", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16")
  x <- ok; x$polls_nat$available_from[1] <- x$F + 1
  expect_error(validate_inputs(x), "poll available after F")
  x <- ok; x$results <- dplyr::bind_rows(x$results, dplyr::mutate(x$results[1, ], election = x$E))
  expect_error(validate_inputs(x), "target election result")
  x <- ok; x$fundamentals$target$voteshare[1] <- 20
  expect_error(validate_inputs(x), "target vote share")
  x <- ok; x$coalitions <- dplyr::bind_rows(x$coalitions, dplyr::mutate(x$coalitions[1, ], from = x$F + 1))
  expect_error(validate_inputs(x), "event after F")
  x <- cell("2017-10-28", "2017-10-14"); x$parties <- c(x$parties, "Sósíalistaflokkurinn")
  expect_error(validate_inputs(x), "party not yet available")
  y <- cell("2021-09-25", "2021-09-24")
  y$kosning_nat <- dplyr::bind_rows(y$kosning_nat, dplyr::mutate(y$kosning_nat[1, ], date = y$E))
  expect_error(validate_inputs(y), "target election result")
  x <- ok; x$polls_kd$date[1] <- x$E
  expect_error(validate_inputs(x), "poll dated on or after E")
  x <- ok; x$polls_nat$date[1] <- as.Date("2015-12-31")
  expect_error(validate_inputs(x), "poll before the window")
})

test_that("a target valid-vote total is refused, as are the other planted leaks", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16")
  x <- ok; x$fundamentals$target$total_valid_votes[1] <- 212470
  expect_error(validate_inputs(x), "target vote share")
  x <- ok; x$fundamentals$target$voteshare_change[1] <- 1
  expect_error(validate_inputs(x), "target vote share")
  x <- ok; x$fundamentals$train$date[1] <- x$E
  expect_error(validate_inputs(x), "target vote share")
  x <- ok; x$coalitions$to[1] <- x$F + 1
  expect_error(validate_inputs(x), "event after F")
  x <- ok; x$polls_kd$available_from[1] <- NA
  expect_error(validate_inputs(x), "poll available after F")
  x <- ok; x$kosning_nat$date[1] <- as.Date("2015-12-31")
  expect_error(validate_inputs(x), "poll before the window")
})
test_that("a leak message names the offending rows", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16"); eve <- cell("2024-11-30", "2024-11-29")
  gallup <- eve$polls_nat[eve$polls_nat$date == as.Date("2024-11-15") & eve$polls_nat$fyrirtaeki == "Gallup", ]
  x <- ok; x$polls_nat <- dplyr::bind_rows(x$polls_nat, gallup)
  expect_error(validate_inputs(x), "poll available after F: national 2024-11-15 Gallup", fixed = TRUE)
  x <- ok; x$polls_kd$date[1] <- x$E
  expect_error(validate_inputs(x), paste("poll dated on or after E: kjordaemi 2024-11-30", x$polls_kd$fyrirtaeki[1]), fixed = TRUE)
  x <- ok; x$kosning_nat$date[1] <- as.Date("2015-12-31")
  expect_error(validate_inputs(x), "poll before the window: national 2015-12-31 Kosning", fixed = TRUE)
  x <- ok; x$results <- dplyr::bind_rows(x$results, dplyr::mutate(x$results[1, ], election = x$E))
  expect_error(validate_inputs(x), "target election result: election 2024-11-30", fixed = TRUE)
  x <- ok; x$fundamentals$target$total_valid_votes[1] <- 212470
  expect_error(validate_inputs(x), paste("target vote share: target", x$fundamentals$target$flokkur[1], "total_valid_votes"), fixed = TRUE)
  x <- ok; x$coalitions$to[1] <- x$F + 1
  expect_error(validate_inputs(x), paste("event after F:", x$coalitions$cabinet[1], "from 2016-04-07 to 2024-11-17"), fixed = TRUE)
  x <- cell("2017-10-28", "2017-10-14"); x$parties <- c(x$parties, "Sósíalistaflokkurinn")
  expect_error(validate_inputs(x), "party not yet available: Sósíalistaflokkurinn", fixed = TRUE)
  x <- ok; x$polls_nat$available_from <- x$F + 1
  expect_error(validate_inputs(x), "\\.\\.\\. \\(279 in all\\)$")
})
test_that("the 2017 cells have nine parties and the others all ten", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) {
    x <- info_set(TARGETS$E[i], TARGETS$F[i], inp)
    expected <- if (TARGETS$confirmatory[i]) setdiff(POLL_PARTIES, "Sósíalistaflokkurinn") else POLL_PARTIES
    expect_identical(x$parties, expected, label = paste(TARGETS$E[i], TARGETS$F[i]))
    expect_true(all(x$polls_nat$flokkur %in% x$parties) && all(x$polls_kd$flokkur %in% x$parties))
  }
})
test_that("the Fréttablaðið 2017-10-10 n is scaled to the 804 respondents reported", {
  skip_without_snapshot()
  key <- function(x) x$date == as.Date("2017-10-10") & x$fyrirtaeki == "Fréttablaðið, Stöð 2 og Vísir"
  raw <- readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE)
  before <- raw$n[key(raw)]; after <- inp$polling$n[key(inp$polling)]
  expect_equal(sum(after), 804, tolerance = 1e-12)
  expect_equal(after, 804 * before / sum(before))
  expect_lt(max(abs(after / sum(after) - before / sum(before))), 1e-12)
  expect_identical(inp$polling$n[!key(inp$polling)], raw$n[!key(raw)])
})

test_that("D's 2017 incumbency is corrected from 5 to 4, at E and in later training rows", {
  skip_without_snapshot()
  d <- "Sjálfstæðisflokkurinn"
  target <- cell("2017-10-28", "2017-10-14")$fundamentals$target
  expect_identical(target$incumbent_years[target$flokkur == d], 4)
  train <- cell("2021-09-25", "2021-09-11")$fundamentals$train
  expect_identical(train$incumbent_years[train$flokkur == d & train$date == as.Date("2017-10-28")], 4)
  raw <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  row <- which(raw$flokkur == d & raw$date == as.Date("2017-10-28"))
  expect_identical(raw$incumbent_years[row], 5)
  raw$incumbent_years[row] <- 4
  # WHY: edition 2 dropped ignore_attr = TRUE here unread, so attributes were compared;
  # under edition 3 the argument would skip them, so it is gone.
  expect_equal(inp$fundamentals, raw)
})

# Synthetic fundamentals for the fundamentals correction.
syn_fund <- tibble::tibble(
  date = as.Date(c("2016-10-29", "2017-10-28", "2017-10-28")),
  flokkur = c("Sjálfstæðisflokkurinn", "Sjálfstæðisflokkurinn", "Viðreisn"),
  incumbent_years = c(3, 5, 1), voteshare = c(29, 25.2, 6.7)
)
fund_fix <- function(date = "2017-10-28", flokkur = "Sjálfstæðisflokkurinn", column = "incumbent_years", data = 5) {
  tibble::tibble(
    date = as.Date(date), flokkur = flokkur, column = column, value_data = data, value_corrected = 4,
    source = "s", note = "n"
  )
}
test_that("a fundamentals correction replaces one value", {
  out <- apply_fundamentals_corrections(syn_fund, fund_fix())
  expect_identical(out$incumbent_years, c(3, 4, 1))
  expect_identical(out[c("date", "flokkur", "voteshare")], syn_fund[c("date", "flokkur", "voteshare")])
})
test_that("a fundamentals correction whose value no longer matches warns and changes nothing", {
  expect_warning(
    out <- apply_fundamentals_corrections(syn_fund, fund_fix(data = 6)),
    "fundamentals correction not applied: 2017-10-28 Sjálfstæðisflokkurinn incumbent_years"
  )
  expect_identical(out, syn_fund)
})
test_that("a fundamentals correction naming no row, a missing column or two rows stops", {
  expect_error(apply_fundamentals_corrections(syn_fund, fund_fix(date = "2017-10-29")), "names no row: 2017-10-29 Sjálfstæðisflokkurinn")
  expect_error(apply_fundamentals_corrections(syn_fund, fund_fix(flokkur = "Píratar")), "names no row: 2017-10-28 Píratar")
  expect_error(apply_fundamentals_corrections(syn_fund, fund_fix(column = "incumbent")), "names no value column: 2017-10-28 Sjálfstæðisflokkurinn incumbent")
  expect_error(apply_fundamentals_corrections(syn_fund, fund_fix(column = "flokkur")), "names no value column")
  expect_error(apply_fundamentals_corrections(dplyr::bind_rows(syn_fund, syn_fund[2, ]), fund_fix()), "names more than one row")
})
test_that("the fundamentals corrections table is read strictly", {
  expect_identical(nrow(read_fundamentals_corrections()), 1L)
  bad <- withr::local_tempfile(fileext = ".csv")
  header <- "date,flokkur,column,value_data,value_corrected,source,note"
  readr::write_lines(c(header, "2017-10-28,Sjálfstæðisflokkurinn,incumbent_years,5,,s,n"), bad)
  expect_error(read_fundamentals_corrections(bad), "missing or unparsable")
  readr::write_lines(c("date,flokkur,column,value_data,value_corrected,source", "2017-10-28,D,incumbent_years,5,4,s"), bad)
  expect_error(suppressWarnings(read_fundamentals_corrections(bad)), "columns must be")
})

# Synthetic polls for the n correction: two national polls of house H, one kjördæmi poll.
syn_nat <- tibble::tibble(
  date = as.Date(rep(c("2017-10-10", "2017-10-16"), c(4, 2))), fyrirtaeki = "H",
  flokkur = c("Annað", "Samfylkingin", "Píratar", "Sósíalistaflokkurinn", "Annað", "Samfylkingin"),
  n = c(57, 28, 15, 0, 40, 60)
)
syn_kd <- tibble::tibble(
  date = as.Date("2017-10-16"), fyrirtaeki = "H", flokkur = c("Annað", "Samfylkingin"),
  kjordaemi = "Suður", n = c(4, 6)
)
syn_fix <- function(level = "national", date = "2017-10-10", data = 100, reported = 10) {
  tibble::tibble(
    level = level, date = as.Date(date), fyrirtaeki = "H", n_total_data = data,
    n_total_reported = reported, source = "s"
  )
}
test_that("an n correction scales one poll to the reported total, without rounding", {
  out <- apply_n_corrections(syn_nat, syn_kd, syn_fix())
  expect_equal(out$polling$n, c(5.7, 2.8, 1.5, 0, 40, 60))
  expect_identical(out$polling$n[5:6], syn_nat$n[5:6])
  expect_identical(out$constituency, syn_kd)
  kd <- apply_n_corrections(syn_nat, syn_kd, syn_fix("kjordaemi", "2017-10-16", 10, 5))
  expect_equal(kd$constituency$n, c(2, 3))
  expect_identical(kd$polling, syn_nat)
  # A fractional total counts when it rounds to n_total_data, and is scaled by its own value.
  frac <- dplyr::mutate(syn_nat, n = n + c(0.2, 0.1, 0.1, 0, 0, 0))
  expect_equal(apply_n_corrections(frac, syn_kd, syn_fix())$polling$n[1:4], 10 * frac$n[1:4] / 100.4)
})
test_that("an n correction whose total no longer matches warns and changes nothing", {
  expect_warning(out <- apply_n_corrections(syn_nat, syn_kd, syn_fix(data = 1000)), "n correction not applied: national 2017-10-10 H")
  expect_identical(out$polling, syn_nat)
})
test_that("an n correction naming no poll stops", {
  expect_error(apply_n_corrections(syn_nat, syn_kd, syn_fix(date = "2017-10-11")), "names no poll: national 2017-10-11 H")
  expect_error(apply_n_corrections(syn_nat, syn_kd, syn_fix("kjordaemi")), "names no poll: kjordaemi 2017-10-10 H")
  expect_error(apply_n_corrections(syn_nat, syn_kd, syn_fix("regional")), "names no poll: regional 2017-10-10 H")
})
test_that("the n corrections table is read strictly", {
  expect_identical(nrow(read_n_corrections()), 1L)
  bad <- withr::local_tempfile(fileext = ".csv")
  readr::write_lines(c("level,date,fyrirtaeki,n_total_data,n_total_reported,source", "national,2017-10-10,H,,804,s"), bad)
  expect_error(read_n_corrections(bad), "missing or unparsable")
  readr::write_lines(c("level,date,fyrirtaeki,n_total_data,n_total_reported,source", "national,2017-10-xx,H,8038,804,s"), bad)
  expect_error(read_n_corrections(bad), "missing or unparsable")
  readr::write_lines(c("level,date,fyrirtaeki,n_total_reported,source", "national,2017-10-10,H,804,s"), bad)
  expect_error(suppressWarnings(read_n_corrections(bad)), "columns must be")
})

# Synthetic cell: one national and one kjördæmi poll of house H, both available on 5 Oct.
syn_av <- tibble::tibble(
  level = c("national", "kjordaemi"), date = as.Date("2017-10-01"), fyrirtaeki = "H",
  fieldwork_end = as.Date("2017-10-04"), available_from = as.Date("2017-10-05"), rule = c("lokadagur", "twin")
)
syn_cell_nat <- tibble::tibble(
  date = as.Date("2017-10-01"), fyrirtaeki = "H",
  flokkur = c("Annað", "Samfylkingin", "Sósíalistaflokkurinn"), n = c(10, 20, 0)
)
syn_cell_kd <- dplyr::mutate(syn_cell_nat, kjordaemi = "Suður", n = c(1, 2, 0))
syn_cell <- function(nat = syn_cell_nat, kd = syn_cell_kd, av = syn_av, F = "2017-10-14") {
  cell_polls(as.Date("2017-10-28"), as.Date(F), nat, kd, av)
}
test_that("cell polls keep the parties polled above zero by F", {
  x <- syn_cell()
  expect_identical(x$parties, c("Samfylkingin", "Annað"))
  expect_identical(x$polls_kd$flokkur, c("Samfylkingin", "Annað"))
  expect_identical(nrow(syn_cell(F = "2017-10-04")$polls_nat), 0L)
})
test_that("restricting to the cell's parties never drops a row with n > 0", {
  kd <- syn_cell_kd; kd$n[3] <- 1
  expect_error(syn_cell(kd = kd), "would drop kjördæmi row\\(s\\) with n > 0: H 2017-10-01 Suður Sósíalistaflokkurinn")
  nat <- syn_cell_nat; nat$flokkur[3] <- "Björt framtíð"; nat$n[3] <- 5
  expect_error(syn_cell(nat = nat), "would drop national row\\(s\\) with n > 0: H 2017-10-01 Björt framtíð")
})
test_that("a window poll without an availability row stops", {
  expect_error(syn_cell(av = syn_av[1, ]), "no availability row: kjordaemi H 2017-10-01")
})

# Fix round 1 (R40-R42).
test_that("F must fall inside E's election cycle", {
  skip_without_snapshot()
  expect_error(cell("2024-11-30", "2021-09-01"), "F = 2021-09-01 is outside the cycle of E = 2024-11-30", fixed = TRUE)
  expect_error(cell("2021-09-25", "2017-06-01"), "F = 2017-06-01 is outside the cycle of E = 2021-09-25", fixed = TRUE)
  expect_error(cell("2024-11-30", "2024-11-30"), "F = 2024-11-30 is outside the cycle of E = 2024-11-30", fixed = TRUE)
  expect_error(cell("2024-11-30", "2021-09-25"), "F = 2021-09-25 is outside the cycle of E = 2024-11-30", fixed = TRUE)
  # WHY: since R50 the day after the previous election is inside the cycle but before the
  # economy covariates' first releases, so the earliest F that builds is the later of them.
  expect_error(cell("2024-11-30", "2021-09-26"), "F = 2021-09-26 is before the first release of economy covariate(s)", fixed = TRUE)
  expect_error(cell("2024-11-30", "2024-02-28"), "growth (2023, first released 2024-02-29)", fixed = TRUE)
  expect_no_error(validate_inputs(cell("2024-11-30", "2024-02-29")))
})
test_that("a result, Kosning row, training row or previous share dated on or after F is refused", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16")
  x <- ok; x$results <- dplyr::bind_rows(x$results, dplyr::mutate(x$results[1, ], election = x$F + 4))
  expect_error(validate_inputs(x), "election result not known at F: election 2024-11-20", fixed = TRUE)
  x <- ok; x$kosning_nat <- dplyr::bind_rows(x$kosning_nat, dplyr::mutate(x$kosning_nat[1, ], date = x$F))
  expect_error(validate_inputs(x), "election result not known at F: national 2024-11-16 Kosning", fixed = TRUE)
  x <- ok; x$fundamentals$train$date[1] <- x$F
  expect_error(validate_inputs(x), "election result not known at F: train 2024-11-16", fixed = TRUE)
  x <- ok; x$prev_shares$previous[1] <- x$F
  expect_error(validate_inputs(x), "election result not known at F: prev_shares 2024-11-16", fixed = TRUE)
})
test_that("availability and entry carry nothing past F", {
  skip_without_snapshot(); x <- cell("2017-10-28", "2017-10-27")
  y <- x; y$availability <- inp$availability
  expect_error(validate_inputs(y), "^poll available after F: national ")
  gallup <- inp$availability[inp$availability$level == "kjordaemi" & inp$availability$date == as.Date("2017-10-25"), ]
  y <- x; y$availability <- dplyr::bind_rows(x$availability, gallup)
  expect_error(validate_inputs(y), "poll available after F: kjordaemi 2017-10-25 Gallup", fixed = TRUE)
  y <- x; y$entry <- dplyr::bind_rows(x$entry, inp$entry[inp$entry$flokkur == "Sósíalistaflokkurinn", ])
  expect_error(validate_inputs(y), "party not yet available: Sósíalistaflokkurinn", fixed = TRUE)
  y <- x; y$entry$first_nonzero[y$entry$flokkur == "Miðflokkurinn"] <- x$F + 1
  expect_error(validate_inputs(y), "party not yet available: Miðflokkurinn", fixed = TRUE)
  y <- x; y$parties <- setdiff(x$parties, "Miðflokkurinn")
  expect_error(validate_inputs(y), "party not yet available: Miðflokkurinn", fixed = TRUE)
})
test_that("read_inputs() builds availability with availability.R's own readers", {
  skip_without_snapshot()
  expect_identical(inp$availability, poll_availability())
})
test_that("houses are those with an admitted poll, and Kosning", {
  skip_without_snapshot(); x <- cell("2017-10-28", "2017-10-27")
  expect_identical(x$houses$fyrirtaeki, c("Kosning", "Félagsvísindastofnun", "Fréttablaðið, Stöð 2 og Vísir", "Gallup", "MMR"))
  expect_identical(cell("2024-11-30", "2024-11-29")$houses$fyrirtaeki, inp$houses$fyrirtaeki)
  y <- x; y$houses <- inp$houses
  expect_error(validate_inputs(y), "house with no admitted poll: Maskína; Prósent", fixed = TRUE)
})
test_that("the 2017 Kosning rows hold the nine parties", {
  skip_without_snapshot(); x <- cell("2017-10-28", "2017-10-14")
  expect_identical(x$kosning_nat$flokkur, x$parties)
})
test_that("a Kosning poll, a poll available by its midpoint and incumbency not as at F are refused", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16")
  x <- ok; x$polls_nat$fyrirtaeki[1] <- "Kosning"
  expect_error(validate_inputs(x), paste("Kosning row among the polls: national", format(x$polls_nat$date[1]), "Kosning"), fixed = TRUE)
  x <- ok; x$polls_nat$date[1] <- x$F + 3; x$polls_nat$available_from[1] <- x$F
  expect_error(validate_inputs(x), "poll available on or before its midpoint: national 2024-11-19", fixed = TRUE)
  x <- ok; x$polls_kd$date[1] <- x$polls_kd$available_from[1]
  expect_error(validate_inputs(x), paste("poll available on or before its midpoint: kjordaemi", format(x$polls_kd$date[1])), fixed = TRUE)
  x <- ok; x$fundamentals$target$incumbent[1] <- 1 - x$fundamentals$target$incumbent[1]
  expect_error(validate_inputs(x), paste("incumbency not as at F: target", x$fundamentals$target$flokkur[1], "incumbent"), fixed = TRUE)
  x <- ok; x$fundamentals$target$incumbent_prime_minister[2] <- 1
  expect_error(validate_inputs(x), paste("incumbency not as at F: target", x$fundamentals$target$flokkur[2], "incumbent_prime_minister"), fixed = TRUE)
})
test_that("Kosning rows keep the cell's parties and never drop a vote", {
  kos <- tibble::tibble(
    date = as.Date("2016-10-29"), fyrirtaeki = "Kosning", flokkur = c("Annað", "Samfylkingin", "Sósíalistaflokkurinn"), n = c(5, 6, 0)
  )
  av <- dplyr::bind_rows(syn_av, tibble::tibble(
    level = "national", date = as.Date("2016-10-29"), fyrirtaeki = "Kosning",
    fieldwork_end = as.Date("2016-10-29"), available_from = as.Date("2016-10-30"), rule = "election"
  ))
  expect_identical(syn_cell(nat = dplyr::bind_rows(syn_cell_nat, kos), av = av)$kosning_nat$flokkur, c("Samfylkingin", "Annað"))
  kos$n[3] <- 1
  expect_error(
    syn_cell(nat = dplyr::bind_rows(syn_cell_nat, kos), av = av),
    "would drop Kosning row\\(s\\) with n > 0: Kosning 2016-10-29 Sósíalistaflokkurinn"
  )
})

# Final fix wave (R50): the economy covariates must be public at F.
test_that("info_set() refuses an F before an economy covariate's first release, naming it", {
  skip_without_snapshot()
  expect_error(
    cell("2021-09-25", "2021-02-25"),
    "F = 2021-02-25 is before the first release of economy covariate(s) of E = 2021-09-25: growth (2020, first released 2021-02-26)",
    fixed = TRUE
  )
  expect_no_error(validate_inputs(cell("2021-09-25", "2021-02-26")))
  expect_error(
    cell("2024-11-30", "2023-07-20"),
    "of E = 2024-11-30: vnv (2023-07, first released 2023-07-21); growth (2023, first released 2024-02-29)",
    fixed = TRUE
  )
  expect_error(cell("2017-10-28", "2017-03-08"), "of E = 2017-10-28: growth (2016, first released 2017-03-09)", fixed = TRUE)
  expect_no_error(validate_inputs(cell("2017-10-28", "2017-03-09")))
})
test_that("every cell carries the economy row its target uses, first released by F", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) {
    x <- info_set(TARGETS$E[i], TARGETS$F[i], inp)
    expect_identical(names(x$economy), c("covariate", "period", "value", "first_release"))
    expect_identical(x$economy$covariate, c("vnv", "growth"))
    expect_true(all(x$economy$first_release <= x$F))
    expect_identical(unique(x$fundamentals$target$vnv), x$economy$value[1])
    expect_identical(unique(x$fundamentals$target$growth), x$economy$value[2])
  }
  x <- cell("2021-09-25", "2021-03-29")
  expect_identical(x$economy$period, c("2020-07", "2020"))
  expect_identical(x$economy$first_release, as.Date(c("2020-07-24", "2021-02-26")))
  expect_identical(x$economy$value[2], -7.8)
})
test_that("an economy covariate not known at F, or a target value that differs from it, is refused", {
  skip_without_snapshot(); ok <- cell("2021-09-25", "2021-03-29"); g <- ok$economy$covariate == "growth"
  x <- ok; x$economy$first_release[g] <- x$F + 1
  expect_error(validate_inputs(x), "economy covariate not known at F: economy growth 2020 first released 2021-03-30", fixed = TRUE)
  x <- ok; x$economy$first_release[!g] <- NA
  expect_error(validate_inputs(x), "economy covariate not known at F: economy vnv 2020-07 first released NA", fixed = TRUE)
  x <- ok; x$economy$value[!g] <- 0.5
  expect_error(validate_inputs(x), paste("economy covariate not known at F: target", x$fundamentals$target$flokkur[1], "vnv"), fixed = TRUE)
  x <- ok; x$fundamentals$target$growth[2] <- 2.6
  expect_error(validate_inputs(x), paste("economy covariate not known at F: target", x$fundamentals$target$flokkur[2], "growth"), fixed = TRUE)
  x <- ok; x$economy <- x$economy[!g, ]
  expect_error(validate_inputs(x), "economy covariate not known at F: economy growth has no row", fixed = TRUE)
})
test_that("info_set() stops on a release table that disagrees with the snapshot or lacks a row", {
  skip_without_snapshot()
  row <- inp$economy_release$election == as.Date("2021-09-25") & inp$economy_release$covariate == "growth"
  bad <- inp; bad$economy_release$value[row] <- -6.6
  expect_error(info_set("2021-09-25", "2021-03-29", bad), "differs from the snapshot's economy_data.csv at E = 2021-09-25: growth -6.6 against -7.8", fixed = TRUE)
  bad <- inp; bad$economy_release <- bad$economy_release[!row, ]
  expect_error(info_set("2021-09-25", "2021-03-29", bad), "economy_release.csv has no row for E = 2021-09-25: growth", fixed = TRUE)
})
test_that("the economy release table is read strictly", {
  x <- read_economy_release()
  expect_identical(names(x), c("election", "covariate", "period", "value", "first_release", "source", "note"))
  expect_identical(paste(x$election, x$covariate), paste(rep(ELECTIONS[-1], each = 2), c("vnv", "growth")))
  header <- "election,covariate,period,value,first_release,source,note"
  bad <- withr::local_tempfile(fileext = ".csv")
  readr::write_lines(c(header, "2021-09-25,growth,2020,-7.8,,s,n"), bad)
  expect_error(read_economy_release(bad), "missing or unparsable")
  readr::write_lines(c(header, "2021-09-25,gdp,2020,-7.8,2021-02-26,s,n"), bad)
  expect_error(read_economy_release(bad), "covariate(s) other than vnv and growth: gdp", fixed = TRUE)
  readr::write_lines(c(header, rep("2021-09-25,growth,2020,-7.8,2021-02-26,s,n", 2)), bad)
  expect_error(read_economy_release(bad), "listed twice: 2021-09-25 growth", fixed = TRUE)
})
