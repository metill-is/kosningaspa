box::use(R/harness/tables[epoch_of, entry_table, read_houses, cabinet_at], R/harness/inputs[snapshot_file])
test_that("epochs close on election day", {
  d <- as.Date(c("2016-03-13", "2016-10-29", "2016-10-30", "2017-09-21", "2018-10-16", "2024-11-29", "2024-12-01"))
  expect_equal(epoch_of(d), c(1L, 1L, 2L, 2L, 3L, 4L, 5L))
})
test_that("the entry table is the design's", {
  skip_without_snapshot()
  e <- entry_table(readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE))
  nc <- e[e$newcomer, ] |> dplyr::arrange(first_nonzero)
  expect_equal(nc$flokkur, c("Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn"))
  expect_equal(as.character(nc$first_nonzero), c("2016-03-13", "2016-08-13", "2017-09-21", "2018-10-16"))
  expect_equal(nc$entry_epoch, c(1L, 1L, 2L, 3L))
  expect_equal(sum(!e$newcomer), 6)
})
test_that("every pollster has a house", {
  skip_without_snapshot()
  f <- c(readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE)$fyrirtaeki,
         readr::read_csv(snapshot_file("constituency_data"), show_col_types = FALSE)$fyrirtaeki)
  expect_true(all(unique(f) %in% read_houses()$fyrirtaeki))
})

house_csv <- function(dir, ...) {
  path <- file.path(dir, "house_table.csv")
  writeLines(c("fyrirtaeki,house,lineage", ...), path)
  path
}
test_that("read_houses keeps file order and stops on a pollster listed twice", {
  dir <- withr::local_tempdir()
  expect_equal(read_houses(house_csv(dir, "MMR,MMR,MMR", "Gallup,Gallup,Gallup"))$fyrirtaeki, c("MMR", "Gallup"))
  expect_error(read_houses(house_csv(dir, "MMR,MMR,MMR", "Gallup,Gallup,Gallup", "MMR,MMR,MMR")), "more than once: MMR")
})
test_that("read_houses stops on a missing value", {
  dir <- withr::local_tempdir()
  expect_error(read_houses(house_csv(dir, "MMR,MMR,", "Gallup,Gallup,Gallup")), "missing value")
  expect_error(read_houses(house_csv(dir, "MMR,NA,MMR")), "missing value")
})
test_that("epoch_of takes ISO strings and keeps NA", {
  expect_identical(epoch_of(c("2016-10-30", NA)), c(2L, NA))
})
test_that("entry_table ignores the Kosning rows", {
  polling <- tibble::tibble(
    date = as.Date(c("2013-04-27", "2016-02-01", "2016-03-01")),
    fyrirtaeki = c("Kosning", "Gallup", "MMR"),
    flokkur = "Píratar",
    n = c(100, 50, 60)
  )
  e <- entry_table(polling)
  expect_equal(e$first_nonzero, as.Date("2016-02-01"))
  expect_true(e$newcomer)
})

box::use(R/harness/tables[read_coalitions, coalition_event, incumbents_at, pm_at])
co <- read_coalitions()
ev <- function(F) sort(coalition_event(as.Date(F), co)$parties)
test_that("the Brier coalition is the composition in office at F", {
  expect_equal(ev("2017-10-14"), sort(c("Sjálfstæðisflokkurinn", "Viðreisn")))
  expect_equal(coalition_event(as.Date("2017-10-27"), co)$dropped, "A")
  for (F in c("2021-03-29", "2021-09-24", "2024-06-03", "2024-10-14"))
    expect_equal(ev(F), sort(c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Vinstri Græn")), label = F)
  for (F in c("2024-11-16", "2024-11-29"))
    expect_equal(ev(F), sort(c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn")), label = F)
})
test_that("incumbency at F equals the fundamentals coding in every cell", {
  skip_without_snapshot()
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  for (i in seq_len(nrow(TARGETS))) {
    E <- TARGETS$E[i]; F <- TARGETS$F[i]; fe <- f[f$date == E, ]
    coded <- fe$flokkur[fe$incumbent == 1] |> dplyr::recode(Framsókn = "Framsóknarflokkurinn")
    expect_equal(sort(incumbents_at(F, co)), sort(intersect(coded, POLL_PARTIES)), label = paste(E, F))
    pm <- dplyr::recode(fe$flokkur[fe$incumbent_prime_minister == 1], Framsókn = "Framsóknarflokkurinn")
    expect_equal(pm_at(F, co), pm, label = paste("pm", E, F))
  }
})

coalition_csv <- function(dir, ...) {
  path <- file.path(dir, "coalition_events.csv")
  writeLines(c("cabinet,from,to,parties,pm,caretaker,source_url", ...), path)
  path
}
two_rows <- c("A,2016-01-01,2017-01-01,B;D,B,FALSE,u", "B,2017-01-01,,D;C;A,D,FALSE,u")
test_that("the coalition helpers return poll-party labels in POLL_PARTIES order", {
  x <- read_coalitions(coalition_csv(withr::local_tempdir(), two_rows))
  expect_identical(coalition_event(as.Date("2016-06-01"), x),
                   list(parties = c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn"), dropped = character(0)))
  expect_identical(coalition_event("2017-01-01", x),
                   list(parties = c("Sjálfstæðisflokkurinn", "Viðreisn"), dropped = "A"))
  expect_identical(incumbents_at("2030-01-01", x), c("Sjálfstæðisflokkurinn", "Viðreisn"))
  expect_identical(pm_at("2016-12-31", x), "Framsóknarflokkurinn")
})
test_that("incumbents_at takes the cabinet's opening composition", {
  x <- read_coalitions(coalition_csv(withr::local_tempdir(), c(
    "A,2016-01-01,2017-01-01,D;B;V,D,FALSE,u", "A,2017-01-01,2018-01-01,D;B,D,TRUE,u", "B,2018-01-01,,S,S,FALSE,u"
  )))
  expect_identical(coalition_event("2017-06-01", x)$parties, c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn"))
  expect_identical(incumbents_at("2017-06-01", x), c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Vinstri Græn"))
})
test_that("read_coalitions stops on a gap, an overlap or a row that does not end after it starts", {
  dir <- withr::local_tempdir()
  expect_equal(nrow(read_coalitions(coalition_csv(dir, two_rows))), 2)
  expect_error(read_coalitions(coalition_csv(dir, two_rows[1], "B,2017-01-02,,D;C,D,FALSE,u")), "gap or overlap")
  expect_error(read_coalitions(coalition_csv(dir, two_rows[1], "B,2016-12-31,,D;C,D,FALSE,u")), "gap or overlap")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,2016-01-01,B;D,B,FALSE,u", "B,2016-01-01,,D,D,FALSE,u")),
               "not after")
})
test_that("read_coalitions allows an open end only on the last row", {
  dir <- withr::local_tempdir()
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;D,B,FALSE,u", "B,2017-01-01,,D,D,FALSE,u")),
               "open end")
})
test_that("read_coalitions needs a row and an open end on the last row", {
  dir <- withr::local_tempdir()
  expect_error(read_coalitions(coalition_csv(dir)), "no rows")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,2017-01-01,B;D,B,FALSE,u")), "open end")
})
test_that("read_coalitions stops on a list letter it does not know", {
  dir <- withr::local_tempdir()
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;Q,B,FALSE,u")), "unknown list letter")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B; D,B,FALSE,u")), "unknown list letter")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;D;,B,FALSE,u")), "unknown list letter")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;D,Q,FALSE,u")), "unknown list letter")
})
test_that("read_coalitions stops on a prime minister outside the row's parties", {
  expect_error(read_coalitions(coalition_csv(withr::local_tempdir(), "A,2016-01-01,,B;D,S,FALSE,u")), "pm not among")
})
test_that("read_coalitions stops on a cabinet whose rows are not contiguous", {
  rows <- c(
    "A,2016-01-01,2016-06-01,B;D,B,FALSE,u", "A,2016-06-01,2017-01-01,B;D,B,TRUE,u",
    "B,2017-01-01,2018-01-01,D,D,FALSE,u", "A,2018-01-01,,B;D,B,FALSE,u"
  )
  expect_error(read_coalitions(coalition_csv(withr::local_tempdir(), rows)), "not contiguous: A$")
})
test_that("read_coalitions stops on other columns", {
  path <- file.path(withr::local_tempdir(), "coalition_events.csv")
  writeLines(c("cabinet,from,to,parties,pm,caretaker", "A,2016-01-01,,B;D,B,FALSE"), path)
  expect_error(suppressWarnings(read_coalitions(path)), "columns must be")
})
test_that("read_coalitions stops on a missing or unparsable value", {
  dir <- withr::local_tempdir()
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;D,,FALSE,u")), "missing value")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-13-01,,B;D,B,FALSE,u")), "unparsable")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,2017-02-30,B;D,B,FALSE,u", "B,2017-02-30,,D,D,FALSE,u")),
               "unparsable")
  expect_error(read_coalitions(coalition_csv(dir, "A,2016-01-01,,B;D,B,maybe,u")), "unparsable")
})
test_that("cabinet_at needs exactly one cabinet in office", {
  x <- read_coalitions(coalition_csv(withr::local_tempdir(), two_rows))
  expect_error(cabinet_at(as.Date("2015-12-31"), x), "no cabinet")
  expect_error(cabinet_at(as.Date(c("2016-06-01", "2016-07-01")), x), "one date")
  expect_error(cabinet_at(NA, x), "one date")
  x$to[1] <- as.Date("2017-06-01")
  expect_error(cabinet_at(as.Date("2017-03-01"), x), "2 cabinets")
})

box::use(R/harness/tables[valid_votes, expected_valid_votes], R/harness/truth[build_truth], R/harness/constants[KJORDAEMI])
test_that("expected valid votes are the previous election's, by name", {
  skip_without_snapshot(); t <- build_truth()
  expect_equal(sum(expected_valid_votes(t, as.Date("2024-11-30"))$votes), 199730)
  expect_equal(sum(expected_valid_votes(t, as.Date("2017-10-28"))$votes), 189648)
  withr::local_seed(1)
  expect_identical(valid_votes(t[sample(nrow(t)), ], as.Date("2021-09-25")), valid_votes(t, as.Date("2021-09-25")))
  expect_equal(sum(valid_votes(t, as.Date("2021-09-25"))$w), 1)
  expect_error(expected_valid_votes(t, as.Date("2016-10-29")))
})

# Two lists per kjördæmi in reverse KJORDAEMI order; the i-th kjördæmi in KJORDAEMI order
# has 100 i + i votes.
vv_truth <- function() {
  k <- KJORDAEMI$kjordaemi
  tibble::tibble(
    election = as.Date("2016-10-29"), kjordaemi = rep(rev(k), each = 2), list_letter = rep(c("D", "S"), 6),
    votes = rep(rev(seq_along(k)), each = 2) * c(100L, 1L)
  )
}
test_that("valid_votes joins kjördæmi by name and returns them in KJORDAEMI order", {
  v <- valid_votes(vv_truth(), "2016-10-29")
  expect_identical(v$kjordaemi, KJORDAEMI$kjordaemi)
  expect_identical(v$votes, 101L * 1:6)
  expect_equal(v$w, (1:6) / 21)
})
test_that("valid_votes stops on a missing election or a kjördæmi missing or not in KJORDAEMI", {
  t <- vv_truth()
  expect_error(valid_votes(t, "2017-10-28"), "no kjördæmi results for 2017-10-28")
  expect_error(valid_votes(t, NA), "one date")
  expect_error(valid_votes(t, c("2016-10-29", "2016-10-29")), "one date")
  expect_error(valid_votes(t[t$kjordaemi != "Suður", ], "2016-10-29"), "missing: Suður")
  extra <- t[1, ]
  extra$kjordaemi <- "Útlönd"
  expect_error(valid_votes(rbind(t, extra), "2016-10-29"), "not in KJORDAEMI: Útlönd")
})
test_that("expected_valid_votes takes the previous election's and names a missing one", {
  t <- vv_truth()
  expect_identical(expected_valid_votes(t, "2017-10-28"), valid_votes(t, "2016-10-29"))
  expect_error(expected_valid_votes(t, "2016-10-29"), "2013-04-27, the election before 2016-10-29")
})
