box::use(
  R / harness / anchors[fold_anchor],
  R / harness / availability[excluded_near, merge_same_day_polls, read_polling],
  R / harness / constants[ELECTION_HOUSE, KJORDAEMI, window_elections, window_start],
  R / harness / info_set[info_set, read_inputs, validate_inputs],
  R / harness / tables[valid_votes]
)
E16 <- as.Date("2016-10-29")
START16 <- as.Date("2013-04-27")
DAYS16 <- as.Date(c("2016-05-02", "2016-09-12", "2016-10-15", "2016-10-28"))
FRETT16 <- "Fr\u00e9ttabla\u00f0i\u00f0, St\u00f6\u00f0 2 og V\u00edsir"
FEL16 <- "F\u00e9lagsv\u00edsindastofnun"
inp <- if (snapshot_present()) read_inputs()
cells16 <- if (snapshot_present()) lapply(DAYS16, \(F) info_set(E16, F, inp))

# Canonical text of a table or vector: column names and classes, then one line per row,
# dates as ISO strings and doubles to 17 significant digits; its sha256 is over the bytes.
canon_text <- function(x) {
  if (!is.data.frame(x)) x <- tibble::tibble(value = x)
  cell <- function(v) if (inherits(v, "Date")) format(v) else if (is.double(v)) sprintf("%.17g", v) else as.character(v)
  c(
    paste(names(x), vapply(x, \(v) class(v)[1], ""), sep = ":", collapse = ","),
    if (nrow(x)) do.call(paste, c(unname(lapply(x, cell)), sep = "\t"))
  )
}
DIGEST_FIELDS <- c("parties", "polls_nat", "polls_kd", "kosning_nat", "results", "valid_votes_open", "entry")
cell_digest <- function(info) {
  text <- unlist(lapply(DIGEST_FIELDS, \(f) c(f, canon_text(info[[f]]))))
  digest::digest(paste(text, collapse = "\n"), algo = "sha256", serialize = FALSE)
}
# WHY: re-recorded on 2026-11-11 with cell_digest() once the 2016 cells became targets, so a
# change that reaches an existing cell shows here. The 2016 publication rows moved only the
# available_from of rows dated before 2017; ADMITTED_DIGESTS ties each cell to the tree at b507364.
TARGET_DIGESTS <- tibble::tribble(
  ~E, ~F, ~digest,
  "2017-10-28", "2017-10-14", "fa33ba350ff48d1109022fd9eb9f9d2759d28a43005b1792937e620db2fe1edd",
  "2017-10-28", "2017-10-27", "c736805cc373fe9b42f342d9a99cf100525cc9910e66edf2aef2978fed36ebb3",
  "2021-09-25", "2021-03-29", "6fba4135c3af3c3ffee1c4aa896ac50e3832d886a1609ada6fc3ba8c9c2848da",
  "2021-09-25", "2021-08-09", "bdbacb99cd217e613dec2f8f051df2f5b5de7ffcdb10d96af6bca4b0edfc6183",
  "2021-09-25", "2021-09-11", "56a4e6f393aea99a50967b5922e22d031ba2ea9ce845a4d971d05732d40d68a4",
  "2021-09-25", "2021-09-24", "a59c3b270e65826566e9482cfd9a126427500613dc3a7401c8a4a2d2c223ff27",
  "2024-11-30", "2024-06-03", "ebe32a604cb18c2aee3f357f9bb18d9041f2f5ae2ca4bbe16f5d21aa50f7af82",
  "2024-11-30", "2024-10-14", "1a7edb9dbd89617452e19db079b88799aed7b4145ccee2346e046e84f3a2c59b",
  "2024-11-30", "2024-11-16", "36d02956448fdc44076836ab96034183542456f950dae0b87f0bcfda35202a98",
  "2024-11-30", "2024-11-29", "4cc4187ed09d0f903a089b367eb89caad10f4f41ea309001a3d5eee93f253037"
)

test_that("2016's window opens at 2013-04-27 with 2013 and 2016 as its elections, and every other window is as before (a global window moved, which would change every finished run's data)", {
  expect_identical(window_start(E16), START16)
  expect_identical(window_elections(E16), as.Date(c("2013-04-27", "2016-10-29")))
  expect_identical(WINDOW_START, as.Date("2016-01-01"))
  expect_identical(ELECTIONS, as.Date(c("2016-10-29", "2017-10-28", "2021-09-25", "2024-11-30")))
  others <- ELECTIONS[-1]
  expect_identical(window_start(others), rep(WINDOW_START, 3))
  for (E in format(others)) expect_identical(window_elections(E), ELECTIONS, label = E)
  expect_identical(window_start(c("2024-11-30", "2016-10-29")), as.Date(c("2016-01-01", "2013-04-27")))
  expect_identical(min(window_start(ELECTIONS)), START16)
  expect_error(window_start("2013-04-27"), "not a window election: 2013-04-27", fixed = TRUE)
  expect_error(window_elections(ELECTIONS), "E must be one date", fixed = TRUE)
})

test_that("each 2016 forecast day builds and validates, reads no poll before 2013-04-27 or past F, and takes its previous results from the 2013 anchor (a cell that silently reads no previous result, or 2016's own)", {
  skip_without_snapshot()
  anchor <- fold_anchor(inp$anchor_2013)
  for (x in cells16) {
    lab <- format(x$F)
    expect_no_error(validate_inputs(x))
    expect_identical(x$window_start, START16)
    expect_identical(x$elections, as.Date(c("2013-04-27", "2016-10-29")))
    polls <- rbind(x$polls_nat[c("date", "available_from")], x$polls_kd[c("date", "available_from")])
    expect_true(all(polls$date >= START16), label = lab)
    expect_true(all(polls$available_from <= x$F), label = lab)
    expect_identical(unique(x$results$election), START16)
    expect_setequal(x$results$kjordaemi, KJORDAEMI$kjordaemi)
    expect_equal(sum(x$results$votes), 189023)
    expect_equal(x$results, anchor)
    expect_equal(x$valid_votes_open, valid_votes(anchor, START16))
    expect_equal(sum(x$valid_votes_open$votes), 189023)
    # WHY: the polling data hold no 2013 Kosning row, so info_set() builds one from the anchor
    # for R0; without it R0 fits a 2016 cell with no national 2013 result.
    k <- x$kosning_nat
    expect_identical(unique(k$date), START16, label = lab)
    expect_identical(unique(k$fyrirtaeki), ELECTION_HOUSE, label = lab)
    expect_identical(anyDuplicated(k$flokkur), 0L, label = lab)
    expect_setequal(k$flokkur, x$parties)
    expect_equal(sum(k$n), 189023, label = lab)
  }
})

test_that("each 2016 cell admits every national poll the snapshot dates from 2013-04-27 to 2015-12-31 (a window cut at 2016-01-01, or one that drops the early polls)", {
  skip_without_snapshot()
  raw <- read_polling()
  early <- dplyr::distinct(raw[raw$date >= START16 & raw$date < WINDOW_START & raw$fyrirtaeki != ELECTION_HOUSE, ], date, fyrirtaeki)
  expect_identical(nrow(early), 57L)
  key <- function(rows) sort(unique(paste(format(rows$date), rows$fyrirtaeki)), method = "radix")
  for (x in cells16) {
    expect_identical(key(x$polls_nat[x$polls_nat$date < WINDOW_START, ]), key(early), label = format(x$F))
  }
})

test_that("MMR's two polls keyed 2013-05-16 enter a 2016 cell as one poll whose n per party is the sum of the two (a duplicate key that stops the build, or a half dropped)", {
  skip_without_snapshot()
  raw <- read_polling()
  halves <- raw[raw$date == as.Date("2013-05-16") & raw$fyrirtaeki == "MMR", ]
  expect_identical(nrow(halves), 20L)
  expect_true(all(table(halves$flokkur) == 2))
  sums <- c(tapply(halves$n, halves$flokkur, sum))
  for (x in cells16) {
    m <- x$polls_nat[x$polls_nat$date == as.Date("2013-05-16") & x$polls_nat$fyrirtaeki == "MMR", ]
    expect_identical(anyDuplicated(m$flokkur), 0L)
    expect_identical(m$flokkur, x$parties)
    expect_equal(m$n, unname(sums[m$flokkur]))
    expect_equal(sum(m$n), sum(halves$n))
  }
  av <- inp$availability[inp$availability$date == as.Date("2013-05-16") & inp$availability$fyrirtaeki == "MMR", ]
  expect_identical(nrow(av), 1L)
})

test_that("the merge sums only same-day polls dated before 2016, and stops on polls that report different parties (a merge that reaches the window's rows, or sums unlike polls)", {
  p <- tibble::tibble(
    date = as.Date(rep(c("2013-05-16", "2016-03-01"), c(4, 2))), fyrirtaeki = "H",
    flokkur = c("A", "B", "A", "B", "A", "A"), n = c(1, 2, 3, 4, 5, 6), lokadagur = "NA"
  )
  out <- merge_same_day_polls(p)
  expect_identical(out$flokkur, c("A", "B", "A", "A"))
  expect_equal(out$n, c(4, 6, 5, 6))
  expect_identical(out[3:4, ], p[5:6, ])
  expect_identical(merge_same_day_polls(p[c(1, 2, 5, 6), ]), p[c(1, 2, 5, 6), ])
  q <- p[1:4, ]
  q$n[3] <- 0
  expect_error(merge_same_day_polls(q), "same-day polls by one house report different parties: 2013-05-16 H", fixed = TRUE)
})

test_that("a 2016 cell numbers entry epochs from 2013, Vi\u00f0reisn and Flokkur f\u00f3lksins in epoch 2 and the six older parties in epoch 1, while a 2024 cell keeps the committed epochs (epochs counted from the wrong elections)", {
  skip_without_snapshot()
  newcomers <- POLL_PARTIES[6:7]
  older <- POLL_PARTIES[c(1:5, 10)]
  for (x in cells16) {
    e <- x$entry
    expect_setequal(e$flokkur, x$parties)
    expect_true(all(older %in% e$flokkur), label = format(x$F))
    expect_identical(e$entry_epoch, ifelse(e$flokkur %in% newcomers, 2L, 1L), label = format(x$F))
    expect_identical(e$newcomer, e$flokkur %in% newcomers)
  }
  expect_true(all(newcomers %in% cells16[[4]]$parties))
  committed <- readr::read_csv(
    root("harness", "entry_table.csv"),
    col_types = readr::cols(flokkur = "c", first_nonzero = "D", entry_epoch = "i", newcomer = "l"), progress = FALSE
  )
  x <- info_set("2024-11-30", "2024-11-29", inp)
  expect_identical(canon_text(x$entry), canon_text(committed[committed$flokkur %in% x$parties, ]))
  expect_identical(x$entry$entry_epoch[match(newcomers, x$entry$flokkur)], c(1L, 1L))
})

test_that("a 2016 cell with a poll from before 2013-04-27, or any cell whose window is not its election's, is refused (a pre-window poll admitted, or a moved window handed to the arms)", {
  skip_without_snapshot()
  x <- cells16[[1]]
  x$polls_nat$date[1] <- as.Date("2013-04-26")
  expect_error(validate_inputs(x), paste("poll before the window: national 2013-04-26", x$polls_nat$fyrirtaeki[1]), fixed = TRUE)
  x <- cells16[[1]]
  x$window_start <- WINDOW_START
  expect_error(validate_inputs(x), "window not E's: window_start 2016-01-01", fixed = TRUE)
  x <- cells16[[1]]
  x$elections <- ELECTIONS
  expect_error(validate_inputs(x), "window not E's: elections 2016-10-29 2017-10-28 2021-09-25 2024-11-30", fixed = TRUE)
  y <- info_set("2024-11-30", "2024-11-29", inp)
  y$window_start <- START16
  expect_error(validate_inputs(y), "window not E's: window_start 2013-04-27", fixed = TRUE)
  y$window_start <- NULL
  expect_error(validate_inputs(y), "window not E's: window_start none", fixed = TRUE)
})

test_that("a 2016 forecast before the 2015 GDP figure's first release is refused (an economy figure not public at F)", {
  skip_without_snapshot()
  expect_error(
    info_set(E16, "2016-03-09", inp),
    "of E = 2016-10-29: growth (2015, first released 2016-03-10)",
    fixed = TRUE
  )
  x <- cells16[[1]]
  expect_identical(x$economy$period, c("2015-07", "2015"))
  expect_identical(x$economy$first_release, as.Date(c("2015-07-23", "2016-03-10")))
})

test_that("every recorded TARGETS cell has the polls, results and entry table it had when the 2016 cells became targets (an input change that reaches an existing cell)", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGET_DIGESTS))) {
    d <- TARGET_DIGESTS[i, ]
    expect_identical(cell_digest(info_set(d$E, d$F, inp)), d$digest, label = paste(d$E, d$F))
  }
})

test_that("TARGETS holds the four 2016 cells, 180, 47, 14 and 1 days before E, ahead of the ten earlier cells, which keep their days, tags and the 2017 hold-out (a 2016 cell on the wrong day, or a 2017 cell no longer confirmatory)", {
  earlier <- tibble::tibble(
    E = as.Date(rep(c("2017-10-28", "2021-09-25", "2024-11-30"), c(2, 4, 4))),
    F = as.Date(c(
      "2017-10-14", "2017-10-27",
      "2021-03-29", "2021-08-09", "2021-09-11", "2021-09-24",
      "2024-06-03", "2024-10-14", "2024-11-16", "2024-11-29"
    )),
    tag = c("f14", "eve", rep(c("f180", "f47", "f14", "eve"), 2)),
    confirmatory = rep(c(TRUE, FALSE), c(2, 8))
  )
  expect_identical(nrow(TARGETS), 14L)
  expect_identical(TARGETS[-(1:4), ], earlier)
  t16 <- TARGETS[1:4, ]
  expect_identical(t16$E, rep(E16, 4))
  expect_identical(t16$F, DAYS16)
  expect_identical(as.integer(E16 - t16$F), c(180L, 47L, 14L, 1L))
  expect_identical(t16$tag, c("f180", "f47", "f14", "eve"))
  expect_identical(t16$confirmatory, rep(FALSE, 4))
})

test_that("each 2016 cell admits, by house, only polls first published by F, withholds MMR 2016-04-24 at f180 and the two eve polls whose fieldwork ended on F, and dates the polls published on their last fieldwork day end + 1 (a poll admitted before it was public, or on its last fieldwork day)", {
  skip_without_snapshot()
  pub <- readr::read_csv(root("harness", "sources", "poll_publication.csv"),
    col_types = readr::cols(date = "D", published = "D", .default = "c"), progress = FALSE
  )
  pub <- pub[pub$level == "national", c("date", "fyrirtaeki", "published")]
  admitted <- list(
    stats::setNames(c(38L, 29L), c("Gallup", "MMR")),
    stats::setNames(c(1L, 42L, 33L), c(FRETT16, "Gallup", "MMR")),
    stats::setNames(c(2L, 4L, 45L, 36L), c(FEL16, FRETT16, "Gallup", "MMR")),
    stats::setNames(c(4L, 7L, 45L, 37L), c(FEL16, FRETT16, "Gallup", "MMR"))
  )
  # Dated by F but not admitted. Publication keeps out two polls whose fieldwork had ended: MMR
  # 2016-04-24 (ended 26 April, first published 4 May) and the kjordaemi poll of
  # Felagsvisindastofnun 2016-10-24 (ended 27 October, first published by kjordaemi on 29 October).
  # Gallup 2016-09-07 was in the field until 14 September. Gallup 2016-10-26 and MMR 2016-10-27
  # ended on the eve itself; they have no publication row, and the fallback dates them.
  withheld <- list(
    "national 2016-04-24 MMR published",
    "national 2016-09-07 Gallup published",
    character(0),
    c("national 2016-10-26 Gallup house_max", "national 2016-10-27 MMR house_max", paste("kjordaemi 2016-10-24", FEL16, "published"))
  )
  for (i in seq_along(cells16)) {
    x <- cells16[[i]]
    lab <- format(x$F)
    polls <- dplyr::distinct(x$polls_nat, date, fyrirtaeki, available_from)
    expect_setequal(unique(polls$fyrirtaeki), names(admitted[[i]]))
    expect_identical(vapply(names(admitted[[i]]), \(h) sum(polls$fyrirtaeki == h), integer(1)), admitted[[i]], label = lab)
    seen <- dplyr::inner_join(polls, pub, by = c("date", "fyrirtaeki"))
    expect_true(all(seen$published <= x$F), label = lab)
    near <- polls[polls$available_from > x$F - 45, ]
    expect_identical(nrow(dplyr::anti_join(near, pub, by = c("date", "fyrirtaeki"))), 0L, label = lab)
    ex <- excluded_near(inp$availability, x$F)
    ex <- ex[ex$fyrirtaeki != ELECTION_HOUSE, ]
    expect_identical(paste(ex$level, format(ex$date), ex$fyrirtaeki, ex$rule), withheld[[i]], label = lab)
  }
  # WHY: Gallup 2016-04-21 was first published at 19:16 UTC on the f180 day, and the rule works in days.
  f180 <- cells16[[1]]$polls_nat
  expect_identical(unique(f180$available_from[f180$date == as.Date("2016-04-21") & f180$fyrirtaeki == "Gallup"]), DAYS16[1])
  mmr <- inp$availability[inp$availability$date == as.Date("2016-04-24") & inp$availability$fyrirtaeki == "MMR", ]
  expect_identical(mmr$available_from, as.Date("2016-05-04"))
  # WHY: Gallup 2016-04-05 and MMR 2016-10-23 were first published on their last fieldwork day, which
  # their publication rows alone would make the day they are admitted.
  av <- inp$availability
  last_day <- av[av$level == "national" & paste(format(av$date), av$fyrirtaeki) %in% c("2016-04-05 Gallup", "2016-10-23 MMR"), ]
  expect_identical(last_day$rule, c("correction", "correction"))
  expect_identical(last_day$fieldwork_end, as.Date(c("2016-04-06", "2016-10-26")))
  expect_identical(last_day$available_from, last_day$fieldwork_end + 1)
  expect_identical(unique(f180$available_from[f180$date == as.Date("2016-04-05") & f180$fyrirtaeki == "Gallup"]), as.Date("2016-04-07"))
  eve <- cells16[[4]]$polls_nat
  expect_identical(unique(eve$available_from[eve$date == as.Date("2016-10-23") & eve$fyrirtaeki == "MMR"]), as.Date("2016-10-27"))
})

# WHY: recorded on 2026-11-11 with admitted_digest() from the tree at b507364, before the 2016
# publication rows. Those rows move the available_from of 2016 polls, which every earlier cell's
# window holds; this digest blanks available_from on rows dated before 2017, so it pins which polls
# each earlier cell admits, their n, and the date of every poll from 2017.
admitted_digest <- function(info) {
  for (f in c("polls_nat", "polls_kd", "kosning_nat")) {
    info[[f]]$available_from[info[[f]]$date < as.Date("2017-01-01")] <- as.Date(NA)
  }
  cell_digest(info)
}
ADMITTED_DIGESTS <- tibble::tribble(
  ~E, ~F, ~digest,
  "2017-10-28", "2017-10-14", "0dd4c1b9a6dc2ab17ed551cd75fff4b33eb4575212aeddcb0926342109324798",
  "2017-10-28", "2017-10-27", "b7df959a45e17471df5bba72ac21fd46187c09f0141680c5f4dac3ca78c67076",
  "2021-09-25", "2021-03-29", "375c61220c7d28099af60f3f85c22e752c6c949b27fcc39dc036efcf0e01d306",
  "2021-09-25", "2021-08-09", "db4533954a49528b53b37380e10e4721886bc43c7bff0cf4638f2b74b53e5fab",
  "2021-09-25", "2021-09-11", "086611013720dd6b99417506fe39a21ab29c612c060fa39bb9d4be844b76f8e5",
  "2021-09-25", "2021-09-24", "3aa25173fd0cf7788910c89e3a221472d5698f230d8a520437c13f84b349f93d",
  "2024-11-30", "2024-06-03", "728279013dd37c50f3ac802d18ec1fde11ea64f1d1d6e2521ab092abcc1ecafe",
  "2024-11-30", "2024-10-14", "fcff6df683a2b2b6f86748a942a582e99acb97033187cd5c127d389b319a464e",
  "2024-11-30", "2024-11-16", "168fc9ceb3b0c4b7ff2916645a36cd73d81dc306f84a4f6cd58848d484d33e84",
  "2024-11-30", "2024-11-29", "fcd72a23a06324bb3d785e526beca97e45465147fce186bb0c436b5082c7a4d9"
)

test_that("every recorded TARGETS cell admits the polls it admitted before the 2016 publication rows, with the same n, and dates every poll from 2017 as it did (a publication row that admits or drops a poll in an existing cell)", {
  skip_without_snapshot()
  expect_identical(paste(ADMITTED_DIGESTS$E, ADMITTED_DIGESTS$F), paste(TARGET_DIGESTS$E, TARGET_DIGESTS$F))
  for (i in seq_len(nrow(ADMITTED_DIGESTS))) {
    d <- ADMITTED_DIGESTS[i, ]
    expect_identical(admitted_digest(info_set(d$E, d$F, inp)), d$digest, label = paste(d$E, d$F))
  }
})
