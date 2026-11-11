box::use(
  R / harness / anchors[fold_anchor],
  R / harness / availability[merge_same_day_polls, read_polling],
  R / harness / constants[ELECTION_HOUSE, KJORDAEMI, window_elections, window_start],
  R / harness / info_set[info_set, read_inputs, validate_inputs],
  R / harness / tables[valid_votes]
)
E16 <- as.Date("2016-10-29")
START16 <- as.Date("2013-04-27")
DAYS16 <- as.Date(c("2016-05-02", "2016-09-12", "2016-10-15", "2016-10-28"))
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
# WHY: recorded on 2026-11-11 with cell_digest() from the tree at b91407d, before the
# per-election window existed, so a change that reaches an existing cell shows here.
TARGET_DIGESTS <- tibble::tribble(
  ~E, ~F, ~digest,
  "2017-10-28", "2017-10-14", "77a87250ef1cc9e843a9ee9d891fd6fc8a12f48f6eb1744f6b87f140ac41095e",
  "2017-10-28", "2017-10-27", "763fe67c4117afda87425f596ef56d129cc34c154a2280cf2afc7f8c955bbc93",
  "2021-09-25", "2021-03-29", "8890c41ff415cbdd00e2ae890c6d01893528d02fccdd37dd9c41ac3e6356d37a",
  "2021-09-25", "2021-08-09", "e117d3290e7dd1aa926d3cb3674516a8853bbf0ac6abaecbba92bc71bf4cae9b",
  "2021-09-25", "2021-09-11", "7285a2eecd8148250c8236e92878d6e6054b3033f67f3a48cafba8768e308571",
  "2021-09-25", "2021-09-24", "480c17780a8aba3c3d17a6c77a54fc9876eb77c2d52cd27863b263e9262895a8",
  "2024-11-30", "2024-06-03", "1532ef8871dd57a48c2fe76acbd267aee2d2f84d7a1de8650234f38ca4f2722e",
  "2024-11-30", "2024-10-14", "29fde3dd720394ca905edc85c07441d055b0ed32b75449a5a4250c7b2f888db8",
  "2024-11-30", "2024-11-16", "cb179220045e6cc87a3f97493834bbc99a3021f507e975912c8f187430f9ff46",
  "2024-11-30", "2024-11-29", "b3cd8adac136088b91722ca7c7fad278f897719ab61c62502ad05d0129c5c144"
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
    expect_identical(nrow(x$kosning_nat), 0L, label = lab)
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

test_that("every recorded TARGETS cell has the polls, results and entry table it had before the 2016 window (an input change that reaches an existing cell)", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGET_DIGESTS))) {
    d <- TARGET_DIGESTS[i, ]
    expect_identical(cell_digest(info_set(d$E, d$F, inp)), d$digest, label = paste(d$E, d$F))
  }
})
