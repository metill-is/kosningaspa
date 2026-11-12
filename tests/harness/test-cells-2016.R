box::use(
  R / harness / anchors[fold_anchor],
  R / harness / availability[excluded_near, read_polling, with_older_polls],
  R / harness / constants[blank_party_rule, ELECTION_HOUSE, KJORDAEMI, window_elections, window_start],
  R / harness / info_set[blank_party_polls, cell_polls, info_set, read_inputs, validate_inputs],
  R / harness / older_polls[read_older_polls],
  R / harness / tables[entry_table, valid_votes],
  R / harness / truth[prev_national_shares]
)
E16 <- as.Date("2016-10-29")
START16 <- as.Date("2012-07-01")
ANCHOR16 <- as.Date("2013-04-27")
DAYS16 <- as.Date(c("2016-05-02", "2016-09-12", "2016-10-15", "2016-10-28"))
# The snapshot keys (date and house) that the older polls table's replacement polls name.
REPLACED16 <- c("2013-01-09 MMR", "2013-05-16 MMR", "2014-03-30 MMR", "2014-05-08 MMR", "2014-06-20 MMR")
older16 <- read_older_polls()
FRETT16 <- "Fr\u00e9ttabla\u00f0i\u00f0, St\u00f6\u00f0 2 og V\u00edsir"
FEL16 <- "F\u00e9lagsv\u00edsindastofnun"
inp <- if (snapshot_present()) read_inputs()
cells16 <- if (snapshot_present()) lapply(DAYS16, \(F) info_set(E16, F, inp))
# The national rows a 2016 cell reads before the blank-party rule: the snapshot's, with the n
# corrections, and the older polls table's.
raw16 <- if (snapshot_present()) with_older_polls(inp$polling, inp$older_polls)
# The admitted polls whose Annad a 2016 cell reads as not reported, found from the raw rows on
# 2026-11-12, by date: the f180, f47, f14 and eve cells read the first 2, 5, 10 and 13. `party`
# is the blank party's POLL_PARTIES index: 5 Piratar, 6 Vidreisn, 7 Flokkur folksins.
BLANK16 <- tibble::tribble(
  ~date, ~house, ~party,
  "2013-01-18", "MMR", 5L,
  "2016-04-04", "MMR", 6L,
  "2016-04-24", "MMR", 6L,
  "2016-08-26", "MMR", 7L,
  "2016-09-06", FRETT16, 7L,
  "2016-09-16", "MMR", 7L,
  "2016-09-23", "MMR", 7L,
  "2016-09-26", FRETT16, 7L,
  "2016-10-03", FRETT16, 7L,
  "2016-10-10", FRETT16, 7L,
  "2016-10-17", FRETT16, 7L,
  "2016-10-24", FRETT16, 7L,
  "2016-10-25", FRETT16, 7L
)
BLANK16_N <- c(2L, 5L, 10L, 13L)

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

test_that("2016's window opens on 2012-07-01, ten months before the 2013 election, with 2013 and 2016 as its elections, and every other window is as before (a window still opening at the 2013 election, which leaves it no poll before that election, or a global window moved, which would change every finished run's data)", {
  expect_identical(window_start(E16), START16)
  expect_identical(window_elections(E16), c(ANCHOR16, E16))
  expect_identical(WINDOW_START, as.Date("2016-01-01"))
  expect_identical(ELECTIONS, as.Date(c("2016-10-29", "2017-10-28", "2021-09-25", "2024-11-30")))
  others <- ELECTIONS[-1]
  expect_identical(window_start(others), rep(WINDOW_START, 3))
  for (E in format(others)) expect_identical(window_elections(E), ELECTIONS, label = E)
  expect_identical(window_start(c("2024-11-30", "2016-10-29")), as.Date(c("2016-01-01", "2012-07-01")))
  expect_identical(min(window_start(ELECTIONS)), START16)
  expect_error(window_start("2013-04-27"), "not a window election: 2013-04-27", fixed = TRUE)
  expect_error(window_elections(ELECTIONS), "E must be one date", fixed = TRUE)
})

test_that("each 2016 forecast day builds and validates, reads no poll before 2012-07-01 or past F, and takes its previous results from the 2013 anchor (a cell that silently reads no previous result, or 2016's own)", {
  skip_without_snapshot()
  anchor <- fold_anchor(inp$anchor_2013)
  for (x in cells16) {
    lab <- format(x$F)
    expect_no_error(validate_inputs(x))
    expect_identical(x$window_start, START16)
    expect_identical(x$elections, c(ANCHOR16, E16))
    polls <- rbind(x$polls_nat[c("date", "available_from")], x$polls_kd[c("date", "available_from")])
    expect_true(all(polls$date >= START16), label = lab)
    expect_true(all(polls$available_from <= x$F), label = lab)
    expect_identical(unique(x$results$election), ANCHOR16)
    expect_setequal(x$results$kjordaemi, KJORDAEMI$kjordaemi)
    expect_equal(sum(x$results$votes), 189023)
    expect_equal(x$results, anchor)
    expect_equal(x$valid_votes_open, valid_votes(anchor, ANCHOR16))
    expect_equal(sum(x$valid_votes_open$votes), 189023)
    # WHY: the polling data hold no 2013 Kosning row, so info_set() builds one from the anchor
    # for R0; without it R0 fits a 2016 cell with no national 2013 result.
    k <- x$kosning_nat
    expect_identical(unique(k$date), ANCHOR16, label = lab)
    expect_identical(unique(k$fyrirtaeki), ELECTION_HOUSE, label = lab)
    expect_identical(anyDuplicated(k$flokkur), 0L, label = lab)
    expect_setequal(k$flokkur, x$parties)
    expect_equal(sum(k$n), 189023, label = lab)
  }
})

# The national rows of `rows` dated before 2016, for `parties`, sorted; and the poll key of each row.
early_rows <- function(rows, parties) {
  rows <- rows[rows$date < WINDOW_START & rows$fyrirtaeki != ELECTION_HOUSE & rows$flokkur %in% parties, c("date", "fyrirtaeki", "flokkur", "n")]
  rows[order(rows$date, rows$fyrirtaeki, match(rows$flokkur, POLL_PARTIES), method = "radix"), ]
}
poll_key <- function(rows) paste(format(rows$date), rows$fyrirtaeki)

test_that("each 2016 cell reads every poll of the older polls table with the table's counts (but MMR 2013-01-18's Annad, read as not reported), replacements included, the snapshot's other polls before 2016 with their own, no row of a replaced snapshot key and no poll twice (a misdated or doubled MMR row read beside its replacement, a table poll dropped, or a poll read twice)", {
  skip_without_snapshot()
  raw <- read_polling()
  polls <- dplyr::distinct(older16, poll_id, date, fyrirtaeki, replaces)
  expect_identical(nrow(polls), 77L)
  expect_identical(sum(!is.na(polls$replaces)), 6L)
  expect_identical(sort(unique(paste(format(polls$replaces), polls$fyrirtaeki)[!is.na(polls$replaces)]), method = "radix"), REPLACED16)
  snap <- raw[!poll_key(raw) %in% REPLACED16, ]
  expect_length(unique(poll_key(early_rows(raw, POLL_PARTIES))), 58)
  expect_length(unique(poll_key(early_rows(snap, POLL_PARTIES))), 53)
  for (x in cells16) {
    lab <- format(x$F)
    got <- early_rows(x$polls_nat, x$parties)
    expect_identical(anyDuplicated(got[c("date", "fyrirtaeki", "flokkur")]), 0L, label = lab)
    from_table <- poll_key(got) %in% poll_key(polls)
    expect_setequal(unique(poll_key(got)[from_table]), poll_key(polls))
    # WHY: MMR 2013-01-18 leaves blank Piratar, whom Gallup reported from 2012-12-13, so the cell reads its Annad as not reported.
    want <- early_rows(older16, x$parties)
    want$n[poll_key(want) == "2013-01-18 MMR" & want$flokkur == POLL_PARTIES[10]] <- 0
    expect_equal(got[from_table, ], want, ignore_attr = TRUE, label = lab)
    expect_equal(got[!from_table, ], early_rows(snap, x$parties), ignore_attr = TRUE, label = lab)
    expect_false("2013-01-09 MMR" %in% poll_key(got), label = lab)
  }
})

test_that("each 2016 cell admits 40 national polls dated before the 2013 election, all from the older polls table: Gallup 13, MMR 15, Frettabladid 7 and Felagsvisindastofnun 5 (a window still opening at the election, which leaves that election no poll before it)", {
  skip_without_snapshot()
  want <- stats::setNames(c(5L, 7L, 13L, 15L), c(FEL16, FRETT16, "Gallup", "MMR"))
  for (x in cells16) {
    pre <- dplyr::distinct(x$polls_nat[x$polls_nat$date < ANCHOR16, ], date, fyrirtaeki)
    expect_identical(vapply(names(want), \(h) sum(pre$fyrirtaeki == h), integer(1)), want, label = format(x$F))
    expect_true(all(poll_key(pre) %in% poll_key(older16)), label = format(x$F))
  }
})

test_that("MMR's doubled key 2013-05-16 enters a 2016 cell as the table's two polls of 17 May and 18 June 2013, each with its own row per party and its own availability row (two polls summed under one key, or read beside their replacements)", {
  skip_without_snapshot()
  two <- older16[older16$replaces %in% as.Date("2013-05-16"), ]
  expect_identical(sort(unique(two$poll_id)), c("M-2013-05-16", "M-2013-06-17"))
  for (x in cells16) {
    for (id in unique(two$poll_id)) {
      t <- two[two$poll_id == id & two$flokkur %in% x$parties, ]
      m <- x$polls_nat[x$polls_nat$date == t$date[1] & x$polls_nat$fyrirtaeki == "MMR", ]
      expect_identical(m$flokkur, x$parties, label = id)
      expect_equal(m$n, t$n[match(m$flokkur, t$flokkur)], label = id)
    }
  }
  av <- inp$availability[inp$availability$fyrirtaeki == "MMR" & inp$availability$date %in% unique(two$date), ]
  expect_identical(av$date, sort(unique(two$date)))
  expect_identical(av$rule, rep("older_polls", 2))
  expect_identical(av$available_from, av$fieldwork_end + 1)
})

test_that("with_older_polls drops the rows of each replaced key and adds the table's, and stops on a table poll from 2016, two table polls on one key, a replaced key naming no poll, or a table poll on a kept poll's key (a replacement read beside the row it replaces, or a snapshot row corrected by a refresh read twice)", {
  p <- tibble::tibble(
    date = as.Date(rep(c("2013-01-09", "2013-06-01", "2016-03-01"), each = 2)), fyrirtaeki = "H",
    flokkur = rep(c("A", "B"), 3), n = c(1, 2, 3, 4, 5, 6), lokadagur = "NA"
  )
  o <- tibble::tibble(
    date = as.Date(rep(c("2013-07-10", "2012-08-01"), each = 2)), fyrirtaeki = "H", flokkur = rep(c("A", "B"), 2),
    n = c(10, 20, 30, 40), poll_id = rep(c("H1", "H2"), each = 2), replaces = as.Date(rep(c("2013-01-09", NA), each = 2))
  )
  out <- with_older_polls(p, o)
  expect_identical(out[1:4, ], p[3:6, ])
  expect_identical(out$date[5:8], o$date)
  expect_equal(out$n, c(3, 4, 5, 6, 10, 20, 30, 40))
  expect_identical(out$lokadagur, c(rep("NA", 4), rep(NA_character_, 4)))
  late <- o
  late$date[3:4] <- as.Date("2016-01-01")
  expect_error(with_older_polls(p, late), "the older polls table holds polls dated from 2016-01-01: H2", fixed = TRUE)
  same <- o
  same$date[3:4] <- as.Date("2013-07-10")
  expect_error(with_older_polls(p, same), "older polls share a date and house: 2013-07-10 H", fixed = TRUE)
  gone <- o
  gone$replaces[1:2] <- as.Date("2013-01-10")
  expect_error(with_older_polls(p, gone), "replaced key(s) naming no poll: 2013-01-10 H", fixed = TRUE)
  clash <- o
  clash$date[3:4] <- as.Date("2013-06-01")
  expect_error(with_older_polls(p, clash), "older poll(s) with the date and house of a poll they do not replace: 2013-06-01 H", fixed = TRUE)
})

test_that("a cell's national polls stop on a poll that lists a party twice (two polls under one key read as one, as the snapshot's MMR 2013-05-16 would be)", {
  nat <- tibble::tibble(date = as.Date("2013-05-16"), fyrirtaeki = "H", flokkur = rep(POLL_PARTIES[c(1, 10)], 2), n = c(1, 2, 3, 4))
  kd <- tibble::tibble(date = as.Date(character()), fyrirtaeki = character(), flokkur = character(), kjordaemi = character(), n = numeric())
  av <- tibble::tibble(
    level = "national", date = as.Date("2013-05-16"), fyrirtaeki = "H", fieldwork_end = as.Date("2013-05-17"),
    available_from = as.Date("2013-05-18"), rule = "older_polls"
  )
  expect_error(cell_polls(E16, DAYS16[1], nat, kd, av, START16), "national poll(s) listing a party twice: H 2013-05-16", fixed = TRUE)
  expect_identical(nrow(cell_polls(E16, DAYS16[1], nat[1:2, ], kd, av, START16)$polls_nat), 2L)
})

test_that("the blank-party rule on made-up rows: Annad is read as not reported in a poll that leaves blank a party some admitted poll of any house, its own included, reported on or before its date, and nothing else changes; a poll before that day, one reporting the party, one with no Annad and one whose only earlier report was not public by F are untouched; with the rule off nothing changes; only 2016 cells take the rule (a remainder read as Annad alone while it holds an existing party's support)", {
  D <- POLL_PARTIES[1]
  C <- POLL_PARTIES[6]
  A <- POLL_PARTIES[10]
  poll <- function(date, house, flokkur, n) tibble::tibble(date = as.Date(date), fyrirtaeki = house, flokkur = flokkur, n = n)
  nat <- rbind(
    poll("2016-02-01", "H2", c(D, C, A), c(10, 2, 3)),
    poll("2016-02-15", "H1", c(D, C, A), c(10, 0, 5)),
    poll("2016-03-13", "H1", c(D, C, A), c(10, 3, 2)),
    # WHY: taken on the day of the first admitted report of C; `<` for `<=` in the rule leaves its Annad.
    poll("2016-03-13", "H3", c(D, C, A), c(10, 0, 6)),
    poll("2016-04-04", "H2", c(D, C, A), c(10, 0, 4)),
    poll("2016-04-10", "H2", c(D, A), c(10, 4)),
    # WHY: the reporting house's own later poll, with no other house's report of C admitted before it;
    # a rule that counts other houses' reports only leaves its Annad.
    poll("2016-04-12", "H1", c(D, C, A), c(10, 0, 3)),
    poll("2016-04-15", "H1", c(D, C, A), c(10, 0, 0)),
    poll("2016-04-20", "H2", c(D, C, A), c(10, 2, 3))
  )
  F <- DAYS16[1]
  av <- dplyr::mutate(dplyr::distinct(nat, date, fyrirtaeki), level = "national", fieldwork_end = date + 1, available_from = date + 2, rule = "older_polls")
  av$available_from[av$date == as.Date("2016-02-01")] <- F + 1
  kd <- tibble::tibble(date = as.Date(character()), fyrirtaeki = character(), flokkur = character(), kjordaemi = character(), n = numeric())
  admitted <- nat[nat$date != as.Date("2016-02-01"), ]
  off <- cell_polls(E16, F, nat, kd, av, START16)
  on <- cell_polls(E16, F, nat, kd, av, START16, blank_party_rule = TRUE)
  expect_identical(paste(off$polls_nat$date, off$polls_nat$fyrirtaeki, off$polls_nat$flokkur), paste(admitted$date, admitted$fyrirtaeki, admitted$flokkur))
  expect_equal(off$polls_nat$n, admitted$n)
  want <- admitted$n
  named <- c("2016-03-13 H3", "2016-04-04 H2", "2016-04-10 H2", "2016-04-12 H1")
  want[admitted$flokkur == A & paste(admitted$date, admitted$fyrirtaeki) %in% named] <- 0
  expect_equal(on$polls_nat$n, want)
  expect_identical(on$polls_nat[c("date", "fyrirtaeki", "flokkur", "available_from")], off$polls_nat[c("date", "fyrirtaeki", "flokkur", "available_from")])
  expect_identical(on$parties, c(D, C, A))
  expect_identical(on$parties, off$parties)
  blank <- blank_party_polls(admitted)
  expect_identical(paste(blank$date, blank$fyrirtaeki, blank$blank), paste(named, C))
  expect_identical(nrow(blank_party_polls(admitted[0, ])), 0L)
  expect_identical(blank_party_rule(ELECTIONS), c(TRUE, FALSE, FALSE, FALSE))
  expect_error(blank_party_rule("2013-04-27"), "not a window election: 2013-04-27", fixed = TRUE)
})

test_that("blank_party_polls() stops on a positive count that rounds to 0, which it would call reported while both arms, which round the counts, read it as blank (a party reported for the rule and blank for the arms, its poll's Annad kept)", {
  poll <- function(date, house, flokkur, n) tibble::tibble(date = as.Date(date), fyrirtaeki = house, flokkur = flokkur, n = n)
  rows <- rbind(
    poll("2016-03-13", "H1", POLL_PARTIES[c(1, 6, 10)], c(10, 3, 2)),
    poll("2016-04-04", "H2", POLL_PARTIES[c(1, 6, 10)], c(10, 0.5, 4))
  )
  expect_error(
    blank_party_polls(rows),
    paste("a positive count that rounds to 0, which the arms read as not reported: 2016-04-04 H2", POLL_PARTIES[6], "0.5"),
    fixed = TRUE
  )
  rows$n[5] <- 0.3
  expect_error(blank_party_polls(rows), "rounds to 0", fixed = TRUE)
  rows$n[5] <- 0.6
  expect_identical(nrow(blank_party_polls(rows)), 0L)
  rows$n[5] <- 0
  expect_identical(blank_party_polls(rows)$blank, POLL_PARTIES[6])
})

# The oracle of the blank-party rule, written apart from blank_party_polls(): each admitted poll
# with a positive Annad, and each party that some admitted poll of any house reported with a
# positive count on or before its date and that it does not, as BLANK16's columns.
blank_oracle <- function(adm) {
  out <- list()
  for (key in unique(paste(adm$date, adm$fyrirtaeki))) {
    poll <- adm[paste(adm$date, adm$fyrirtaeki) == key, ]
    if (!sum(poll$n[poll$flokkur == POLL_PARTIES[10]]) > 0) next
    for (p in 1:9) {
      seen <- adm$date[adm$flokkur == POLL_PARTIES[p] & adm$n > 0]
      if (any(seen <= poll$date[1]) && !any(poll$n[poll$flokkur == POLL_PARTIES[p]] > 0)) {
        out[[length(out) + 1]] <- tibble::tibble(date = format(poll$date[1]), house = poll$fyrirtaeki[1], party = p)
      }
    }
  }
  out <- dplyr::bind_rows(out)
  out[order(out$date, out$house, out$party, method = "radix"), ]
}

# The raw national rows a 2016 cell admits on F, with availability.csv's available_from.
admitted_raw <- function(F) {
  av <- inp$availability[inp$availability$level == "national", c("date", "fyrirtaeki", "available_from")]
  rows <- raw16[raw16$date >= START16 & raw16$fyrirtaeki != ELECTION_HOUSE, c("date", "fyrirtaeki", "flokkur", "n")]
  rows <- dplyr::inner_join(rows, av, by = c("date", "fyrirtaeki"), relationship = "many-to-one")
  rows[rows$available_from <= F, ]
}

test_that("each 2016 cell reads Annad as not reported in exactly the admitted polls that leave blank a party some admitted poll reported on or before their date, and every other count as the raw row's (the rule changing a poll it should not, or missing one)", {
  skip_without_snapshot()
  for (i in seq_along(cells16)) {
    x <- cells16[[i]]
    lab <- format(x$F)
    want <- BLANK16[seq_len(BLANK16_N[i]), ]
    adm <- admitted_raw(x$F)
    expect_identical(blank_oracle(adm), want, label = lab)
    got <- dplyr::left_join(x$polls_nat, adm[c("date", "fyrirtaeki", "flokkur", "n")],
      by = c("date", "fyrirtaeki", "flokkur"), suffix = c("", "_raw"), relationship = "one-to-one"
    )
    expect_false(anyNA(got$n_raw), label = lab)
    moved <- got[got$n != got$n_raw, ]
    expect_identical(paste(format(moved$date), moved$fyrirtaeki, moved$flokkur, moved$n), paste(want$date, want$house, POLL_PARTIES[10], 0), label = lab)
    expect_identical(nrow(dplyr::anti_join(adm[adm$n > 0 & adm$flokkur != POLL_PARTIES[10], ], x$polls_nat, by = c("date", "fyrirtaeki", "flokkur"))), 0L, label = lab)
  }
})

# WHY: the only real-data days on which a rule that decided existence from every poll of the
# window would differ from one reading the admitted polls (checked on 2026-11-12 against the raw
# rows and availability.csv for every buildable day of 2016).
test_that("a 2016 cell decides that a party exists from the polls it admits: on 2016-08-30 and 08-31 it admits MMR 2016-08-26, which leaves Flokkur folksins blank, but not Gallup 2016-08-13, the first poll to report it, public from 2016-09-01, so it reads MMR's Annad as the data hold it and Flokkur folksins is not a party; from 2016-09-01 it reads that Annad as not reported (deciding that a party exists from polls not yet public on F)", {
  skip_without_snapshot()
  ff <- POLL_PARTIES[7]
  annad <- POLL_PARTIES[10]
  av <- inp$availability[inp$availability$level == "national", ]
  available <- function(date, house) av$available_from[av$date == as.Date(date) & av$fyrirtaeki == house]
  expect_identical(available("2016-08-26", "MMR"), as.Date("2016-08-30"))
  expect_identical(available("2016-08-13", "Gallup"), as.Date("2016-09-01"))
  reports <- raw16[raw16$flokkur == ff & raw16$n > 0 & raw16$fyrirtaeki != ELECTION_HOUSE, ]
  first <- reports[order(reports$date, method = "radix"), ][1, ]
  expect_identical(paste(format(first$date), first$fyrirtaeki), "2016-08-13 Gallup")
  expect_equal(first$n, 3.1581197604790421, tolerance = 1e-12)
  mmr <- raw16[raw16$date == as.Date("2016-08-26") & raw16$fyrirtaeki == "MMR", ]
  expect_identical(mmr$n[mmr$flokkur == ff], 0)
  expect_equal(mmr$n[mmr$flokkur == annad], 113.993993993994, tolerance = 1e-12)
  read_on <- function(F) {
    x <- info_set(E16, F, inp)
    p <- x$polls_nat
    list(annad = p$n[p$date == as.Date("2016-08-26") & p$fyrirtaeki == "MMR" & p$flokkur == annad], ff = ff %in% x$parties)
  }
  for (F in c("2016-08-30", "2016-08-31")) {
    r <- read_on(F)
    expect_identical(r$annad, mmr$n[mmr$flokkur == annad], label = F)
    expect_false(r$ff, label = F)
  }
  r <- read_on("2016-09-01")
  expect_identical(r$annad, 0)
  expect_true(r$ff)
})

test_that("validate_inputs() refuses a 2016 cell with a positive Annad beside a blank existing party, and passes a 2021 cell that holds the same raw 2016 row (a build path that skips the rule, or the rule reaching another election's cells)", {
  skip_without_snapshot()
  is_poll <- function(rows) rows$date == as.Date("2016-10-25") & rows$fyrirtaeki == FRETT16 & rows$flokkur == POLL_PARTIES[10]
  raw <- raw16$n[is_poll(raw16)]
  expect_length(raw, 1)
  expect_gt(raw, 0)
  x <- cells16[[4]]
  expect_identical(x$polls_nat$n[is_poll(x$polls_nat)], 0)
  x$polls_nat$n[is_poll(x$polls_nat)] <- raw
  expect_error(validate_inputs(x), paste(POLL_PARTIES[10], "counted beside a blank party: national 2016-10-25", FRETT16), fixed = TRUE)
  y <- info_set("2021-09-25", "2021-09-24", inp)
  expect_equal(y$polls_nat$n[is_poll(y$polls_nat)], raw)
  expect_true("2016-10-25" %in% format(blank_party_polls(y$polls_nat)$date))
  expect_no_error(validate_inputs(y))
})

test_that("a 2016 cell's previous shares are the official 2013 result, each party's votes over the 189 023 valid votes of anchor_2013.csv and NA for a party with no list, not the fundamentals table's rounded shares, while a 2021 cell's are prev_national_shares() of each election as before (R0's newcomer check comparing the fundamentals table with itself, or a later cell's inputs moved)", {
  skip_without_snapshot()
  a <- readr::read_csv(root("harness", "truth", "anchor_2013.csv"),
    col_types = readr::cols(election = "D", votes = "d", seats = "d", ran = "l", .default = "c"), progress = FALSE
  )
  expect_identical(sum(a$votes), 189023)
  votes <- tapply(a$votes, a$flokkur, sum)
  ran <- intersect(POLL_PARTIES, a$flokkur[a$ran])
  want <- stats::setNames(rep(NA_real_, length(POLL_PARTIES)), POLL_PARTIES)
  want[ran] <- votes[ran] / 189023
  expect_identical(names(want)[is.na(want)], POLL_PARTIES[6:9])
  expect_lt(abs(100 * want[[10]] - 20.04), 0.005)
  fund <- prev_national_shares(E16, inp$truth, inp$fundamentals)
  expect_lt(abs(100 * fund[[10]] - 20.16), 0.005)
  for (x in cells16) {
    s <- x$prev_shares
    expect_identical(s$election, rep(E16, 10))
    expect_identical(s$previous, rep(ANCHOR16, 10))
    expect_identical(s$flokkur, POLL_PARTIES)
    expect_equal(s$share, unname(want), label = format(x$F))
  }
  y <- info_set("2021-09-25", "2021-09-24", inp)
  window <- ELECTIONS[ELECTIONS <= as.Date("2021-09-25")]
  expect_identical(y$prev_shares$share, unlist(lapply(window, \(e) unname(prev_national_shares(e, inp$truth, inp$fundamentals)[POLL_PARTIES]))))
})

test_that("a 2016 cell takes its entry dates from the polls it reads and numbers epochs from 2013: the five older lists present from the window's first poll, Piratar entering with its first reported share in the older polls table, Vidreisn and Flokkur folksins in epoch 2, while a 2024 cell keeps the committed table (an entry date taken from the misdated snapshot row, epochs counted from the wrong elections, or every party a newcomer for want of a poll before the window)", {
  skip_without_snapshot()
  pirates <- POLL_PARTIES[5]
  lists5 <- POLL_PARTIES[c(1:4, 10)]
  newcomers <- POLL_PARTIES[6:7]
  first_poll <- min(older16$date)
  first_p <- min(older16$date[older16$flokkur == pirates & older16$n > 0])
  expect_identical(c(first_poll, first_p), as.Date(c("2012-07-12", "2012-12-13")))
  for (x in cells16) {
    lab <- format(x$F)
    e <- x$entry
    expect_setequal(e$flokkur, x$parties)
    expect_true(all(c(lists5, pirates) %in% e$flokkur), label = lab)
    expect_identical(e$first_nonzero[match(c(lists5, pirates), e$flokkur)], c(rep(first_poll, 5), first_p), label = lab)
    expect_identical(e$entry_epoch, ifelse(e$flokkur %in% newcomers, 2L, 1L), label = lab)
    expect_identical(e$newcomer, e$flokkur %in% c(pirates, newcomers), label = lab)
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

test_that("entry_table with first_poll_present counts the parties of the window's first poll as present and later ones as newcomers, and stops if a poll precedes the window (old lists made newcomers for want of a poll before the window, or the stand-in used where such polls exist)", {
  p <- tibble::tibble(
    date = as.Date(c("2012-07-12", "2012-07-12", "2012-12-13", "2013-04-27")), fyrirtaeki = c("G", "G", "G", "Kosning"),
    flokkur = c("A", "B", "C", "D"), n = c(5, 5, 1, 9)
  )
  el <- c(ANCHOR16, E16)
  e <- entry_table(p, START16, el, first_poll_present = TRUE)
  expect_identical(e$flokkur, c("A", "B", "C"))
  expect_identical(e$newcomer, c(FALSE, FALSE, TRUE))
  expect_identical(e$entry_epoch, c(1L, 1L, 1L))
  expect_identical(entry_table(p, START16, el)$newcomer, c(TRUE, TRUE, TRUE))
  early <- rbind(p, tibble::tibble(date = as.Date("2012-06-30"), fyrirtaeki = "G", flokkur = "A", n = 1))
  expect_error(entry_table(early, START16, el, first_poll_present = TRUE), "a poll is dated before window_start", fixed = TRUE)
  expect_identical(entry_table(early, START16, el)$newcomer, c(FALSE, TRUE, TRUE))
})

test_that("a 2016 cell with a poll from before 2012-07-01, a table poll admitted before it was public, or any cell whose window is not its election's, is refused (a pre-window poll admitted, a leak through the older polls table, or a moved window handed to the arms)", {
  skip_without_snapshot()
  x <- cells16[[1]]
  x$polls_nat$date[1] <- as.Date("2012-06-30")
  expect_error(validate_inputs(x), paste("poll before the window: national 2012-06-30", x$polls_nat$fyrirtaeki[1]), fixed = TRUE)
  table_poll <- cells16[[1]]$polls_nat$date == as.Date("2013-01-16") & cells16[[1]]$polls_nat$fyrirtaeki == FRETT16
  expect_true(any(table_poll))
  x <- cells16[[1]]
  x$polls_nat$available_from[table_poll] <- x$F + 1
  expect_error(validate_inputs(x), paste("poll available after F: national 2013-01-16", FRETT16), fixed = TRUE)
  x <- cells16[[1]]
  x$polls_nat$available_from[table_poll] <- as.Date("2013-01-16")
  expect_error(validate_inputs(x), paste("poll available on or before its midpoint: national 2013-01-16", FRETT16), fixed = TRUE)
  x <- cells16[[1]]
  x$window_start <- WINDOW_START
  expect_error(validate_inputs(x), "window not E's: window_start 2016-01-01", fixed = TRUE)
  x <- cells16[[1]]
  x$elections <- ELECTIONS
  expect_error(validate_inputs(x), "window not E's: elections 2016-10-29 2017-10-28 2021-09-25 2024-11-30", fixed = TRUE)
  y <- info_set("2024-11-30", "2024-11-29", inp)
  y$window_start <- START16
  expect_error(validate_inputs(y), "window not E's: window_start 2012-07-01", fixed = TRUE)
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
  # WHY: the snapshot's polls plus the table's (Felagsvisindastofnun 7, Frettabladid 17, Gallup 13, MMR 40)
  # less the five replaced snapshot keys in the window, all MMR.
  houses <- c(FEL16, FRETT16, "Gallup", "MMR")
  admitted <- list(
    stats::setNames(c(7L, 17L, 51L, 65L), houses),
    stats::setNames(c(7L, 18L, 55L, 69L), houses),
    stats::setNames(c(9L, 21L, 58L, 72L), houses),
    stats::setNames(c(11L, 24L, 58L, 73L), houses)
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

# WHY: recorded on 2026-11-11 from harness/availability.csv at a6e2cf7, before the older polls were
# read, without the national rows of the four replaced keys it held (MMR 2013-05-16, 2014-03-30,
# 2014-05-08 and 2014-06-20): the header and its 366 other lines.
AVAILABILITY_KEPT_DIGEST <- "ec06fd8fab47c232e6f599d861beb64e6912f4de03bfda1ecfc30384fab387ef"

test_that("availability.csv keeps every row it held before the older polls but those of the replaced keys, and gives each of the table's 77 polls one row, dated by its fieldwork end and publication day, or the house fallback where it has neither (a snapshot row moved, a replaced row left in, or a table poll admitted before it was public)", {
  lines <- readLines(root("harness", "availability.csv"), encoding = "UTF-8")
  av <- readr::read_csv(I(lines), col_types = readr::cols(date = "D", fieldwork_end = "D", available_from = "D", .default = "c"), progress = FALSE)
  polls <- dplyr::distinct(older16, date, fyrirtaeki, fieldwork_end, published)
  expect_identical(nrow(polls), 77L)
  from_table <- av$level == "national" & poll_key(av) %in% poll_key(polls)
  expect_identical(sum(from_table), 77L)
  kept <- c(lines[1], lines[-1][!from_table])
  expect_identical(digest::digest(paste(kept, collapse = "\n"), algo = "sha256", serialize = FALSE), AVAILABILITY_KEPT_DIGEST)
  expect_false(any(poll_key(av[av$level == "national", ]) %in% setdiff(REPLACED16, poll_key(polls))))
  t <- av[from_table, ]
  t <- t[match(poll_key(polls), poll_key(t)), ]
  known <- !is.na(polls$fieldwork_end) | !is.na(polls$published)
  expect_identical(sum(!known), 2L)
  expect_identical(t$available_from[known], pmax(polls$fieldwork_end + 1, polls$published, na.rm = TRUE)[known])
  rule <- ifelse(is.na(polls$fieldwork_end), "published", ifelse(!is.na(polls$published) & polls$published > polls$fieldwork_end + 1, "published", "older_polls"))
  expect_identical(t$rule[known], rule[known])
  expect_identical(t$rule[!known], rep("house_max", 2))
  expect_true(all(t$available_from > t$date & t$available_from < as.Date("2016-01-01")))
})

# WHY: the oracle reads harness/availability.csv itself, not the code under test, as
# tests/harness/test-sweep.R does for 2017, 2021 and 2024, whose oracle starts at WINDOW_START
# and so cannot cover 2016.
test_that("every day of the 2016 cycle from 2016-04-07 builds and validates, admits exactly the national polls of the 2016 window that availability.csv makes public by F, reads every count as the raw row holds it but the Annad of the polls the raw-row oracle names for that day, read as 0, never drops a poll or party, and keeps its results and previous shares; from 2016-03-10 to 2016-04-06 it stops for want of a cabinet, and before that on the economy release (an availability row or window rule that admits a poll before it was public, or the blank-party rule changing a poll it should not, on a 2016 day other than the four target days)", {
  skip_without_snapshot()
  skip_unless_slow()
  av <- readr::read_csv(
    root("harness", "availability.csv"),
    col_types = readr::cols(date = "D", fieldwork_end = "D", available_from = "D", .default = "c"),
    progress = FALSE
  )
  keys <- function(rows) sort(unique(paste(rows$date, rows$fyrirtaeki)), method = "radix")
  withheld <- setdiff(REPLACED16, poll_key(dplyr::distinct(older16, date, fyrirtaeki)))
  expect_identical(withheld, "2013-01-09 MMR")
  oracle <- function(F) {
    rows <- av[av$level == "national" & av$fyrirtaeki != ELECTION_HOUSE & av$date >= START16 & av$date < E16 & av$available_from <= F, ]
    setdiff(keys(rows), withheld)
  }
  # The cell's counts that are not the raw row's, and the Annad rows, read as 0, of the polls the
  # oracle names on F; each as "date house flokkur n".
  moved <- function(x) {
    got <- dplyr::left_join(x$polls_nat, admitted_raw(x$F)[c("date", "fyrirtaeki", "flokkur", "n")],
      by = c("date", "fyrirtaeki", "flokkur"), suffix = c("", "_raw"), relationship = "one-to-one"
    )
    got <- got[is.na(got$n_raw) | got$n != got$n_raw, ]
    paste(format(got$date), got$fyrirtaeki, got$flokkur, got$n)
  }
  named <- function(F) {
    want <- blank_oracle(admitted_raw(F))
    unique(paste(want$date, want$house, POLL_PARTIES[10], 0))
  }
  attempt <- function(F) tryCatch(
    {
      x <- info_set(E16, F, inp)
      validate_inputs(x)
      list(
        nat = keys(x$polls_nat), kd = keys(x$polls_kd), parties = x$parties, fixed = list(x$results, x$prev_shares),
        late = max(x$polls_nat$date) >= F || max(c(x$polls_nat$available_from, x$polls_kd$available_from)) > F,
        moved = moved(x), named = named(F)
      )
    },
    error = conditionMessage
  )
  days <- seq(as.Date("2016-04-07"), E16 - 1, by = "day")
  expect_length(days, 205)
  expect_true(all(DAYS16 %in% days))
  res <- lapply(days, attempt)
  ok <- vapply(res, is.list, logical(1))
  expect_true(all(ok), label = paste("days that do not build:", paste(format(days[!ok]), collapse = " ")))
  good <- res[ok]
  on <- days[ok]
  expect_false(any(vapply(good, `[[`, logical(1), "late")))
  same <- vapply(seq_along(good), \(j) identical(good[[j]]$nat, oracle(on[j])), logical(1))
  expect_true(all(same), label = paste("days whose polls are not the oracle's:", paste(format(on[!same]), collapse = " ")))
  as_named <- vapply(good, \(g) identical(g$moved, g$named), logical(1))
  expect_true(all(as_named), label = paste("days whose changed counts are not the named polls' Annad:", paste(format(on[!as_named]), collapse = " ")))
  expect_identical(lengths(lapply(good[match(DAYS16, on)], `[[`, "named")), BLANK16_N)
  grows <- vapply(seq_along(good)[-1], \(j) {
    a <- good[[j - 1]]
    b <- good[[j]]
    all(a$nat %in% b$nat) && all(a$kd %in% b$kd) && all(a$parties %in% b$parties)
  }, logical(1))
  expect_true(all(grows), label = paste("days that drop a poll or party:", paste(format(on[-1][!grows]), collapse = " ")))
  expect_true(all(vapply(good[-1], \(g) identical(g$fixed, good[[1]]$fixed), logical(1))))

  cabinet <- seq(as.Date("2016-03-10"), as.Date("2016-04-06"), by = "day")
  expect_length(cabinet, 28)
  expect_identical(vapply(cabinet, attempt, ""), paste("no cabinet in office on", format(cabinet)))
  economy <- c(ANCHOR16 + 1, seq(as.Date("2016-03-01"), as.Date("2016-03-09"), by = "day"))
  msg <- vapply(economy, attempt, "")
  expect_true(all(grepl("is before the first release of economy covariate(s) of E = 2016-10-29: ", msg, fixed = TRUE)))
  expect_true(all(grepl("growth (2015, first released 2016-03-10)", msg, fixed = TRUE)))
})
