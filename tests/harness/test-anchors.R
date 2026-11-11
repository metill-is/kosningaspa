box::use(
  R / harness / anchors[anchor_national, build_anchor_2013, fold_anchor, read_lists_2013, write_anchor_2013],
  R / harness / constants[FUNDAMENTALS_RENAMED, POLL_PARTIES],
  R / harness / inputs[snapshot_file],
  R / election_utils[calculate_seats, seats_layout_2021]
)
source_2013 <- root("harness", "sources", "hagstofa", "KOS02118c_2013.json")
a <- build_anchor_2013()

# A copy of KOS02118c with `change` applied to its parsed JSON, as a temp file.
tampered <- function(change) {
  p <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  jsonlite::write_json(change(jsonlite::read_json(source_2013)), p, auto_unbox = TRUE, null = "null", digits = NA)
  p
}
# The 1-based position in the row-major value array of one cell, found by its labels.
cell <- function(x, measure, list, kd) {
  pos <- function(d, label) {
    cat <- x$dimension[[d]]$category
    cat$index[[names(cat$label)[unlist(cat$label) == label]]]
  }
  size <- unlist(x$size)
  (pos("Atri\u00f0i", measure) * size[2] + pos("Flokkur", list)) * size[3] + pos("Kj\u00f6rd\u00e6mi", kd) + 1
}
add <- function(x, measure, list, kd, v) {
  i <- cell(x, measure, list, kd)
  x$value[[i]] <- (if (is.null(x$value[[i]])) 0L else x$value[[i]]) + v
  x
}
# `v` added to one cell and to the totals it enters, so the cells still add up.
add_with_totals <- function(x, measure, list, kd, v) {
  for (l in c(list, "Alls")) for (k in c(kd, "Alls")) x <- add(x, measure, l, k, v)
  x
}
with_lists <- function(l) {
  p <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  readr::write_csv(l, p)
  p
}

test_that("each of the fifteen 2013 lists maps to one poll party, and the list table and KOS02118c name the same lists (a renamed or missing list)", {
  l <- read_lists_2013()
  expect_named(l, c("list_name", "flokkur"))
  expect_equal(nrow(l), 15)
  expect_equal(anyDuplicated(l$list_name), 0)
  expect_true(all(l$flokkur %in% POLL_PARTIES))
  expect_setequal(unique(a$list_name), l$list_name)
  expect_equal(nrow(a), 15 * 6)
  expect_equal(nrow(dplyr::distinct(a, list_name, flokkur)), 15)
  expect_equal(a$flokkur, l$flokkur[match(a$list_name, l$list_name)])
})

test_that("a list missing from the list table, renamed in it or given a party outside POLL_PARTIES stops the build (a list that would drop out or take no party)", {
  l <- read_lists_2013()
  expect_error(
    build_anchor_2013(lists = with_lists(l[l$list_name != "Bj\u00f6rt framt\u00ed\u00f0", ])),
    "only in KOS02118c_2013.json: Bj\u00f6rt framt\u00ed\u00f0; only in",
    fixed = TRUE
  )
  renamed <- dplyr::mutate(l, list_name = dplyr::if_else(list_name == "P\u00edratar", "P\u00edrataflokkurinn", list_name))
  expect_error(
    build_anchor_2013(lists = with_lists(renamed)),
    "only in KOS02118c_2013.json: P\u00edratar; only in .*: P\u00edrataflokkurinn"
  )
  extra <- dplyr::bind_rows(l, tibble::tibble(list_name = "L\u00fd\u00f0r\u00e6\u00f0ishreyfingin", flokkur = "Anna\u00f0"))
  expect_error(build_anchor_2013(lists = with_lists(extra)), ": none; only in .*: L\u00fd\u00f0r\u00e6\u00f0ishreyfingin")
  wrong <- dplyr::mutate(l, flokkur = dplyr::if_else(list_name == "Bj\u00f6rt framt\u00ed\u00f0", "Bj\u00f6rt framt\u00ed\u00f0", flokkur))
  expect_error(build_anchor_2013(lists = with_lists(wrong)), "not poll parties: Bj\u00f6rt framt\u00ed\u00f0", fixed = TRUE)
})

test_that("P\u00edratar are a party of their own and Regnboginn and Landsbygg\u00f0arflokkurinn are in Anna\u00f0 (the letter rule gives J to S\u00f3s\u00edalistaflokkurinn, M to Mi\u00f0flokkurinn and puts P\u00edratar in Anna\u00f0)", {
  l <- read_lists_2013()
  party <- stats::setNames(l$flokkur, l$list_name)
  named <- c("Sj\u00e1lfst\u00e6\u00f0isflokkur", "Frams\u00f3knarflokkur", "Samfylkingin", "Vinstrihreyfingin - gr\u00e6nt frambo\u00f0", "P\u00edratar")
  expect_equal(
    unname(party[named]),
    c("Sj\u00e1lfst\u00e6\u00f0isflokkurinn", "Frams\u00f3knarflokkurinn", "Samfylkingin", "Vinstri Gr\u00e6n", "P\u00edratar")
  )
  expect_equal(unname(party[c("Regnboginn", "Landsbygg\u00f0arflokkurinn", "Bj\u00f6rt framt\u00ed\u00f0")]), rep("Anna\u00f0", 3))
  expect_equal(sum(party != "Anna\u00f0"), 5)
  n <- anchor_national(a)
  expect_equal(n$flokkur, POLL_PARTIES)
  expect_equal(n$votes[n$flokkur == "P\u00edratar"], 9649L)
  expect_equal(n$votes[n$flokkur %in% c("Vi\u00f0reisn", "Flokkur F\u00f3lksins", "Mi\u00f0flokkurinn", "S\u00f3s\u00edalistaflokkurinn")], rep(0L, 4))
})

test_that("national and kj\u00f6rd\u00e6mi totals are Hagstofa's (a dropped cell, or kj\u00f6rd\u00e6mi joined by position)", {
  expect_equal(sum(a$votes), 189023L)
  kd <- c(tapply(a$votes, a$kjordaemi, sum))
  want <- stats::setNames(
    c(23607L, 17355L, 35050L, 35301L, 26884L, 50826L),
    c("Nor\u00f0austur", "Nor\u00f0vestur", "Reykjav\u00edk Nor\u00f0ur", "Reykjav\u00edk Su\u00f0ur", "Su\u00f0ur", "Su\u00f0vestur")
  )
  expect_equal(kd[order(names(kd))], want[order(names(want))])
  f <- fold_anchor(a)
  expect_equal(nrow(f), 60)
  expect_equal(sum(f$votes), 189023L)
  expect_equal(sum(anchor_national(a)$share), 1)
})

test_that("the seats sum to 63, per kj\u00f6rd\u00e6mi to the 2013-2021 layout, and calculate_seats() reproduces every list's seats from the votes (a wrong vote cell or a wrong layout)", {
  expect_equal(sum(a$seats), 63L)
  tot <- dplyr::summarise(a, s = sum(seats), .by = kjordaemi)
  lay <- dplyr::transmute(seats_layout_2021(), kjordaemi, s_layout = n_seats + n_jofnun)
  j <- dplyr::inner_join(tot, lay, by = "kjordaemi")
  expect_equal(nrow(j), 6)
  expect_equal(j$s, j$s_layout)
  s <- calculate_seats(dplyr::transmute(a, flokkur = list_name, kjordaemi, votes), seats_layout_2021())
  got <- dplyr::inner_join(s, dplyr::transmute(a, flokkur = list_name, kjordaemi, official = seats),
    by = c("flokkur", "kjordaemi")
  )
  expect_equal(nrow(got), 90)
  expect_equal(got$seats, got$official)
  nat <- c(tapply(a$seats, a$list_name, sum))
  nat <- nat[nat > 0]
  won <- stats::setNames(
    c(6L, 19L, 3L, 9L, 19L, 7L),
    c(
      "Bj\u00f6rt framt\u00ed\u00f0", "Frams\u00f3knarflokkur", "P\u00edratar", "Samfylkingin", "Sj\u00e1lfst\u00e6\u00f0isflokkur",
      "Vinstrihreyfingin - gr\u00e6nt frambo\u00f0"
    )
  )
  expect_equal(nat[order(names(nat))], won[order(names(won))])
})

test_that("a list ran where its vote cell says so: Landsbygg\u00f0arflokkurinn in Nor\u00f0vestur only, though its seat cell there is '.' (ran read from the seat cells would make 326 votes those of a list that did not run)", {
  lb <- a[a$list_name == "Landsbygg\u00f0arflokkurinn", ]
  expect_equal(lb$kjordaemi[lb$ran], "Nor\u00f0vestur")
  expect_equal(sum(lb$votes), 326L)
  expect_equal(sum(lb$seats), 0L)
  x <- jsonlite::read_json(source_2013)
  i <- cell(x, "\u00deings\u00e6ti", "Landsbygg\u00f0arflokkurinn", "Nor\u00f0vesturkj\u00f6rd\u00e6mi")
  expect_identical(x$status[[as.character(i - 1)]], ".")
  expect_equal(x$value[[cell(x, "Gild atkv\u00e6\u00f0i", "Landsbygg\u00f0arflokkurinn", "Nor\u00f0vesturkj\u00f6rd\u00e6mi")]], 326)
  expect_true(all(a$votes[!a$ran] == 0))
  expect_equal(sum(!a$ran), 18)
})

test_that("the build stops on a source whose cells, totals, seats, kj\u00f6rd\u00e6mi or ran marks are wrong (a corrupted or changed table read silently)", {
  # WHY: 100 votes moved round a rectangle keep every total, so only the ran check can see them.
  moved <- tampered(\(x) {
    x |>
      add("Gild atkv\u00e6\u00f0i", "Landsbygg\u00f0arflokkurinn", "Nor\u00f0austurkj\u00f6rd\u00e6mi", 100L) |>
      add("Gild atkv\u00e6\u00f0i", "Landsbygg\u00f0arflokkurinn", "Nor\u00f0vesturkj\u00f6rd\u00e6mi", -100L) |>
      add("Gild atkv\u00e6\u00f0i", "Sj\u00e1lfst\u00e6\u00f0isflokkur", "Nor\u00f0vesturkj\u00f6rd\u00e6mi", 100L) |>
      add("Gild atkv\u00e6\u00f0i", "Sj\u00e1lfst\u00e6\u00f0isflokkur", "Nor\u00f0austurkj\u00f6rd\u00e6mi", -100L)
  })
  expect_error(
    build_anchor_2013(moved),
    "votes for a list that did not run: Landsbygg\u00f0arflokkurinn in Nor\u00f0austur",
    fixed = TRUE
  )
  seat <- tampered(\(x) add_with_totals(x, "\u00deings\u00e6ti", "Landsbygg\u00f0arflokkurinn", "Nor\u00f0austurkj\u00f6rd\u00e6mi", 1L))
  expect_error(
    build_anchor_2013(seat),
    "seats for a list that did not run: Landsbygg\u00f0arflokkurinn in Nor\u00f0austur",
    fixed = TRUE
  )
  off <- tampered(\(x) add(x, "Gild atkv\u00e6\u00f0i", "Sj\u00e1lfst\u00e6\u00f0isflokkur", "Nor\u00f0vesturkj\u00f6rd\u00e6mi", 1L))
  expect_error(build_anchor_2013(off), "cells do not add up to the totals for Gild atkv\u00e6\u00f0i", fixed = TRUE)
  more <- tampered(\(x) add_with_totals(x, "Gild atkv\u00e6\u00f0i", "Sj\u00e1lfst\u00e6\u00f0isflokkur", "Nor\u00f0vesturkj\u00f6rd\u00e6mi", 1L))
  expect_error(build_anchor_2013(more), "189024 valid votes, not 189023", fixed = TRUE)
  seats64 <- tampered(\(x) add_with_totals(x, "\u00deings\u00e6ti", "Sj\u00e1lfst\u00e6\u00f0isflokkur", "Nor\u00f0vesturkj\u00f6rd\u00e6mi", 1L))
  expect_error(build_anchor_2013(seats64), "64 seats, not 63", fixed = TRUE)
  renamed <- tampered(\(x) {
    lab <- x$dimension[["Kj\u00f6rd\u00e6mi"]]$category$label
    lab[unlist(lab) == "Nor\u00f0vesturkj\u00f6rd\u00e6mi"] <- "Vesturkj\u00f6rd\u00e6mi"
    x$dimension[["Kj\u00f6rd\u00e6mi"]]$category$label <- lab
    x
  })
  expect_error(build_anchor_2013(renamed), "unknown kjordaemi label(s): Vesturkj\u00f6rd\u00e6mi", fixed = TRUE)
  # WHY: the rewrite alone changes nothing but the file name, so each stop above is the change's.
  expect_identical(dplyr::select(build_anchor_2013(tampered(identity)), -source), dplyr::select(a, -source))
})

test_that("national shares of the five named parties agree with fundamentals_data.csv's 2013 rows to its rounding (a mapping that moves votes between parties)", {
  skip_without_snapshot()
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE) |>
    dplyr::filter(date == as.Date("2013-04-27"))
  renamed <- f$flokkur %in% names(FUNDAMENTALS_RENAMED)
  f$flokkur[renamed] <- unname(FUNDAMENTALS_RENAMED[f$flokkur[renamed]])
  five <- c("Sj\u00e1lfst\u00e6\u00f0isflokkurinn", "Frams\u00f3knarflokkurinn", "Samfylkingin", "Vinstri Gr\u00e6n", "P\u00edratar")
  n <- anchor_national(a)
  ours <- 100 * n$share[match(five, n$flokkur)]
  theirs <- f$voteshare[match(five, f$flokkur)]
  expect_false(anyNA(theirs))
  expect_true(all(abs(ours - theirs) <= 0.05 + 1e-9), label = paste(five, round(ours, 3), theirs, collapse = "; "))
  expect_equal(unique(f$total_valid_votes), 189023)
})

test_that("the committed anchor table is a fresh build (a stale generated file)", {
  fresh <- withr::local_tempfile(fileext = ".csv")
  write_anchor_2013(fresh)
  committed <- root("harness", "truth", "anchor_2013.csv")
  expect_identical(readLines(fresh, encoding = "UTF-8"), readLines(committed, encoding = "UTF-8"))
  expect_identical(unname(tools::md5sum(fresh)), unname(tools::md5sum(committed)))
})

test_that("the committed KOS02118c file has the sha256 it was fetched with (a silently changed source)", {
  expect_identical(
    digest::digest(file = source_2013, algo = "sha256"),
    "c50bb3e119c6acfa21ad470234fe9ef91c45fcba9b51c4a826de6f8fb445197b"
  )
})
