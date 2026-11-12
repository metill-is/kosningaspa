# The older polls table, harness/older_polls.csv, and the files it is built from
# (harness/sources/older_polls/).
box::use(R / harness / older_polls[read_older_polls, OLDER_POLL_HOUSES, OLDER_POLL_COLUMNS])

ANNAD <- POLL_PARTIES[10]
GALLUP <- OLDER_POLL_HOUSES[1]
MMR <- OLDER_POLL_HOUSES[2]
FEL <- OLDER_POLL_HOUSES[4]
POOL_DAYS <- 730
# The size rule pools only polls dated before this day, the day after the table's period (J08).
POOL_END <- as.Date("2016-01-01")
SHARE_CODES <- c("D", "B", "S", "V", "P")
FIELDWORK_ORDER <- c("mmr_item", "print", "is2013", "chart")

older <- function(...) root("harness", "sources", "older_polls", ...)
read_chr <- function(name) readr::read_csv(older(name), col_types = readr::cols(.default = "c"), na = "")
num <- function(x) as.numeric(x)

skip_without_python <- function() {
  if (!nzchar(Sys.which("python3"))) testthat::skip("python3 is not available")
}

run_python <- function(...) {
  log <- withr::local_tempfile(fileext = ".log")
  status <- withr::with_dir(root(), system2(Sys.which("python3"), c("-I", ...), stdout = log, stderr = log))
  list(status = status, log = paste(readLines(log), collapse = "\n"))
}

# One row per source row a poll can name, in the builder's terms.
source_facts <- function() {
  blank <- function(n, src, ref) {
    tibble::tibble(
      ref = ref, src = src, fw_start = NA_character_, fw_end = NA_character_, published = NA_character_,
      release = NA_character_, sample = NA_real_, rr = NA_real_, ans = NA_real_, has_shares = FALSE
    )[seq_len(n), ]
  }
  is13 <- read_chr("wikipedia_is_2013.csv")
  rate <- num(is13$response_rate)
  # WHY: the Icelandic table's Gallup rate of 6% is a misprint (J25), not a rate
  rate[!is.na(rate) & rate < 20] <- NA
  f_is <- dplyr::mutate(blank(nrow(is13), "is2013", paste0("is2013:", is13$row)),
    fw_start = is13$fieldwork_start, fw_end = is13$fieldwork_end, published = is13$published,
    sample = num(is13$sample), rr = rate, ans = num(is13$n_answering), has_shares = TRUE
  )
  items <- read_chr("mmr_items.csv")
  f_items <- dplyr::mutate(blank(nrow(items), "mmr_item", paste0("mmr_item:", items$item)),
    fw_start = items$fieldwork_start, fw_end = items$fieldwork_end, ans = num(items$svarfjoldi)
  )
  chart <- read_chr("mmr_chart.csv")
  f_chart <- dplyr::mutate(blank(nrow(chart), "chart", paste0("chart:", chart$label)), fw_end = chart$fieldwork_end)
  released <- function(name, src, key) {
    t <- read_chr(name)
    dplyr::mutate(blank(nrow(t), src, paste0(src, ":", t[[key]])), release = t$date)
  }
  pr <- read_chr("print_reports.csv")
  fig <- function(r, f, exact = TRUE) {
    v <- pr$value[pr$report == r & pr$figure == f & (!exact | is.na(pr$qualifier))]
    if (length(v)) v[1] else NA_character_
  }
  reports <- unique(pr$report)
  f_print <- dplyr::mutate(blank(length(reports), "print", paste0("print:", reports)),
    fw_start = vapply(reports, fig, "", f = "fieldwork_start"),
    fw_end = vapply(reports, fig, "", f = "fieldwork_end"),
    published = vapply(reports, fig, "", f = "first_print"),
    sample = num(vapply(reports, fig, "", f = "sample")),
    rr = num(vapply(reports, fig, "", f = "response_rate")),
    ans = num(vapply(reports, fig, "", f = "n_answering")),
    has_shares = reports %in% pr$report[pr$figure %in% SHARE_CODES]
  )
  dplyr::bind_rows(
    f_is, f_items, f_chart, f_print, released("wikipedia_en_2013.csv", "en2013", "row"),
    released("wikipedia_en_2016.csv", "en2016", "row"), released("mbl_box.csv", "mbl", "konnun_id")
  )
}

members_of <- function(facts, pins) {
  refs <- strsplit(pins, ";", fixed = TRUE)[[1]]
  m <- facts[match(refs, facts$ref), ]
  stopifnot(!anyNA(m$ref))
  m
}

first_by <- function(m, srcs, keep) {
  for (s in srcs) {
    hit <- m[m$src == s & keep, ]
    if (nrow(hit)) {
      return(hit[1, ])
    }
  }
  NULL
}

# The size a pinned source prints, in the snapshot's meaning for the house, or NA.
rule_published <- function(m, house) {
  if (house %in% c(GALLUP, FEL)) {
    hit <- first_by(m, c("print", "is2013"), !is.na(m$sample) & !is.na(m$rr))
    if (!is.null(hit)) {
      return(hit$sample * hit$rr / 100)
    }
    if (house == FEL) {
      hit <- first_by(m, c("print", "is2013"), !is.na(m$ans))
      if (!is.null(hit)) {
        return(hit$ans)
      }
    }
    return(NA_real_)
  }
  if (house == MMR) {
    for (step in list(
      list("mmr_item", !is.na(m$ans)), list(c("print", "is2013"), !is.na(m$ans) & m$has_shares),
      list("print", !is.na(m$ans) & !m$has_shares)
    )) {
      hit <- first_by(m, step[[1]], step[[2]])
      if (!is.null(hit)) {
        return(hit$ans)
      }
    }
    return(NA_real_)
  }
  hit <- first_by(m, c("print", "is2013"), !is.na(m$ans))
  if (is.null(hit)) NA_real_ else hit$ans
}

printed <- function(m, what) {
  hit <- first_by(m, c("print", "is2013"), !is.na(m[[what]]))
  if (is.null(hit)) NA_real_ else hit[[what]]
}

# J33: a pool poll's date is its printed fieldwork midpoint, else its fieldwork end, else its
# first print or release day.
pool_date <- function(m) {
  fw <- m[!is.na(m$fw_end) & m$src %in% FIELDWORK_ORDER, ]
  fw <- fw[order(match(fw$src, FIELDWORK_ORDER)), ]
  start <- fw[!is.na(fw$fw_start), ]
  if (nrow(start)) {
    return(as.Date(start$fw_start[1]) + (as.Date(start$fw_end[1]) - as.Date(start$fw_start[1])) / 2)
  }
  if (nrow(fw)) {
    return(as.Date(fw$fw_end[1]))
  }
  min(as.Date(c(stats::na.omit(m$published), stats::na.omit(m$release))))
}

table_polls <- function(x) dplyr::distinct(x, poll_id, date, fyrirtaeki, n_total, n_source, pins, replaces)

# The published sizes and printed response rates the size rule (J08) pools, each a tibble
# (fyrirtaeki, date, n or rr) dated before `before`: the snapshot's polls, the table's ordinary
# polls and the register's pool polls.
size_pools <- function(polls, facts, before = POOL_END) {
  register <- read_chr("polls.csv")
  pool_rows <- register[register$status == "pool", ]
  pool_m <- lapply(pool_rows$sources, members_of, facts = facts)
  pool_polls <- tibble::tibble(
    fyrirtaeki = pool_rows$fyrirtaeki, date = do.call(c, lapply(pool_m, pool_date)),
    n = mapply(rule_published, pool_m, pool_rows$fyrirtaeki),
    rr = vapply(pool_m, printed, 0, what = "rr")
  )
  ordinary <- dplyr::filter(polls, is.na(replaces))
  ordinary_m <- lapply(ordinary$pins, members_of, facts = facts)
  # WHY: the two half-sample sets under one key (J28) are not published sizes
  snap <- snapshot_polls() |>
    dplyr::filter(parts == 1) |>
    dplyr::group_by(date, fyrirtaeki) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  sizes <- dplyr::bind_rows(
    snap,
    dplyr::filter(ordinary, n_source == "published") |> dplyr::select(date, fyrirtaeki, n = n_total),
    dplyr::filter(pool_polls, !is.na(n)) |> dplyr::select(date, fyrirtaeki, n)
  ) |> dplyr::filter(date < before)
  rates <- dplyr::bind_rows(
    tibble::tibble(fyrirtaeki = ordinary$fyrirtaeki, date = ordinary$date, rr = vapply(ordinary_m, printed, 0, what = "rr")),
    dplyr::select(pool_polls, fyrirtaeki, date, rr)
  ) |> dplyr::filter(!is.na(rr), date < before)
  list(sizes = sizes, rates = rates)
}

shares_long <- function(x) {
  dplyr::filter(x, !is.na(share), flokkur != ANNAD) |>
    dplyr::select(poll = poll_id, date, fyrirtaeki, flokkur, share)
}

# Pairs of polls of one house, dated within 7 days, whose shares agree within 0.25 points on
# average over four or more parties: the matching rule of J06.
matching_pairs <- function(a, b) {
  dplyr::inner_join(a, b, by = c("fyrirtaeki", "flokkur"), suffix = c("", "_b"), relationship = "many-to-many") |>
    dplyr::filter(abs(as.numeric(date - date_b)) <= 7) |>
    dplyr::group_by(poll, poll_b, date_b) |>
    dplyr::summarise(k = dplyr::n(), mad = mean(abs(share - share_b)), .groups = "drop") |>
    dplyr::filter(k >= 4, mad <= 0.25 + 1e-9)
}

snapshot_polls <- function() {
  readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki %in% OLDER_POLL_HOUSES, date < as.Date("2016-10-29")) |>
    dplyr::group_by(date, fyrirtaeki, flokkur) |>
    dplyr::mutate(part = dplyr::row_number()) |>
    dplyr::group_by(date, fyrirtaeki) |>
    dplyr::mutate(parts = max(part)) |>
    dplyr::group_by(date, fyrirtaeki, part) |>
    dplyr::mutate(share = 100 * n / sum(n)) |>
    dplyr::ungroup()
}

# WHY: a table that changes an existing cell's polls (one dated from 2016-01-01) or holds a
# house the harness does not model, or a reader that lets a mislabelled file through, such as a
# snapshot size on a poll that replaces no snapshot row.
test_that("the table reads strictly, in the period, from the four houses, and none from 2016", {
  x <- read_older_polls()
  expect_gt(nrow(x), 0)
  expect_true(all(x$fyrirtaeki %in% OLDER_POLL_HOUSES))
  expect_true(all(x$date >= as.Date("2012-07-01") & x$date <= as.Date("2015-12-31")))
  expect_false(any(x$date >= WINDOW_START))
  raw <- readLines(root("harness", "older_polls.csv"), encoding = "UTF-8")
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(sub(ANNAD, "Annad", raw, fixed = TRUE), bad, useBytes = TRUE)
  expect_error(read_older_polls(bad), "POLL_PARTIES")
  writeLines(sub(",replaces$", ",replaced", raw), bad, useBytes = TRUE)
  expect_error(read_older_polls(bad), "columns differ")
  writeLines(c(raw[1], raw[3:length(raw)]), bad, useBytes = TRUE)
  expect_error(read_older_polls(bad), "one row per poll party")
  ordinary <- which(endsWith(raw, ","))[1]
  writeLines(replace(raw, ordinary, sub(",(published|assumed),", ",snapshot,", raw[ordinary])), bad, useBytes = TRUE)
  expect_error(read_older_polls(bad), "a snapshot size on a poll with no replaces key")
})

# WHY: a poll counted twice: one the snapshot already holds, or one the table holds twice.
test_that("no added poll matches a snapshot poll, and no two polls of the table match", {
  x <- read_older_polls()
  a <- shares_long(x)
  self <- matching_pairs(a, dplyr::rename(a, poll_b = poll, date_b = date, share_b = share))
  expect_equal(nrow(dplyr::filter(self, poll != poll_b)), 0, info = paste(self$poll, self$poll_b))
  skip_without_snapshot()
  snap <- snapshot_polls() |>
    dplyr::filter(n > 0, flokkur != ANNAD) |>
    dplyr::transmute(poll_b = paste(date, fyrirtaeki, part), date_b = date, fyrirtaeki, flokkur, share_b = share)
  hits <- matching_pairs(a, snap)
  replaces <- dplyr::distinct(x, poll = poll_id, replaces)
  hits <- dplyr::left_join(hits, replaces, by = "poll")
  # WHY: a replacement poll is the snapshot poll it replaces, by design
  wrong <- dplyr::filter(hits, is.na(replaces) | replaces != date_b)
  expect_equal(nrow(wrong), 0, info = paste(wrong$poll, wrong$poll_b))
  expect_gt(nrow(dplyr::filter(hits, !is.na(replaces))), 0)
})

# WHY: a poll whose shares and Annad do not make 100 (Annad left blank, or not the remainder)
# with no flag naming the parties it did not report; a count not scaled to the poll's size; a
# poll short of a party row. A party folded into Annad still makes 100: the next test.
test_that("shares and Annad sum to 100 or the flags say what was not reported; n is share x n_total", {
  x <- read_older_polls()
  per_poll <- x |>
    dplyr::group_by(poll_id) |>
    dplyr::summarise(
      total = sum(share, na.rm = TRUE), flags = flags[1], rows = dplyr::n(),
      parties = identical(sort(flokkur), sort(POLL_PARTIES)), .groups = "drop"
    )
  expect_true(all(per_poll$rows == 10 & per_poll$parties))
  short <- dplyr::filter(per_poll, abs(total - 100) > 1.5)
  expect_true(all(grepl("not reported:", short$flags, fixed = TRUE) & grepl(ANNAD, short$flags, fixed = TRUE)),
    info = paste(short$poll_id, collapse = ", ")
  )
  expect_lte(max(abs(x$n - dplyr::coalesce(x$share, 0) / 100 * x$n_total)), 1e-5)
  expect_true(all(x$flokkur %in% POLL_PARTIES))
})

# WHY: a poll whose Piratar share was dropped and folded silently into Annad, which still sums to
# 100, so the sum check above cannot see it.
test_that("every poll from its house's first Piratar share on reports Piratar or flags that it does not", {
  x <- read_older_polls()
  pirates <- dplyr::filter(x, flokkur == POLL_PARTIES[5])
  first <- pirates |>
    dplyr::filter(!is.na(share)) |>
    dplyr::group_by(fyrirtaeki) |>
    dplyr::summarise(first = min(date), .groups = "drop")
  expect_setequal(first$fyrirtaeki, OLDER_POLL_HOUSES)
  after <- dplyr::inner_join(pirates, first, by = "fyrirtaeki") |> dplyr::filter(date >= first)
  expect_gt(nrow(after), 0)
  silent <- dplyr::filter(after, is.na(share), !grepl("not reported: ([DBSV], )*P, ", flags))
  expect_equal(nrow(silent), 0, info = paste(silent$poll_id, collapse = ", "))
})

# WHY: a published size that no source prints, an assumed size where a source prints one, or
# an assumed size the stated rule (J08) does not give.
test_that("every published size is printed by a source the poll cites, and every assumed one is the rule's", {
  x <- read_older_polls()
  facts <- source_facts()
  polls <- table_polls(x)
  for (i in seq_len(nrow(polls))) {
    m <- members_of(facts, polls$pins[i])
    found <- c(m$sample * m$rr / 100, m$ans)
    found <- found[!is.na(found)]
    if (polls$n_source[i] == "published") {
      expect_true(any(abs(found - polls$n_total[i]) < 1e-5), info = polls$poll_id[i])
    } else {
      expect_true(is.na(rule_published(m, polls$fyrirtaeki[i])), info = polls$poll_id[i])
    }
  }
  skip_without_snapshot()
  pools <- size_pools(polls, facts)
  sizes <- pools$sizes
  rates <- pools$rates
  assumed <- dplyr::filter(polls, n_source == "assumed")
  expect_gt(nrow(assumed), 0)
  for (i in seq_len(nrow(assumed))) {
    p <- assumed[i, ]
    near <- function(t) t$fyrirtaeki == p$fyrirtaeki & abs(as.numeric(t$date - p$date)) <= POOL_DAYS
    sample <- printed(members_of(facts, p$pins), "sample")
    want <- if (p$fyrirtaeki %in% c(GALLUP, FEL) && !is.na(sample)) {
      sample * stats::median(rates$rr[near(rates)]) / 100
    } else {
      stats::median(sizes$n[near(sizes)])
    }
    expect_equal(p$n_total, want, tolerance = 1e-9, info = p$poll_id)
  }
})

# WHY: a size that depends on polls taken after a forecast day: a pool holding the snapshot's
# polls of 2016, some taken after the forecast day of the three 2016 cells before the eve.
test_that("no assumed size rests on a poll dated from 2016-01-01: the count each assumed poll's flags state is the count of its house's sizes, or response rates, within 730 days dated before 2016, and the same pool run to the 2016 election holds polls of 2016 that would change ten of those counts", {
  skip_without_snapshot()
  x <- read_older_polls()
  pools <- size_pools(table_polls(x), source_facts())
  # WHY: the pool with no end before the 2016 election, as the builder pooled before J08 stated its
  # end; if it held no poll of 2016, the counts below could not tell the two apart.
  late <- size_pools(table_polls(x), source_facts(), before = as.Date("2016-10-29"))
  expect_true(any(late$sizes$date >= POOL_END))
  assumed <- dplyr::filter(dplyr::distinct(x, poll_id, date, fyrirtaeki, n_source, flags), n_source == "assumed")
  expect_gt(nrow(assumed), 0)
  moved <- character(0)
  for (i in seq_len(nrow(assumed))) {
    p <- assumed[i, ]
    by_size <- regmatches(p$flags, regexec("n assumed: the median of ([0-9]+) published sizes within 730 days", p$flags))[[1]]
    by_rate <- regmatches(p$flags, regexec("x the median response rate [0-9.]+% of ([0-9]+) polls within 730 days", p$flags))[[1]]
    expect_true(xor(length(by_size) > 0, length(by_rate) > 0), label = p$poll_id)
    stated <- as.integer(if (length(by_size)) by_size[2] else by_rate[2])
    count <- function(pools) {
      pool <- if (length(by_size)) pools$sizes else pools$rates
      sum(pool$fyrirtaeki == p$fyrirtaeki & abs(as.numeric(pool$date - p$date)) <= POOL_DAYS)
    }
    expect_identical(stated, count(pools), label = p$poll_id)
    if (count(late) != stated) moved <- c(moved, p$poll_id)
  }
  expect_identical(sort(moved), paste0("M-", c(
    "2014-02-13", "2014-04-19", "2014-05-20", "2014-10-18", "2015-01-26", "2015-02-16", "2015-03-15", "2015-04-05",
    "2015-10-17", "2015-12-14"
  )))
})

# WHY: a replacement poll given the size rule's assumed size instead of the count the snapshot
# holds for the row it replaces (J37), or, under the key that holds two polls at half their
# sample, the other poll's half or the half itself.
test_that("a replacement poll with no printed size takes the snapshot's count for the row it replaces", {
  x <- read_older_polls()
  polls <- table_polls(x)
  expect_true(all(polls$n_source[!is.na(polls$replaces)] %in% c("published", "snapshot")))
  took <- dplyr::filter(polls, n_source == "snapshot")
  expect_gt(nrow(took), 0)
  skip_without_snapshot()
  snap <- snapshot_polls()
  for (i in seq_len(nrow(took))) {
    p <- took[i, ]
    held <- dplyr::filter(snap, date == took$replaces[i], fyrirtaeki == took$fyrirtaeki[i])
    totals <- held |>
      dplyr::group_by(part, parts) |>
      dplyr::summarise(n = sum(n), .groups = "drop")
    part <- 1
    if (nrow(totals) > 1) {
      mine <- dplyr::filter(x, poll_id == took$poll_id[i], !is.na(share), flokkur != ANNAD)
      fit <- dplyr::inner_join(held, dplyr::select(mine, flokkur, share_p = share), by = "flokkur") |>
        dplyr::group_by(part) |>
        dplyr::summarise(mad = mean(abs(share - share_p)), .groups = "drop")
      expect_lte(min(fit$mad), 0.25, label = p$poll_id)
      part <- fit$part[which.min(fit$mad)]
    }
    want <- totals$parts[1] * totals$n[totals$part == part]
    expect_equal(p$n_total, want, tolerance = 1e-8, info = p$poll_id)
  }
})

# WHY: the owner's ruling broken: a Gallup poll resting on Gallup's own series or data host.
test_that("no Gallup poll rests on a pin of Gallup's own site or data host, and every source row has a pin", {
  pins <- read_chr("pins.csv")
  pr <- read_chr("print_reports.csv")
  pin_urls <- function(ref) {
    kind <- sub(":.*$", "", ref)
    key <- sub("^[^:]*:", "", ref)
    pin <- switch(kind,
      en2013 = "en2013",
      is2013 = "is2013",
      en2016 = "en2016",
      chart = "mmr_chart",
      mbl = "mbl_box",
      mmr_item = paste0("mmr_item_", key),
      print = paste0("timarit_", unique(pr$page_id[pr$report == key]))
    )
    pins$url[pins$pin %in% pin]
  }
  register <- read_chr("polls.csv")
  refs <- unique(unlist(strsplit(register$sources, ";", fixed = TRUE)))
  refs <- refs[!is.na(refs)]
  expect_true(all(vapply(refs, function(r) length(pin_urls(r)) > 0, logical(1))))
  gallup <- c(register$sources[register$fyrirtaeki == GALLUP], unique(read_older_polls()$pins))
  urls <- unlist(lapply(unlist(strsplit(stats::na.omit(gallup), ";", fixed = TRUE)), pin_urls))
  expect_gt(length(urls), 0)
  expect_false(any(grepl("gallup|capacent|looker", urls, ignore.case = TRUE)))
})

# WHY: a midpoint rounded otherwise than the snapshot's (R/data.R adds half the fieldwork to its
# start with R's date arithmetic).
test_that("date is the fieldwork midpoint by R's date arithmetic wherever both ends are known", {
  x <- dplyr::filter(read_older_polls(), !is.na(fieldwork_start), !is.na(fieldwork_end))
  expect_gt(nrow(x), 0)
  expect_equal(x$date, x$fieldwork_start + (x$fieldwork_end - x$fieldwork_start) / 2)
})

# WHY: a stale generated table, or a stale extract of the snapshot's sizes.
test_that("the builder rewrites the table byte for byte from the committed inputs", {
  skip_without_python()
  fresh <- withr::local_tempfile(fileext = ".csv")
  run <- run_python(file.path("harness", "sources", "older_polls", "build.py"), fresh)
  expect_equal(run$status, 0, info = run$log)
  expect_identical(unname(tools::md5sum(fresh)), unname(tools::md5sum(root("harness", "older_polls.csv"))))
  skip_without_snapshot()
  sizes <- withr::local_tempfile(fileext = ".csv")
  run <- run_python(file.path("harness", "sources", "older_polls", "build.py"), "--snapshot-sizes", sizes)
  expect_equal(run$status, 0, info = run$log)
  expect_identical(unname(tools::md5sum(sizes)), unname(tools::md5sum(older("snapshot_sizes.csv"))))
})

# WHY: a stale extract of the snapshot's sizes where python3 is missing.
test_that("snapshot_sizes.csv holds the snapshot's polls and sizes", {
  skip_without_snapshot()
  want <- snapshot_polls() |>
    dplyr::group_by(date, fyrirtaeki, part, parts) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::arrange(date, fyrirtaeki, part)
  got <- read_chr("snapshot_sizes.csv")
  expect_equal(nrow(got), nrow(want))
  got <- dplyr::arrange(got, date, fyrirtaeki, part)
  expect_equal(as.Date(got$date), want$date)
  expect_equal(got$fyrirtaeki, want$fyrirtaeki)
  expect_equal(num(got$parts), as.numeric(want$parts))
  expect_equal(num(got$n_total), want$n, tolerance = 1e-9)
})

# WHY: a parsed table edited by hand, which the parser would no longer rebuild.
test_that("each parsed table's sha256 is the one recorded for it", {
  m <- read_chr("manifest.csv")
  expect_setequal(m$table, unique(stats::na.omit(read_chr("pins.csv")$table[read_chr("pins.csv")$kind != "timarit"])))
  for (i in seq_len(nrow(m))) {
    expect_identical(digest::digest(file = older(m$table[i]), algo = "sha256"), m$sha256[i], label = m$table[i])
    expect_equal(nrow(read_chr(m$table[i])), num(m$rows[i]), label = m$table[i])
  }
})
