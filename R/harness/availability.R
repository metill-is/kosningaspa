# Poll availability (spec §5.3 rule 1): the first day a forecaster could have seen each
# poll. A forecast made on day F admits a poll iff available_from <= F. Every rule errs
# late where evidence is missing: a late date only withholds a poll, an early one leaks
# the future into a backtest. The binding fieldwork end (the one the rule order takes),
# a poll's first publication and, for a kjördæmi poll, its national twin's date are all
# floors: no poll is available before that end + 1, before it was published, or before
# its twin.

# The availability rules of each level, in the order they are tried, except `election`:
# a national `Kosning` pseudo-poll (official results) always takes it, and no other row can.
RULES <- list(
  national = c("correction", "lokadagur", "wikipedia", "published", "house_max", "election"),
  kjordaemi = c("correction", "twin", "published", "house_max")
)

WIKIPEDIA_YEARS <- c(2017L, 2021L, 2024L)
WIKIPEDIA_PARTIES <- c("D", "S", "B", "V", "P", "C", "F", "M", "J")
NEAR_DAYS <- 14
# Wikipedia matching: the largest mean absolute share difference (pp), the largest distance
# from `date` to the Wikipedia midpoint (days) and the fewest parties both must report.
TOL_PP <- 0.25
MAX_DAYS <- 7
MIN_SHARED_PARTIES <- 5
# WHY: shares are computed from counts, so a difference printed as exactly the tolerance
# must not fail on floating-point noise.
TOL_EPS <- 1e-9

read_source <- function(path, col_types, keys, na = character()) {
  x <- readr::read_csv(path, col_types = col_types, na = na, progress = FALSE)
  p <- readr::problems(x)
  if (nrow(p)) {
    stop(basename(path), ": unparsable value(s) in row(s) ", paste(unique(p$row), collapse = ", "))
  }
  missing <- keys[vapply(keys, \(k) anyNA(x[[k]]), logical(1))]
  if (length(missing)) stop(basename(path), ": missing value(s) in ", paste(missing, collapse = ", "))
  x
}

#' The snapshot's national polls, every row (pre-window and `Kosning` rows included).
#' @return a tibble (date, fyrirtaeki, flokkur, n, lokadagur): `lokadagur` as text, the
#'   string "NA" kept; `p` is dropped. An error on an unparsable value or a missing date,
#'   fyrirtaeki, flokkur or n
#' @export
read_polling <- function() {
  box::use(R / harness / inputs[snapshot_file])
  read_source(
    snapshot_file("polling_data"),
    readr::cols(date = "D", fyrirtaeki = "c", flokkur = "c", n = "d", lokadagur = "c", p = "_"),
    keys = c("date", "fyrirtaeki", "flokkur", "n")
  )
}

#' The snapshot's kjördæmi polls, every row (`Kosning` rows included).
#' @return a tibble (date, fyrirtaeki, flokkur, kjordaemi, n); an error on an unparsable
#'   value or a missing one
#' @export
read_constituency <- function() {
  box::use(R / harness / inputs[snapshot_file])
  # WHY: read_source()'s na = character() keeps the Norðaustur code "NA" a string.
  read_source(
    snapshot_file("constituency_data"),
    readr::cols(date = "D", fyrirtaeki = "c", flokkur = "c", kjordaemi = "c", n = "d"),
    keys = c("date", "fyrirtaeki", "flokkur", "kjordaemi", "n")
  )
}

# The Wikipedia fieldwork tables, one row per Wikipedia poll; a blank share is a party
# the page does not report.
read_fieldwork <- function(sources_dir) {
  tables <- lapply(WIKIPEDIA_YEARS, \(y) {
    x <- read_source(
      file.path(sources_dir, sprintf("wikipedia_%d_poll_fieldwork.csv", y)),
      readr::cols(fieldwork = "c", house = "c", start = "D", end = "D", note = "c", n = "_", .default = "d"),
      keys = c("house", "start", "end"), na = ""
    )
    absent <- setdiff(WIKIPEDIA_PARTIES, names(x))
    if (length(absent)) stop("wikipedia_", y, "_poll_fieldwork.csv lacks ", paste(absent, collapse = ", "))
    tibble::add_column(x, table = y, .before = 1)
  })
  dplyr::bind_rows(tables)
}

read_sources <- function(sources_dir) {
  house_map <- read_source(
    file.path(sources_dir, "wikipedia_house_map.csv"),
    readr::cols(wikipedia_house = "c", fyrirtaeki = "c"),
    keys = c("wikipedia_house", "fyrirtaeki")
  )
  if (anyDuplicated(house_map$wikipedia_house)) stop("wikipedia_house_map.csv maps a house twice")
  corrections <- read_source(
    file.path(sources_dir, "fieldwork_corrections.csv"),
    readr::cols(level = "c", date = "D", fyrirtaeki = "c", end = "D", source = "c", note = "c"),
    keys = c("level", "date", "fyrirtaeki", "end", "source")
  )
  publication <- read_source(
    file.path(sources_dir, "poll_publication.csv"),
    readr::cols(level = "c", date = "D", fyrirtaeki = "c", published = "D", source = "c", note = "c"),
    keys = c("level", "date", "fyrirtaeki", "published", "source")
  )
  for (x in list(corrections, publication)) {
    if (!all(x$level %in% names(RULES))) stop("a source row has a level other than ", paste(names(RULES), collapse = "/"))
    if (anyDuplicated(x[c("level", "date", "fyrirtaeki")])) stop("a source table gives one poll two rows")
  }
  list(
    fieldwork = read_fieldwork(sources_dir), house_map = house_map,
    corrections = corrections, publication = publication
  )
}

#' Calendar date of a `lokadagur` value.
#' @param x character: ISO datetimes ("2024-11-13T00:00:00Z"), ISO dates, NA or "NA"
#' @return a Date: the UTC calendar date; NA for NA or "NA"; an error for anything else
#' @export
parse_lokadagur <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA")] <- NA
  dt <- suppressWarnings(readr::parse_datetime(x, na = character()))
  bad <- !is.na(x) & is.na(dt)
  if (any(bad)) stop("unparsable lokadagur: ", paste(unique(x[bad]), collapse = ", "))
  as.Date(dt, tz = "UTC")
}

# Percentage shares of one poll's rows, named by list letter; Annað has no letter and is
# left out, but its count stays in the total.
letter_shares <- function(flokkur, n) {
  box::use(R / harness / constants[LETTER_PARTY])
  letter <- names(LETTER_PARTY)[match(flokkur, LETTER_PARTY)]
  if (anyDuplicated(stats::na.omit(letter))) stop("a poll lists one party twice")
  share <- 100 * n / sum(n)
  keep <- !is.na(letter)
  stats::setNames(share[keep], letter[keep])
}

#' One row per poll, national and kjördæmi.
#' @param polling the snapshot's `polling_data` (date, fyrirtaeki, flokkur, n, lokadagur)
#' @param constituency the snapshot's `constituency_data` (date, fyrirtaeki, flokkur,
#'   kjordaemi, n)
#' @return a tibble (level, date, fyrirtaeki, lokadagur, shares) keyed by (level, date,
#'   fyrirtaeki): a kjördæmi poll is one row over all its kjördæmi, with no `lokadagur`.
#'   `shares` holds each poll's percentages, 100 × n / sum(n) over its rows, named by list
#'   letter (Annað is left out). Errors if one poll carries two `lokadagur` values.
#' @export
poll_list <- function(polling, constituency) {
  box::use(dplyr[arrange, bind_rows, filter, first, mutate, n_distinct, summarise, transmute])
  rows <- bind_rows(
    transmute(polling, level = "national", date, fyrirtaeki, flokkur, n, lokadagur = parse_lokadagur(lokadagur)),
    constituency |>
      summarise(n = sum(n), .by = c(date, fyrirtaeki, flokkur)) |>
      mutate(level = "kjordaemi", lokadagur = as.Date(NA))
  )
  two <- rows |>
    summarise(k = n_distinct(lokadagur), .by = c(level, date, fyrirtaeki)) |>
    filter(k > 1)
  if (nrow(two)) {
    stop("poll(s) with two lokadagur values: ", paste(two$fyrirtaeki, format(two$date), collapse = "; "))
  }
  rows |>
    summarise(lokadagur = first(lokadagur), shares = list(letter_shares(flokkur, n)), .by = c(level, date, fyrirtaeki)) |>
    arrange(match(level, names(RULES)), date, fyrirtaeki)
}

#' Match polls to Wikipedia fieldwork rows.
#' @param polls a tibble (date, fyrirtaeki, shares), as `poll_list()`
#' @param fieldwork Wikipedia rows (house, start, end, and share columns named by list
#'   letter, NA where the page reports none)
#' @param house_map a tibble (wikipedia_house, fyrirtaeki); every `fieldwork` house must
#'   be in it
#' @param tol_pp largest mean absolute share difference, in percentage points, over the
#'   parties both report
#' @param max_days largest distance in days between `date` and the Wikipedia midpoint
#' @param min_parties fewest parties both must report; `poll_availability()` passes
#'   `MIN_SHARED_PARTIES` (5)
#' @return a tibble (date, fyrirtaeki, start, end) of matched polls. A candidate pair needs
#'   the same house, the midpoint within `max_days`, at least `min_parties` shared parties
#'   and the share difference within `tol_pp`; pairs are taken greedily by smallest share
#'   difference, so each poll and each Wikipedia row is used at most once.
#' @export
match_fieldwork <- function(polls, fieldwork, house_map, tol_pp = TOL_PP, max_days = MAX_DAYS, min_parties = 1) {
  box::use(dplyr[arrange])
  cand <- match_candidates(polls, fieldwork, house_map, tol_pp, max_days, min_parties)
  take <- logical(nrow(cand))
  for (i in seq_len(nrow(cand))) {
    take[i] <- !cand$poll[i] %in% cand$poll[take] && !cand$row[i] %in% cand$row[take]
  }
  cand[take, c("date", "fyrirtaeki", "start", "end")] |>
    arrange(fyrirtaeki, date)
}

# Every candidate pair of a poll (`poll`, its row in `polls`) and a Wikipedia row (`row`,
# its row in `fieldwork`), by smallest share difference first.
match_candidates <- function(polls, fieldwork, house_map, tol_pp, max_days, min_parties) {
  box::use(dplyr[arrange, filter, inner_join, mutate, select])
  unmapped <- setdiff(fieldwork$house, house_map$wikipedia_house)
  if (length(unmapped)) stop("Wikipedia house(s) not in the house map: ", paste(unmapped, collapse = ", "))
  parties <- intersect(WIKIPEDIA_PARTIES, names(fieldwork))
  wiki <- fieldwork |>
    mutate(
      row = dplyr::row_number(),
      fyrirtaeki = house_map$fyrirtaeki[match(house, house_map$wikipedia_house)],
      mid = (as.numeric(start) + as.numeric(end)) / 2
    ) |>
    select(row, fyrirtaeki, start, end, mid, dplyr::all_of(parties))
  pairs <- polls |>
    mutate(poll = dplyr::row_number()) |>
    select(poll, date, fyrirtaeki, shares) |>
    inner_join(wiki, by = "fyrirtaeki", relationship = "many-to-many") |>
    mutate(gap = abs(as.numeric(date) - mid)) |>
    filter(gap <= max_days)
  poll_shares <- matrix(
    vapply(pairs$shares, \(s) unname(s[parties]), numeric(length(parties))),
    ncol = length(parties), byrow = TRUE
  )
  diff <- abs(poll_shares - as.matrix(pairs[parties]))
  reported <- rowSums(!is.na(diff))
  pairs$reported <- reported
  pairs$mad <- ifelse(reported > 0, rowMeans(diff, na.rm = TRUE), NA_real_)
  pairs |>
    filter(reported >= min_parties, !is.na(mad), mad <= tol_pp + TOL_EPS) |>
    select(poll, row, date, fyrirtaeki, start, end, mad, gap) |>
    arrange(mad, gap, poll, row)
}

#' Longest observed fieldwork per house.
#' @param spans a tibble (fyrirtaeki, start, end) of known fieldwork spans
#' @return a tibble (fyrirtaeki, max_days), max_days = max(end - start) in days, with
#'   attribute `overall`: the maximum over all houses, for a house with no span (NA when
#'   `spans` is empty)
#' @export
house_max_fieldwork <- function(spans) {
  box::use(dplyr[arrange, summarise])
  if (!nrow(spans)) {
    return(structure(tibble::tibble(fyrirtaeki = character(), max_days = integer()), overall = NA_integer_))
  }
  h <- spans |>
    summarise(max_days = max(as.integer(end - start)), .by = fyrirtaeki) |>
    arrange(fyrirtaeki)
  attr(h, "overall") <- max(h$max_days)
  h
}

# Everything poll_availability() computes; write_availability() reports from the parts.
availability_parts <- function(polling, constituency, sources_dir) {
  box::use(
    R / harness / constants[ELECTION_HOUSE, WINDOW_START],
    dplyr[anti_join, arrange, bind_rows, case_when, filter, if_else, left_join, mutate, rename, select, transmute]
  )
  src <- read_sources(sources_dir)
  # WHY: the window is cut before poll_list(), whose checks would otherwise reject
  # pre-window rows such as the two MMR polls the data keys as one on 2013-05-16.
  polls <- poll_list(filter(polling, date >= WINDOW_START), filter(constituency, date >= WINDOW_START)) |>
    filter(!(level == "kjordaemi" & fyrirtaeki == ELECTION_HOUSE))

  # WHY: a Kosning row is always `election`, so a source row naming one would be ignored.
  dated <- filter(polls, fyrirtaeki != ELECTION_HOUSE)
  for (x in list(src$corrections, src$publication)) {
    orphan <- anti_join(x, dated, by = c("level", "date", "fyrirtaeki"))
    if (nrow(orphan)) {
      stop("source row(s) naming no poll in the window: ", paste(orphan$level, format(orphan$date), orphan$fyrirtaeki, collapse = "; "))
    }
  }

  national_polls <- filter(dated, level == "national")
  cand <- match_candidates(national_polls, src$fieldwork, src$house_map, TOL_PP, MAX_DAYS, MIN_SHARED_PARTIES)
  two <- cand[cand$poll %in% cand$poll[duplicated(cand$poll)] | cand$row %in% cand$row[duplicated(cand$row)], ]
  if (nrow(two)) {
    stop(
      "a poll or a Wikipedia row has more than one match candidate: ",
      paste(two$fyrirtaeki, format(two$date), "~", format(two$start), format(two$end), collapse = "; ")
    )
  }
  matched <- match_fieldwork(national_polls, src$fieldwork, src$house_map, min_parties = MIN_SHARED_PARTIES)
  spans <- transmute(src$fieldwork,
    fyrirtaeki = src$house_map$fyrirtaeki[match(house, src$house_map$wikipedia_house)], start, end
  )
  hmax <- house_max_fieldwork(spans)
  max_days <- function(f) {
    m <- hmax$max_days[match(f, hmax$fyrirtaeki)]
    ifelse(is.na(m), attr(hmax, "overall"), m)
  }

  source_rows <- function(x, lvl, col, name) {
    y <- x[x$level == lvl, c("date", "fyrirtaeki", col)]
    names(y)[3] <- name
    y
  }
  join <- function(x, y) left_join(x, y, by = c("date", "fyrirtaeki"), relationship = "many-to-one")

  national <- polls |>
    filter(level == "national") |>
    join(source_rows(src$corrections, "national", "end", "corrected_end")) |>
    join(source_rows(src$publication, "national", "published", "published")) |>
    join(rename(matched, wikipedia_end = end) |> select(-start)) |>
    mutate(
      rule_order = case_when(
        fyrirtaeki == ELECTION_HOUSE ~ "election",
        !is.na(corrected_end) ~ "correction",
        !is.na(lokadagur) ~ "lokadagur",
        !is.na(wikipedia_end) ~ "wikipedia",
        !is.na(published) ~ "published",
        .default = "house_max"
      ),
      fieldwork_end = case_when(
        rule_order == "election" ~ date,
        rule_order == "correction" ~ corrected_end,
        rule_order == "lokadagur" ~ lokadagur,
        rule_order == "wikipedia" ~ wikipedia_end,
        .default = as.Date(NA)
      ),
      order_from = case_when(
        rule_order == "election" ~ date + 1,
        rule_order == "correction" ~ corrected_end + 1,
        rule_order == "lokadagur" ~ lokadagur + 1,
        rule_order == "published" ~ published,
        rule_order == "wikipedia" ~ wikipedia_end + 1,
        rule_order == "house_max" ~ date + max_days(fyrirtaeki) + 1
      ),
      available_from = pmax(order_from, published, na.rm = TRUE),
      rule = if_else(available_from > order_from, "published", rule_order)
    )

  twins <- transmute(national, date, fyrirtaeki, twin_end = fieldwork_end, twin_from = available_from)
  kjordaemi <- polls |>
    filter(level == "kjordaemi") |>
    join(source_rows(src$corrections, "kjordaemi", "end", "corrected_end")) |>
    join(twins) |>
    join(source_rows(src$publication, "kjordaemi", "published", "published")) |>
    mutate(
      rule_order = case_when(
        !is.na(corrected_end) ~ "correction",
        !is.na(twin_from) ~ "twin",
        !is.na(published) ~ "published",
        .default = "house_max"
      ),
      fieldwork_end = case_when(
        rule_order == "correction" ~ corrected_end,
        rule_order == "twin" ~ twin_end,
        .default = as.Date(NA)
      ),
      order_from = case_when(
        rule_order == "correction" ~ corrected_end + 1,
        rule_order == "twin" ~ twin_from,
        rule_order == "published" ~ published,
        rule_order == "house_max" ~ date + max_days(fyrirtaeki) + 1
      ),
      available_from = pmax(order_from, twin_from, published, na.rm = TRUE),
      rule = case_when(
        available_from == order_from ~ rule_order,
        available_from == twin_from ~ "twin",
        .default = "published"
      )
    )

  av <- bind_rows(national, kjordaemi)
  before_end <- !is.na(av$published) & !is.na(av$fieldwork_end) & av$published < av$fieldwork_end
  if (any(before_end)) {
    stop(
      "publication before fieldwork end: ",
      paste(av$level[before_end], av$fyrirtaeki[before_end], format(av$date[before_end]),
        "published", format(av$published[before_end]), "end", format(av$fieldwork_end[before_end]),
        collapse = "; "
      )
    )
  }
  av <- av |>
    select(level, date, fyrirtaeki, fieldwork_end, available_from, rule) |>
    arrange(match(level, names(RULES)), date, fyrirtaeki)
  if (anyNA(av$available_from)) stop("a poll has no availability date")
  early <- av$rule != "election" & av$available_from <= av$date
  if (any(early)) {
    stop("poll(s) available on or before their midpoint: ", paste(av$fyrirtaeki[early], format(av$date[early]), collapse = "; "))
  }
  list(av = av, polls = polls, matched = matched, fieldwork = src$fieldwork, house_map = src$house_map, house_max = hmax)
}

#' When each poll became available.
#' @param polling the snapshot's `polling_data`
#' @param constituency the snapshot's `constituency_data`
#' @param sources_dir the directory holding the Wikipedia tables, the house map,
#'   `fieldwork_corrections.csv` and `poll_publication.csv`
#' @return a tibble (level, date, fyrirtaeki, fieldwork_end, available_from, rule): one row
#'   per poll dated from `WINDOW_START`, plus the national `Kosning` rows, which are always
#'   `election` (`date` + 1). Any other poll first gets the date of the first rule of its
#'   level that applies. National: `correction` (end + 1), `lokadagur` (+ 1), `wikipedia`
#'   (matched end + 1), `published` (that day), `house_max` (`date` + the house's longest
#'   Wikipedia fieldwork + 1), so `published` decides only where no fieldwork end is
#'   known. Kjördæmi: `correction`, `twin` (the national poll with the same date and
#'   house, whose `fieldwork_end` and `available_from` it takes), `published`,
#'   `house_max`. Then floors apply: `available_from` is the latest of that date and the
#'   poll's `published` date, and at kjördæmi level its twin's `available_from`. A floor
#'   never lowers a date. `rule` names the rule whose date binds: the rule-order rule if
#'   no floor raises it, else `published`, or `twin` at kjördæmi level (`twin` on a tie).
#'   `fieldwork_end` comes from the rule-order rule, and is NA where that rule gives none
#'   (`published`, `house_max`). A Wikipedia match needs `MIN_SHARED_PARTIES` shared
#'   parties. Errors if a poll or a Wikipedia row has more than one match candidate, if a
#'   correction or publication row names no dated poll (a `Kosning` row included), if a
#'   poll's `published` date is before its `fieldwork_end`, or if a non-election poll
#'   would be available on or before `date`.
#' @export
poll_availability <- function(polling = read_polling(), constituency = read_constituency(),
                              sources_dir = harness_path("sources")) {
  # WHY: the default arguments are promises, forced after this import.
  box::use(R / harness / constants[harness_path])
  availability_parts(polling, constituency, sources_dir)$av
}

#' Polls per rule.
#' @param av a `poll_availability()` table
#' @return a tibble (level, rule, n) with every rule of `RULES`, zeros included
#' @export
availability_counts <- function(av) {
  box::use(dplyr[anti_join, count, left_join])
  all_rules <- tibble::tibble(level = rep(names(RULES), lengths(RULES)), rule = unlist(RULES, use.names = FALSE))
  stray <- anti_join(av, all_rules, by = c("level", "rule"))
  if (nrow(stray)) stop("rule(s) not in RULES: ", paste(unique(paste(stray$level, stray$rule)), collapse = ", "))
  counts <- left_join(all_rules, count(av, level, rule), by = c("level", "rule"))
  counts$n[is.na(counts$n)] <- 0L
  counts
}

#' Polls fielded by F but not yet available on F.
#' @param av a `poll_availability()` table
#' @param F a forecast date (Date or ISO string)
#' @return the rows of `av` with `date <= F < available_from`
#' @export
excluded_near <- function(av, F) {
  F <- as.Date(F)
  stopifnot(length(F) == 1, !is.na(F))
  av[av$date <= F & F < av$available_from, ]
}

#' Same-house polls whose known fieldwork spans overlap. A report, never a filter: both
#' polls are kept (spec §5.3 rule 2).
#' @param av a `poll_availability()` table
#' @return a tibble (level, fyrirtaeki, date_1, start_1, end_1, rule_1, date_2, start_2,
#'   end_2, rule_2), one row per overlapping pair at one level, with `date_1 < date_2`. A
#'   span is known where `fieldwork_end` is; since `date` is the fieldwork midpoint, it
#'   starts at `date - (fieldwork_end - date)`. Days are inclusive, so spans sharing a day
#'   overlap.
#' @export
fieldwork_overlaps <- function(av) {
  box::use(dplyr[arrange, filter, inner_join, mutate, select, transmute])
  known <- av |>
    filter(!is.na(fieldwork_end), rule != "election") |>
    transmute(level, fyrirtaeki, date, start = date - (fieldwork_end - date), end = fieldwork_end, rule)
  inner_join(known, known, by = c("level", "fyrirtaeki"), suffix = c("_1", "_2"), relationship = "many-to-many") |>
    filter(date_1 < date_2, start_2 <= end_1, start_1 <= end_2) |>
    select(level, fyrirtaeki, date_1, start_1, end_1, rule_1, date_2, start_2, end_2, rule_2) |>
    arrange(match(level, names(RULES)), fyrirtaeki, date_1, date_2)
}

md_table <- function(x) {
  cells <- lapply(x, \(v) {
    v <- if (inherits(v, "Date")) format(v, "%Y-%m-%d") else as.character(v)
    ifelse(is.na(v), "NA", v)
  })
  rows <- if (nrow(x)) do.call(paste, c(unname(cells), sep = " | ")) else character()
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    if (length(rows)) paste0("| ", rows, " |")
  )
}

# Per Wikipedia table and house: its rows, the national polls dated within the table's
# span (first start to last end), and how many of those matched a row.
match_summary <- function(parts) {
  box::use(R / harness / constants[ELECTION_HOUSE], dplyr[arrange, bind_rows, full_join, mutate, summarise])
  fw <- mutate(parts$fieldwork, fyrirtaeki = parts$house_map$fyrirtaeki[match(house, parts$house_map$wikipedia_house)])
  polls <- parts$polls[parts$polls$level == "national" & parts$polls$fyrirtaeki != ELECTION_HOUSE, ]
  polls$matched <- paste(polls$date, polls$fyrirtaeki) %in% paste(parts$matched$date, parts$matched$fyrirtaeki)
  per_house <- lapply(WIKIPEDIA_YEARS, \(y) {
    t <- fw[fw$table == y, ]
    in_span <- polls$date >= min(t$start) & polls$date <= max(t$end)
    full_join(
      summarise(t, wikipedia_rows = dplyr::n(), .by = fyrirtaeki),
      summarise(polls[in_span, ], national_polls = dplyr::n(), matched = sum(matched), .by = fyrirtaeki),
      by = "fyrirtaeki"
    ) |>
      arrange(fyrirtaeki) |>
      mutate(table = y, .before = 1)
  })
  out <- bind_rows(lapply(per_house, \(h) {
    bind_rows(h, summarise(h,
      table = h$table[1], fyrirtaeki = "all", wikipedia_rows = sum(wikipedia_rows, na.rm = TRUE),
      national_polls = sum(national_polls, na.rm = TRUE), matched = sum(matched, na.rm = TRUE)
    ))
  }))
  out$wikipedia_rows[is.na(out$wikipedia_rows)] <- 0L
  out$national_polls[is.na(out$national_polls)] <- 0L
  out$matched[is.na(out$matched)] <- 0L
  out
}

availability_report <- function(parts) {
  box::use(R / harness / constants[TARGETS])
  av <- parts$av
  cells <- unlist(lapply(seq_len(nrow(TARGETS)), \(i) {
    F <- TARGETS$F[i]
    ex <- excluded_near(av, F)
    ex$fallback_near_F <- ifelse(ex$rule == "house_max" & as.integer(F - ex$date) <= NEAR_DAYS, "yes", "")
    c(
      "", sprintf("### E %s, F %s (%s)", format(TARGETS$E[i]), format(F), TARGETS$tag[i]), "",
      if (nrow(ex)) md_table(ex) else "None."
    )
  }))
  overlaps <- fieldwork_overlaps(av)
  c(
    "# Poll availability",
    "",
    "Generated by `write_availability()` in `R/harness/availability.R` from the snapshot and `harness/sources/`; do not edit by hand. A forecast made on day F admits a poll iff `available_from <= F`. The first rule that applies gives a date (spec §5.3 rule 1):",
    "",
    "- National: `correction` (fieldwork end + 1), `lokadagur` (+ 1), `wikipedia` (matched fieldwork end + 1), `published` (that day), `house_max` (`date` + the house's longest fieldwork + 1). `Kosning` rows are `election` (`date` + 1).",
    "- Kjördæmi: `correction`, `twin` (the national poll with the same date and house), `published`, `house_max`.",
    "",
    "The first publication is a floor: `available_from` is the later of that date and the poll's `published` date in `poll_publication.csv`, and a kjördæmi poll is never available before its national twin. So a known fieldwork end always gives at least end + 1, and `published` decides alone only where no end is known. `rule` names the binding rule: `published` (or `twin`) where a floor raised the date. A publication before a poll's known fieldwork end stops the build.",
    "",
    "## Polls per rule",
    "",
    md_table(availability_counts(av)),
    "",
    "## Longest fieldwork per house",
    "",
    sprintf("Over every Wikipedia row of the house (a month-only row spans its month). A house with none would use the overall maximum, %d days.", attr(parts$house_max, "overall")),
    "",
    md_table(parts$house_max),
    "",
    "## Wikipedia matches",
    "",
    sprintf("A match needs the same house, a Wikipedia midpoint within %d days of `date`, at least %d parties both report and a mean absolute share difference of at most %.2f pp over them. The build stops if a poll or a row has more than one candidate. `national_polls` counts the polls dated within the table's span (its first start to its last end), `matched` those that matched a row.", MAX_DAYS, MIN_SHARED_PARTIES, TOL_PP),
    "",
    md_table(match_summary(parts)),
    "",
    "## Polls withheld at each forecast date",
    "",
    sprintf("The polls with `date <= F < available_from`. `fallback_near_F` marks a `house_max` poll dated within %d days of F: one that only the fallback keeps out.", NEAR_DAYS),
    cells,
    "",
    "## Overlapping fieldwork",
    "",
    "Same-house polls at one level whose known spans overlap. A span is known where `fieldwork_end` is, and starts at `date - (fieldwork_end - date)`, since `date` is the midpoint. Both polls are kept (spec §5.3 rule 2).",
    "",
    if (nrow(overlaps)) md_table(overlaps) else "None."
  )
}

#' Write the availability table and its report.
#' @param csv destination of the `poll_availability()` table
#' @param report destination of the Markdown report: polls per rule, longest fieldwork per
#'   house, Wikipedia matches, the withheld polls of each `TARGETS` cell and the fieldwork
#'   overlaps
#' @return the availability table, invisibly
#' @export
write_availability <- function(csv = harness_path("availability.csv"),
                               report = harness_path("availability_report.md")) {
  # WHY: the default arguments are promises, forced after this import.
  box::use(R / harness / constants[harness_path])
  parts <- availability_parts(read_polling(), read_constituency(), harness_path("sources"))
  readr::write_csv(parts$av, csv)
  readr::write_lines(availability_report(parts), report)
  invisible(parts$av)
}
