# The information set of one forecast cell (spec §5.3, §7 D4): everything a forecast of
# election E made on day F may use, read once from the snapshot and harness/sources/, and
# validate_inputs(), the leak check every arm's inputs pass. info_set() is the single gate
# through which an arm gets its inputs.

POLL_LEVELS <- c("national", "kjordaemi")
# The economy covariates of a fundamentals target row, in the order of info_set()$economy.
ECONOMY_COVARIATES <- c("vnv", "growth")
# The most offending rows a validate_inputs() message names.
MAX_NAMED <- 10

# A hand-entered corrections table: exactly the columns of `col_types`, every value
# present and parsable.
read_corrections <- function(path, col_types) {
  # WHY: a parse issue becomes the error below, so readr's warning about it is muffled.
  x <- withCallingHandlers(
    readr::read_csv(path, progress = FALSE, col_types = col_types),
    vroom_parse_issue = function(w) invokeRestart("muffleWarning")
  )
  if (!identical(names(x), names(col_types$cols))) {
    stop(basename(path), ": columns must be ", paste(names(col_types$cols), collapse = ", "))
  }
  if (nrow(readr::problems(x)) || anyNA(x)) stop(basename(path), ": missing or unparsable value(s)")
  x
}

#' The respondent-count corrections: polls whose `n` in the data differs from the
#' respondents their source reports.
#' @param path the CSV (level, date, fyrirtaeki, n_total_data, n_total_reported, source)
#' @return a tibble with those columns; an error on other columns or a missing or
#'   unparsable value
#' @export
read_n_corrections <- function(path = harness_path("sources", "poll_n_corrections.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  read_corrections(path, readr::cols(
    level = "c", date = "D", fyrirtaeki = "c", n_total_data = "d", n_total_reported = "d", source = "c"
  ))
}

#' The fundamentals corrections: values of `fundamentals_data.csv` that break the rule the
#' rest of their column follows.
#' @param path the CSV (date, flokkur, column, value_data, value_corrected, source, note)
#' @return a tibble with those columns; an error on other columns or a missing or
#'   unparsable value
#' @export
read_fundamentals_corrections <- function(path = harness_path("sources", "fundamentals_corrections.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  read_corrections(path, readr::cols(
    date = "D", flokkur = "c", column = "c", value_data = "d", value_corrected = "d", source = "c", note = "c"
  ))
}

#' The economy release table: the day Hagstofa first published each economy covariate
#' of a window election's target rows (`harness/sources/README.md`).
#' @param path the CSV (election, covariate, period, value, first_release, source, note):
#'   `covariate` is vnv or growth, `period` the month (vnv) or year (growth) it measures,
#'   `value` the snapshot's value at `election`
#' @return a tibble with those columns; an error on other columns, a missing or
#'   unparsable value, another covariate, or an election and covariate listed twice
#' @export
read_economy_release <- function(path = harness_path("sources", "economy_release.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  x <- read_corrections(path, readr::cols(
    election = "D", covariate = "c", period = "c", value = "d", first_release = "D", source = "c", note = "c"
  ))
  other <- setdiff(x$covariate, ECONOMY_COVARIATES)
  if (length(other)) stop(basename(path), ": covariate(s) other than vnv and growth: ", paste(other, collapse = ", "))
  twice <- duplicated(x[c("election", "covariate")])
  if (any(twice)) {
    stop(basename(path), ": listed twice: ", paste(format(x$election[twice]), x$covariate[twice], collapse = "; "))
  }
  x
}

#' Replace the fundamentals values named in `corrections`.
#' @param fund the fundamentals table (`fundamentals_data.csv`)
#' @param corrections a `read_fundamentals_corrections()` tibble
#' @return `fund`, where each correction's value (row `date` and `flokkur`, column
#'   `column`) is `value_corrected`, as long as it still equals `value_data`; a value that
#'   no longer does (the source sheet was fixed) gets a warning and is left as it is. An
#'   error if a correction names a column other than `fund`'s value columns (`date` and
#'   `flokkur` are keys), or no row or more than one.
#' @export
apply_fundamentals_corrections <- function(fund, corrections) {
  for (i in seq_len(nrow(corrections))) {
    fix <- corrections[i, ]
    cell <- paste(format(fix$date), fix$flokkur, fix$column)
    if (!fix$column %in% setdiff(names(fund), c("date", "flokkur"))) {
      stop("fundamentals correction names no value column: ", cell)
    }
    row <- which(fund$date == fix$date & fund$flokkur == fix$flokkur)
    if (!length(row)) stop("fundamentals correction names no row: ", cell)
    if (length(row) > 1) stop("fundamentals correction names more than one row: ", cell)
    value <- fund[[fix$column]][row]
    if (!isTRUE(value == fix$value_data)) {
      warning(
        "fundamentals correction not applied: ", cell, " is ", format(value), ", not ", format(fix$value_data),
        call. = FALSE
      )
      next
    }
    fund[[fix$column]][row] <- fix$value_corrected
  }
  fund
}

#' Scale the respondent counts of the polls named in `corrections`.
#' @param polling national poll rows (date, fyrirtaeki, flokkur, n, ...)
#' @param constituency kjördæmi poll rows (date, fyrirtaeki, flokkur, kjordaemi, n, ...)
#' @param corrections a `read_n_corrections()` tibble
#' @return `list(polling, constituency)`. Each correction picks the rows of its poll
#'   (`level`, `date`, `fyrirtaeki`) and, while their n total rounded to a whole number
#'   equals `n_total_data`, scales their n by `n_total_reported` / that total, the poll's
#'   own unrounded total. The data's n are weighted, fractional counts, so nothing is
#'   rounded: the shares are unchanged and the rows sum to `n_total_reported` to
#'   floating-point precision. A poll whose rounded total no longer equals `n_total_data`
#'   (the source sheet was fixed) gets a warning and is left as it is. An error if a
#'   correction names no poll.
#' @export
apply_n_corrections <- function(polling, constituency, corrections) {
  tables <- list(national = polling, kjordaemi = constituency)
  for (i in seq_len(nrow(corrections))) {
    fix <- corrections[i, ]
    poll <- paste(fix$level, format(fix$date), fix$fyrirtaeki)
    x <- if (fix$level %in% POLL_LEVELS) tables[[fix$level]]
    rows <- if (is.null(x)) integer() else which(x$date == fix$date & x$fyrirtaeki == fix$fyrirtaeki)
    if (!length(rows)) stop("n correction names no poll: ", poll)
    total <- sum(x$n[rows])
    if (round(total) != fix$n_total_data) {
      warning(
        "n correction not applied: ", poll, " has an n total of ", format(total),
        ", not ", format(fix$n_total_data),
        call. = FALSE
      )
      next
    }
    x$n[rows] <- x$n[rows] * fix$n_total_reported / total
    tables[[fix$level]] <- x
  }
  list(polling = tables$national, constituency = tables$kjordaemi)
}

#' Every input of the harness, read once.
#' @return a list:
#'   - `polling`, `constituency`: `read_polling()` and `read_constituency()`, the
#'     snapshot's national and kjördæmi poll rows as availability.R reads them, after
#'     `apply_n_corrections()` with `harness/sources/poll_n_corrections.csv`;
#'   - `fundamentals`: the snapshot's fundamentals table, after
#'     `apply_fundamentals_corrections()` with `harness/sources/fundamentals_corrections.csv`;
#'   - `economy`: the snapshot's economy table;
#'   - `economy_release`: `read_economy_release()`;
#'   - `truth`: `build_truth()`;
#'   - `availability`: `poll_availability()` of the snapshot's polls as they are, before
#'     the n corrections, so it is the table `write_availability()` commits;
#'   - `entry`: `entry_table()` of the snapshot's national polls as they are;
#'   - `houses`: `read_houses()`; `coalitions`: `read_coalitions()`;
#'   - `n_corrections`, `fundamentals_corrections`: `read_n_corrections()` and
#'     `read_fundamentals_corrections()`.
#'
#'   The snapshot is verified once, and each snapshot file again as it is read. Nothing
#'   is read from the live `data/` or from the committed generated tables. An error
#'   under a locale that is not UTF-8.
#' @export
read_inputs <- function() {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop("read_inputs() needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  }
  box::use(
    R / harness / availability[poll_availability, read_constituency, read_polling],
    R / harness / inputs[snapshot_file, verify_snapshot],
    R / harness / tables[entry_table, read_coalitions, read_houses],
    R / harness / truth[build_truth]
  )
  verify_snapshot()
  polling <- read_polling()
  constituency <- read_constituency()
  # WHY: availability and entry come from the snapshot as it is, so the committed
  # availability.csv and entry_table.csv stay rebuilds of it; only the rows a cell reads
  # carry the n corrections.
  availability <- poll_availability(polling, constituency)
  entry <- entry_table(polling)
  n_corrections <- read_n_corrections()
  fixed <- apply_n_corrections(polling, constituency, n_corrections)
  fund <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE, progress = FALSE)
  fundamentals_corrections <- read_fundamentals_corrections()
  list(
    polling = fixed$polling,
    constituency = fixed$constituency,
    fundamentals = apply_fundamentals_corrections(fund, fundamentals_corrections),
    economy = readr::read_csv(snapshot_file("economy_data"), show_col_types = FALSE, progress = FALSE),
    economy_release = read_economy_release(),
    truth = build_truth(),
    availability = availability,
    entry = entry,
    houses = read_houses(),
    coalitions = read_coalitions(),
    n_corrections = n_corrections,
    fundamentals_corrections = fundamentals_corrections
  )
}

# The rows of `rows` whose flokkur is in `parties`; an error if that drops a row with n > 0.
keep_parties <- function(rows, parties, what) {
  drop <- !rows$flokkur %in% parties
  lost <- rows[drop & rows$n > 0, ]
  if (nrow(lost)) {
    where <- paste(lost$fyrirtaeki, format(lost$date), lost$flokkur)
    if ("kjordaemi" %in% names(lost)) where <- paste(lost$fyrirtaeki, format(lost$date), lost$kjordaemi, lost$flokkur)
    stop("restricting to the cell's parties would drop ", what, " row(s) with n > 0: ", paste(where, collapse = "; "))
  }
  rows[!drop, ]
}

#' The polls of one forecast cell.
#' @param E the election (Date)
#' @param F the forecast date (Date)
#' @param polling,constituency national and kjördæmi poll rows, as `read_inputs()`
#' @param availability a `poll_availability()` table
#' @return `list(parties, polls_nat, polls_kd, kosning_nat)`:
#'   - `parties`: the `POLL_PARTIES` (in that order) with n > 0 in an admitted national
#'     poll: dated from `WINDOW_START`, not `Kosning`, `available_from <= F`;
#'   - `polls_nat`: the rows of those polls for `parties`, a tibble (date, fyrirtaeki,
#'     flokkur, n, available_from);
#'   - `polls_kd`: the rows of every admitted kjördæmi poll (not `Kosning`) for `parties`,
#'     a tibble (date, fyrirtaeki, kjordaemi, flokkur, n, available_from);
#'   - `kosning_nat`: the national `Kosning` rows dated from `WINDOW_START` and before E
#'     for `parties`, with polls_nat's columns.
#'
#'   Rows are sorted by date, fyrirtaeki, `KJORDAEMI` order and `POLL_PARTIES` order. An
#'   error if a poll dated from `WINDOW_START` has no `availability` row, or if keeping
#'   only `parties` would drop a national, kjördæmi or `Kosning` row with n > 0.
#' @export
cell_polls <- function(E, F, polling, constituency, availability) {
  box::use(R / harness / constants[ELECTION_HOUSE, KJORDAEMI, POLL_PARTIES, WINDOW_START])
  with_availability <- function(rows, level) {
    av <- availability[availability$level == level, c("date", "fyrirtaeki", "available_from")]
    none <- dplyr::distinct(dplyr::anti_join(rows, av, by = c("date", "fyrirtaeki")), date, fyrirtaeki)
    if (nrow(none)) {
      stop("poll(s) with no availability row: ", paste(level, none$fyrirtaeki, format(none$date), collapse = "; "))
    }
    dplyr::left_join(rows, av, by = c("date", "fyrirtaeki"), relationship = "many-to-one")
  }
  sorted <- function(rows, cols) {
    rows <- rows[cols]
    kd <- if ("kjordaemi" %in% cols) match(rows$kjordaemi, KJORDAEMI$kjordaemi) else integer(nrow(rows))
    # WHY: radix ordering compares strings bytewise, whatever the locale.
    rows[order(rows$date, rows$fyrirtaeki, kd, match(rows$flokkur, POLL_PARTIES), method = "radix"), ]
  }
  nat_cols <- c("date", "fyrirtaeki", "flokkur", "n", "available_from")
  kd_cols <- c("date", "fyrirtaeki", "kjordaemi", "flokkur", "n", "available_from")

  nat <- with_availability(polling[polling$date >= WINDOW_START, ], "national")
  kd <- with_availability(
    constituency[constituency$date >= WINDOW_START & constituency$fyrirtaeki != ELECTION_HOUSE, ], "kjordaemi"
  )
  admitted <- nat[nat$fyrirtaeki != ELECTION_HOUSE & nat$available_from <= F, ]
  parties <- POLL_PARTIES[POLL_PARTIES %in% admitted$flokkur[admitted$n > 0]]
  list(
    parties = parties,
    polls_nat = sorted(keep_parties(admitted, parties, "national"), nat_cols),
    polls_kd = sorted(keep_parties(kd[kd$available_from <= F, ], parties, "kj\u00f6rd\u00e6mi"), kd_cols),
    kosning_nat = sorted(keep_parties(nat[nat$fyrirtaeki == ELECTION_HOUSE & nat$date < E, ], parties, "Kosning"), nat_cols)
  )
}

# The economy covariates of E's target rows as `release` records them, a tibble
# (covariate, period, value, first_release) in ECONOMY_COVARIATES order; an error unless
# each has a row whose value is the snapshot's at E and which was first released by F.
cell_economy <- function(E, F, release, econ) {
  rows <- release[release$election == E, ]
  missing <- setdiff(ECONOMY_COVARIATES, rows$covariate)
  if (length(missing)) {
    stop("economy_release.csv has no row for E = ", format(E), ": ", paste(missing, collapse = ", "))
  }
  rows <- rows[match(ECONOMY_COVARIATES, rows$covariate), ]
  at_E <- econ[which(econ$date == E), ]
  if (nrow(at_E) != 1) stop("no complete economy row for E = ", format(E))
  snapshot <- vapply(ECONOMY_COVARIATES, \(cov) at_E[[cov]], numeric(1))
  differ <- is.na(snapshot) | rows$value != snapshot
  if (any(differ)) {
    stop(
      "economy_release.csv differs from the snapshot's economy_data.csv at E = ", format(E), ": ",
      paste0(ECONOMY_COVARIATES[differ], " ", format(rows$value[differ], digits = 15), " against ",
        format(snapshot[differ], digits = 15),
        collapse = "; "
      )
    )
  }
  late <- rows$first_release > F
  if (any(late)) {
    stop(
      "F = ", format(F), " is before the first release of economy covariate(s) of E = ", format(E), ": ",
      paste0(rows$covariate[late], " (", rows$period[late], ", first released ", format(rows$first_release[late]), ")",
        collapse = "; "
      )
    )
  }
  tibble::tibble(covariate = rows$covariate, period = rows$period, value = rows$value, first_release = rows$first_release)
}

#' The information set of one forecast cell: everything a forecast of election E made on
#' day F may use.
#' @param E a window election (Date or ISO string); 2016-10-29 has no previous kjördæmi
#'   results, so it fails
#' @param F the forecast date: any date after the election before E
#'   (`previous_election(E)`) and before E, not only a `TARGETS` one, and on or after the
#'   day Hagstofa first published each economy covariate of E's target rows
#'   (`harness/sources/economy_release.csv`); anything else is an error, naming the
#'   covariates first released after F, so no cell sees a result or an economy figure
#'   that was not public at F. At an F outside `TARGETS` the poll bound is weaker:
#'   `poll_publication.csv` is complete only for the national polls a `TARGETS` F admits
#'   with `available_from` in (F - 45, F], so any other poll is admitted from its rule
#'   date (a known fieldwork end + 1, or the fallback), which can precede its
#'   publication. The economy bound holds at every F. The carried fundamentals columns
#'   are known before E, not necessarily at F (`harness/README.md`)
#' @param inp a `read_inputs()` list
#' @return a list:
#'   - `E`, `F`;
#'   - `parties`, `polls_nat`, `polls_kd`, `kosning_nat`: as `cell_polls()`, with the n
#'     corrections applied;
#'   - `results`: the `fold_truth()` rows of the elections before E;
#'   - `fundamentals`: `fundamentals_inputs(E, F, ..., parties)` with `coalitions` below;
#'   - `economy`: a tibble (covariate, period, value, first_release), the rows of
#'     `economy_release.csv` for E, vnv then growth: the value E's target rows carry
#'     (the snapshot's, which must equal the table's) and the day Hagstofa first
#'     published that figure, on or before F;
#'   - `coalitions`: the coalition rows with `from <= F`, a `to` after F set to NA;
#'   - `coalition_event`: `coalition_event(F, coalitions)`;
#'   - `layout`: `layout_for(E)`;
#'   - `valid_votes_open`: `expected_valid_votes(truth, E)`;
#'   - `prev_shares`: a tibble (election, previous, flokkur, share), for each window
#'     election up to and including E, `prev_national_shares()`: the national shares of
#'     every `POLL_PARTIES` party at `previous`, the election before it (NA for a party
#'     with no list);
#'   - `entry`: the entry table's rows for `parties`;
#'   - `houses`: the house table's rows for the houses with an admitted poll
#'     (`polls_nat` or `polls_kd`) and for `ELECTION_HOUSE`;
#'   - `availability`: the `poll_availability()` rows of the polls in `polls_nat` and
#'     `polls_kd`, for `validate_inputs()`'s overlap report.
#' @export
info_set <- function(E, F, inp = read_inputs()) {
  box::use(
    R / election_utils[layout_for],
    R / harness / constants[ELECTION_HOUSE, ELECTIONS, POLL_PARTIES, previous_election],
    R / harness / fundamentals[fundamentals_inputs],
    R / harness / tables[coalition_event, expected_valid_votes],
    R / harness / truth[fold_truth, prev_national_shares]
  )
  E <- as.Date(E)
  F <- as.Date(F)
  if (length(E) != 1 || length(F) != 1 || is.na(E) || is.na(F)) stop("E and F must be one date each")
  prev <- previous_election(E)
  if (!(prev < F && F < E)) {
    stop(
      "F = ", format(F), " is outside the cycle of E = ", format(E), ": F must be after ", format(prev),
      ", the election before E, and before E"
    )
  }
  economy <- cell_economy(E, F, inp$economy_release, inp$economy)
  polls <- cell_polls(E, F, inp$polling, inp$constituency, inp$availability)
  parties <- polls$parties

  coalitions <- inp$coalitions[inp$coalitions$from <= F, ]
  coalitions$to[!is.na(coalitions$to) & coalitions$to > F] <- NA

  results <- fold_truth(inp$truth)
  window <- ELECTIONS[ELECTIONS <= E]
  prev_shares <- dplyr::bind_rows(lapply(seq_along(window), \(i) {
    s <- prev_national_shares(window[i], inp$truth, inp$fundamentals)
    tibble::tibble(election = window[i], previous = previous_election(window[i]), flokkur = POLL_PARTIES, share = unname(s[POLL_PARTIES]))
  }))
  keys <- dplyr::bind_rows(
    national = dplyr::distinct(polls$polls_nat, date, fyrirtaeki),
    kjordaemi = dplyr::distinct(polls$polls_kd, date, fyrirtaeki),
    .id = "level"
  )

  list(
    E = E,
    F = F,
    parties = parties,
    polls_nat = polls$polls_nat,
    polls_kd = polls$polls_kd,
    kosning_nat = polls$kosning_nat,
    results = results[results$election < E, ],
    fundamentals = fundamentals_inputs(E, F, inp$fundamentals, inp$economy, coalitions, parties),
    economy = economy,
    coalitions = coalitions,
    coalition_event = coalition_event(F, coalitions),
    layout = layout_for(E),
    valid_votes_open = expected_valid_votes(inp$truth, E),
    prev_shares = prev_shares,
    entry = inp$entry[inp$entry$flokkur %in% parties, ],
    houses = inp$houses[inp$houses$fyrirtaeki %in% c(ELECTION_HOUSE, keys$fyrirtaeki), ],
    availability = dplyr::semi_join(inp$availability, keys, by = c("level", "date", "fyrirtaeki"))
  )
}

#' The leak check of an information set (spec §5.3).
#' @param info an `info_set()` list
#' @return `invisible(list(admitted, overlaps))`: `admitted` a tibble (level, date,
#'   fyrirtaeki, available_from), one row per admitted poll, national first, then by date
#'   and fyrirtaeki, which Phase 3 writes into `FREEZE.md`; `overlaps` the
#'   `fieldwork_overlaps()` of those polls, a report, never a filter (§5.3 rule 2). Stops,
#'   at the first check that fails, with:
#'   - "poll available after F": a `polls_nat`, `polls_kd` or `availability` row with
#'     `available_from` after F or NA;
#'   - "poll dated on or after E": such a row dated E or later;
#'   - "poll before the window": such a row, or a `kosning_nat` row, dated before
#'     `WINDOW_START`;
#'   - "Kosning row among the polls": a `polls_nat` or `polls_kd` row of `ELECTION_HOUSE`;
#'   - "poll available on or before its midpoint": such a row with `available_from` on or
#'     before its `date` (the fieldwork midpoint);
#'   - "target election result": a `results` row of E or later, or a `kosning_nat` row
#'     dated E or later;
#'   - "target vote share": a non-NA `voteshare`, `voteshare_change` or
#'     `total_valid_votes` in `fundamentals$target`, or a `fundamentals$train` row dated E
#'     or later;
#'   - "election result not known at F": a `results` election, `kosning_nat` row,
#'     `fundamentals$train` row or `prev_shares` previous election dated F or later;
#'   - "event after F": a `coalitions` row starting after F, or ending after F;
#'   - "incumbency not as at F": a `fundamentals$target` row whose `incumbent` or
#'     `incumbent_prime_minister` differs from `incumbents_at(F)` or `pm_at(F)` on
#'     `coalitions`;
#'   - "economy covariate not known at F": an `economy` row first released after F (or
#'     with no date), a covariate (vnv, growth) with no `economy` row, or a
#'     `fundamentals$target` `vnv` or `growth` that differs from its `economy` value;
#'   - "party not yet available": a party in `parties` with no admitted national poll
#'     row with n > 0, or an `entry` row for a party outside `parties`, or whose
#'     `first_nonzero` is neither an admitted national poll of the party with n > 0 nor
#'     a poll from before `WINDOW_START` (whose availability is not recorded) dated
#'     before F;
#'   - "house with no admitted poll": a `houses` row, other than `ELECTION_HOUSE`, with
#'     no `polls_nat` or `polls_kd` row.
#'
#'   Each message goes on, after ": ", to name the offending rows, `MAX_NAMED` (10) at
#'   most and then the count: a poll as level, date and fyrirtaeki (a `kosning_nat` row
#'   as national), a result as "election" and its date, a fundamentals value as "target",
#'   the party and the column, a training row as "train", its date and party, a previous
#'   share as "prev_shares" and the date of its previous election, a coalition row as its
#'   cabinet, `from` and `to`, an economy row as "economy", its covariate, period and
#'   first release, and a party by its name. An NA where a date is compared
#'   counts as a failure.
#' @export
validate_inputs <- function(info) {
  box::use(
    R / harness / availability[fieldwork_overlaps],
    R / harness / constants[ELECTION_HOUSE, WINDOW_START],
    R / harness / tables[incumbents_at, pm_at]
  )
  E <- info$E
  F <- info$F
  hit <- function(cond) cond | is.na(cond)
  fail <- function(message, items) {
    items <- unique(items)
    if (!length(items)) return(invisible())
    more <- if (length(items) > MAX_NAMED) paste0("; ... (", length(items), " in all)") else ""
    stop(message, ": ", paste(utils::head(items, MAX_NAMED), collapse = "; "), more, call. = FALSE)
  }
  polls <- dplyr::bind_rows(national = info$polls_nat, kjordaemi = info$polls_kd, .id = "level")
  ids <- paste(polls$level, format(polls$date), polls$fyrirtaeki)
  kosning <- info$kosning_nat
  kosning_ids <- paste(rep("national", nrow(kosning)), format(kosning$date), kosning$fyrirtaeki)
  av <- info$availability
  av_ids <- paste(av$level, format(av$date), av$fyrirtaeki)
  results <- info$results$election
  target <- info$fundamentals$target
  train <- info$fundamentals$train
  train_ids <- paste("train", format(train$date), train$flokkur)
  previous <- info$prev_shares$previous
  co <- info$coalitions

  fail("poll available after F", c(ids[hit(polls$available_from > F)], av_ids[hit(av$available_from > F)]))
  fail("poll dated on or after E", ids[hit(polls$date >= E)])
  fail("poll before the window", c(ids[hit(polls$date < WINDOW_START)], kosning_ids[hit(kosning$date < WINDOW_START)]))
  fail("Kosning row among the polls", ids[hit(polls$fyrirtaeki == ELECTION_HOUSE)])
  fail("poll available on or before its midpoint", ids[hit(polls$available_from <= polls$date)])
  fail(
    "target election result",
    c(paste("election", format(results))[hit(results >= E)], kosning_ids[hit(kosning$date >= E)])
  )
  shares <- unlist(lapply(c("voteshare", "voteshare_change", "total_valid_votes"), \(col) {
    paste("target", target$flokkur, col)[!is.na(target[[col]])]
  }))
  fail("target vote share", c(shares, train_ids[hit(train$date >= E)]))
  fail("election result not known at F", c(
    paste("election", format(results))[hit(results >= F)], kosning_ids[hit(kosning$date >= F)],
    train_ids[hit(train$date >= F)], paste("prev_shares", format(previous))[hit(previous >= F)]
  ))
  events <- paste(co$cabinet, "from", format(co$from), "to", format(co$to))
  fail("event after F", events[hit(co$from > F) | (!is.na(co$to) & co$to > F)])
  incumbents <- incumbents_at(F, co)
  pm <- pm_at(F, co)
  fail("incumbency not as at F", c(
    paste("target", target$flokkur, "incumbent")[hit(target$incumbent != (target$flokkur %in% incumbents))],
    paste("target", target$flokkur, "incumbent_prime_minister")[hit(target$incumbent_prime_minister != (target$flokkur == pm))]
  ))
  econ <- info$economy
  no_row <- setdiff(ECONOMY_COVARIATES, econ$covariate)
  fail("economy covariate not known at F", c(
    if (length(no_row)) paste("economy", no_row, "has no row"),
    paste("economy", econ$covariate, econ$period, "first released", format(econ$first_release))[hit(econ$first_release > F)],
    unlist(lapply(ECONOMY_COVARIATES, \(cov) {
      paste("target", target$flokkur, cov)[hit(target[[cov]] != econ$value[match(cov, econ$covariate)])]
    }))
  ))
  nat <- info$polls_nat
  known <- nat[which(nat$n > 0 & nat$available_from <= F), ]
  entry <- info$entry
  # WHY: no availability is recorded before the window, so a first poll from before it
  # counts as known once F is past it.
  first_known <- (entry$first_nonzero < WINDOW_START & entry$first_nonzero < F) |
    paste(entry$flokkur, format(entry$first_nonzero)) %in% paste(known$flokkur, format(known$date))
  fail("party not yet available", c(
    setdiff(info$parties, known$flokkur),
    entry$flokkur[!(entry$flokkur %in% info$parties & first_known %in% TRUE)]
  ))
  houses <- info$houses$fyrirtaeki
  fail("house with no admitted poll", houses[!houses %in% c(ELECTION_HOUSE, polls$fyrirtaeki)])

  admitted <- dplyr::distinct(polls, level, date, fyrirtaeki, available_from)
  admitted <- admitted[order(match(admitted$level, POLL_LEVELS), admitted$date, admitted$fyrirtaeki, method = "radix"), ]
  overlaps <- fieldwork_overlaps(dplyr::semi_join(info$availability, admitted, by = c("level", "date", "fyrirtaeki")))
  invisible(list(admitted = admitted, overlaps = overlaps))
}
