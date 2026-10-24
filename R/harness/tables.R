# Lookup tables of the v2 harness (spec §7): houses and their lineage (D7), each poll
# party's entry (D8), the epoch of a date, the cabinet in office on a day (the Brier
# event's coalition, D9, and the incumbents), and each kjördæmi's valid votes (D6).

#' Epoch of each date: 1 + the number of elections strictly before it, so an election
#' day belongs to the epoch it closes.
#' @param date a Date (or ISO string) vector
#' @param elections the elections that close the epochs
#' @return an integer vector; NA for an NA date
#' @export
epoch_of <- function(date, elections = ELECTIONS) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[ELECTIONS])
  1L + findInterval(as.Date(date), sort(as.Date(elections)), left.open = TRUE)
}

#' The house table: each pollster's house and house lineage.
#' @param path the CSV (fyrirtaeki, house, lineage)
#' @return a tibble (fyrirtaeki, house, lineage) in file order; an error on a missing
#'   value or a fyrirtaeki listed twice
#' @export
read_houses <- function(path = harness_path("sources", "house_table.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  h <- readr::read_csv(path, col_types = readr::cols(fyrirtaeki = "c", house = "c", lineage = "c"))
  if (anyNA(h)) stop(basename(path), ": missing value(s)")
  twice <- unique(h$fyrirtaeki[duplicated(h$fyrirtaeki)])
  if (length(twice)) stop(basename(path), ": fyrirtaeki listed more than once: ", paste(twice, collapse = ", "))
  h
}

#' Each poll party's first non-zero poll and its entry epoch.
#' @param polling national polls (date, fyrirtaeki, flokkur, n), pre-window rows included;
#'   the `Kosning` rows (official results) are ignored
#' @param window_start first day of the polling window
#' @return a tibble (flokkur, first_nonzero, entry_epoch, newcomer) sorted by
#'   first_nonzero, then flokkur; a newcomer has no n > 0 poll dated before window_start.
#'   A party polled before the window gets epoch 1, and `epoch_of(E) - entry_epoch`
#'   counts the window elections it contested before E.
#' @export
entry_table <- function(polling, window_start = WINDOW_START) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[ELECTION_HOUSE, WINDOW_START])
  polling |>
    dplyr::filter(fyrirtaeki != ELECTION_HOUSE, n > 0) |>
    dplyr::summarise(first_nonzero = min(as.Date(date)), .by = flokkur) |>
    dplyr::mutate(
      entry_epoch = epoch_of(first_nonzero),
      newcomer = first_nonzero >= as.Date(window_start)
    ) |>
    dplyr::arrange(first_nonzero, flokkur)
}

#' Write the entry table of the snapshot's national polls.
#' @param path the CSV to write
#' @return the `entry_table()` tibble, invisibly
#' @export
write_entry_table <- function(path = harness_path("entry_table.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path], R / harness / inputs[snapshot_file])
  polling <- readr::read_csv(
    snapshot_file("polling_data"),
    col_types = readr::cols_only(date = "D", fyrirtaeki = "c", flokkur = "c", n = "d")
  )
  e <- entry_table(polling)
  readr::write_csv(e, path)
  invisible(e)
}

COALITION_COLUMNS <- c("cabinet", "from", "to", "parties", "pm", "caretaker", "source_url")

#' The coalition events: one row per composition of the cabinet in office, a new row
#' wherever the parties holding ministries or the caretaker status change.
#' @param path the CSV (cabinet, from, to, parties, pm, caretaker, source_url)
#' @return a tibble in file order; `parties` and `pm` hold list letters (`parties` joined
#'   by `;`), and `to` is NA for the cabinet still in office. An error on other columns, a
#'   missing or unparsable value, no rows, a list letter outside `LETTER_PARTY` and A, a
#'   `pm` not among the row's `parties`, a cabinet whose rows are not contiguous, an open
#'   end anywhere but on the last row (which must have one), a `to` not after its `from`,
#'   or consecutive rows that overlap or leave a gap (each `from` must equal the previous
#'   row's `to`)
#' @export
read_coalitions <- function(path = harness_path("sources", "coalition_events.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path, LETTER_PARTY])
  # WHY: a parse issue becomes the error below, so readr's warning about it is muffled.
  co <- withCallingHandlers(
    readr::read_csv(path, progress = FALSE, col_types = readr::cols(
      cabinet = "c", from = "D", to = "D", parties = "c", pm = "c", caretaker = "l", source_url = "c"
    )),
    vroom_parse_issue = function(w) invokeRestart("muffleWarning")
  )
  where <- function(rows) if (length(rows)) paste(rows, collapse = ", ") else "none"
  if (!identical(names(co), COALITION_COLUMNS)) {
    stop(basename(path), ": columns must be ", paste(COALITION_COLUMNS, collapse = ", "))
  }
  bad <- readr::problems(co)
  if (nrow(bad)) stop(basename(path), ": unparsable value(s) in line(s) ", where(unique(bad$row)))
  if (anyNA(co[setdiff(COALITION_COLUMNS, "to")])) stop(basename(path), ": missing value(s)")
  n <- nrow(co)
  if (n == 0) stop(basename(path), ": no rows")
  lists <- strsplit(co$parties, ";", fixed = TRUE)
  known <- vapply(seq_len(n), \(i) {
    all(c(lists[[i]], co$pm[i]) %in% c(names(LETTER_PARTY), "A")) &&
      identical(paste(lists[[i]], collapse = ";"), co$parties[i])
  }, logical(1))
  if (!all(known)) stop(basename(path), ": unknown list letter in row(s) ", where(which(!known)))
  no_pm <- which(!vapply(seq_len(n), \(i) co$pm[i] %in% lists[[i]], logical(1)))
  if (length(no_pm)) stop(basename(path), ": pm not among the row's parties in row(s) ", where(no_pm))
  runs <- rle(co$cabinet)$values
  scattered <- unique(runs[duplicated(runs)])
  if (length(scattered)) stop(basename(path), ": rows of a cabinet not contiguous: ", paste(scattered, collapse = ", "))
  open <- which(is.na(co$to))
  if (!identical(open, n)) {
    stop(basename(path), ": the last row, and only it, must have an open end; open row(s): ", where(open))
  }
  backwards <- which(co$to <= co$from)
  if (length(backwards)) stop(basename(path), ": `to` is not after `from` in row(s) ", where(backwards))
  broken <- which(co$from[-1] != co$to[-n]) + 1L
  if (length(broken)) {
    stop(basename(path), ": gap or overlap: row(s) ", where(broken), " do not start where the previous row ends")
  }
  co
}

#' The coalition row in office on day F: `from <= F < to`, an NA `to` counting as open.
#' @param F one Date (or ISO string)
#' @param coalitions the `read_coalitions()` tibble
#' @return a one-row tibble; an error unless exactly one row is in office on F
#' @export
cabinet_at <- function(F, coalitions) {
  F <- as.Date(F)
  if (length(F) != 1 || is.na(F)) stop("F must be one date")
  hit <- coalitions$from <= F & (is.na(coalitions$to) | F < coalitions$to)
  if (sum(hit) != 1) {
    stop(if (any(hit)) paste(sum(hit), "cabinets") else "no cabinet", " in office on ", format(F))
  }
  coalitions[hit, ]
}

list_letters <- function(parties) strsplit(parties, ";", fixed = TRUE)[[1]]

# Poll-party labels of list letters in POLL_PARTIES order; letters that map to Annað are
# dropped.
named_parties <- function(lists) {
  box::use(R / harness / constants[POLL_PARTIES, party_of_letter])
  setdiff(intersect(POLL_PARTIES, party_of_letter(lists)), "Annað")
}

#' The Brier event's coalition (spec §5.6): the composition in office on day F.
#' @param F one Date (or ISO string)
#' @param coalitions the `read_coalitions()` tibble
#' @return `list(parties, dropped)`: `parties` the poll-party labels in `POLL_PARTIES`
#'   order, `dropped` the sorted list letters that map to Annað (`character(0)` if none),
#'   which win no seat in a forecast
#' @export
coalition_event <- function(F, coalitions) {
  box::use(R / harness / constants[party_of_letter])
  lists <- list_letters(cabinet_at(F, coalitions)$parties)
  list(parties = named_parties(lists), dropped = sort(lists[party_of_letter(lists) == "Annað"]))
}

#' The incumbents on day F, as `fundamentals_data.csv` codes them: the opening composition
#' of the cabinet in office (its earliest row), so a party that left during the cabinet's
#' term still counts.
#' @param F one Date (or ISO string)
#' @param coalitions the `read_coalitions()` tibble
#' @return poll-party labels in `POLL_PARTIES` order; letters that map to Annað are dropped
#' @export
incumbents_at <- function(F, coalitions) {
  rows <- coalitions[coalitions$cabinet == cabinet_at(F, coalitions)$cabinet, ]
  named_parties(list_letters(rows$parties[which.min(rows$from)]))
}

#' The prime minister's party on day F.
#' @param F one Date (or ISO string)
#' @param coalitions the `read_coalitions()` tibble
#' @return one poll-party label
#' @export
pm_at <- function(F, coalitions) {
  box::use(R / harness / constants[party_of_letter])
  party_of_letter(cabinet_at(F, coalitions)$pm)
}

#' Valid votes per kjördæmi at one election: the votes of every list summed, matched to
#' `KJORDAEMI` by name. These are the kjördæmi weights w_{e,k} of a closed election.
#' @param truth a `build_truth()` table
#' @param election one election (Date or ISO string)
#' @return a tibble (kjordaemi, votes, w) in `KJORDAEMI` order, w = votes / sum(votes); an
#'   error if `truth` has no rows for the election, or its kjördæmi there are not exactly
#'   the six of `KJORDAEMI`
#' @export
valid_votes <- function(truth, election) {
  box::use(R / harness / constants[KJORDAEMI])
  election <- as.Date(election)
  if (length(election) != 1 || is.na(election)) stop("election must be one date")
  te <- truth[which(truth$election == election), ]
  if (!nrow(te)) stop("no kjördæmi results for ", format(election))
  votes <- tapply(te$votes, te$kjordaemi, sum)
  missing <- setdiff(KJORDAEMI$kjordaemi, names(votes))
  extra <- setdiff(names(votes), KJORDAEMI$kjordaemi)
  if (length(missing) || length(extra)) {
    none <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"
    stop(format(election), ": kjördæmi missing: ", none(missing), "; not in KJORDAEMI: ", none(extra))
  }
  votes <- as.vector(votes[KJORDAEMI$kjordaemi])
  tibble::tibble(kjordaemi = KJORDAEMI$kjordaemi, votes = votes, w = votes / sum(votes))
}

#' Expected valid votes per kjördæmi for a forecast of E: the previous election's
#' (spec §5.3 rule 8, §6.7), the N_k and w_open of the forecast.
#' @param truth a `build_truth()` table
#' @param E a window election (Date or ISO string)
#' @return `valid_votes(truth, previous_election(E))`; an error if `truth` has no results
#'   for that election, as for E = 2016-10-29 (2013-04-27 is outside `truth`)
#' @export
expected_valid_votes <- function(truth, E) {
  box::use(R / harness / constants[previous_election])
  prev <- previous_election(E)
  if (!any(truth$election == prev, na.rm = TRUE)) {
    stop("no kjördæmi results for ", format(prev), ", the election before ", format(as.Date(E)))
  }
  valid_votes(truth, prev)
}
