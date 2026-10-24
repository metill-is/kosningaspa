# Fixed constants of the v2 harness: the window, its elections, the forecast cells, the
# poll party set and the kjördæmi labels.

root_dir <- normalizePath(box::file("..", ".."), mustWork = TRUE)

#' Absolute path of the project root, fixed when this module loads, so it does not
#' depend on the working directory.
#' @export
project_root <- function() {
  root_dir
}

#' Absolute path under `harness/`.
#' @param ... path components, as in `file.path()`
#' @export
harness_path <- function(...) {
  file.path(project_root(), "harness", ...)
}

#' First day of the polling window.
#' @export
WINDOW_START <- as.Date("2016-01-01")

#' The pseudo-pollster (`fyrirtaeki`) whose poll rows are official election results.
#' @export
ELECTION_HOUSE <- "Kosning"

#' The Alþingi elections in the window.
#' @export
ELECTIONS <- as.Date(c("2016-10-29", "2017-10-28", "2021-09-25", "2024-11-30"))

#' The election before each window election (2013-04-27 for 2016-10-29).
#' @param E a Date (or ISO string) vector of window elections; anything else is an error
#' @return a Date vector
#' @export
previous_election <- function(E) {
  E <- as.Date(E)
  i <- match(E, ELECTIONS)
  if (anyNA(i)) {
    stop("not a window election: ", paste(format(E[is.na(i)]), collapse = ", "))
  }
  c(as.Date("2013-04-27"), ELECTIONS)[i]
}

#' Forecast cells: election E, forecast date F, horizon tag and whether the cell is the
#' held-out confirmatory 2017 target.
#' @export
TARGETS <- tibble::tibble(
  E = as.Date(rep(c("2017-10-28", "2021-09-25", "2024-11-30"), c(2, 4, 4))),
  F = as.Date(c(
    "2017-10-14", "2017-10-27",
    "2021-03-29", "2021-08-09", "2021-09-11", "2021-09-24",
    "2024-06-03", "2024-10-14", "2024-11-16", "2024-11-29"
  )),
  tag = c("f14", "eve", rep(c("f180", "f47", "f14", "eve"), 2)),
  confirmatory = E == as.Date("2017-10-28")
)

#' The ten poll parties, labelled as in the data.
#' @export
POLL_PARTIES <- c(
  "Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Samfylkingin", "Vinstri Græn",
  "Píratar", "Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn",
  "Annað"
)

#' List letter to poll party, for the letters that map to a named party.
#' @export
LETTER_PARTY <- c(
  B = "Framsóknarflokkurinn", C = "Viðreisn", D = "Sjálfstæðisflokkurinn",
  F = "Flokkur Fólksins", J = "Sósíalistaflokkurinn", M = "Miðflokkurinn",
  P = "Píratar", S = "Samfylkingin", V = "Vinstri Græn"
)

#' Poll party of a list letter: `LETTER_PARTY[letter]`, and Annað for every other letter.
#' @param letter character vector of list letters (no NA)
#' @return an unnamed character vector
#' @export
party_of_letter <- function(letter) {
  letter <- as.character(letter)
  stopifnot("a list letter is missing" = !anyNA(letter))
  party <- unname(LETTER_PARTY[letter])
  party[is.na(party)] <- "Annað"
  party
}

#' Poll-party label of each `fundamentals_data.csv` party label that differs from it.
#' @export
FUNDAMENTALS_RENAMED <- c("Framsókn" = "Framsóknarflokkurinn")

#' Kjördæmi labels: poll label, Hagstofa sheet label and KOS02018 code. Join on these
#' by name, never by position. The code "NA" (Norðaustur) is a string, not a missing
#' value: read it with `na = character()`.
#' @export
KJORDAEMI <- tibble::tibble(
  kjordaemi = c(
    "Reykjavík Suður", "Reykjavík Norður", "Suðvestur", "Suður", "Norðaustur", "Norðvestur"
  ),
  hagstofa = c(
    "Reykjavíkurkjördæmi suður", "Reykjavíkurkjördæmi norður", "Suðvesturkjördæmi",
    "Suðurkjördæmi", "Norðausturkjördæmi", "Norðvesturkjördæmi"
  ),
  code = c("RS", "RN", "SV", "SU", "NA", "NV")
)
