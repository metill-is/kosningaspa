# Fundamentals inputs of the v2 harness (spec §5.3 rules 4–5, §6.5, §7 D3): the
# training rows of the stage-1 regression, the target rows of election E as known at
# forecast date F, and S_f, the parties the fundamentals speak for.

# The fund columns of a target row besides date and flokkur: known before E and carried
# from the party's row at E, coded as at F, or outcomes of E and masked.
CARRIED <- c("year", "new_party", "party_split", "voteshare_prior", "incumbent_years")
AT_F <- c("incumbent", "incumbent_prime_minister")
MASKED <- c("voteshare", "voteshare_change", "total_valid_votes")

relabel <- function(flokkur) {
  box::use(R / harness / constants[FUNDAMENTALS_RENAMED])
  flokkur <- as.character(flokkur)
  hit <- flokkur %in% names(FUNDAMENTALS_RENAMED)
  flokkur[hit] <- unname(FUNDAMENTALS_RENAMED[flokkur[hit]])
  flokkur
}

#' Poll party of a fundamentals label: Framsókn is Framsóknarflokkurinn, a label already
#' in `POLL_PARTIES` is kept, and every other label is Annað.
#' @param flokkur character vector of `fundamentals_data.csv` labels (no NA)
#' @return an unnamed character vector of `POLL_PARTIES` labels
#' @export
fundamentals_party <- function(flokkur) {
  box::use(R / harness / constants[POLL_PARTIES])
  if (anyNA(flokkur)) stop("a fundamentals party label is missing")
  party <- relabel(flokkur)
  party[!party %in% POLL_PARTIES] <- "Anna\u00f0"
  party
}

#' The fundamentals inputs for election E as known at forecast date F.
#' @param E a window election (Date or ISO string)
#' @param F the forecast date, before E
#' @param fund the fundamentals table (`fundamentals_data.csv`)
#' @param econ the economy table (`economy_data.csv`: vnv, date, growth), one row per date
#' @param coalitions the `read_coalitions()` tibble
#' @param parties the cell's poll parties, a subset of `POLL_PARTIES`
#' @return `list(train, target, S_f)`:
#'   - `train`: the `fund` rows dated before E in file order, inner-joined to `econ` by
#'     `date`, with `party` (the α label: Framsóknarflokkurinn for Framsókn, every other
#'     label as in the data, so historic parties keep theirs) after `flokkur` and `in_sf`
#'     last: `flokkur != "Annað"`, `voteshare_prior > 0` and `new_party == 0`, FALSE where
#'     `voteshare_prior` is NA.
#'   - `S_f`: the non-Annað `parties` whose row at E, labels mapped by
#'     `fundamentals_party()`, has `voteshare_prior > 0`, in `POLL_PARTIES` order. These
#'     must be the same parties as those with votes at `previous_election(E)`.
#'   - `target`: one row per `S_f` party, in `S_f` order, with `train`'s columns and
#'     types. `flokkur` and `party` are the poll label and `date` is E. `year`,
#'     `new_party`, `party_split`, `voteshare_prior` and `incumbent_years`, known before
#'     E, come from the party's row at E. `incumbent` and `incumbent_prime_minister` are
#'     as at F (`incumbents_at()`, `pm_at()`). `voteshare`, `voteshare_change` and
#'     `total_valid_votes`, outcomes of E, are NA. `vnv` and `growth` come from `econ` at
#'     E (`info_set()` admits only an F on or after their first release), and `in_sf` is
#'     TRUE.
#'
#'   An error if F is not before E, a party is outside `POLL_PARTIES`, `fund` has a
#'   column other than `date`, `flokkur` and those three groups, `econ` repeats a date,
#'   lacks a training election or has no complete row for E, a non-Annað party maps from
#'   more than one row at E, or S_f and the parties with votes at the previous election
#'   differ.
#' @export
fundamentals_inputs <- function(E, F, fund, econ, coalitions, parties) {
  box::use(
    R / harness / constants[POLL_PARTIES, previous_election],
    R / harness / tables[incumbents_at, pm_at]
  )
  E <- as.Date(E)
  F <- as.Date(F)
  if (length(E) != 1 || length(F) != 1 || is.na(E) || is.na(F)) stop("E and F must be one date each")
  if (F >= E) stop("F (", format(F), ") must be before E (", format(E), ")")
  outside <- setdiff(parties, POLL_PARTIES)
  if (length(outside)) stop("parties outside POLL_PARTIES: ", paste(outside, collapse = ", "))
  unclassified <- setdiff(names(fund), c("date", "flokkur", CARRIED, AT_F, MASKED))
  if (length(unclassified)) {
    stop("fundamentals column(s) neither carried, taken at F nor masked: ", paste(unclassified, collapse = ", "))
  }
  dates <- function(d) paste(format(sort(unique(d))), collapse = ", ")
  names_or_none <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"

  repeated <- econ$date[duplicated(econ$date)]
  if (length(repeated)) stop("economy table: more than one row for ", dates(repeated))
  earlier <- dplyr::filter(fund, date < E)
  no_econ <- setdiff(earlier$date, econ$date)
  if (length(no_econ)) stop("no economy row for training election(s) ", dates(as.Date(no_econ)))
  train <- earlier |>
    dplyr::inner_join(econ, by = "date") |>
    dplyr::mutate(party = relabel(flokkur), .after = flokkur) |>
    dplyr::mutate(in_sf = dplyr::coalesce(flokkur != "Anna\u00f0" & voteshare_prior > 0 & new_party == 0, FALSE))

  candidates <- intersect(POLL_PARTIES, setdiff(parties, "Anna\u00f0"))
  at_E <- fund[which(fund$date == E), ]
  mapped <- fundamentals_party(at_E$flokkur)
  twice <- intersect(candidates, mapped[duplicated(mapped)])
  if (length(twice)) stop(format(E), ": more than one row maps to ", paste(twice, collapse = ", "))
  prior <- at_E$voteshare_prior[match(candidates, mapped)]
  S_f <- candidates[!is.na(prior) & prior > 0]

  prev <- previous_election(E)
  at_prev <- fund[which(fund$date == prev), ]
  listed <- fundamentals_party(at_prev$flokkur)[!is.na(at_prev$voteshare) & at_prev$voteshare > 0]
  had_list <- intersect(candidates, listed)
  if (!setequal(S_f, had_list)) {
    stop(
      format(E), ": S_f and the parties with votes at ", format(prev), " differ; only by voteshare_prior: ",
      names_or_none(setdiff(S_f, had_list)), "; only by votes at ", format(prev), ": ",
      names_or_none(setdiff(had_list, S_f))
    )
  }

  econ_E <- econ[which(econ$date == E), ]
  if (nrow(econ_E) != 1 || anyNA(econ_E[c("vnv", "growth")])) {
    stop("no complete economy row for E = ", format(E))
  }
  coded <- function(x, like) {
    storage.mode(x) <- storage.mode(like)
    x
  }
  incumbents <- incumbents_at(F, coalitions)
  pm <- pm_at(F, coalitions)
  target <- at_E[match(S_f, mapped), ] |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(MASKED), \(x) x[NA_integer_]),
      date = E,
      flokkur = S_f,
      party = S_f,
      incumbent = coded(S_f %in% incumbents, fund$incumbent),
      incumbent_prime_minister = coded(S_f == pm, fund$incumbent_prime_minister),
      vnv = econ_E$vnv,
      growth = econ_E$growth,
      in_sf = TRUE
    ) |>
    dplyr::select(dplyr::all_of(names(train)))

  list(train = train, target = target, S_f = S_f)
}
