# Senate race layer for usa/R/build_generic_ballot.R races=senate: the target year's Senate
# races, their polls, and the past Senate results whose deviations from the national House
# vote teach the race regression (the R, X_race, d_hist and *_r fields of
# usa/stan/us_midterm_v1.stan to v9).
#
# Each race is a contest between a D slot and an R slot; everything else is Other. The R
# slot holds the Republican vote; the D slot holds the Democratic vote, or, where one ran
# ahead of every Democrat, a Democratic-aligned independent's (d_aligned_independents:
# Sanders, King, Orman 2014, McMullin 2022, Osborn 2024 and 2026), which the indep_d
# covariate marks. A race that is not a contest of the two slots is left out: any other
# candidate ahead of every Democrat (Lieberman 2006, Osborn the Indiana Libertarian 2006,
# Murkowski's 2010 write-in, Crist 2010, Miller 2016, Harrington 2020), a second Republican
# ahead of the D slot (Alaska 2022), or no candidate in a slot (California's
# Democrat-on-Democrat runoffs). So are odd-year specials, which have no national House vote
# to deviate from.
#
# Sources (paths under raw= and tables=; usa/README.md has where they come from):
#   results 2006-2024: Cha, Kuriwaki and Snyder, Candidates in American General Elections
#     (Harvard Dataverse doi:10.7910/DVN/DGDRDT, CC0), with each candidate's incumbency;
#   state presidential vote 1976-2024: MEDSL (doi:10.7910/DVN/42MVDX, CC0);
#   race polling error 1998-2022: FiveThirtyEight pollster-ratings raw_polls.csv (CC BY 4.0);
#   race polls 2018-2024: FiveThirtyEight senate_polls_historical.csv (CC BY 4.0), the
#     target cycle's general-election questions, not hypothetical, first-round only;
#   2026: tables/senate_races_2026.csv, senate_polls_2026.csv and
#     senate_polls_2026_by_race.csv (Wikipedia, CC BY-SA 4.0), whose pair names each race's
#     main contest; a regular race is that cycle's class (2 in 2026), a special that of
#     its sitting senator in unitedstates/congress-legislators legislators-current.csv (CC0).
# Covariates (Q = 4): intercept; the state's presidential log(D / R) minus the nation's at
# the latest presidential election before the race (the scale of the deviations it
# predicts; 0.2 is about 10 points of margin); incumbency, +1 for a D-slot incumbent, -1 for a Republican one, 0 open; indep_d.
# A race's deviation is its centred log shares minus those of the national House vote that
# year, with Other floored at 0.5% of the vote; with P = 2, of the two-party shares. Polls give the D and R slots' counts only,
# as the national polls do. senate_holdover_d counts the Democrats and the independents
# who caucus with them (Sanders, King) in seats not up that year, specials included (the
# last winner of each seat that no target race contests, with a term running past it), and
# the seats up that year with no Republican on the ballot (California's runoffs).

library(dplyr)
library(readr)

clr <- function(x) log(x) - mean(log(x))
dem_caucus_independents <- c("SANDERS", "KING")
d_aligned_independents <- c("VT SANDERS", "ME KING", "KS ORMAN", "UT MCMULLIN", "NE OSBORN")

# WHY: MEDSL flags Biden's and Trump's 2020 rows in DC as write-ins, so the flag is not
# filtered on; the other flagged major-party rows are real write-ins of the nominees.
presidential_votes <- function(medsl_president) {
  read_csv(medsl_president, show_col_types = FALSE) |>
    group_by(year, state_po) |>
    summarise(
      d = sum(candidatevotes[party_simplified %in% "DEMOCRAT"], na.rm = TRUE),
      r = sum(candidatevotes[party_simplified %in% "REPUBLICAN"], na.rm = TRUE), .groups = "drop"
    )
}

presidential_lean <- function(medsl_president) {
  pres <- presidential_votes(medsl_president)
  nat <- pres |>
    group_by(year) |>
    summarise(nat = log(sum(d) / sum(r)))
  pres |>
    left_join(nat, by = "year") |>
    transmute(year, state = state_po, lean = log(d / r) - nat)
}

lean_before <- function(lean, years, states) {
  y <- vapply(years, function(x) max(lean$year[lean$year < x]), numeric(1))
  key <- paste(y, states)
  out <- lean$lean[match(key, paste(lean$year, lean$state))]
  stopifnot("a race has no presidential lean" = !anyNA(out))
  out
}

# WHY: CKS's dist and nextup each misstate a few seats' Senate class (AK 2010, LA 2014 and
# 2020, ME and VT 2024) and nextup an odd-year special's term (AL 2017); where the two
# disagree in an even year, the seat was up in that year's regular cycle.
cycle_class <- function(year) as.integer((year %/% 2 - 1012) %% 3 + 1)

senate_class <- function(year, dist, nextup) {
  by_term <- c(3L, NA, 1L, NA, 2L, NA)[nextup %% 6 + 1]
  ifelse(year %% 2 == 1 | as.integer(dist) == by_term, as.integer(dist), cycle_class(year))
}

term_end <- function(year, cls) {
  first <- year + 2 - year %% 2
  vapply(seq_along(year), function(i) {
    y <- first[i] + c(0, 2, 4)
    y[cycle_class(y) == cls[i]]
  }, numeric(1))
}

senate_results <- function(cks_file) {
  haven::read_dta(cks_file) |>
    filter(office == "S", year %% 2 == 0, nextup > year) |>
    mutate(cls = senate_class(year, dist, nextup), is_d = party == "D", is_r = party == "R", inc = inc > 0) |>
    group_by(year, state, cls) |>
    summarise(
      d = sum(candidatevotes[is_d]), r = sum(candidatevotes[is_r]),
      ind_votes = max(c(0, candidatevotes[!is_d & !is_r])),
      ind_name = c(name_snyder[!is_d & !is_r][which.max(candidatevotes[!is_d & !is_r])], NA)[1],
      ind_inc = c(inc[!is_d & !is_r][which.max(candidatevotes[!is_d & !is_r])], FALSE)[1],
      d_inc = any(inc[is_d]), r_inc = any(inc[is_r]),
      r_second = c(sort(candidatevotes[is_r], decreasing = TRUE), 0, 0)[2],
      total = sum(candidatevotes), .groups = "drop"
    ) |>
    mutate(
      ind_last = sub(",.*$", "", ind_name),
      ind_ahead = ind_votes > d,
      indep_d = ind_ahead & paste(state, ind_last) %in% d_aligned_independents,
      d_slot = ifelse(indep_d, ind_votes, d),
      other = total - d_slot - r,
      inc = ifelse(indep_d, ind_inc, d_inc) - r_inc
    ) |>
    filter(!ind_ahead | indep_d, r_second <= d_slot) |>
    select(-ind_ahead, -r_second)
}

holdover_d <- function(cks_file, target, races) {
  ty <- as.integer(format(target, "%Y"))
  haven::read_dta(cks_file) |>
    filter(office == "S", won == 1, year < ty) |>
    mutate(cls = senate_class(year, dist, nextup), ends = term_end(year, cls)) |>
    arrange(year) |>
    group_by(state, cls) |>
    slice_tail(n = 1) |>
    ungroup() |>
    filter(ends > ty, !paste(state, cls) %in% paste(races$state, races$cls)) |>
    summarise(n = sum(party == "D" | sub(",.*$", "", name_snyder) %in% dem_caucus_independents)) |>
    pull(n)
}

national_clr <- function(pv, years, P) {
  t(vapply(years, function(y) {
    row <- pv[pv$year == y, ]
    clr(c(row$dem_votes, row$rep_votes, row$other_votes)[seq_len(P)])
  }, numeric(P)))
}

race_covariates <- function(lean, inc, indep_d) cbind(1, lean, inc, as.numeric(indep_d))

#' The SD per direction of the polling error a race's polls share (sigma_race_poll of
#' usa/stan/us_midterm_v9.stan), from FiveThirtyEight's pollster-ratings raw_polls.csv
#' (CC BY 4.0): the Democrat-against-Republican polls of type (Sen-G: Senate; Pres-G: a
#' state's presidential vote) in the final 60 days of the even cycles before the target,
#' races decided by less than max_margin points. Each race's
#' mean poll miss in log(D / R), less its cycle's mean (the industry bias the national
#' polls carry), has its sampling variance taken out; a zero-sum pair's SD per direction
#' is that of log(D / R) over sqrt(2).
race_poll_error_sd <- function(raw_polls, ty, type = "Sen-G", max_margin = 20) {
  race <- race_poll_misses(raw_polls, ty, type, national = FALSE, max_margin) |>
    group_by(cycle) |>
    mutate(dev = e - mean(e)) |>
    ungroup()
  sqrt(max(var(race$dev) - mean(race$sv), 0) / 2)
}

#' The SD per direction of the polling error every race poll of one type shares in a cycle
#' beyond the national polls' miss, which race_poll_error_sd takes out and mu_gamma does not
#' carry (the national polls pin it). Per cycle before the target: the mean of its races'
#' mean poll misses in log(D / R) (races decided by less than max_margin points), less the
#' national polls' mean miss (House-G-US for Senate races, the national Pres-G polls for
#' states), with both means' sampling variance taken out. Returns the SD and the cycle table.
race_type_error_sd <- function(raw_polls, ty, type = "Sen-G", national_type = "House-G-US", max_margin = 20) {
  nat <- race_poll_misses(raw_polls, ty, national_type, national = TRUE) |>
    transmute(cycle, g = e, g_sv = sv)
  cycles <- race_poll_misses(raw_polls, ty, type, national = FALSE, max_margin) |>
    group_by(cycle) |>
    summarise(races = n(), m = mean(e), m_sv = var(e) / n(), .groups = "drop") |>
    inner_join(nat, by = "cycle") |>
    mutate(eta = m - g, eta_sv = m_sv + g_sv)
  list(sd = sqrt(max(var(cycles$eta) - mean(cycles$eta_sv), 0) / 2), cycles = cycles)
}

race_poll_misses <- function(raw_polls, ty, type, national, max_margin = Inf) {
  read_csv(raw_polls, show_col_types = FALSE) |>
    filter(
      type_simple == type, (location == "US") == national, cycle %% 2 == 0, cycle < ty, samplesize > 0,
      paste(cand1_party, cand2_party) %in% c("DEM REP", "REP DEM")
    ) |>
    mutate(
      d = ifelse(cand1_party == "DEM", cand1_pct, cand2_pct), r = ifelse(cand1_party == "DEM", cand2_pct, cand1_pct),
      a_d = ifelse(cand1_party == "DEM", cand1_actual, cand2_actual),
      a_r = ifelse(cand1_party == "DEM", cand2_actual, cand1_actual),
      err = log(d / r) - log(a_d / a_r), p = d / (d + r),
      v = 100 / (samplesize * (d + r) * p * (1 - p))
    ) |>
    filter(is.finite(err), abs(100 * (a_d - a_r) / (a_d + a_r)) < max_margin) |>
    group_by(cycle, race_id) |>
    summarise(e = mean(err), sv = mean(v) / n(), .groups = "drop")
}

#' The Senate layer for one target election: a list of races (one row per target race,
#' with its covariates and, for a past target, its result), hist (past results, d and X),
#' polls (one row per race poll, with house, end_date, population, y_d, y_r and race index)
#' and holdover.
senate_layer <- function(target, asof, window_start, raw, tables, pv, normalise_house, P = 3L) {
  ty <- as.integer(format(target, "%Y"))
  cks <- file.path(raw, "dataverse-DGDRDT", "candidates_2006-2024.dta")
  lean <- presidential_lean(file.path(raw, "medsl", "president.csv"))
  all_res <- senate_results(cks)
  res <- all_res |>
    filter(d_slot > 0, r > 0)
  d_unopposed <- sum(all_res$year == ty & all_res$r == 0 & all_res$d_slot > 0)

  past <- res |>
    filter(year < ty, d_slot >= 0.05 * total, r >= 0.05 * total)
  shares <- if (P == 2) {
    cbind(past$d_slot, past$r) / (past$d_slot + past$r)
  } else {
    cbind(past$d_slot, past$r, pmax(past$other, 0.005 * past$total)) / past$total
  }
  d_hist <- t(apply(shares, 1, clr)) - national_clr(pv, past$year, P)
  hist <- list(
    X = race_covariates(lean_before(lean, past$year, past$state), past$inc, past$indep_d),
    d = d_hist, n = nrow(past)
  )

  if (ty <= 2024) {
    races <- res |>
      filter(year == ty) |>
      transmute(state, cls, indep_d, ind_last, inc, d_share = d_slot / total, r_share = r / total)
    polls <- senate_polls_538(file.path(raw, "538-polls-historical", "senate_polls_historical.csv"), ty, races)
  } else {
    races <- senate_races_2026(tables, file.path(raw, "congress-legislators", "legislators-current.csv"))
    polls <- senate_polls_2026(tables, races)
  }
  races$lean <- lean_before(lean, rep(ty, nrow(races)), races$state)
  races$X <- race_covariates(races$lean, races$inc, races$indep_d)

  polls <- polls |>
    filter(end_date >= window_start, end_date < asof, sample_size > 0, d > 0, r > 0) |>
    mutate(
      house = normalise_house(pollster),
      pop_rank = match(population, c("LV", "RV", "V", "A"), nomatch = 5L)
    ) |>
    arrange(race, house, end_date, pop_rank, desc(sample_size)) |>
    distinct(race, house, end_date, .keep_all = TRUE) |>
    mutate(
      population = ifelse(population %in% c("LV", "RV", "V", "A"), population, "LV"),
      y_d = as.integer(round(sample_size * d / 100)),
      y_r = as.integer(round(sample_size * r / 100))
    )
  list(races = races, hist = hist, polls = polls, holdover = holdover_d(cks, target, races) + d_unopposed)
}

senate_polls_538 <- function(file, ty, races) {
  polls <- read_csv(file, show_col_types = FALSE, guess_max = 1e5) |>
    filter(
      cycle == ty, stage == "general", !hypothetical, is.na(ranked_choice_reallocated) | !ranked_choice_reallocated
    ) |>
    mutate(
      state = state.abb[match(state, state.name)],
      cls = match(seat_name, c("Class I", "Class II", "Class III")),
      last = toupper(sub("^.* ", "", candidate_name))
    ) |>
    filter(!is.na(state), !is.na(cls)) |>
    inner_join(races |> transmute(state, cls, indep_d, ind_last, race = row_number()), by = c("state", "cls")) |>
    group_by(race, question_id) |>
    summarise(
      pollster = first(pollster), end_date = as.Date(first(end_date), "%m/%d/%y"),
      sample_size = first(sample_size), population = toupper(first(population)),
      d = ifelse(first(indep_d), sum(pct[last == first(ind_last)]), sum(pct[party == "DEM"])),
      r = sum(pct[party == "REP"]), .groups = "drop"
    ) |>
    select(-question_id)
  stopifnot("538 end dates did not parse" = !anyNA(polls$end_date))
  polls
}

senate_races_2026 <- function(tables, legislators) {
  r <- read_csv(file.path(tables, "senate_races_2026.csv"), show_col_types = FALSE)
  sen <- read_csv(legislators, show_col_types = FALSE) |>
    filter(type == "sen")
  st <- state.abb[match(r$state, state.name)]
  last <- sub("^.* ", "", sub("[[].*$", "", r$incumbent))
  special_cls <- sen$senate_class[match(paste(st, last), paste(sen$state, sen$last_name))]
  cls <- ifelse(r$race == "special", special_cls, (2026 / 2 - 1012) %% 3 + 1)
  stopifnot("a 2026 special's incumbent is not a current senator" = !anyNA(cls))
  pairs <- read_csv(file.path(tables, "senate_polls_2026_by_race.csv"), show_col_types = FALSE)
  r |>
    left_join(pairs |> select(state, race, pair_parties), by = c("state", "race")) |>
    transmute(
      state = state.abb[match(state, state.name)], cls = as.integer(cls), race_kind = race,
      indep_d = !is.na(pair_parties) & grepl("Independent", pair_parties),
      ind_last = NA_character_,
      running = !grepl("retiring|ineligible|lost|defeated", status, ignore.case = TRUE),
      inc = ifelse(running & !indep_d & incumbent_party %in% c("Democratic", "DFL"), 1,
        ifelse(running & incumbent_party == "Republican", -1, 0)
      ),
      d_share = NA_real_, r_share = NA_real_
    ) |>
    select(-running)
}

senate_polls_2026 <- function(tables, races) {
  p <- read_csv(file.path(tables, "senate_polls_2026.csv"), show_col_types = FALSE)
  slot <- function(party, share, indep) {
    ifelse(party == "Republican", NA, ifelse(indep, ifelse(party == "Independent", share, NA),
      ifelse(party %in% c("Democratic", "DFL"), share, NA)
    ))
  }
  idx <- match(paste(state.abb[match(p$state, state.name)], p$race), paste(races$state, races$race_kind))
  stopifnot("a 2026 poll matches no race" = !anyNA(idx))
  ind <- races$indep_d[idx]
  tibble(
    race = idx, pollster = p$pollster, end_date = as.Date(p$end_date), sample_size = p$sample_size,
    population = toupper(ifelse(is.na(p$population), "LV", p$population)),
    d = coalesce(slot(p$cand1_party, p$cand1_share, ind), slot(p$cand2_party, p$cand2_share, ind)),
    r = ifelse(p$cand1_party == "Republican", p$cand1_share, ifelse(p$cand2_party == "Republican", p$cand2_share, NA))
  ) |>
    filter(!is.na(d), !is.na(r), !is.na(sample_size))
}
