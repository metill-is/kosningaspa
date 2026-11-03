# House race layer for usa/R/build_generic_ballot.R races=house: the target year's House
# districts, their polls, and the past district results whose deviations from the national
# House vote teach the race regression (the R, X_race, d_hist and *_r fields of
# usa/stan/us_midterm_v9.stan to v12, every race with is_senate = 0).
#
# Each district is its leading Democrat against its leading Republican (usa/R/house_races.R).
# A district without both on the ballot is no race: its seat goes to the party running, and
# the Democratic ones are counted in `unopposed_d`, which seat totals add to the model's
# seats_house_d. Covariates (Q = 3): intercept; the district's presidential lean, the logit
# of its two-party Democratic share minus the nation's at the latest presidential election
# before the race, on the map in force that cycle (usa/R/house_lean.R, house_lean_2026.R);
# incumbency, +1 for a Democratic incumbent on the ballot, -1 Republican, 0 open.
#
# Sources (paths under raw=, tables= and house=; usa/README.md has where they come from):
#   results 2018-2024: Cha, Kuriwaki and Snyder (CC0), via house_history();
#   leans: house=<dir>/house_lean_best.csv (2018-2024; backtests pass house_lean_best_clean.csv)
#     and house_lean_2026.csv;
#   2026: tables/house_2026_candidates_major_party.csv and house_district_polls_2026.csv
#     (Wikipedia, CC BY-SA); incumbents from unitedstates/congress-legislators
#     legislators-current.csv (CC0), matched to a district's candidates by surname.
# Past results with no lean (the cycles before 2018) are left out.

source("usa/R/house_races.R")
source("usa/R/senate_races.R") # clr(), national_clr()

national_pres_margin <- function(medsl_president) {
  p <- read_csv(medsl_president, show_col_types = FALSE) |>
    filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) |>
    group_by(year) |>
    summarise(m = 100 * (sum(candidatevotes[party_simplified == "DEMOCRAT"]) - sum(candidatevotes[party_simplified == "REPUBLICAN"])) /
      sum(candidatevotes), .groups = "drop")
  setNames(p$m, p$year)
}

# Margin (points of two-party) to the logit-scale lean the Senate layer uses.
logit_lean <- function(margin, national) qlogis((1 + margin / 100) / 2) - qlogis((1 + national / 100) / 2)

house_races_2026 <- function(tables, legislators) {
  cand <- read_csv(file.path(tables, "house_2026_candidates_major_party.csv"), show_col_types = FALSE)
  reps <- read_csv(legislators, show_col_types = FALSE) |>
    filter(type == "rep")
  st <- state.abb[match(sub(" (at-large|[0-9]+)$", "", cand$district, ignore.case = TRUE), state.name)]
  dist <- suppressWarnings(as.integer(sub("^.* ", "", cand$district)))
  dist[is.na(dist)] <- 1L
  stopifnot("a 2026 district has no state" = !anyNA(st))
  up <- toupper(cand$candidates)
  inc <- vapply(seq_along(st), function(i) {
    r <- reps[reps$state == st[i], ]
    hit <- r[vapply(toupper(r$last_name), function(ln) grepl(paste0("\\b", ln, "\\b"), up[i]), logical(1)), ]
    if (nrow(hit) == 0) return(0)
    # WHY: two members of a state can share a surname; the one who held this district wins.
    if (nrow(hit) > 1) hit <- hit[order(hit$district != dist[i]), ][1, ]
    if (hit$party[1] == "Democrat") 1 else if (hit$party[1] == "Republican") -1 else 0
  }, numeric(1))
  tibble(state = st, dist = dist, has_dem = cand$has_dem, has_rep = cand$has_rep, inc = inc,
    d_share = NA_real_, r_share = NA_real_)
}

house_polls_2026 <- function(tables, races) {
  p <- read_csv(file.path(tables, "house_district_polls_2026.csv"), show_col_types = FALSE)
  st <- state.abb[match(p$state, state.name)]
  dist <- suppressWarnings(as.integer(sub("^District ", "", p$district)))
  dist[is.na(dist)] <- 1L
  idx <- match(paste(st, dist), paste(races$state, races$dist))
  tibble(race = idx, pollster = p$pollster, end_date = as.Date(p$end_date), sample_size = p$sample_size,
    population = toupper(ifelse(is.na(p$population), "LV", p$population)), d = p$dem_share, r = p$rep_share,
    hypothetical = p$hypothetical) |>
    filter(!is.na(race), !hypothetical, !is.na(d), !is.na(r), !is.na(sample_size)) |>
    select(-hypothetical)
}

house_layer <- function(target, asof, window_start, raw, tables, house_dir, pv, normalise_house, P = 2L, lean_file = "house_lean_best.csv") {
  stopifnot("the House layer is two-party" = P == 2L)
  ty <- as.integer(format(target, "%Y"))
  nat_pres <- national_pres_margin(file.path(raw, "medsl", "president.csv"))
  maps <- read.csv("usa/house_maps.csv")
  hh <- house_history(haven::read_dta(file.path(raw, "dataverse-DGDRDT", "candidates_2006-2024.dta")), maps)
  best <- read_csv(file.path(house_dir, lean_file), show_col_types = FALSE)

  past <- hh |>
    filter(year < ty, contested, d_votes >= 0.05 * total, r_votes >= 0.05 * total) |>
    inner_join(best |> select(year = cycle, state, dist = district, lean_margin = margin, pres_year), by = c("year", "state", "dist"))
  shares <- cbind(past$d_votes, past$r_votes) / (past$d_votes + past$r_votes)
  d_hist <- t(apply(shares, 1, clr)) - national_clr(pv, past$year, P)
  lean_past <- logit_lean(past$lean_margin, nat_pres[as.character(past$pres_year)])
  hist <- list(X = cbind(1, lean_past, past$inc), d = d_hist, n = nrow(past), year = past$year, state = past$state)

  if (ty <= 2024) {
    all <- hh |> filter(year == ty)
    lean_t <- best |> filter(cycle == ty) |> select(state, dist = district, lean_margin = margin, pres_year)
  } else {
    all <- house_races_2026(tables, file.path(raw, "congress-legislators", "legislators-current.csv"))
    all <- all |> mutate(contested = has_dem & has_rep, winner = ifelse(has_dem & !has_rep, "D", ifelse(has_rep & !has_dem, "R", NA)))
    lean_t <- read_csv(file.path(house_dir, "house_lean_2026.csv"), show_col_types = FALSE) |>
      transmute(state, dist = district, lean_margin = margin, pres_year = 2024)
  }
  unopposed_d <- sum(!all$contested & all$winner %in% "D")
  races <- all |>
    filter(contested) |>
    left_join(lean_t, by = c("state", "dist"))
  stopifnot("a contested district has no lean" = !anyNA(races$lean_margin))
  if (ty <= 2024) races <- races |> mutate(d_share = d_votes / total, r_share = r_votes / total)
  races$lean <- logit_lean(races$lean_margin, nat_pres[as.character(races$pres_year)])
  races$X <- cbind(1, races$lean, races$inc)

  polls <- if (ty > 2024) house_polls_2026(tables, races) else house_polls_538(file.path(raw, "538-polls-historical", "house_polls_historical.csv"), ty, races)
  polls <- polls |>
    filter(end_date >= window_start, end_date < asof, sample_size > 0, d > 0, r > 0) |>
    mutate(house = normalise_house(pollster), pop_rank = match(population, c("LV", "RV", "V", "A"), nomatch = 5L)) |>
    arrange(race, house, end_date, pop_rank, desc(sample_size)) |>
    distinct(race, house, end_date, .keep_all = TRUE) |>
    mutate(
      population = ifelse(population %in% c("LV", "RV", "V", "A"), population, "LV"),
      y_d = as.integer(round(sample_size * d / 100)), y_r = as.integer(round(sample_size * r / 100))
    )
  list(races = races, hist = hist, polls = polls, unopposed_d = unopposed_d)
}

house_polls_538 <- function(file, ty, races) {
  polls <- read_csv(file, show_col_types = FALSE, guess_max = 1e5) |>
    filter(cycle == ty, stage == "general", !hypothetical) |>
    mutate(state = state.abb[match(state, state.name)], dist = as.integer(ifelse(seat_number == 0, 1, seat_number))) |>
    filter(!is.na(state)) |>
    inner_join(races |> transmute(state, dist, race = row_number()), by = c("state", "dist")) |>
    group_by(race, question_id) |>
    summarise(
      pollster = first(pollster), end_date = as.Date(first(end_date), "%m/%d/%y"),
      sample_size = first(sample_size), population = toupper(first(population)),
      d = sum(pct[party == "DEM"]), r = sum(pct[party == "REP"]), .groups = "drop"
    ) |>
    select(-question_id)
  stopifnot("538 end dates did not parse" = !anyNA(polls$end_date))
  polls
}
