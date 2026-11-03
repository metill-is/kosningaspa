# House race layer for usa/R/build_generic_ballot.R races=house: the target year's House
# districts, their polls, and the past district results whose deviations from the national
# House vote teach the race regression (the R, X_race, d_hist and *_r fields of
# usa/stan/us_midterm_v9.stan to v12, every race with is_senate = 0).
#
# Each district is its leading Democrat against its leading Republican (usa/R/house_races.R).
# A district without both on the ballot is no race: its seat goes to the party running, and
# the Democratic ones are counted in `unopposed_d`, which seat totals add to the model's
# seats_house_d. Covariates (Q = 8, house_covariates()): intercept; the district's presidential lean, the logit
# of its two-party Democratic share minus the nation's at the latest presidential election
# before the race, on the map in force that cycle (usa/R/house_lean.R, house_lean_2026.R);
# incumbency, +1 for a Democratic incumbent on the ballot, -1 Republican, 0 open; and
# competitiveness, a bump in the district's expected margin (competitiveness(), below).
# Past rows and backtests read leans that take no party split from their own cycle's House
# votes (house_lean_best_backtest.csv), so training and forecast leans are alike.
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

# WHY: districts expected to be close have run ahead of the linear lean for the Democrats in
# all four history cycles (the 3 November review; the 2024 backtest missed 14 seats that
# way), so a bump in the expected margin m enters as a covariate: exp(-(m / 10)^2). m is the
# district's presidential lean moved by the national House margin, the actual one for a past
# result and the generic-ballot poll average before asof for the target.
competitiveness <- function(m) exp(-(m / 10)^2)

# The House covariates (Q = 8): intercept, lean, incumbency, competitiveness; has_prev,
# lean x has_prev and y_prev x has_prev, the previous House result on an unchanged map
# (its coefficient is a stable 0.51 across 2018-2024, and with it lean's slope halves, hence
# the interaction); and the national House margin / 100, since contested districts run
# against the national tide in every cycle (slope about -0.3; the improvement review).
house_covariates <- function(lean, inc, comp, prev, nat) {
  cbind(1, lean, inc, comp, prev$has, lean * prev$has, prev$y * prev$has, nat / 100)
}

# Margin (points of two-party) to the logit-scale lean the Senate layer uses.
logit_lean <- function(margin, national) qlogis((1 + margin / 100) / 2) - qlogis((1 + national / 100) / 2)

# Folds accents and punctuation so "Mario Díaz-Balart" on the ballot meets "Mario
# Diaz-Balart" in congress-legislators.
fold_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  trimws(gsub("\\s+", " ", gsub("[^a-z ]", " ", tolower(x))))
}

# A candidate is a sitting member running again when their ballot name holds a member's
# surname as whole words, their ballot party is the member's, and either the first three
# letters of their first name match the member's first, middle or usual name (Jen and
# Jennifer; Mike Johnson is James Michael), after common nicknames
# are mapped to their full form (Bob Latta is legally Robert). A sibling with the member's
# surname and another first name (Trever Nehls for Troy Nehls in TX-22) is not the member. Matching only surnames across the state coded namesakes, siblings and a
# "(Green)" party label as incumbents (the 3 November review).
nick <- c(bob = "rob", bobby = "rob", bill = "wil", billy = "wil", jim = "jam", jimmy = "jam", mike = "mic", tom = "tho",
  tommy = "tho", dick = "ric", rick = "ric", chuck = "cha", ted = "edw", ed = "edw", dave = "dav", joe = "jos", tony = "ant",
  andy = "and", chris = "chr", kate = "kat", liz = "eli", beth = "eli", greg = "gre", nick = "nic", matt = "mat",
  steve = "ste", jerry = "ger", larry = "law", buddy = "bud", don = "don", ron = "ron", jen = "jen", jenn = "jen",
  becca = "reb", tim = "tim", pete = "pet", vince = "vin", jake = "jac", josh = "jos", zach = "zac", fred = "fre",
  hank = "hen", jack = "joh", gus = "gus", nate = "nat", rob = "rob", sam = "sam", abe = "abr", dan = "dan", danny = "dan",
  al = "alb", alex = "ale", ben = "ben", jeff = "jef", jon = "jon", johnny = "joh", jay = "jay", gabe = "gab", wes = "wes", lou = "lui")

house_races_2026 <- function(tables, legislators) {
  cand <- read_csv(file.path(tables, "house_2026_candidates_major_party.csv"), show_col_types = FALSE)
  reps <- read_csv(legislators, show_col_types = FALSE) |>
    filter(type == "rep") |>
    mutate(party_code = ifelse(party == "Democrat", "D", ifelse(party == "Republican", "R", "I")))
  st <- state.abb[match(sub(" (at-large|[0-9]+)$", "", cand$district, ignore.case = TRUE), state.name)]
  dist <- suppressWarnings(as.integer(sub("^.* ", "", cand$district)))
  dist[is.na(dist)] <- 1L
  stopifnot("a 2026 district has no state" = !anyNA(st))
  word_in <- function(w, txt) nzchar(w) & grepl(paste0("(^| )", w, "( |$)"), txt)
  info <- lapply(seq_along(st), function(i) {
    parts <- trimws(strsplit(cand$candidates[i], "\u258c", fixed = FALSE)[[1]])
    parts <- parts[nzchar(parts)]
    nm <- fold_name(sub("\\s*\\(.*$", "", parts))
    pty <- sub("^.*\\((.*)\\)\\s*$", "\\1", parts)
    pc <- ifelse(grepl("Democrat|DFL", pty), "D", ifelse(grepl("Republican", pty), "R", "I"))
    r <- reps[reps$state == st[i], ]
    inc <- 0
    ind_inc <- FALSE
    for (k in seq_len(nrow(r))) {
      ln <- fold_name(r$last_name[k])
      given <- c(r$first_name[k], r$nickname[k], r$middle_name[k], sub(" .*$", "", r$full_name[k]))
      given <- fold_name(given[!is.na(given)])
      given <- ifelse(given %in% names(nick), nick[given], given)
      fn3 <- substr(given, 1, 3)
      first <- sub(" .*$", "", sub("^[a-z] ", "", nm))
      first <- ifelse(first %in% names(nick), nick[first], first)
      hit <- which(vapply(nm, function(n) word_in(ln, n), logical(1)) & pc == r$party_code[k] & substr(first, 1, 3) %in% fn3[nzchar(fn3)])
      if (length(hit)) {
        inc <- if (r$party_code[k] == "D") 1 else if (r$party_code[k] == "R") -1 else 0
        ind_inc <- r$party_code[k] == "I"
      }
    }
    list(inc = inc, ind_inc = ind_inc)
  })
  inc <- vapply(info, `[[`, numeric(1), "inc")
  ind_inc <- vapply(info, `[[`, logical(1), "ind_inc")
  # WHY: a sitting independent member against a Democrat (Kevin Kiley, CA-6) is a contest,
  # not a certain Democratic seat; the independent takes the R slot.
  tibble(state = st, dist = dist, has_dem = cand$has_dem, has_rep = cand$has_rep | (ind_inc & cand$has_dem),
    inc = ifelse(ind_inc, -1, inc), d_share = NA_real_, r_share = NA_real_)
}

house_polls_2026 <- function(tables, races, polls_file = "house_district_polls_2026.csv") {
  p <- read_csv(file.path(tables, polls_file), show_col_types = FALSE)
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

house_layer <- function(target, asof, window_start, raw, tables, house_dir, pv, normalise_house, P = 2L, lean_file = "house_lean_best_backtest.csv", polls_file = "house_district_polls_2026.csv", nat_now = NA_real_) {
  stopifnot("the House layer is two-party" = P == 2L)
  ty <- as.integer(format(target, "%Y"))
  nat_pres <- national_pres_margin(file.path(raw, "medsl", "president.csv"))
  maps <- read.csv("usa/house_maps.csv")
  hh <- house_history(haven::read_dta(file.path(raw, "dataverse-DGDRDT", "candidates_2006-2024.dta")), maps)
  best <- read_csv(file.path(house_dir, lean_file), show_col_types = FALSE)
  # WHY: past rows read the regular leans; the clean 2022 leans are off by tens of points in
  # places (AZ-3, FL-24, TN-9; the 3 November improvement review), and a past cycle's own
  # House votes predate every target. The clean leans serve only a backtest's target.
  hist_lean <- read_csv(file.path(house_dir, "house_lean_best.csv"), show_col_types = FALSE)
  nat_house <- setNames(100 * (pv$dem_votes - pv$rep_votes) / (pv$dem_votes + pv$rep_votes), pv$year)
  # The previous House result where the map did not change and the race was contested:
  # its D-direction deviation from that year's national House vote, on d_hist's scale.
  dev1 <- function(d, r, y) 0.5 * (log(d / (d + r)) - log(r / (d + r))) - national_clr(pv, y, P)[, 1]
  prev_of <- function(state, dist, year, same_map) {
    pr <- hh |> filter(contested, d_votes >= 0.05 * total, r_votes >= 0.05 * total) |>
      transmute(state, dist, year = year + 2L, pd = d_votes, prr = r_votes)
    x <- tibble(state, dist, year, same_map) |> left_join(pr, by = c("state", "dist", "year"))
    has <- x$same_map & !is.na(x$pd)
    list(has = as.numeric(has), y = ifelse(has, dev1(x$pd, x$prr, x$year - 2L), 0))
  }

  past <- hh |>
    filter(year < ty, contested, d_votes >= 0.05 * total, r_votes >= 0.05 * total) |>
    inner_join(hist_lean |> select(year = cycle, state, dist = district, lean_margin = margin, pres_year), by = c("year", "state", "dist"))
  shares <- cbind(past$d_votes, past$r_votes) / (past$d_votes + past$r_votes)
  d_hist <- t(apply(shares, 1, clr)) - national_clr(pv, past$year, P)
  lean_past <- logit_lean(past$lean_margin, nat_pres[as.character(past$pres_year)])
  comp_past <- competitiveness(past$lean_margin - nat_pres[as.character(past$pres_year)] + nat_house[as.character(past$year)])
  pp <- prev_of(past$state, past$dist, past$year, past$same_map)
  hist <- list(
    X = house_covariates(lean_past, past$inc, comp_past, pp, nat_house[as.character(past$year)]),
    d = d_hist, n = nrow(past), year = past$year, state = past$state
  )

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
  stopifnot("the competitiveness term needs the national poll average" = is.finite(nat_now))
  races$comp <- competitiveness(races$lean_margin - nat_pres[as.character(races$pres_year)] + nat_now)
  if (ty <= 2024) {
    same <- races$same_map
  } else {
    m26 <- maps[maps$cycle == 2026, ]
    same <- m26$changed[match(races$state, m26$state)] == 0 & m26$map[match(races$state, m26$state)] != "at-large"
  }
  pt <- prev_of(races$state, races$dist, rep(ty, nrow(races)), same)
  races$has_prev <- pt$has
  races$X <- house_covariates(races$lean, races$inc, races$comp, pt, if (ty <= 2024) nat_now else nat_now)

  polls <- if (ty > 2024) house_polls_2026(tables, races, polls_file) else house_polls_538(file.path(raw, "538-polls-historical", "house_polls_historical.csv"), ty, races)
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
