# R0, production mapped to E (spec §5.4): one cell's admitted information set in the
# format production's fit script hands to prepare_stan_data(), that function run as it is,
# and a fixed table of overrides (harness/README.md, R0's inputs).

#' R0's kjördæmi order: production's level order in the 2024 run, so the last level,
#' Suður, closes δ in every cell.
#' @export
R0_KJORDAEMI <- c("Reykjav\u00edk Nor\u00f0ur", "Reykjav\u00edk Su\u00f0ur", "Su\u00f0vestur", "Nor\u00f0vestur", "Nor\u00f0austur", "Su\u00f0ur")

#' The keys of `harness/configs/r0.json`, in order.
R0_CONFIG_KEYS <- c("campaign_days", "desired_weight", "weight_time", "debut")

#' The newcomer debut rules `read_r0_config()` accepts.
DEBUT_RULES <- "median_new_party_before_E"

#' The `prepare_stan_data()` fields that are not in the Stan data block.
NOT_DATA <- c("N", "xm", "n_election", "time_diff_f")

#' The fundamentals matrices that get a zero row for each newcomer.
NEWCOMER_ROWS <- c("y_f", "x_f", "incumbent_f", "incumbent_years", "vnv", "growth")

#' The columns of production's national and kjördæmi poll rows.
NAT_COLS <- c("date", "fyrirtaeki", "flokkur", "n")
KD_COLS <- c("date", "fyrirtaeki", "kjordaemi", "flokkur", "n")

#' Stop with "R0 input: " and `...`.
fail <- function(...) stop("R0 input: ", ..., call. = FALSE)

#' Stop with "R0 input: `what`" unless `ok` is TRUE.
check <- function(ok, what) if (!isTRUE(ok)) fail(what)

#' The values of `x` joined for a message, or "none".
listed <- function(x) if (length(x)) paste(unique(x), collapse = "; ") else "none"

#' R0's configuration.
#' @param path the JSON file
#' @return `list(campaign_days, desired_weight, weight_time, debut)`: the campaign length in
#'   days (a whole number, at least 1), the fundamentals weight (in (0, 1)), the day it is
#'   reached (a whole number, at least 0) and the newcomer debut rule (only
#'   `median_new_party_before_E`); an error on a missing or extra key or a value outside
#'   those
#' @export
read_r0_config <- function(path = harness_path("configs", "r0.json")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  cfg <- jsonlite::read_json(path)
  missing <- setdiff(R0_CONFIG_KEYS, names(cfg))
  extra <- setdiff(names(cfg), R0_CONFIG_KEYS)
  if (length(missing) || length(extra)) {
    stop(basename(path), ": missing key(s): ", listed(missing), "; extra key(s): ", listed(extra), call. = FALSE)
  }
  whole <- function(x, lo) is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x) && x >= lo
  if (!whole(cfg$campaign_days, 1)) stop(basename(path), ": campaign_days must be a whole number of days, at least 1")
  if (!(is.numeric(cfg$desired_weight) && length(cfg$desired_weight) == 1 && cfg$desired_weight > 0 && cfg$desired_weight < 1)) {
    stop(basename(path), ": desired_weight must be one number in (0, 1)")
  }
  if (!whole(cfg$weight_time, 0)) stop(basename(path), ": weight_time must be a whole number of days, at least 0")
  if (!identical(cfg$debut, DEBUT_RULES)) stop(basename(path), ": debut must be ", paste(DEBUT_RULES, collapse = " or "))
  list(
    campaign_days = as.integer(cfg$campaign_days),
    desired_weight = as.numeric(cfg$desired_weight),
    weight_time = as.integer(cfg$weight_time),
    debut = cfg$debut
  )
}

#' The party order of a cell: Annað, then the other parties in `POLL_PARTIES` order.
#' @param parties the cell's parties (`info_set()$parties`)
#' @return a character vector; an error if Annað is not among `parties`
party_order_of <- function(parties) {
  box::use(R / harness / constants[POLL_PARTIES])
  if (!"Anna\u00f0" %in% parties) fail("Anna\u00f0 is not among the cell's parties")
  c("Anna\u00f0", intersect(POLL_PARTIES, setdiff(parties, "Anna\u00f0")))
}

#' One cell's admitted inputs in the format production's fit script gives
#' `prepare_stan_data()` (`R/fit_polling_and_fundamentals_kjordaemi_model.R`).
#' @param info an `info_set()` list
#' @return `list(polling, constituency, fundamentals)`:
#'   - `polling`: the `kosning_nat` and `polls_nat` rows (date, fyrirtaeki, flokkur, n).
#'     `flokkur` is a factor in the order Annað, then the other `info$parties` in
#'     `POLL_PARTIES` order; `fyrirtaeki` a factor with `Kosning` first, then the other
#'     houses by their first admitted poll date (ties by name, radix order). Rows are
#'     sorted by date, fyrirtaeki and flokkur.
#'   - `constituency`: the `results` of `previous_election(E)` as `Kosning` rows (votes as
#'     n) and the `polls_kd` rows dated after it (date, fyrirtaeki, kjordaemi, flokkur, n).
#'     `kjordaemi` is a factor in `R0_KJORDAEMI` order, `flokkur` and `fyrirtaeki` have
#'     polling's levels, and rows are sorted by flokkur, date and kjordaemi.
#'   - `fundamentals`: the training rows (flokkur := the α label `party`), E's target rows
#'     and one Annað target row (date E, the target rows' year, vnv and growth,
#'     voteshare_prior 100 × Annað's previous share, the incumbency, new-party and split
#'     columns 0, the masked columns NA). `flokkur` is a factor with the party order's
#'     labels first, then every other label in first-appearance order; rows are sorted by
#'     flokkur and date.
#'
#'   An error, "R0 input: ...", if Annað is not among the parties, a party outside them has
#'   votes or n > 0 in a kjördæmi row, a kjördæmi n is missing, a kjördæmi label is outside
#'   `R0_KJORDAEMI`, a kjördæmi date is not a national date or has more than one house, or
#'   the Annað target row cannot be built (no target row, more than one year, vnv or
#'   growth among them, or no single previous share for Annað).
#' @export
r0_production_inputs <- function(info) {
  box::use(R / harness / constants[ELECTION_HOUSE, previous_election])
  party_order <- party_order_of(info$parties)

  first <- dplyr::summarise(info$polls_nat, first = min(date), .by = fyrirtaeki)
  houses <- c(ELECTION_HOUSE, first$fyrirtaeki[order(first$first, first$fyrirtaeki, method = "radix")])
  nat <- rbind(info$kosning_nat[NAT_COLS], info$polls_nat[NAT_COLS])
  polling <- tibble::tibble(
    date = nat$date,
    fyrirtaeki = factor(nat$fyrirtaeki, levels = houses),
    flokkur = factor(nat$flokkur, levels = party_order),
    n = nat$n
  )
  polling <- polling[order(polling$date, as.integer(polling$fyrirtaeki), as.integer(polling$flokkur), method = "radix"), ]

  prev <- previous_election(info$E)
  res <- info$results[info$results$election == prev, ]
  kd <- rbind(
    tibble::tibble(
      date = rep(prev, nrow(res)), fyrirtaeki = rep(ELECTION_HOUSE, nrow(res)), kjordaemi = res$kjordaemi,
      flokkur = res$flokkur, n = as.numeric(res$votes)
    ),
    info$polls_kd[info$polls_kd$date > prev, KD_COLS]
  )
  if (anyNA(kd$n)) fail("a kj\u00f6rd\u00e6mi n is missing: ", listed(paste(format(kd$date), kd$fyrirtaeki)[is.na(kd$n)]))
  dropped <- !kd$flokkur %in% party_order & kd$n > 0
  if (any(dropped)) {
    fail("a dropped party has votes: ", listed(paste(format(kd$date), kd$fyrirtaeki, kd$flokkur)[dropped]))
  }
  kd <- kd[kd$flokkur %in% party_order, ]
  if (!all(kd$kjordaemi %in% R0_KJORDAEMI)) {
    fail("a kj\u00f6rd\u00e6mi label is outside R0_KJORDAEMI: ", listed(setdiff(kd$kjordaemi, R0_KJORDAEMI)))
  }
  off <- !kd$date %in% polling$date
  if (any(off)) fail("a kj\u00f6rd\u00e6mi date is not a national date: ", listed(format(kd$date[off])))
  per_date <- dplyr::count(dplyr::distinct(kd, date, fyrirtaeki), date)
  if (any(per_date$n > 1)) {
    fail("a kj\u00f6rd\u00e6mi date has more than one house: ", listed(format(per_date$date[per_date$n > 1])))
  }
  constituency <- tibble::tibble(
    date = kd$date,
    fyrirtaeki = factor(kd$fyrirtaeki, levels = levels(polling$fyrirtaeki)),
    kjordaemi = factor(kd$kjordaemi, levels = R0_KJORDAEMI),
    flokkur = factor(kd$flokkur, levels = levels(polling$flokkur)),
    n = kd$n
  )
  constituency <- constituency[order(
    as.integer(constituency$flokkur), constituency$date, as.integer(constituency$kjordaemi),
    method = "radix"
  ), ]

  list(polling = polling, constituency = constituency, fundamentals = fundamentals_rows(info, party_order))
}

#' The fundamentals table of a cell, as production's `read_fundamentals_data()` and the fit
#' script's economy join give it; see `r0_production_inputs()`.
fundamentals_rows <- function(info, party_order) {
  train <- info$fundamentals$train
  target <- info$fundamentals$target
  cols <- setdiff(names(train), c("party", "in_sf"))
  train$flokkur <- train$party
  target$flokkur <- target$party
  one <- function(x, what) {
    u <- unique(x)
    if (length(u) != 1 || is.na(u)) fail("the target rows have one ", what, ", not ", listed(format(u)))
    u
  }
  share <- info$prev_shares$share[info$prev_shares$election == info$E & info$prev_shares$flokkur == "Anna\u00f0"]
  if (length(share) != 1 || is.na(share)) fail("Anna\u00f0 has no single previous share at ", format(info$E))
  annad <- tibble::tibble(
    year = one(target$year, "year"), date = info$E, flokkur = "Anna\u00f0", new_party = 0, incumbent = 0,
    incumbent_years = 0, incumbent_prime_minister = 0, party_split = 0, voteshare = NA_real_,
    voteshare_prior = 100 * share, voteshare_change = NA_real_, total_valid_votes = NA_real_,
    vnv = one(target$vnv, "vnv"), growth = one(target$growth, "growth")
  )
  if (!setequal(names(annad), cols)) {
    fail("the Anna\u00f0 target row and the training rows differ in columns: ", listed(c(
      setdiff(names(annad), cols), setdiff(cols, names(annad))
    )))
  }
  fund <- dplyr::bind_rows(train[cols], target[cols], annad[cols])
  labels <- fund$flokkur
  fund$flokkur <- factor(labels, levels = c(intersect(party_order, labels), setdiff(unique(labels), party_order)))
  fund[order(as.integer(fund$flokkur), fund$date, method = "radix"), ]
}

#' R0's Stan data for one cell: production's `prepare_stan_data()` on
#' `r0_production_inputs()`, with the overrides of the plan's Task 8 table and only those.
#' @param info an `info_set()` list
#' @param cfg a `read_r0_config()` list
#' @return `list(data, meta)`.
#'
#'   `data` is `prepare_stan_data(polling, fundamentals, constituency, election_date = E)`
#'   with these fields replaced or added:
#'   - `days_between_stjornarslit_and_election`: `cfg$campaign_days`;
#'   - `n_pred_k`, `constituency_weights_pred`: `info$valid_votes_open` votes and shares,
#'     matched by name to `R0_KJORDAEMI`;
#'   - `constituency_weights`, `N_obs_k`, `obs_k`: one row per kjördæmi date (named by the
#'     date), each kjördæmi's share of that date's n in `R0_KJORDAEMI` columns, and the row
#'     of each `y_k` row's date;
#'   - `desired_weight`, `weight_time`: from `cfg`;
#'   - `mu_gamma_scale`: for each party but Annað, 1/sqrt(n_e), 0 when n_e = 0, where n_e =
#'     `epoch_of(E)` - the party's entry epoch;
#'   - `n_active_d`, `active_index_d`: for each date level, the parties (in party order,
#'     0-padded) whose first national row with n > 0 is on or before it;
#'   - the prediction column D_f + 1 of `x_f`, `index_f` and `n_parties_f`, and a zero row
#'     of `y_f`, `x_f`, `incumbent_f`, `incumbent_years`, `vnv` and `growth` for each
#'     newcomer (a party with no share at `previous_election(E)`), `P_f` grown to match.
#'     The column holds logit(s) - c for each party with a previous share s (the training
#'     rows' voteshare at that election, labels by `fundamentals_party()`, lists outside
#'     the party order in Annað, normalised over the shares above 0) and logit(debut) - c
#'     for a newcomer, c the mean logit of the former and debut the median voteshare / 100
#'     of the training rows with `new_party == 1` and voteshare > 0; 0 for every other row.
#'
#'   Data frames become matrices, and `N`, `xm`, `n_election` and `time_diff_f`, which are
#'   not in the Stan data block, are dropped.
#'
#'   `meta` holds `party_order`, `kjordaemi_levels`, `house_levels`, `n_e` (named by
#'   party), `newcomers`, `debut_share`, `eday_index` (D + `pred_y_time_diff`), `n_pred`,
#'   `n_pred_k` (named by kjördæmi), `n_polls_nat`, `n_polls_kd` (the kjördæmi polls used),
#'   `max_poll_date` (of the national polls) and `coalition_event`.
#'
#'   An error, "R0 input: <check>", on any error of `r0_production_inputs()` or when a
#'   check fails: the columns of `y_n` and `y_k` are the party order and P its length;
#'   `house_n` and `house_k` lie in 1..H and house 1 is Kosning; every party has an entry
#'   epoch at or before E's; the fundamentals levels are the rows of `y_f` (production's
#'   `index_f` counts levels); a newcomer is not already a row of `y_f`; every party with a
#'   previous share is a row of `y_f`; `index_f` has a row per party; `mu_gamma_scale` has
#'   P - 1 finite values in [0, 1]; `x_f`, `incumbent_f`, `incumbent_years`, `vnv` and
#'   `growth` have the rows of `y_f` and D_f + 1 columns, as `index_f` has; the prediction
#'   column's `index_f` names the party order (the S/B-swap guard); `n_parties_f[D_f + 1]`
#'   is P; K is 6, `n_pred_k` is K positive whole numbers, `obs_k` lies in 1..`N_obs_k` and
#'   each row of `constituency_weights` is positive and sums to 1; `pred_y_time_diff` and
#'   `days_between_stjornarslit_and_election` are at least 1; no field holds an NA; every
#'   national and kjördæmi date is on or before F.
#' @export
r0_stan_data <- function(info, cfg = read_r0_config()) {
  box::use(
    R / stan_data[prepare_stan_data],
    R / harness / constants[ELECTION_HOUSE],
    R / harness / tables[epoch_of]
  )
  inputs <- r0_production_inputs(info)
  polling <- inputs$polling
  constituency <- inputs$constituency
  party_order <- levels(polling$flokkur)
  P <- length(party_order)
  d <- prepare_stan_data(polling, inputs$fundamentals, constituency, election_date = info$E)
  check(
    identical(colnames(d$y_n), party_order) && identical(colnames(d$y_k), party_order) && d$P == P,
    "the columns of y_n and y_k are the party order, and P is its length"
  )
  check(
    all(c(d$house_n, d$house_k) %in% seq_len(d$H)) && identical(levels(polling$fyrirtaeki)[1], ELECTION_HOUSE),
    "house_n and house_k lie in 1..H, and house 1 is Kosning"
  )

  d$days_between_stjornarslit_and_election <- cfg$campaign_days
  vv <- info$valid_votes_open
  k <- match(R0_KJORDAEMI, vv$kjordaemi)
  n_pred_k <- stats::setNames(as.integer(vv$votes[k]), R0_KJORDAEMI)
  d$n_pred_k <- unname(n_pred_k)
  d$constituency_weights_pred <- vv$w[k]

  date_levels <- as.Date(levels(factor(unique(c(polling$date, constituency$date)))))
  kd_dates <- sort(unique(constituency$date))
  n_kd <- tapply(constituency$n, list(factor(constituency$date, levels = kd_dates), constituency$kjordaemi), sum)
  weights <- n_kd / rowSums(n_kd)
  dimnames(weights) <- list(format(kd_dates), R0_KJORDAEMI)
  d$constituency_weights <- weights
  d$N_obs_k <- nrow(weights)
  d$obs_k <- match(date_levels[d$date_k], kd_dates)
  d$desired_weight <- cfg$desired_weight
  d$weight_time <- cfg$weight_time

  n_e <- stats::setNames(epoch_of(info$E) - info$entry$entry_epoch[match(party_order, info$entry$flokkur)], party_order)
  check(!anyNA(n_e) && all(n_e >= 0), "every party has an entry epoch at or before E's")
  scale <- 1 / sqrt(n_e[-1])
  scale[n_e[-1] == 0] <- 0
  d$mu_gamma_scale <- unname(scale)

  live <- polling[polling$n > 0, ]
  first_live <- tapply(live$date, live$flokkur, min)
  d$n_active_d <- integer(length(date_levels))
  d$active_index_d <- matrix(0L, length(date_levels), P)
  for (t in seq_along(date_levels)) {
    on <- which(first_live <= as.numeric(date_levels[t]))
    d$n_active_d[t] <- length(on)
    d$active_index_d[t, seq_along(on)] <- on
  }

  pred <- prediction_column(d, info, party_order, levels(inputs$fundamentals$flokkur))
  d <- pred$data

  check(
    length(d$mu_gamma_scale) == P - 1 && all(is.finite(d$mu_gamma_scale)) &&
      all(d$mu_gamma_scale >= 0 & d$mu_gamma_scale <= 1),
    "mu_gamma_scale has P - 1 finite values in [0, 1]"
  )
  col <- d$D_f + 1
  shaped <- vapply(c("x_f", "incumbent_f", "incumbent_years", "vnv", "growth"), \(m) {
    identical(rownames(d[[m]]), rownames(d$y_f)) && ncol(d[[m]]) == col
  }, logical(1))
  check(
    all(shaped) && ncol(d$index_f) == col,
    "x_f, incumbent_f, incumbent_years, vnv and growth have the rows of y_f and D_f + 1 columns, as index_f has"
  )
  check(
    identical(rownames(d$y_f)[d$index_f[seq_len(P), col]], party_order),
    "the prediction column's index_f names the party order"
  )
  check(d$n_parties_f[col] == P, "n_parties_f[D_f + 1] is P")
  check(
    d$K == 6 && length(d$n_pred_k) == d$K && all(d$n_pred_k > 0) && all(d$n_pred_k == round(d$n_pred_k)),
    "K is 6 and n_pred_k is K positive whole numbers"
  )
  check(
    length(d$obs_k) == d$N_k && all(d$obs_k %in% seq_len(d$N_obs_k)),
    "obs_k lies in 1..N_obs_k"
  )
  check(
    all(weights > 0) && all(abs(rowSums(weights) - 1) <= 1e-12),
    "each row of constituency_weights is positive and sums to 1"
  )
  check(
    d$pred_y_time_diff >= 1 && d$days_between_stjornarslit_and_election >= 1,
    "pred_y_time_diff and days_between_stjornarslit_and_election are at least 1"
  )
  d <- lapply(d[setdiff(names(d), NOT_DATA)], \(x) if (is.data.frame(x)) as.matrix(x) else x)
  na <- names(d)[vapply(d, anyNA, logical(1))]
  if (length(na)) fail("no field holds an NA; NA in ", listed(na))
  check(
    max(polling$date) <= info$F && max(constituency$date) <= info$F,
    "every national and kj\u00f6rd\u00e6mi date is on or before F"
  )

  meta <- list(
    party_order = party_order,
    kjordaemi_levels = levels(constituency$kjordaemi),
    house_levels = levels(polling$fyrirtaeki),
    n_e = n_e,
    newcomers = pred$newcomers,
    debut_share = pred$debut,
    eday_index = as.integer(d$D + d$pred_y_time_diff),
    n_pred = d$n_pred,
    n_pred_k = n_pred_k,
    n_polls_nat = nrow(dplyr::distinct(info$polls_nat, date, fyrirtaeki)),
    n_polls_kd = nrow(dplyr::distinct(constituency[constituency$fyrirtaeki != ELECTION_HOUSE, ], date, fyrirtaeki)),
    max_poll_date = max(info$polls_nat$date),
    coalition_event = info$coalition_event
  )
  list(data = d, meta = meta)
}

#' The prediction-column rule (the 2021 study's `prediction_column()`, ported): rebuild
#' column D_f + 1 of `x_f`, `index_f` and `n_parties_f` for the cell's party order, adding
#' a zero fundamentals row for each newcomer. See `r0_stan_data()`.
#' @param fund_levels the levels of the fundamentals table's `flokkur`, which production's
#'   `index_f` counts
#' @return `list(data, newcomers, debut)`
prediction_column <- function(d, info, party_order, fund_levels) {
  box::use(
    R / harness / constants[previous_election],
    R / harness / fundamentals[fundamentals_party]
  )
  check(
    identical(fund_levels, rownames(d$y_f)),
    "the fundamentals levels are the rows of y_f"
  )
  col <- d$D_f + 1
  P <- length(party_order)
  train <- info$fundamentals$train
  prev <- train[which(train$date == previous_election(info$E)), ]
  label <- fundamentals_party(prev$flokkur)
  label[!label %in% party_order] <- "Anna\u00f0"
  share <- vapply(party_order, \(p) sum(prev$voteshare[label == p]), numeric(1))
  check(!anyNA(share) && all(share >= 0), "every previous share is a number of at least 0")
  old <- party_order[share > 0]
  newcomers <- party_order[share == 0]
  s <- share[old] / sum(share[old])
  centre <- mean(stats::qlogis(s))
  debut <- stats::median(train$voteshare[which(train$new_party == 1 & train$voteshare > 0)] / 100)

  check(!any(newcomers %in% rownames(d$y_f)), "a newcomer is not already a row of y_f")
  check(all(old %in% rownames(d$y_f)), "every party with a previous share is a row of y_f")
  for (m in NEWCOMER_ROWS) {
    zero <- matrix(0, length(newcomers), ncol(d[[m]]), dimnames = list(newcomers, colnames(d[[m]])))
    d[[m]] <- rbind(d[[m]], zero)
  }
  d$P_f <- d$P_f + length(newcomers)

  x <- stats::setNames(numeric(nrow(d$x_f)), rownames(d$x_f))
  x[old] <- stats::qlogis(s) - centre
  if (length(newcomers)) {
    check(is.finite(debut) && debut > 0 && debut < 1, "the debut share is in (0, 1)")
    x[newcomers] <- stats::qlogis(debut) - centre
  }
  d$x_f[, col] <- x
  check(nrow(d$index_f) >= P, "index_f has a row for every party")
  d$index_f[, col] <- c(match(party_order, rownames(d$y_f)), integer(nrow(d$index_f) - P))
  d$n_parties_f[col] <- P
  list(data = d, newcomers = newcomers, debut = debut)
}
