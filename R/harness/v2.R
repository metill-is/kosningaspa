# Arm V2's inputs (spec §6): its configuration, and one cell's info_set() as the Stan data
# of Stan/forecast_v2.stan, with the meta its extractor and the scorer read; and its
# extraction (spec §5.5): the variables a fit's draws must hold, and their parquets.

#' The prior locations and scales V2 takes as data (Tier 3b's and Decision 1's), in data
#' order.
V2_PRIORS <- c("prior_sd_log_kappa", "prior_mu_phi", "prior_sd_mu1", "prior_rate_sigma_gamma")

#' The keys of a V2 config, in order.
V2_CONFIG_KEYS <- c("campaign_days", "desired_weight", "weight_time", "order", V2_PRIORS, "stage1")

#' The optional keys of a V2 config: `adapt_delta`, the sampler's target acceptance rate in the
#' arm's fits by `R/harness/fit_cell.R` (absent: CmdStan's default, 0.8).
V2_OPTIONAL_KEYS <- "adapt_delta"

#' The keys of a V2 config's `stage1`.
STAGE1_KEYS <- c("seed", "iter_warmup", "iter_sampling")

#' The party and kjördæmi orders a V2 config may name.
V2_ORDERS <- c("default", "reversed")

#' The data fields declared as a one-dimensional array or vector. They are stored as 1-d R
#' arrays, so `cmdstanr::write_stan_json()` writes a length-1 field as `[x]`, not as the
#' scalar that CmdStan's JSON reader refuses for an `array[1]`.
V2_VECTOR_FIELDS <- c(
  "delta_days", "campaign", "grid_epoch", "grid_nat", "house_nat", "n_rep_nat", "poll_kd", "kd_kd", "grid_kd",
  "house_kd", "n_rep_kd", "grid_res", "n_rep_res", "entry_epoch", "N_open", "sf_idx", "mu_f"
)

#' Stop with "v2 input: " and `...`.
fail <- function(...) stop("v2 input: ", ..., call. = FALSE)

#' Stop with "v2 input: `what`" unless `ok` is TRUE.
check <- function(ok, what) if (!isTRUE(ok)) fail(what)

#' The values of `x` joined for a message, or "none".
listed <- function(x) if (length(x)) paste(unique(x), collapse = "; ") else "none"

#' V2's configuration.
#' @param path the JSON file
#' @return a list in `V2_CONFIG_KEYS` order: `campaign_days` (an integer, at least 1),
#'   `desired_weight` (in (0, 1)), `weight_time` (days, at least `campaign_days`), `order`
#'   (`"default"`: `POLL_PARTIES` and `KJORDAEMI$kjordaemi`, restricted to the cell;
#'   `"reversed"`: both reversed), the `V2_PRIORS` (`prior_mu_phi` finite, the others
#'   positive) and `stage1` = `list(seed, iter_warmup, iter_sampling)` (integers; the seed
#'   at least 0, the iterations at least 1), then `adapt_delta` (in (0, 1)) when the config
#'   sets that optional key (`V2_OPTIONAL_KEYS`). An error, naming the file, on a missing or
#'   unknown key (in `stage1` too) or any other value
#' @export
read_v2_config <- function(path) {
  cfg <- jsonlite::read_json(path)
  bad <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  keys <- function(x, want, where, optional = character(0)) {
    missing <- setdiff(want, names(x))
    unknown <- setdiff(names(x), c(want, optional))
    if (length(missing) || length(unknown)) {
      bad(where, "missing key(s): ", listed(missing), "; unknown key(s): ", listed(unknown))
    }
  }
  number <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  whole <- function(x, lo) number(x) && x == round(x) && x >= lo
  if (!is.list(cfg) || is.null(names(cfg))) bad("not a JSON object")
  keys(cfg, V2_CONFIG_KEYS, "", V2_OPTIONAL_KEYS)
  if (!whole(cfg$campaign_days, 1)) bad("campaign_days must be a whole number of days, at least 1")
  if (!(number(cfg$desired_weight) && cfg$desired_weight > 0 && cfg$desired_weight < 1)) {
    bad("desired_weight must be one number in (0, 1)")
  }
  if (!(number(cfg$weight_time) && cfg$weight_time >= cfg$campaign_days)) {
    bad("weight_time must be a number of days, at least campaign_days")
  }
  if (!(is.character(cfg$order) && length(cfg$order) == 1 && cfg$order %in% V2_ORDERS)) {
    bad("order must be ", paste(V2_ORDERS, collapse = " or "))
  }
  if (!number(cfg$prior_mu_phi)) bad("prior_mu_phi must be one finite number")
  for (p in setdiff(V2_PRIORS, "prior_mu_phi")) if (!(number(cfg[[p]]) && cfg[[p]] > 0)) bad(p, " must be one positive number")
  s1 <- cfg$stage1
  if (!is.list(s1) || is.null(names(s1))) bad("stage1 must be an object with keys ", paste(STAGE1_KEYS, collapse = ", "))
  keys(s1, STAGE1_KEYS, "stage1: ")
  if (!whole(s1$seed, 0)) bad("stage1: seed must be a whole number, at least 0")
  for (k in c("iter_warmup", "iter_sampling")) if (!whole(s1[[k]], 1)) bad("stage1: ", k, " must be a whole number, at least 1")
  out <- list(
    campaign_days = as.integer(cfg$campaign_days),
    desired_weight = as.numeric(cfg$desired_weight),
    weight_time = as.numeric(cfg$weight_time),
    order = cfg$order
  )
  out[V2_PRIORS] <- lapply(cfg[V2_PRIORS], as.numeric)
  out$stage1 <- lapply(s1[STAGE1_KEYS], as.integer)
  ad <- cfg[["adapt_delta"]]
  if (!is.null(ad)) {
    if (!(number(ad) && ad > 0 && ad < 1)) bad("adapt_delta must be one number in (0, 1)")
    out$adapt_delta <- as.numeric(ad)
  }
  out
}

#' The stage-1 output of a 2017 cell before the freeze: no fundamentals forecast is made.
#' @param info an `info_set()` list
#' @return `list(mu_f, sha256, diagnostics)`: `mu_f` 0 for every party of
#'   `info$fundamentals$S_f`, named by them; `sha256` "placeholder"; `diagnostics` an empty
#'   list
#' @export
stage1_placeholder <- function(info) {
  S_f <- info$fundamentals$S_f
  list(mu_f = stats::setNames(rep(0, length(S_f)), S_f), sha256 = "placeholder", diagnostics = list())
}

#' The keys of rows as single strings, one per row of `cols`.
row_key <- function(rows, cols) do.call(paste, c(lapply(rows[cols], as.character), sep = "\r"))

#' Rows sorted by `cols`, radix order (bytewise for strings, whatever the locale).
sorted_by <- function(rows, cols) rows[do.call(order, c(unname(as.list(rows[cols])), method = "radix")), ]

#' The count matrix of long rows (`cols`, flokkur, n): one row per row of `units`, one column
#' per party of `party_order`, the counts rounded to integers as production rounds them, 0
#' where a party has no row.
count_matrix <- function(units, rows, cols, party_order) {
  y <- matrix(0L, nrow(units), length(party_order))
  y[cbind(match(row_key(rows, cols), row_key(units, cols)), match(rows$flokkur, party_order))] <- as.integer(round(rows$n))
  y
}

#' The reported parties of each row of a count matrix: the columns with a count above 0, in
#' column order, padded with 0 to the matrix's width.
reported <- function(y) {
  idx <- matrix(0L, nrow(y), ncol(y))
  for (i in seq_len(nrow(y))) {
    s <- which(y[i, ] > 0)
    idx[i, seq_along(s)] <- s
  }
  idx
}

#' Arm V2's Stan data and meta for one cell.
#' @param info an `info_set()` list
#' @param cfg a `read_v2_config()` list
#' @param stage1 the stage-1 fit, called as `stage1(info, cfg, root = root)` unless
#'   `forecast_free(E, root)` holds; it returns `list(mu_f, sha256, diagnostics)` with
#'   `mu_f` named by `info$fundamentals$S_f`. The default is `stage1_mu_f()`
#'   (`R/harness/stage1.R`)
#' @param party_order for tests only: the party order, overriding `cfg$order` for the
#'   parties (the kjördæmi keep `cfg$order`); NULL takes `POLL_PARTIES` restricted to
#'   `info$parties`, reversed when `cfg$order` is `"reversed"`
#' @param root the project root whose freeze files `forecast_free()` reads
#' @return `list(data, meta)`. `data` holds the fields of the Stan file's data block, in its
#'   order (`stan_data_sha()` depends on it), with no dimnames and each one-dimensional field
#'   a 1-d array (`V2_VECTOR_FIELDS`):
#'   - sizes and the grid: `P`, `K`, `H`, `G`, `n_epoch` (`epoch_of(E)`), `delta_days`
#'     (the G - 1 steps of `grid_dates()`), `campaign` (1 for a step starting on or after
#'     an election - `campaign_days` and ending on or before it, for an election before E
#'     or E itself), `grid_epoch` (`epoch_of()` of each grid date: an election day belongs
#'     to the epoch it closes), `exists` (G × P, 1 from a party's first non-zero poll in
#'     the entry table on) and `w` (n_epoch × K: each closed epoch's closing election's
#'     valid vote shares, the open epoch's from `info$valid_votes_open`, the previous
#'     election's);
#'   - national polls, one per (date, fyrirtaeki) sorted by date and fyrirtaeki: `N_nat`,
#'     `grid_nat`, `house_nat`, `y_nat` (N_nat × P, the counts rounded), `n_rep_nat` and
#'     `idx_nat` (the parties with a count above 0, in party order, padded with 0); houses
#'     are the house table's `house` of each pollster, ordered by the date of its first
#'     admitted national or kjördæmi poll, ties by name;
#'   - kjördæmi polls: `J` polls, one per (date, fyrirtaeki), numbered 1..J in date order
#'     (ties by fyrirtaeki), and `N_kd` rows, one per poll and kjördæmi, sorted by poll and
#'     kjördæmi: `poll_kd`, `kd_kd`, `grid_kd`, `house_kd`, `y_kd`, `n_rep_kd`, `idx_kd`;
#'   - results of the `n_closed` window elections before E: `grid_res`, `y_res`
#'     (n_closed × K × P, the official kjördæmi votes from `info$results`), `n_rep_res`
#'     and `idx_res` (the parties with national votes);
#'   - `entry_epoch` (0 for a party present at the window's start, else its entry epoch);
#'   - `N_open` (`info$valid_votes_open`'s votes), `draw_forecast` and `latent_path`;
#'   - `n_sf`, `sf_idx` (each `mu_f` party's index in the party order) and `mu_f`, from
#'     stage 1; `desired_weight`, `weight_time` and `campaign_days`, then the `V2_PRIORS`,
#'     from `cfg`.
#'
#'   `draw_forecast` is 1 and `latent_path` 0, except on a 2017 cell before the freeze
#'   (`forecast_free(E, root)`: E = 2017-10-28, the confirmatory target of `TARGETS`, while
#'   `is_frozen(root)` is FALSE): there `draw_forecast` is 0 and `mu_f` comes from
#'   `stage1_placeholder(info)`, never from `stage1`.
#'
#'   `meta`: `party_order`, `kjordaemi_levels`, `house_levels`, `newcomers` (the parties
#'   absent at the window's start, §6.4), `eday_index` (G), `n_pred` (Σ N_open), `n_pred_k`
#'   (N_open named by kjördæmi), `n_polls_nat` (N_nat), `n_polls_kd` (J), `max_poll_date`
#'   (the last national poll date), `coalition_event`, `coordinate_party` (the first party),
#'   `n_grid` (G), `n_epoch`, `n_closed`, `closed_elections` (the ISO dates of the n_closed
#'   elections before E, in date order), `n_kd_polls` (N_kd, the kjördæmi poll rows) and
#'   `stage1_sha256`.
#'
#'   An error, "v2 input: ...", if `party_order` is not a permutation of the cell's
#'   parties; no national poll is admitted; a national or kjördæmi poll lists a party (and
#'   kjördæmi) twice; a count is missing or negative; a poll row's party is outside the
#'   cell or its kjördæmi outside `KJORDAEMI`; a poll is a Kosning row; a pollster has no
#'   house or a party no entry row; a closed election lacks kjördæmi results, gives votes
#'   to a party outside the cell, or gives a party with national votes no votes in some
#'   kjördæmi; the coordinate party has no votes at some closed election; the grid has a
#'   non-positive step or does not end at E; an observation (poll or closed election) is
#'   dated after F, or a grid date after F is neither E - `campaign_days` nor E; a poll
#'   reports fewer than two parties, or a poll or result reports a party before its entry;
#'   a party enters after E's epoch or exists at no grid date; the open epoch's valid votes
#'   are not positive whole numbers or not the previous election's; stage 1's `mu_f` is not
#'   finite and named by `info$fundamentals$S_f`; or a field holds an NA.
#' @export
v2_stan_data <- function(info, cfg, stage1 = stage1_mu_f, party_order = NULL, root = project_root()) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(
    R / harness / constants[ELECTION_HOUSE, ELECTIONS, KJORDAEMI, POLL_PARTIES, project_root],
    R / harness / freeze[forecast_free],
    R / harness / stage1[stage1_mu_f],
    R / harness / tables[epoch_of, grid_dates, valid_votes]
  )
  check(all(V2_CONFIG_KEYS %in% names(cfg)) && isTRUE(cfg$order %in% V2_ORDERS), "cfg is not a read_v2_config() list")
  E <- as.Date(info$E)
  F <- as.Date(info$F)
  reversed <- identical(cfg$order, "reversed")
  if (is.null(party_order)) {
    party_order <- intersect(POLL_PARTIES, info$parties)
    if (reversed) party_order <- rev(party_order)
  }
  check(
    is.character(party_order) && length(party_order) == length(info$parties) && !anyDuplicated(party_order) &&
      setequal(party_order, info$parties),
    "party_order is not a permutation of the cell's parties"
  )
  kd_order <- if (reversed) rev(KJORDAEMI$kjordaemi) else KJORDAEMI$kjordaemi
  P <- length(party_order)
  K <- length(kd_order)
  cd <- cfg$campaign_days

  nat <- info$polls_nat
  kd <- info$polls_kd
  check(nrow(nat) > 0, "no national poll is admitted")
  twice <- duplicated(nat[c("date", "fyrirtaeki", "flokkur")])
  if (any(twice)) fail("national poll ", listed(paste(nat$date, nat$fyrirtaeki, "lists", nat$flokkur)[twice]), " twice")
  twice <- duplicated(kd[c("date", "fyrirtaeki", "kjordaemi", "flokkur")])
  if (any(twice)) {
    fail("kj\u00f6rd\u00e6mi poll ", listed(paste(kd$date, kd$fyrirtaeki, "lists", kd$kjordaemi, kd$flokkur)[twice]), " twice")
  }
  # WHY: counts are rounded as production rounds them, so float noise below 0 (-1e-14) is a 0.
  check(!anyNA(c(nat$n, kd$n)) && all(round(c(nat$n, kd$n)) >= 0), "a poll count is missing or negative")
  outside <- setdiff(c(nat$flokkur, kd$flokkur), party_order)
  if (length(outside)) fail("a poll row's party is outside the cell: ", listed(outside))
  outside <- setdiff(kd$kjordaemi, kd_order)
  if (length(outside)) fail("a kj\u00f6rd\u00e6mi poll row's kj\u00f6rd\u00e6mi is outside KJORDAEMI: ", listed(outside))
  check(!ELECTION_HOUSE %in% c(nat$fyrirtaeki, kd$fyrirtaeki), "a Kosning row is among the polls")
  pollsters <- unique(c(nat$fyrirtaeki, kd$fyrirtaeki))
  no_house <- setdiff(pollsters, info$houses$fyrirtaeki)
  if (length(no_house)) fail("a pollster has no house: ", listed(no_house))
  house_of <- function(fyrirtaeki) info$houses$house[match(fyrirtaeki, info$houses$fyrirtaeki)]
  entry <- info$entry[match(party_order, info$entry$flokkur), ]
  no_entry <- party_order[is.na(entry$flokkur)]
  if (length(no_entry)) fail("a party has no entry row: ", listed(no_entry))
  entry_date <- as.Date(entry$first_nonzero)

  n_epoch <- epoch_of(E)
  closed <- ELECTIONS[ELECTIONS < E]
  n_closed <- length(closed)
  check(n_closed >= 1 && n_closed == n_epoch - 1L, "E is not a window election after the first")
  y_res <- array(0L, c(n_closed, K, P))
  for (e in seq_len(n_closed)) {
    r <- info$results[which(info$results$election == closed[e]), ]
    if (!setequal(r$kjordaemi, kd_order)) fail("no kj\u00f6rd\u00e6mi results for ", format(closed[e]))
    check(!anyNA(r$votes) && all(r$votes >= 0), "a result's votes are missing or negative")
    outside <- r$flokkur[!r$flokkur %in% party_order & r$votes > 0]
    if (length(outside)) fail("a party outside the cell has votes at ", format(closed[e]), ": ", listed(outside))
    r <- r[r$flokkur %in% party_order, ]
    if (anyDuplicated(r[c("kjordaemi", "flokkur")])) fail("a result lists a party twice at ", format(closed[e]))
    votes <- matrix(0L, K, P)
    votes[cbind(match(r$kjordaemi, kd_order), match(r$flokkur, party_order))] <- as.integer(r$votes)
    gap <- which(colSums(votes) > 0 & colSums(votes == 0) > 0)
    if (length(gap)) {
      p <- gap[1]
      fail(party_order[p], " has votes at ", format(closed[e]), " but no votes in ", listed(kd_order[votes[, p] == 0]))
    }
    y_res[e, , ] <- votes
  }
  nat_res <- apply(y_res, c(1, 3), sum)
  absent <- closed[nat_res[, 1] == 0]
  if (length(absent)) fail("coordinate party ", party_order[1], " has no votes at ", listed(format(absent)))

  g <- grid_dates(info, cd)
  G <- length(g)
  steps <- as.numeric(diff(g))
  if (any(steps <= 0)) fail("non-positive grid step after ", listed(format(g[-G][steps <= 0])))
  check(G >= 2 && g[G] == E, "the grid does not end at E")
  obs <- as.Date(c(nat$date, kd$date, closed))
  late <- c(obs[obs > F], g[g > F & !g %in% c(E - cd, E)])
  if (length(late)) fail("observation after F: ", listed(format(sort(late))))

  elections <- c(closed, E)
  campaign <- vapply(seq_len(G - 1), \(i) as.integer(any(g[i] >= elections - cd & g[i + 1] <= elections)), integer(1))
  exists <- matrix(as.integer(outer(as.numeric(g), as.numeric(entry_date), ">=")), G, P)

  firsts <- dplyr::summarise(
    tibble::tibble(house = house_of(c(nat$fyrirtaeki, kd$fyrirtaeki)), date = c(nat$date, kd$date)),
    first = min(date), .by = house
  )
  firsts <- sorted_by(firsts, c("first", "house"))
  house_levels <- firsts$house

  units_nat <- sorted_by(dplyr::distinct(nat, date, fyrirtaeki), c("date", "fyrirtaeki"))
  y_nat <- count_matrix(units_nat, nat, c("date", "fyrirtaeki"), party_order)
  grid_nat <- match(units_nat$date, g)

  polls_kd <- sorted_by(dplyr::distinct(kd, date, fyrirtaeki), c("date", "fyrirtaeki"))
  units_kd <- dplyr::distinct(kd, date, fyrirtaeki, kjordaemi)
  units_kd$poll <- match(row_key(units_kd, c("date", "fyrirtaeki")), row_key(polls_kd, c("date", "fyrirtaeki")))
  units_kd$k <- match(units_kd$kjordaemi, kd_order)
  units_kd <- sorted_by(units_kd, c("poll", "k"))
  y_kd <- count_matrix(units_kd, kd, c("date", "fyrirtaeki", "kjordaemi"), party_order)
  grid_kd <- match(units_kd$date, g)
  grid_res <- match(closed, g)

  thin <- c(
    paste("national", units_nat$date, units_nat$fyrirtaeki)[rowSums(y_nat > 0) < 2],
    paste("kj\u00f6rd\u00e6mi", units_kd$date, units_kd$fyrirtaeki, units_kd$kjordaemi)[rowSums(y_kd > 0) < 2]
  )
  if (length(thin)) fail("a poll reports fewer than two parties: ", listed(thin))
  early <- function(y, at, label) label[rowSums(y > 0 & exists[at, , drop = FALSE] == 0L) > 0]
  early <- c(
    early(y_nat, grid_nat, paste("national", units_nat$date, units_nat$fyrirtaeki)),
    early(y_kd, grid_kd, paste("kj\u00f6rd\u00e6mi", units_kd$date, units_kd$fyrirtaeki, units_kd$kjordaemi)),
    early(nat_res, grid_res, paste("election", format(closed)))
  )
  if (length(early)) fail("a party is reported before its entry: ", listed(early))

  w <- matrix(0, n_epoch, K)
  for (e in seq_len(n_closed)) {
    v <- valid_votes(info$results, closed[e])
    w[e, ] <- v$w[match(kd_order, v$kjordaemi)]
  }
  vo <- info$valid_votes_open
  check(nrow(vo) == K && setequal(vo$kjordaemi, kd_order), "the open epoch's valid votes do not cover the kj\u00f6rd\u00e6mi")
  N_open <- vo$votes[match(kd_order, vo$kjordaemi)]
  check(!anyNA(N_open) && all(N_open > 0 & N_open == round(N_open)), "the open epoch's valid votes are not positive whole numbers")
  prev <- valid_votes(info$results, closed[n_closed])
  check(
    identical(as.numeric(N_open), as.numeric(prev$votes[match(kd_order, prev$kjordaemi)])),
    "the open epoch's valid votes are not those of the previous election"
  )
  w[n_epoch, ] <- vo$w[match(kd_order, vo$kjordaemi)]

  free <- forecast_free(E, root)
  s1 <- if (free) stage1_placeholder(info) else stage1(info, cfg, root = root)
  S_f <- info$fundamentals$S_f
  check(
    is.numeric(s1$mu_f) && all(is.finite(s1$mu_f)) && !anyDuplicated(names(s1$mu_f)) &&
      setequal(names(s1$mu_f), S_f) && all(S_f %in% party_order),
    "stage 1's mu_f is not finite and named by the parties of S_f"
  )

  entry_epoch <- ifelse(entry$newcomer, as.integer(entry$entry_epoch), 0L)
  check(all(entry_epoch >= 0L & entry_epoch <= n_epoch), "a party enters after E's epoch")
  check(all(colSums(exists) > 0L), "a party exists at no grid date")
  data <- list(
    P = P, K = K, H = length(house_levels), G = G, n_epoch = n_epoch,
    delta_days = steps, campaign = campaign, grid_epoch = epoch_of(g), exists = exists, w = w,
    N_nat = nrow(units_nat), grid_nat = grid_nat, house_nat = match(house_of(units_nat$fyrirtaeki), house_levels),
    y_nat = y_nat, n_rep_nat = as.integer(rowSums(y_nat > 0)), idx_nat = reported(y_nat),
    J = nrow(polls_kd), N_kd = nrow(units_kd), poll_kd = units_kd$poll, kd_kd = units_kd$k, grid_kd = grid_kd,
    house_kd = match(house_of(units_kd$fyrirtaeki), house_levels),
    y_kd = y_kd, n_rep_kd = as.integer(rowSums(y_kd > 0)), idx_kd = reported(y_kd),
    n_closed = n_closed, grid_res = grid_res, y_res = y_res, n_rep_res = as.integer(rowSums(nat_res > 0)),
    idx_res = reported(nat_res),
    entry_epoch = entry_epoch,
    N_open = as.integer(N_open), draw_forecast = if (free) 0L else 1L, latent_path = 0L,
    n_sf = length(s1$mu_f), sf_idx = match(names(s1$mu_f), party_order), mu_f = unname(as.numeric(s1$mu_f)),
    desired_weight = cfg$desired_weight, weight_time = cfg$weight_time, campaign_days = cd
  )
  data[V2_PRIORS] <- cfg[V2_PRIORS]
  data[V2_VECTOR_FIELDS] <- lapply(data[V2_VECTOR_FIELDS], \(x) array(x, dim = length(x)))
  na <- names(data)[vapply(data, anyNA, logical(1))]
  if (length(na)) fail("no field holds an NA; NA in ", listed(na))

  meta <- list(
    party_order = party_order,
    kjordaemi_levels = kd_order,
    house_levels = house_levels,
    newcomers = party_order[entry_epoch > 0],
    eday_index = G,
    n_pred = sum(as.integer(N_open)),
    n_pred_k = stats::setNames(as.integer(N_open), kd_order),
    n_polls_nat = data$N_nat,
    n_polls_kd = data$J,
    max_poll_date = max(nat$date),
    coalition_event = info$coalition_event,
    coordinate_party = party_order[1],
    n_grid = G,
    n_epoch = n_epoch,
    n_closed = n_closed,
    closed_elections = format(closed),
    n_kd_polls = data$N_kd,
    stage1_sha256 = s1$sha256
  )
  list(data = data, meta = meta)
}

#' The forecast and replicate variables `v2_extract()` reads (spec §6.8): the kjördæmi counts
#' at E, their vote-weighted national aggregate, the latent national share at E and the
#' in-sample replicates of the closed elections' kjördæmi counts.
V2_DRAW_VARIABLES <- c("votes_kd", "pi_nat_eday", "pi_latent_E", "y_rep_res")

#' The parameters and transformed parameters of `Stan/forecast_v2.stan` that
#' `param_draws.parquet` keeps, in its column order.
#' @export
V2_PARAM_VARIABLES <- c(
  "sigma", "L_Omega", "tau_camp", "mu_phi", "sigma_phi", "log_phi", "log_phi_K", "log_kappa", "sigma_gamma", "mu",
  "mu_sd", "tau_sd", "s_delta0"
)

#' The variables arm V2's extractor reads from a fit: `V2_DRAW_VARIABLES`, then
#' `V2_PARAM_VARIABLES`.
#' @export
V2_VARIABLES <- c(V2_DRAW_VARIABLES, V2_PARAM_VARIABLES)

#' Stop with "v2_extract: " and `...`.
extract_fail <- function(...) stop("v2_extract: ", ..., call. = FALSE)

#' Write a V2 fit's draws as the spec's §5.5 parquets, and the closed elections' in-sample
#' replicates.
#' @param draws a posterior draws object (a `draws_df`, or anything `as_draws_df()` takes)
#'   holding the elements of every variable of `V2_VARIABLES`; other variables are ignored
#' @param meta the builder's meta (`v2_stan_data()$meta`): `party_order`,
#'   `kjordaemi_levels` and `closed_elections` are read
#' @param dir the directory to write into (created if absent)
#' @return invisibly, the paths written, named `eday_kjordaemi`, `eday_national`,
#'   `latent_national`, `param_draws` and `ppc_results` (each `dir/<name>.parquet`):
#'   - `eday_kjordaemi`: `.chain`, `.iteration`, `.draw` (integer), `kjordaemi`, `flokkur`
#'     (character), `votes` (integer), from `votes_kd[k, p]` labelled `kjordaemi_levels[k]`
#'     and `party_order[p]`;
#'   - `eday_national`: `.chain`, `.iteration`, `.draw`, `flokkur`, `share`, the share
#'     `pi_nat_eday[p]`, the vote-weighted aggregate of `votes_kd`, so its gap to
#'     `kd_to_nat(eday_kjordaemi)` is zero;
#'   - `latent_national`: the same columns, the share `pi_latent_E[p]`;
#'   - `param_draws`: `.chain`, `.iteration`, `.draw`, then one column per element of the
#'     parameters, the variables in `V2_PARAM_VARIABLES` order and each one's elements in
#'     Stan's column-major order;
#'   - `ppc_results`: `.chain`, `.iteration`, `.draw`, `election` (Date), `kjordaemi`,
#'     `flokkur`, `votes` (integer), from `y_rep_res[e, k, p]` labelled
#'     `closed_elections[e]`, `kjordaemi_levels[k]` and `party_order[p]`; 0 for a party
#'     without national votes at that election. Only the past-election checks read it.
#'
#'   `.draw` is numbered across chains, 1..N, as `as_draws_df()` numbers it. Rows run by
#'   draw, then (election,) kjördæmi and party in the meta's orders. Every label comes from
#'   the indices parsed out of the element names, never from a column's position. Nothing is
#'   written unless every table builds; an error, "v2_extract: ...", if the meta lacks a
#'   field, a variable lacks an element or holds one outside the meta's levels, or a count
#'   is not a whole number.
#' @export
v2_extract <- function(draws, meta, dir) {
  box::use(R / harness / r0[indexed, grid_values, long_draws])
  lacks <- setdiff(c("party_order", "kjordaemi_levels", "closed_elections"), names(meta))
  if (length(lacks)) extract_fail("the meta lacks ", paste(lacks, collapse = ", "))
  closed <- tryCatch(as.Date(meta$closed_elections), error = function(e) as.Date(NA))
  if (!length(closed) || anyNA(closed)) extract_fail("closed_elections must be one or more dates")
  draws <- posterior::as_draws_df(draws)
  parties <- meta$party_order
  kd_levels <- meta$kjordaemi_levels
  P <- length(parties)
  K <- length(kd_levels)
  n_closed <- length(closed)
  ids <- list(
    .chain = as.integer(draws$.chain), .iteration = as.integer(draws$.iteration), .draw = as.integer(draws$.draw)
  )
  cells <- function(variable, dims) grid_values(indexed(draws, variable, extract_fail), variable, dims, extract_fail)
  counts <- function(variable, dims) {
    x <- cells(variable, dims)
    if (anyNA(x) || any(x != round(x))) extract_fail(variable, " holds a count that is not a whole number")
    x
  }

  kd_cells <- tibble::tibble(kjordaemi = rep(kd_levels, each = P), flokkur = rep(parties, times = K))
  eday_kd <- long_draws(ids, kd_cells, counts("votes_kd", c(K, P)), "votes")
  eday_kd$votes <- as.integer(eday_kd$votes)
  labels <- tibble::tibble(flokkur = parties)
  eday_nat <- long_draws(ids, labels, cells("pi_nat_eday", P), "share")
  latent <- long_draws(ids, labels, cells("pi_latent_E", P), "share")

  res_cells <- tibble::tibble(
    election = rep(closed, each = K * P),
    kjordaemi = rep(rep(kd_levels, each = P), times = n_closed),
    flokkur = rep(parties, times = n_closed * K)
  )
  ppc <- long_draws(ids, res_cells, counts("y_rep_res", c(n_closed, K, P)), "votes")
  ppc$votes <- as.integer(ppc$votes)

  kept <- lapply(V2_PARAM_VARIABLES, \(v) {
    x <- indexed(draws, v, extract_fail)
    ord <- if (ncol(x$index)) do.call(order, rev(lapply(seq_len(ncol(x$index)), \(j) x$index[, j]))) else 1L
    stats::setNames(lapply(ord, \(i) x$values[, i]), x$columns[ord])
  })
  param_draws <- tibble::as_tibble(c(ids, unlist(kept, recursive = FALSE)))

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(
    eday_kjordaemi = eday_kd, eday_national = eday_nat, latent_national = latent, param_draws = param_draws,
    ppc_results = ppc
  )
  paths <- stats::setNames(file.path(dir, paste0(names(tables), ".parquet")), names(tables))
  for (f in names(tables)) arrow::write_parquet(tables[[f]], paths[[f]])
  invisible(paths)
}
