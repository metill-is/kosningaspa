# National scores of a forecast's draws against a result (spec §5.6): the CRPS, the
# Dawid–Sebastiani score, the alr Mahalanobis d², the 90% interval score, the MAE of
# medians and the energy score, with their batch-means and MCSE standard errors. The
# formulas are ported line for line from the overdisp study, whose published scores the
# scorer self-test reproduces from that study's saved draws. All functions are pure.

#' The eight named poll parties: `POLL_PARTIES` without Annað and Sósíalistaflokkurinn.
#' @export
NAMED8 <- local({
  box::use(R / harness / constants[POLL_PARTIES])
  setdiff(POLL_PARTIES, c("Anna\u00f0", "S\u00f3s\u00edalistaflokkurinn"))
})

# The truth `y` in the column order of `X`; an error unless both name the same parties.
align_truth <- function(X, y) {
  parties <- colnames(X)
  if (is.null(names(y)) || is.null(parties) || anyDuplicated(names(y)) || anyDuplicated(parties) ||
    !setequal(names(y), parties)) {
    stop(
      "truth and draws name different parties; truth: ", paste(names(y), collapse = ", "),
      "; draws: ", paste(parties, collapse = ", ")
    )
  }
  y[parties]
}

# 90% interval score (Gneiting and Raftery 2007): width plus 2/alpha times the miss distance.
is90 <- function(lo, hi, y) (hi - lo) + 20 * pmax(lo - y, 0) + 20 * pmax(y - hi, 0)

#' CRPS of the empirical distribution of draws: E|X − y| − ½E|X − X′| in its sorted-draw
#' V-statistic form.
#' @param x numeric draws
#' @param y the observed value
#' @return numeric(1)
#' @export
crps_draws <- function(x, y) {
  x <- sort(x)
  n <- length(x)
  mean(abs(x - y)) - sum((2 * seq_len(n) - n - 1) * x) / n^2
}

#' Long draws to a draw × party matrix.
#' @param long a data frame with `.chain`, `.iteration`, `.draw`, `flokkur` and the
#'   column named by `value`
#' @param value the name of the column to spread
#' @return a numeric matrix with one row per draw, in `.chain`, `.iteration` order, and
#'   one column per `flokkur` in first-appearance order, with integer attributes `chain`
#'   and `iteration`; an error if a draw lacks a party or a (`.chain`, `.iteration`) pair
#'   repeats
#' @export
wide_draws <- function(long, value) {
  stopifnot(is.character(value), length(value) == 1, value %in% names(long))
  parties <- unique(long$flokkur)
  ids <- unique(long[c(".chain", ".iteration", ".draw")])
  key <- function(d) paste(d$.chain, d$.iteration)
  if (anyDuplicated(key(ids))) stop("a (.chain, .iteration) pair repeats across draws")
  ids <- ids[order(ids$.chain, ids$.iteration), ]
  cell <- cbind(match(key(long), key(ids)), match(long$flokkur, parties))
  if (anyDuplicated(cell)) stop("a (.chain, .iteration) pair repeats for a party")
  if (nrow(cell) != nrow(ids) * length(parties)) {
    stop("draws lack a party: ", nrow(cell), " rows for ", nrow(ids), " draws of ", length(parties), " parties")
  }
  X <- matrix(NA_real_, nrow(ids), length(parties), dimnames = list(NULL, parties))
  X[cell] <- long[[value]]
  attr(X, "chain") <- as.integer(ids$.chain)
  attr(X, "iteration") <- as.integer(ids$.iteration)
  X
}

#' Per-party scores of a draw matrix.
#' @param X a draw × party matrix (`wide_draws()`)
#' @param y the truth: a numeric vector named by the parties of `X`, in any order; an
#'   error if the names differ from `colnames(X)`
#' @return a tibble with one row per column of `X`: `flokkur`, `mean`, `sd` (n − 1),
#'   `median`, `q05`, `q25`, `q75`, `q95` (type 7), `crps`, `dss` = log((100·sd)²) +
#'   ((y − mean)/sd)², `pit` = mean(X < y) + ½·mean(X = y), and `in50`, `in90` (y within
#'   [q25, q75] and [q05, q95])
#' @export
party_scores <- function(X, y) {
  y <- unname(align_truth(X, y))
  q <- unname(apply(X, 2, stats::quantile, probs = c(0.05, 0.25, 0.75, 0.95), names = FALSE, type = 7))
  mu <- unname(colMeans(X))
  s <- unname(apply(X, 2, stats::sd))
  cols <- seq_len(ncol(X))
  tibble::tibble(
    flokkur = colnames(X),
    mean = mu,
    sd = s,
    median = unname(apply(X, 2, stats::median)),
    q05 = q[1, ], q25 = q[2, ], q75 = q[3, ], q95 = q[4, ],
    crps = vapply(cols, \(j) crps_draws(X[, j], y[j]), 0),
    dss = log((100 * s)^2) + ((y - mu) / s)^2,
    pit = vapply(cols, \(j) mean(X[, j] < y[j]) + 0.5 * mean(X[, j] == y[j]), 0),
    in50 = y >= q[2, ] & y <= q[3, ],
    in90 = y >= q[1, ] & y <= q[4, ]
  )
}

#' Multivariate Dawid–Sebastiani score and Mahalanobis d² of the truth in alr coordinates
#' (reference: column 1). Both are invariant to the reference party.
#' @param X a draw × party share matrix
#' @param y the truth, named by the parties of `X`
#' @param zero_share shares below this are raised to it and each row is closed again
#' @return `c(mdss = log det S + d2, d2 = dᵀS⁻¹d)`, where S is the covariance of the alr
#'   draws and d the truth's alr minus their mean
#' @export
alr_dss <- function(X, y, zero_share) {
  y <- align_truth(X, y)
  X <- pmax(X, zero_share)
  X <- X / rowSums(X)
  Z <- log(X[, -1, drop = FALSE] / X[, 1])
  z <- log(y[-1] / y[1])
  S <- stats::cov(Z)
  d <- z - colMeans(Z)
  d2 <- as.numeric(t(d) %*% solve(S, d))
  c(mdss = as.numeric(determinant(S, logarithm = TRUE)$modulus) + d2, d2 = d2)
}

#' Energy score, with E‖X − X′‖ from a fixed pairing of each draw with the draw half the
#' sample away.
#' @param X a draw × party matrix, rows in draw order
#' @param y the truth, named by the parties of `X`
#' @return numeric(1): mean ‖Xᵢ − y‖ − ½ mean ‖Xᵢ − Xⱼ₍ᵢ₎‖ with
#'   j = ((i − 1 + n %/% 2) mod n) + 1
#' @export
energy_score <- function(X, y) {
  y <- align_truth(X, y)
  n <- nrow(X)
  j <- ((seq_len(n) - 1 + n %/% 2) %% n) + 1
  mean(sqrt(rowSums(sweep(X, 2, y)^2))) - 0.5 * mean(sqrt(rowSums((X - X[j, , drop = FALSE])^2)))
}

#' National scores of a draw matrix against the result.
#' @param X a draw × party share matrix (`wide_draws()`), rows in chain order; finite and
#'   non-negative
#' @param y the national vote shares, named by the parties of `X`
#' @param zero_share passed to `alr_dss()`
#' @return a one-row tibble: `n_parties`; `crps10` (100 × the mean CRPS, whatever the
#'   number of parties); `is90` (100 × the mean 90% interval score, type-7 `q05`/`q95`);
#'   `dss10` (the mean `dss`); `mdss` and `d2` (`alr_dss()`); `es`
#'   (`energy_score(100 * X, 100 * y)`); `mae8` and `mae_all` (100 × the mean
#'   |median − y| over the `NAMED8` parties present and over every party); `w90`
#'   (100 × the mean `q95 − q05`); `cov50` and `cov90` (counts); `zero_cells_replaced`
#'   (`sum(X <= 0)`)
#' @export
national_scores <- function(X, y, zero_share) {
  y <- align_truth(X, y)
  if (!all(is.finite(X)) || any(X < 0)) stop("draws must be finite and non-negative")
  ps <- party_scores(X, y)
  named <- ps$flokkur %in% NAMED8
  alr <- alr_dss(X, y, zero_share)
  tibble::tibble(
    n_parties = ncol(X),
    crps10 = 100 * mean(ps$crps),
    is90 = 100 * mean(is90(ps$q05, ps$q95, unname(y))),
    dss10 = mean(ps$dss),
    mdss = alr[["mdss"]],
    d2 = alr[["d2"]],
    es = energy_score(100 * X, 100 * y),
    mae8 = mean(abs(ps$median[named] - unname(y)[named])) * 100,
    mae_all = mean(abs(ps$median - unname(y))) * 100,
    w90 = mean(100 * (ps$q95 - ps$q05)),
    cov50 = sum(ps$in50),
    cov90 = sum(ps$in90),
    zero_cells_replaced = sum(X <= 0)
  )
}

#' Monte Carlo standard error of an MAE of medians.
#' @param X a draw × party matrix from `wide_draws()`: rows in chain order, equal chains,
#'   and its `chain` attribute
#' @param y the truth, named by the parties of `X`
#' @param parties the parties the MAE averages over
#' @return numeric(1): `100 / length(parties) * sqrt(sum(mcse^2))`, where `mcse` is each
#'   party's `posterior::mcse_median()` with one column per chain
#' @export
mae_se <- function(X, y, parties) {
  box::use(posterior[mcse_median])
  align_truth(X, y)
  chain <- attr(X, "chain")
  if (is.null(chain) || length(chain) != nrow(X)) stop("draws without a chain attribute: build them with wide_draws()")
  if (is.unsorted(chain) || length(unique(tabulate(chain)[unique(chain)])) != 1) {
    stop("draws are not in chain order with equal chains")
  }
  stopifnot(length(parties) > 0, all(parties %in% colnames(X)))
  n_chains <- length(unique(chain))
  mcse <- vapply(parties, \(p) mcse_median(matrix(X[, p], ncol = n_chains)), 0)
  100 / length(parties) * sqrt(sum(mcse^2))
}

#' Batch-means block of each draw: `per_chain` consecutive blocks within each chain.
#' @param chain,iteration integer vectors, one element per draw
#' @param per_chain blocks per chain
#' @return an integer vector:
#'   `(chain − 1) × per_chain + ceiling(iteration / (max(iteration) / per_chain))`
#' @export
batch_of <- function(chain, iteration, per_chain = 4L) {
  nit <- max(iteration)
  as.integer((chain - 1) * per_chain + ceiling(iteration / (nit / per_chain)))
}

#' Batch-means standard errors.
#' @param f a function from a draw matrix to a named numeric vector
#' @param X a draw matrix
#' @param block the block of each row of `X` (`batch_of()`)
#' @return a named numeric vector: the sd of `f` across blocks over √(number of blocks)
#' @export
batch_se <- function(f, X, block) {
  stopifnot(length(block) == nrow(X))
  values <- lapply(split(seq_len(nrow(X)), block), \(ix) f(X[ix, , drop = FALSE]))
  bt <- do.call(rbind, values)
  apply(bt, 2, stats::sd) / sqrt(nrow(bt))
}

#' National vote shares from kjördæmi vote draws.
#' @param eday_kd a data frame (`.chain`, `.iteration`, `.draw`, `kjordaemi`, `flokkur`,
#'   `votes`)
#' @return a tibble (`.chain`, `.iteration`, `.draw`, `flokkur`, `votes`, `share`): votes
#'   summed over kjördæmi, then `share = votes / sum(votes)` within each `.draw`
#' @export
kd_to_nat <- function(eday_kd) {
  box::use(dplyr[mutate, summarise])
  eday_kd |>
    summarise(votes = sum(votes), .by = c(.chain, .iteration, .draw, flokkur)) |>
    mutate(share = votes / sum(votes), .by = .draw)
}

#' Kjördæmi scores of kjördæmi vote draws against the result.
#' @param eday_kd a data frame (`.chain`, `.iteration`, `.draw`, `kjordaemi`, `flokkur`,
#'   `votes`), `.draw` numbered across chains; an error if it repeats a (`.draw`,
#'   `kjordaemi`, `flokkur`) cell, as draws numbered within each chain would
#' @param truth_kd a data frame (`kjordaemi`, `flokkur`, `share`), the share being the
#'   list's share of the valid votes in its kjördæmi; an error if it repeats a cell, or if a
#'   (`kjordaemi`, `flokkur`) cell of either table has no match in the other
#' @return a one-row tibble: `kd_cells` (the number of cells); `kd_crps` (100 × the mean
#'   CRPS); `kd_mae_named` (100 × the MAE of medians over the `NAMED8` cells); `kd_w90`
#'   (100 × the mean `q95 − q05`, type 7); `kd_cov90` (the cells whose truth lies within
#'   [`q05`, `q95`]). Draw shares are taken within each (`.draw`, `kjordaemi`).
#' @export
kd_scores <- function(eday_kd, truth_kd) {
  box::use(dplyr[mutate, summarise, first])
  key <- function(d) paste(d$kjordaemi, d$flokkur, sep = " / ")
  if (anyDuplicated(paste(eday_kd$.draw, key(eday_kd)))) stop("eday_kd repeats a (.draw, kjordaemi, flokkur) cell")
  if (anyDuplicated(key(truth_kd))) stop("truth repeats a cell")
  drawn <- unique(key(eday_kd))
  unmatched <- c(setdiff(drawn, key(truth_kd)), setdiff(key(truth_kd), drawn))
  if (length(unmatched)) stop("cells without a match: ", paste(unmatched, collapse = ", "))
  shares <- mutate(eday_kd, share = votes / sum(votes), .by = c(.draw, kjordaemi))
  shares$actual <- truth_kd$share[match(key(shares), key(truth_kd))]
  kd <- summarise(shares,
    median = stats::median(share),
    q05 = stats::quantile(share, 0.05, type = 7, names = FALSE),
    q95 = stats::quantile(share, 0.95, type = 7, names = FALSE),
    crps = crps_draws(share, first(actual)),
    actual = first(actual),
    .by = c(kjordaemi, flokkur)
  )
  named <- kd$flokkur %in% NAMED8
  tibble::tibble(
    kd_cells = nrow(kd),
    kd_crps = 100 * mean(kd$crps),
    kd_mae_named = mean(abs(kd$median[named] - kd$actual[named])) * 100,
    kd_w90 = 100 * mean(kd$q95 - kd$q05),
    kd_cov90 = sum(kd$actual >= kd$q05 & kd$actual <= kd$q95)
  )
}

#' Seat draws from kjördæmi vote draws.
#' @param eday_kd a data frame (`.chain`, `.iteration`, `.draw`, `kjordaemi`, `flokkur`,
#'   `votes`)
#' @param layout the seat layout (`layout_for()`)
#' @param ineligible lists that win no seat, passed to `calculate_seats()`
#' @return a tibble (`.chain`, `.iteration`, `.draw`, `flokkur`, `seats`): each draw's
#'   rows, sorted by `kjordaemi` then `flokkur`, go through `calculate_seats()`, and the
#'   seats are summed per party; draws in `.chain`, `.iteration` order, integer `seats`
#' @export
seat_draws <- function(eday_kd, layout, ineligible = "Anna\u00f0") {
  box::use(dplyr[arrange, select], R / election_utils[calculate_seats])
  # WHY: D'Hondt ties break by row order; arrange() sorts in C order as the overdisp study and calculate_seats() do, not en_US.
  rows <- eday_kd |>
    select(.chain, .iteration, .draw, kjordaemi, flokkur, votes) |>
    arrange(.chain, .iteration, .draw, kjordaemi, flokkur)
  id <- paste(rows$.chain, rows$.iteration, rows$.draw)
  per_draw <- lapply(split(seq_len(nrow(rows)), factor(id, levels = unique(id))), \(ix) {
    s <- calculate_seats(rows[ix, c("flokkur", "kjordaemi", "votes")], layout, ineligible)
    parties <- unique(s$flokkur)
    tibble::tibble(
      .chain = rows$.chain[ix[1]],
      .iteration = rows$.iteration[ix[1]],
      .draw = rows$.draw[ix[1]],
      flokkur = parties,
      seats = as.integer(vapply(parties, \(p) sum(s$seats[s$flokkur == p]), 0))
    )
  })
  dplyr::bind_rows(unname(per_draw))
}

#' Share of draws in which the parties' seats sum to at least `majority`.
#' @param seats seat draws (`seat_draws()`)
#' @param parties the parties of the coalition; an error if one is absent from `seats`
#' @param majority the seats a majority needs
#' @return numeric(1)
#' @export
majority_prob <- function(seats, parties, majority = 32L) {
  stopifnot(length(parties) > 0, !anyDuplicated(parties))
  X <- wide_draws(seats, "seats")
  absent <- setdiff(parties, colnames(X))
  if (length(absent)) stop("parties absent from the draws: ", paste(absent, collapse = ", "))
  mean(rowSums(X[, parties, drop = FALSE]) >= majority)
}

#' Seat scores of seat draws against the result, with the coalition-majority Brier score.
#' @param seats seat draws (`seat_draws()`)
#' @param actual the official seats: an integer vector named by the parties of `seats`, in
#'   any order; an error if the names differ
#' @param coalition the poll labels of the coalition whose majority is scored
#' @param formed the poll labels of the coalition that formed, or NULL
#' @param majority the seats a majority needs
#' @return `list(summary, per_party)`. `per_party` has one row per party: `flokkur`, `crps`
#'   (`crps_draws()` on seat counts), `median`, `q05` and `q95` (type 1) and `p_exact` (the
#'   share of draws with the party's seats exact). `summary` is one row: `seat_crps` (the
#'   sum of `crps`), `seat_abs_err` (the sum of |median − actual|), `seat_w90` (the mean
#'   `q95 − q05`), `seat_cov90` (the parties whose seats lie within [`q05`, `q95`]),
#'   `p_exact` (the share of draws with every party's seats exact), `p_majority`
#'   (`majority_prob()` of `coalition`), `majority_outcome` (`sum(actual[coalition]) >=
#'   majority`), `brier` = (p_majority − majority_outcome)² and `p_formed`
#'   (`majority_prob()` of `formed`, NA when `formed` is NULL; reported, not scored)
#' @export
seat_scores <- function(seats, actual, coalition, formed = NULL, majority = 32L) {
  X <- wide_draws(seats, "seats")
  y <- unname(align_truth(X, actual))
  q <- unname(apply(X, 2, stats::quantile, probs = c(0.05, 0.95), type = 1, names = FALSE))
  exact <- X == rep(y, each = nrow(X))
  per_party <- tibble::tibble(
    flokkur = colnames(X),
    crps = vapply(seq_len(ncol(X)), \(j) crps_draws(X[, j], y[j]), 0),
    median = unname(apply(X, 2, stats::median)),
    q05 = q[1, ],
    q95 = q[2, ],
    p_exact = unname(colMeans(exact))
  )
  p_majority <- majority_prob(seats, coalition, majority)
  outcome <- sum(actual[coalition]) >= majority
  summary <- tibble::tibble(
    seat_crps = sum(per_party$crps),
    seat_abs_err = sum(abs(per_party$median - y)),
    seat_w90 = mean(per_party$q95 - per_party$q05),
    seat_cov90 = sum(y >= per_party$q05 & y <= per_party$q95),
    p_exact = mean(rowSums(!exact) == 0),
    p_majority = p_majority,
    majority_outcome = outcome,
    brier = (p_majority - outcome)^2,
    p_formed = if (is.null(formed)) NA_real_ else majority_prob(seats, formed, majority)
  )
  list(summary = summary, per_party = per_party)
}
