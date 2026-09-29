# Flow-informed prior target for the random-walk correlation.
#
# Idea: month-to-month share movement is voters moving between parties. If net flow on
# each pair (a, b) is an independent zero-mean shock with variance w_ab, share increments
# have covariance equal to the graph Laplacian of W. Flows to and from outside the
# decided electorate (non-voters, first-time voters, blank ballots: the reservoir R)
# move decided shares by -(f / T)(e_a - pi), because every decided share renormalises.
# So, up to scale,
#   Sigma_q = sum_{a<b} w_ab^g (e_a - e_b)(e_a - e_b)' + sum_a w_aR^g (e_a - pi)(e_a - pi)'.
# In share space the reservoir term lets loyal parties co-move positively, which a pure
# Laplacian cannot. In log-ratio (clr) coordinates it reduces exactly to independent
# per-party log shocks, w_aR / pi_a^2, because D^-1 (e_a - pi) = e_a / pi_a - 1 and the
# common 1 is centred away; so it shrinks Omega_0 slightly but adds no co-movement.
# W is the gross exchange in an ÍSKOS switching table (R/build_iskos_switching.R).
#
# Omega_0 is sensitive to the smallest share in pi. Its log-scale shocks scale as
# 1 / pi^2 and, through the clr mean, enter every coordinate; a 0.5% party measured by
# a handful of respondents then dominates the target (2024: mean off-diagonal -0.09 at a
# 2% minimum share, +0.16 at the raw 0.5% Annað). `min_share` regularises that; the
# target is stable from about 2% up (2% vs 3%: max difference 0.03).
#
# The forecast model (polling_and_fundamentals_kjordaemi.stan) puts its RW on
# beta = clr(pi)[2:P] (party 1 = Annað is the implicit -sum), so the target maps exactly:
#   Sigma_0 = A D^-1 Sigma_q D^-1 A',  A = [0 | I_{P-1}] (I - 11'/P),  D = diag(pi),
# and Omega_0 = cov2cor(Sigma_0) is the centre for
#   L_Omega ~ lkj_corr_cholesky(kappa + 1);
#   target += -kappa * sum(Omega0_inv .* multiply_lower_tri_self_transpose(L_Omega));
# which is exactly LKJ(kappa + 1) when Omega_0 = I, and has its mode at Omega_0.
# polling_watch_v4 centres its innovations, so its pre-centring Omega is not identified
# and needs the share-space Sigma_q, not Omega_0 (see Stan/polling_watch_variants.md).

reservoir_statuses <- c("Kaus ekki", "Ekki kosningarétt", "Autt/ógilt")
unknown_statuses <- c("Óþekkt", "Vill ekki segja", "Man ekki", "Kaus, óþekkt", "Óflokkað")

#' Forecast-model party order (party 1 = reference Annað), as in read_polling_data().
#' @export
model_parties <- function() {
  c(
    "Annað", "Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Samfylkingin", "Vinstri Græn",
    "Píratar", "Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn"
  )
}

#' Map a switching-table group to a model node: the party itself, "Annað", the
#' reservoir "Utan", or NA (unknown answer, dropped as missing at random).
#' @export
to_node <- function(group, parties = model_parties()) {
  dplyr::case_when(
    is.na(group) ~ NA_character_,
    group %in% parties ~ group,
    group %in% reservoir_statuses ~ "Utan",
    group %in% unknown_statuses ~ NA_character_,
    .default = "Annað"
  )
}

#' Decided vote shares of one official result over the model parties.
#' @param year election year in the fundamentals data
#' @param min_share minimum share for EVERY model party (did not run, or tiny such as
#'   Annað at 0.5% in 2021), then renormalised. It caps the 1 / pi amplification of the
#'   log-scale map; see the module header.
#' @export
result_shares <- function(year, parties = model_parties(), min_share = 0.02) {
  box::use(R / data[read_fundamentals_data], dplyr[filter, mutate, summarise])
  x <- read_fundamentals_data() |>
    filter(.data$year == .env$year) |>
    mutate(node = to_node(as.character(.data$flokkur), parties)) |>
    summarise(p = sum(.data$voteshare), .by = "node")
  stopifnot("no official result for that year" = nrow(x) > 0)
  pi <- stats::setNames(x$p[match(parties, x$node)], parties)
  pi[is.na(pi)] <- 0
  pi <- pi / sum(pi)
  pi <- pmax(pi, min_share)
  pi / sum(pi)
}

#' Gross exchange matrix W over the model parties plus the reservoir "Utan".
#'
#' A model party absent from the table (founded later) gets imputed IIA-style edges,
#' w_ab = s * pi_a * pi_b, with s the median observed intensity w_ab / (pi_a pi_b);
#' its reservoir edge likewise from the median w_aR / pi_a. A pseudo-count on every
#' pair keeps the graph connected, so the target is positive definite.
#' @param switching switching_long table (read_switching())
#' @param table_year year_cur of the ÍSKOS wave to use
#' @param pi decided shares over `parties` (used only for imputation)
#' @param pseudo pseudo-count (respondents) added to every off-diagonal pair
#' @param reservoir keep the reservoir node; FALSE drops its flows (closed system)
#' @return list(W, imputed, n_used, n_dropped)
#' @export
flow_exchange <- function(switching, table_year, pi, parties = model_parties(),
                          weight_scheme = "none", pseudo = 0.5, reservoir = TRUE) {
  box::use(dplyr[filter, mutate, summarise])
  cells <- switching |>
    filter(.data$year_cur == table_year, .data$weight_scheme == .env$weight_scheme) |>
    mutate(from = to_node(.data$prev_group, parties), to = to_node(.data$cur_group, parties))
  stopifnot("no such table / weight scheme" = nrow(cells) > 0)
  bad <- c(cells$from[!cells$prev_is_party], cells$to[!cells$cur_is_party])
  if (any(!is.na(bad) & bad != "Utan")) {
    stop("A non-party status mapped to a party node: ", paste(unique(c(
      cells$prev_group[!cells$prev_is_party], cells$cur_group[!cells$cur_is_party]
    )), collapse = ", "))
  }
  n_dropped <- sum(cells$n_w[is.na(cells$from) | is.na(cells$to)])
  cells <- cells |>
    filter(!is.na(.data$from), !is.na(.data$to), .data$from != .data$to) |>
    summarise(n = sum(.data$n_w), .by = c("from", "to"))
  nodes <- c(parties, "Utan")
  M <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  M[cbind(cells$from, cells$to)] <- cells$n
  W <- M + t(M)
  observed <- nodes[rowSums(W) > 0]
  imputed <- setdiff(parties, observed)
  if (length(imputed)) {
    obs_p <- intersect(parties, observed)
    pairs <- which(upper.tri(W[obs_p, obs_p]) & W[obs_p, obs_p] > 0, arr.ind = TRUE)
    s_party <- stats::median(W[obs_p, obs_p][pairs] / (pi[obs_p][pairs[, 1]] * pi[obs_p][pairs[, 2]]))
    s_res <- stats::median((W[obs_p, "Utan"] / pi[obs_p])[W[obs_p, "Utan"] > 0])
    for (a in imputed) {
      W[a, obs_p] <- W[obs_p, a] <- s_party * pi[a] * pi[obs_p]
      W[a, "Utan"] <- W["Utan", a] <- s_res * pi[a]
    }
    for (a in imputed) for (b in setdiff(imputed, a)) W[a, b] <- s_party * pi[a] * pi[b]
  }
  W <- W + pseudo * (1 - diag(length(nodes)))
  if (!reservoir) W <- W[parties, parties]
  list(W = W, imputed = imputed, n_used = sum(cells$n), n_dropped = n_dropped)
}

#' Share-space covariance target (up to scale): Laplacian of the party-party exchange
#' plus the reservoir renormalisation term. Rows sum to zero, like share increments.
#' @param gamma exponent on W: 1 = additive edge shocks, 2 = multiplicative (rate) shocks
#' @export
share_cov_target <- function(W, pi, gamma = 1) {
  parties <- names(pi)
  Wg <- W^gamma
  Wp <- Wg[parties, parties]
  diag(Wp) <- 0
  S <- diag(rowSums(Wp)) - Wp
  if ("Utan" %in% rownames(Wg)) {
    for (a in parties) {
      v <- -pi
      v[a] <- v[a] + 1
      S <- S + Wg[a, "Utan"] * tcrossprod(v)
    }
  }
  dimnames(S) <- list(parties, parties)
  S
}

#' Map a share-space target into the forecast model's beta coordinates
#' (clr of parties 2..P; party 1 is the reference carried as -sum).
#' @return list(Sigma0, Omega0, Omega0_inv, min_eigen)
#' @export
model_coords_target <- function(Sigma_q, pi) {
  P <- length(pi)
  A <- cbind(0, diag(P - 1)) %*% (diag(P) - 1 / P)
  Dinv <- diag(1 / pi)
  Sigma0 <- A %*% Dinv %*% Sigma_q %*% Dinv %*% t(A)
  Sigma0 <- (Sigma0 + t(Sigma0)) / 2
  dimnames(Sigma0) <- list(names(pi)[-1], names(pi)[-1])
  ev <- min(eigen(Sigma0, symmetric = TRUE, only.values = TRUE)$values)
  stopifnot("target is not positive definite" = ev > 0)
  Omega0 <- stats::cov2cor(Sigma0)
  list(Sigma0 = Sigma0, Omega0 = Omega0, Omega0_inv = solve(Omega0), min_eigen = ev)
}

#' Switching tables written by R/build_iskos_switching.R.
#' @export
read_switching <- function() {
  arrow::read_parquet(here::here("data", "iskos", "switching_long.parquet"))
}

#' Flow-informed prior target for forecasting `election`.
#'
#' Uses the latest ÍSKOS wave whose election precedes `election` (no leakage: that
#' wave's table is the previous cycle's switching), centred on that election's result.
#' @export
flow_prior_target <- function(election, switching = read_switching(), parties = model_parties(),
                              gamma = 1, pseudo = 0.5, reservoir = TRUE, weight_scheme = "none",
                              min_share = 0.02) {
  waves <- sort(unique(switching$year_cur[switching$weight_scheme == weight_scheme]))
  stopifnot("no ÍSKOS wave before that election with that weight scheme" = any(waves < election))
  table_year <- max(waves[waves < election])
  table_prev <- unique(switching$year_prev[switching$year_cur == table_year])
  stopifnot(length(table_prev) == 1)
  pi <- result_shares(table_year, parties, min_share)
  ex <- flow_exchange(switching, table_year, pi, parties, weight_scheme, pseudo, reservoir)
  Sigma_q <- share_cov_target(ex$W, pi, gamma)
  c(
    list(
      election = election, table = c(prev = table_prev, cur = table_year), pi = pi,
      W = ex$W, imputed = ex$imputed, n_used = ex$n_used, n_dropped = ex$n_dropped,
      Sigma_q = Sigma_q, R_q = stats::cov2cor(Sigma_q)
    ),
    model_coords_target(Sigma_q, pi)
  )
}
