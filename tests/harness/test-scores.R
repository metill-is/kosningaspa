box::use(R / harness / scores[NAMED8, crps_draws, wide_draws, party_scores, alr_dss, energy_score])
box::use(R / harness / scores[national_scores, mae_se, batch_of, batch_se, kd_to_nat])
box::use(R / harness / scores[kd_scores, seat_draws, seat_scores, majority_prob])
box::use(R / election_utils[calculate_seats, seats_layout_2021])

ZERO_SHARE <- 0.5 / 196292

# Independent gamma draws closed to shares: a Dirichlet-like draw matrix.
dirichlet_like <- function(n, alpha) {
  g <- vapply(alpha, \(a) stats::rgamma(n, a), numeric(n))
  g / rowSums(g)
}

# A long draw table: chains × iterations × parties, with `.draw` numbered in chain order.
long_draws <- function(chains, iterations, parties) {
  d <- expand.grid(flokkur = parties, .iteration = seq_len(iterations), .chain = seq_len(chains), stringsAsFactors = FALSE)
  d$.draw <- (d$.chain - 1L) * iterations + d$.iteration
  d$share <- stats::runif(nrow(d), 0.05, 0.5)
  tibble::as_tibble(d[c(".chain", ".iteration", ".draw", "flokkur", "share")])
}

test_that("NAMED8 is the poll parties without Annað and Sósíalistaflokkurinn", {
  expect_identical(NAMED8, setdiff(POLL_PARTIES, c("Annað", "Sósíalistaflokkurinn")))
  expect_length(NAMED8, 8)
})

test_that("crps_draws is the V-statistic E|X - y| - E|X - X'| / 2", {
  expect_equal(crps_draws(c(0, 1), 0), 0.25)
  expect_equal(crps_draws(rep(3, 5), 5), 2)
  x <- c(0.1, 0.4, 0.35, 0.2, 0.9)
  expect_equal(crps_draws(x, 0.3), mean(abs(x - 0.3)) - 0.5 * mean(abs(outer(x, x, "-"))), tolerance = 1e-14)
})

test_that("wide_draws puts draws in chain and iteration order and parties in first-appearance order", {
  set.seed(11)
  long <- long_draws(2, 3, c("B", "A", "C"))
  X <- wide_draws(long[sample(nrow(long)), ], "share")
  expect_identical(dim(X), c(6L, 3L))
  expect_identical(attr(X, "chain"), rep(1:2, each = 3))
  expect_identical(attr(X, "iteration"), rep(1:3, 2))
  shuffled <- long[sample(nrow(long)), ]
  expect_identical(colnames(wide_draws(shuffled, "share")), unique(shuffled$flokkur))
  expect_equal(unname(X[4, "A"]), long$share[long$.chain == 2 & long$.iteration == 1 & long$flokkur == "A"])
})

test_that("wide_draws refuses a draw that lacks a party and a repeated (.chain, .iteration) pair", {
  set.seed(12)
  long <- long_draws(2, 3, c("A", "B"))
  expect_error(wide_draws(long[-4, ], "share"), "lack a party")
  twice <- long
  twice$.iteration[twice$.draw == 2] <- 1L
  expect_error(wide_draws(twice, "share"), "pair repeats")
  expect_error(wide_draws(rbind(long, long[1, ]), "share"), "pair repeats")
})

test_that("party_scores reorders the truth to the columns and scores each party", {
  X <- cbind(A = c(0.1, 0.2, 0.2, 0.4, 0.6), B = c(0.5, 0.4, 0.3, 0.3, 0.2))
  ps <- party_scores(X, c(B = 0.45, A = 0.2))
  expect_identical(ps$flokkur, c("A", "B"))
  expect_equal(ps$mean, c(0.3, 0.34))
  expect_equal(ps$sd, c(sd(X[, "A"]), sd(X[, "B"])))
  expect_equal(ps$median, c(0.2, 0.3))
  expect_equal(ps$q05, c(0.12, 0.22))
  expect_equal(ps$q25, c(0.2, 0.3))
  expect_equal(ps$q75, c(0.4, 0.4))
  expect_equal(ps$q95, c(0.56, 0.48))
  expect_equal(ps$crps, c(crps_draws(X[, "A"], 0.2), crps_draws(X[, "B"], 0.45)))
  expect_equal(ps$dss, log((100 * ps$sd)^2) + ((c(0.2, 0.45) - ps$mean) / ps$sd)^2)
  # WHY: A has one draw below 0.2 and two ties at it; B has four draws below 0.45.
  expect_equal(ps$pit, c(0.4, 0.8))
  expect_identical(ps$in50, c(TRUE, FALSE))
  expect_identical(ps$in90, c(TRUE, TRUE))
  expect_error(party_scores(X, c(A = 0.2, C = 0.45)), "truth and draws name different parties")
})

test_that("d2 and mdss do not depend on which party is the alr reference", {
  set.seed(1)
  X <- dirichlet_like(2000, c(A = 20, B = 10, C = 5, D = 2))
  y <- c(A = 0.45, B = 0.3, C = 0.17, D = 0.08)
  ref <- alr_dss(X, y, ZERO_SHARE)
  expect_named(ref, c("mdss", "d2"))
  for (perm in list(c(2, 1, 3, 4), c(4, 3, 2, 1), c(3, 4, 1, 2))) {
    expect_equal(alr_dss(X[, perm], y, ZERO_SHARE), ref, tolerance = 1e-8)
  }
})

test_that("d2 is 0 at the closed exponential of the alr means, where mdss is log det S", {
  set.seed(2)
  X <- dirichlet_like(2000, c(A = 20, B = 10, C = 5, D = 2))
  Z <- log(X[, -1] / X[, 1])
  y <- c(1, exp(colMeans(Z)))
  y <- stats::setNames(y / sum(y), colnames(X))
  s <- alr_dss(X, y, ZERO_SHARE)
  expect_lt(abs(s[["d2"]]), 1e-10)
  expect_equal(s[["mdss"]], as.numeric(determinant(cov(Z), logarithm = TRUE)$modulus), tolerance = 1e-10)
})

test_that("a zero share is replaced and counted, and mdss stays finite", {
  set.seed(3)
  X <- dirichlet_like(400, c(A = 20, B = 10, C = 5))
  X[5, 2] <- 0
  s <- national_scores(X, c(A = 0.55, B = 0.3, C = 0.15), ZERO_SHARE)
  expect_true(is.finite(s$mdss))
  expect_equal(s$zero_cells_replaced, 1)
})

test_that("IS90 is the interval width inside it, plus 20 times the miss below it", {
  # WHY: B = 1 - A mirrors A's interval, so both parties score the same and the mean is A's.
  a <- (1:100) / 1000
  X <- cbind(A = a, B = 1 - a)
  lo <- unname(stats::quantile(a, 0.05, type = 7))
  hi <- unname(stats::quantile(a, 0.95, type = 7))
  inside <- national_scores(X, c(A = 0.05, B = 0.95), ZERO_SHARE)
  expect_equal(inside$is90, 100 * (hi - lo))
  below <- national_scores(X, c(A = 0.002, B = 0.998), ZERO_SHARE)
  expect_equal(below$is90, 100 * (hi - lo + 20 * (lo - 0.002)))
  expect_equal(below$w90, 100 * (hi - lo))
  expect_equal(below$cov90, 0)
})

test_that("mae8 leaves out Annað and Sósíalistaflokkurinn, and mae_all does not", {
  set.seed(4)
  y <- stats::setNames(c(0.20, 0.10, 0.18, 0.06, 0.05, 0.13, 0.12, 0.08, 0.04, 0.04), POLL_PARTIES)
  # WHY: a shuffled symmetric grid around 0 puts each column's median exactly on y.
  v <- (-50:50) / 5000
  X <- vapply(POLL_PARTIES, \(p) y[[p]] + sample(v), numeric(101))
  X[, "Annað"] <- X[, "Annað"] + 0.10
  s <- national_scores(X, y, ZERO_SHARE)
  expect_identical(s$n_parties, 10L)
  expect_equal(s$mae8, 0)
  expect_equal(s$mae_all, 100 * 0.10 / 10)
  X[, "Sósíalistaflokkurinn"] <- X[, "Sósíalistaflokkurinn"] + 0.05
  s <- national_scores(X, y, ZERO_SHARE)
  expect_equal(s$mae8, 0)
  expect_equal(s$mae_all, 100 * 0.15 / 10)
})

test_that("the energy score pairs each draw with the draw half the sample away", {
  X <- cbind(A = c(1, 2, 4, 7), B = c(0, 3, 1, 5))
  # WHY: distances to y are sqrt(5), 1, sqrt(5), sqrt(34); pairs (1,3), (2,4), (3,1), (4,2) are sqrt(10), sqrt(29) twice.
  by_hand <- (2 * sqrt(5) + 1 + sqrt(34)) / 4 - 0.5 * (2 * sqrt(10) + 2 * sqrt(29)) / 4
  expect_equal(energy_score(X, c(A = 2, B = 2)), by_hand)
  expect_equal(energy_score(X[, 2:1], c(A = 2, B = 2)), by_hand)
})

test_that("national_scores returns its columns and refuses a truth naming other parties", {
  set.seed(7)
  X <- dirichlet_like(200, c(A = 20, B = 10, C = 5))
  s <- national_scores(X, c(C = 0.15, A = 0.55, B = 0.3), ZERO_SHARE)
  expect_named(s, c(
    "n_parties", "crps10", "is90", "dss10", "mdss", "d2", "es", "mae8", "mae_all", "w90",
    "cov50", "cov90", "zero_cells_replaced"
  ))
  expect_identical(nrow(s), 1L)
  y <- c(A = 0.55, B = 0.3, C = 0.15)
  ps <- party_scores(X, y)
  expect_equal(s$crps10, 100 * mean(ps$crps))
  expect_equal(s$dss10, mean(ps$dss))
  expect_equal(s$es, energy_score(100 * X, 100 * y))
  expect_equal(c(s$mdss, s$d2), unname(alr_dss(X, y, ZERO_SHARE)))
  expect_error(national_scores(X, c(A = 0.5, B = 0.3, D = 0.2), ZERO_SHARE), "truth and draws name different parties")
  expect_error(national_scores(X, c(0.55, 0.3, 0.15), ZERO_SHARE), "truth and draws name different parties")
})

test_that("mae_se combines the per-party MCSEs of the median, chains as columns", {
  set.seed(6)
  X <- wide_draws(long_draws(4, 50, c("A", "B", "C")), "share")
  y <- c(A = 0.2, B = 0.3, C = 0.25)
  mcse <- vapply(c("A", "B"), \(p) posterior::mcse_median(matrix(X[, p], ncol = 4)), 0)
  expect_equal(mae_se(X, y, c("A", "B")), 100 / 2 * sqrt(sum(mcse^2)))
  expect_error(mae_se(X[1:100, ], y, "A"), "chain")
})

test_that("batch_of numbers per_chain blocks within each chain", {
  expect_identical(batch_of(rep(1:4, each = 8), rep(1:8, 4)), rep(1:16, each = 2))
})

test_that("batch_se is the sd of the block values over the root of the block count", {
  set.seed(5)
  X <- matrix(stats::rnorm(64 * 2), 64, 2)
  block <- batch_of(rep(1:4, each = 16), rep(1:16, 4))
  means <- vapply(split(X[, 1], block), mean, 0)
  expect_equal(batch_se(\(x) c(m1 = mean(x[, 1])), X, block), c(m1 = sd(means) / 4))
})

test_that("kd_to_nat sums votes over kjördæmi before taking shares", {
  cp <- tibble::tibble(
    .chain = 1L, .iteration = rep(1:2, each = 4), .draw = rep(1:2, each = 4),
    kjordaemi = rep(rep(c("Reykjavík Norður", "Suður"), each = 2), 2),
    flokkur = rep(c("A", "B"), 4),
    votes = c(10L, 30L, 30L, 30L, 1L, 1L, 1L, 1L)
  )
  nat <- kd_to_nat(cp)
  expect_named(nat, c(".chain", ".iteration", ".draw", "flokkur", "votes", "share"))
  expect_identical(nat$votes, c(40L, 60L, 2L, 2L))
  expect_equal(nat$share, c(40 / 100, 60 / 100, 0.5, 0.5))
})

# Long seat draws from a draw × party matrix, rows in chain order with equal chains.
long_seats <- function(M, chains = 1L) {
  n <- nrow(M)
  tibble::tibble(
    .chain = rep(rep(seq_len(chains), each = n %/% chains), ncol(M)),
    .iteration = rep(rep(seq_len(n %/% chains), chains), ncol(M)),
    .draw = rep(seq_len(n), ncol(M)),
    flokkur = rep(colnames(M), each = n),
    seats = as.integer(M)
  )
}

kd_base <- tibble::tribble(
  ~kjordaemi, ~flokkur, ~votes,
  "Suður", "Samfylkingin", 300L,
  "Suður", "Viðreisn", 200L,
  "Suður", "Annað", 100L,
  "Norðvestur", "Samfylkingin", 150L,
  "Norðvestur", "Viðreisn", 250L,
  "Norðvestur", "Annað", 50L
)

test_that("kd_scores scores draws equal to the truth in every cell as exact", {
  # WHY: draw d scales every vote by d, so the shares are equal only within a draw and kjördæmi.
  eday <- tidyr::expand_grid(.draw = 1:4, kd_base) |>
    dplyr::mutate(.chain = 1L, .iteration = .draw, votes = votes * .draw)
  truth <- kd_base |>
    dplyr::mutate(share = votes / sum(votes), .by = kjordaemi) |>
    dplyr::select(kjordaemi, flokkur, share)
  s <- kd_scores(eday, truth)
  expect_named(s, c("kd_cells", "kd_crps", "kd_mae_named", "kd_w90", "kd_cov90"))
  expect_identical(nrow(s), 1L)
  expect_equal(s$kd_cells, 6)
  expect_equal(s$kd_crps, 0)
  expect_equal(s$kd_w90, 0)
  expect_equal(s$kd_mae_named, 0)
  expect_equal(s$kd_cov90, s$kd_cells)
})

test_that("kd_scores takes shares within draw and kjördæmi, and the MAE over NAMED8 cells only", {
  set.seed(21)
  eday <- tidyr::expand_grid(.chain = 1:2, .iteration = 1:10, kd_base[c("kjordaemi", "flokkur")]) |>
    dplyr::mutate(.draw = (.chain - 1L) * 10L + .iteration, votes = stats::rpois(dplyr::n(), 500))
  truth <- dplyr::mutate(kd_base[c("kjordaemi", "flokkur")], share = c(0.5, 0.3, 0.2, 0.4, 0.35, 0.25))
  s <- kd_scores(eday[sample(nrow(eday)), ], truth[6:1, ])
  share <- eday$votes / ave(eday$votes, eday$.draw, eday$kjordaemi, FUN = sum)
  by_cell <- function(f) {
    unname(mapply(\(k, p, y) f(share[eday$kjordaemi == k & eday$flokkur == p], y), truth$kjordaemi, truth$flokkur, truth$share))
  }
  q05 <- by_cell(\(x, y) stats::quantile(x, 0.05, type = 7, names = FALSE))
  q95 <- by_cell(\(x, y) stats::quantile(x, 0.95, type = 7, names = FALSE))
  med <- by_cell(\(x, y) stats::median(x))
  named <- truth$flokkur != "Annað"
  expect_equal(s$kd_crps, 100 * mean(by_cell(crps_draws)))
  expect_equal(s$kd_mae_named, 100 * mean(abs(med - truth$share)[named]))
  expect_false(isTRUE(all.equal(s$kd_mae_named, 100 * mean(abs(med - truth$share)))))
  expect_equal(s$kd_w90, 100 * mean(q95 - q05))
  expect_equal(s$kd_cov90, sum(truth$share >= q05 & truth$share <= q95))
})

test_that("kd_scores refuses a truth that lacks a cell of the draws or adds one", {
  eday <- dplyr::mutate(kd_base, .chain = 1L, .iteration = 1L, .draw = 1L)
  truth <- dplyr::mutate(kd_base[c("kjordaemi", "flokkur")], share = 0.3)
  expect_error(kd_scores(eday, truth[-2, ]), "cells without a match")
  extra <- rbind(truth, tibble::tibble(kjordaemi = "Suður", flokkur = "Píratar", share = 0.1))
  expect_error(kd_scores(eday, extra), "cells without a match")
  expect_error(kd_scores(eday, rbind(truth, truth[1, ])), "repeats a cell")
})

test_that("kd_scores refuses draws that repeat a (.draw, kjordaemi, flokkur) cell", {
  eday <- tidyr::expand_grid(.chain = 1:2, .iteration = 1:3, kd_base) |>
    dplyr::mutate(.draw = (.chain - 1L) * 3L + .iteration)
  truth <- kd_base |>
    dplyr::mutate(share = votes / sum(votes), .by = kjordaemi) |>
    dplyr::select(kjordaemi, flokkur, share)
  expect_equal(kd_scores(eday, truth)$kd_crps, 0)
  per_chain <- dplyr::mutate(eday, .draw = .iteration)
  expect_error(kd_scores(per_chain, truth), "repeats a (.draw, kjordaemi, flokkur) cell", fixed = TRUE)
  expect_error(kd_scores(rbind(eday, eday[1, ]), truth), "repeats a (.draw, kjordaemi, flokkur) cell", fixed = TRUE)
})

test_that("seat_draws sums calculate_seats() over kjördæmi, and Annað wins no seat by default", {
  d <- draw_with(c(Annað = 0.30, D = 0.30, S = 0.25, B = 0.15))
  one <- dplyr::mutate(d, .chain = 1L, .iteration = 1L, .draw = 1L)
  per_party <- function(s) dplyr::summarise(s, seats = sum(seats), .by = flokkur)
  s <- seat_draws(one, seats_layout_2021())
  expect_named(s, c(".chain", ".iteration", ".draw", "flokkur", "seats"))
  ref <- per_party(calculate_seats(d, seats_layout_2021(), ineligible = "Annað"))
  expect_setequal(s$flokkur, ref$flokkur)
  expect_equal(s$seats[match(ref$flokkur, s$flokkur)], ref$seats)
  expect_equal(s$seats[s$flokkur == "Annað"], 0)
  expect_equal(sum(s$seats), 63)
  every_list <- seat_draws(one, seats_layout_2021(), ineligible = character(0))
  ref0 <- per_party(calculate_seats(d, seats_layout_2021()))
  expect_equal(every_list$seats[match(ref0$flokkur, every_list$flokkur)], ref0$seats)
  expect_gt(every_list$seats[every_list$flokkur == "Annað"], 0)
})

test_that("seat_draws allocates each draw on its own and returns draws in chain order", {
  set.seed(31)
  first <- draw_with(c(Annað = 0.05, D = 0.40, S = 0.35, B = 0.20))
  second <- draw_with(c(Annað = 0.05, D = 0.20, S = 0.25, B = 0.50))
  both <- rbind(
    dplyr::mutate(second, .chain = 2L, .iteration = 1L, .draw = 2L),
    dplyr::mutate(first, .chain = 1L, .iteration = 1L, .draw = 1L)
  )
  s <- seat_draws(both[sample(nrow(both)), ], seats_layout_2021())
  expect_identical(unique(s$.draw), 1:2)
  for (k in 1:2) {
    ref <- calculate_seats(list(first, second)[[k]], seats_layout_2021(), ineligible = "Annað") |>
      dplyr::summarise(seats = sum(seats), .by = flokkur)
    got <- s[s$.draw == k, ]
    expect_equal(got$seats[match(ref$flokkur, got$flokkur)], ref$seats)
    expect_identical(unique(got$.chain), k)
  }
})

test_that("seat_scores on draws equal to the result is exact, and the Brier score follows the outcome", {
  actual <- c(A = 20L, B = 13L, C = 30L)
  seats <- long_seats(matrix(actual, 16, 3, byrow = TRUE, dimnames = list(NULL, names(actual))), chains = 4L)
  sc <- seat_scores(seats, actual, coalition = c("A", "B"))
  expect_named(sc, c("summary", "per_party"))
  expect_named(sc$summary, c(
    "seat_crps", "seat_abs_err", "seat_w90", "seat_cov90", "p_exact", "p_majority",
    "majority_outcome", "brier", "p_formed"
  ))
  expect_named(sc$per_party, c("flokkur", "crps", "median", "q05", "q95", "p_exact"))
  expect_equal(sc$summary$seat_crps, 0)
  expect_equal(sc$summary$p_exact, 1)
  expect_equal(sc$summary$p_majority, 1)
  expect_true(sc$summary$majority_outcome)
  expect_equal(sc$summary$brier, 0)
  expect_identical(sc$summary$p_formed, NA_real_)
  lost <- seat_scores(seats, c(A = 18L, B = 12L, C = 33L), coalition = c("A", "B"))
  expect_false(lost$summary$majority_outcome)
  expect_equal(lost$summary$brier, 1)
  expect_equal(seat_scores(seats, actual, c("A", "B"), formed = "C")$summary$p_formed, 0)
})

test_that("seat_scores takes type-1 quantiles, per-party and joint exactness, and the coalition's sum", {
  # WHY: A runs 1..20 and B mirrors it, so C holds 42 in every draw and only draw 5 is exact.
  a <- 1:20
  M <- cbind(A = a, B = 21L - a, C = rep(42L, 20))
  actual <- c(C = 42L, A = 5L, B = 16L)
  sc <- seat_scores(long_seats(M, chains = 2L), actual, coalition = c("A", "C"), formed = c("A", "B", "C"), majority = 52L)
  pp <- sc$per_party
  expect_identical(pp$flokkur, c("A", "B", "C"))
  expect_equal(pp$q05, c(1, 1, 42))
  expect_equal(pp$q95, c(19, 19, 42))
  expect_equal(pp$median, c(10.5, 10.5, 42))
  expect_equal(pp$p_exact, c(0.05, 0.05, 1))
  expect_equal(pp$crps, c(crps_draws(a, 5), crps_draws(21 - a, 16), 0))
  expect_equal(sc$summary$seat_crps, sum(pp$crps))
  expect_equal(sc$summary$seat_abs_err, 11)
  expect_equal(sc$summary$seat_w90, 12)
  expect_equal(sc$summary$seat_cov90, 3)
  expect_equal(sc$summary$p_exact, 0.05)
  # WHY: A + C >= 52 holds for A >= 10 (11 of 20 draws); the result's A + C is 47.
  expect_equal(sc$summary$p_majority, 0.55)
  expect_false(sc$summary$majority_outcome)
  expect_equal(sc$summary$brier, 0.55^2)
  expect_equal(sc$summary$p_formed, 1)
  expect_error(seat_scores(long_seats(M), c(A = 5L, B = 16L, D = 42L), "A"), "truth and draws name different parties")
})

test_that("majority_prob is the share of draws in which the parties hold a majority", {
  held <- c(31L, 32L, 33L, 20L)
  M <- cbind(A = c(20L, 16L, 30L, 10L), C = 63L - held, B = held - c(20L, 16L, 30L, 10L))
  seats <- long_seats(M)
  expect_equal(majority_prob(seats, c("A", "B")), 0.5)
  expect_equal(majority_prob(seats, c("A", "B"), majority = 33L), 0.25)
  expect_error(majority_prob(seats, c("A", "D")), "parties absent from the draws: D")
})
