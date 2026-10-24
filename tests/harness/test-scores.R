box::use(R / harness / scores[NAMED8, crps_draws, wide_draws, party_scores, alr_dss, energy_score])
box::use(R / harness / scores[national_scores, mae_se, batch_of, batch_se, kd_to_nat])

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
