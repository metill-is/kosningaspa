box::use(
  R / harness / v2[v2_stan_data],
  R / harness / v2_reference[
    dirmult_lpmf, dirmult_rng, lkj_corr_cholesky_lpdf, lkj_corr_cholesky_rng, M_e, permute_v2,
    V2_CORE_PARS, V2_MUTANTS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate
  ]
)

test_that("the walk's covariance algebra holds", {
  set.seed(1)
  P <- 10
  C <- diag(P) - 1 / P
  J <- matrix(1, P, P)
  S <- random_sigma(P)
  Q <- C %*% S %*% C
  minors <- vapply(seq_len(P), \(k) determinant(Q[-k, -k])$modulus, numeric(1))
  expect_lt(diff(range(minors)), 1e-10)
  expect_gt(max(abs(S - S %*% J %*% S / sum(S) - Q)), 1e-6)
  iso <- 0.3^2 * diag(P)
  expect_equal(iso - iso %*% J %*% iso / sum(iso), C %*% iso %*% C)
})
test_that("M_e lands in both constraints and commutes with a kjordaemi permutation", {
  set.seed(2)
  K <- 6
  P <- 10
  w <- prop.table(runif(K))
  X <- matrix(rnorm(K * P), K)
  Y <- M_e(X, w)
  expect_equal(rowSums(Y), rep(0, K))
  expect_equal(drop(w %*% Y), rep(0, P))
  o <- sample(K)
  expect_equal(M_e(X[o, ], w[o]), Y[o, ])
})
test_that("the reference density is exactly permutation-invariant up to the L_Omega Jacobian", {
  d <- d_2024eve()
  for (seed in 1:3) {
    s <- v2_simulate(d, seed = seed, observe = FALSE)
    p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
    lp <- v2_lp_ref(s$pars, d)
    # WHY: each lp (about -3e5 at a prior point on the real data) is a double rounded at
    # about 6e-11, so the identity holds to the rounding of the two lp values (4 eps |lp|)
    # and of their terms (1e-10). A tolerance relative to the Jacobian term (0.15 to 1 here)
    # sits below that floor, and passes or fails by how the last bit rounds.
    expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
  }
})
test_that("each core mutant changes the density on real data", {
  d <- d_2024eve()
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  for (m in v2_mutants(d)) expect_gt(abs(v2_lp_ref(pars, d, m) - v2_lp_ref(pars, d)), 1e-6)
})
test_that("simulated data keep the design and replace the counts", {
  d <- d_2024eve()
  sim <- v2_simulate(d, seed = 1)$data
  design <- c(
    "P", "K", "H", "G", "n_epoch", "delta_days", "campaign", "grid_epoch", "exists", "w",
    "grid_nat", "house_nat", "n_rep_nat", "idx_nat", "grid_kd", "house_kd", "kd_kd", "grid_res", "N_open"
  )
  expect_identical(sim[design], d[design])
  expect_identical(rowSums(sim$y_nat), rowSums(d$y_nat))
  expect_identical(apply(sim$y_res, 1:2, sum), apply(d$y_res, 1:2, sum))
  expect_true(all(sim$y_nat[d$y_nat == 0] == 0))
  expect_false(identical(sim$y_nat, d$y_nat))
})

# The tests below go beyond the brief's: they pin what the lp comparison with Stan cannot see
# (the constants it differences away, the samplers recovery relies on, permute_v2's data).

test_that("permute_v2 maps the natural quantities, and its data are the reversed-order build", {
  d <- d_2024eve()
  rp <- rev(seq_len(d$P))
  rk <- rev(seq_len(d$K))
  s <- v2_simulate(d, seed = 4, observe = FALSE)
  p <- permute_v2(s$pars, d, rp, rk)
  a <- v2_natural(s$pars, d)
  b <- v2_natural(p$pars, p$data)
  expect_equal(b$theta, a$theta[rp, ], tolerance = 1e-10)
  expect_equal(b$pi_nat, a$pi_nat[, rp], tolerance = 1e-10)
  expect_equal(b$mu, a$mu[, rp], tolerance = 1e-10)
  expect_equal(b$gamma, a$gamma[, , rp], tolerance = 1e-10)
  expect_equal(b$delta, a$delta[, rk, rp], tolerance = 1e-10)
  expect_equal(b$log_phi, a$log_phi)
  expect_named(p$pars, V2_CORE_PARS$name)
  expect_identical(lengths(p$pars), vapply(v2_par_dims(p$data), \(x) as.integer(prod(x)), integer(1)))
  built <- v2_stan_data(info_2024eve(), utils::modifyList(cfg_m0(), list(order = "reversed")))$data
  expect_identical(p$data, built)
})
test_that("the Dirichlet-multinomial pmf sums to 1 and its sampler draws from it", {
  alpha <- c(0.7, 2.5, 1.2)
  ys <- as.matrix(expand.grid(0:4, 0:4, 0:4))
  ys <- ys[rowSums(ys) == 4, ]
  pmf <- exp(apply(ys, 1, dirmult_lpmf, alpha = alpha))
  expect_equal(sum(pmf), 1, tolerance = 1e-12)
  n <- 20000
  draws <- withr::with_seed(3, replicate(n, dirmult_rng(4, alpha)))
  key <- function(m, margin) apply(m, margin, paste, collapse = ",")
  freq <- as.numeric(table(factor(key(draws, 2), levels = key(ys, 1)))) / n
  expect_lt(max(abs(freq - pmf) / sqrt(pmf * (1 - pmf) / n)), 4.5)
  expect_equal(dirmult_lpmf(c(2, 0, 1), c(1, 0, 2)), dirmult_lpmf(c(2, 1), c(1, 2)))
  expect_identical(dirmult_lpmf(c(2, 1, 1), c(1, 0, 2)), -Inf)
})
test_that("the LKJ-Cholesky density integrates to 1, and its sampler has LKJ margins", {
  eta <- 10
  l2 <- function(r) matrix(c(1, r, 0, sqrt(1 - r^2)), 2)
  f2 <- function(r) vapply(r, \(x) exp(lkj_corr_cholesky_lpdf(l2(x), eta)), numeric(1))
  expect_equal(integrate(f2, -1, 1)$value, 1, tolerance = 1e-8)
  # P = 3 over the free elements (L21, L31, L32) = (a, b, c sqrt(1 - b^2)), by Gauss-Legendre.
  n <- 30
  jac <- matrix(0, n, n)
  jac[cbind(1:(n - 1), 2:n)] <- jac[cbind(2:n, 1:(n - 1))] <- seq_len(n - 1) / sqrt(4 * seq_len(n - 1)^2 - 1)
  e <- eigen(jac, symmetric = TRUE)
  x <- e$values
  wt <- 2 * e$vectors[1, ]^2
  l3 <- function(a, b, c) {
    L <- diag(3)
    L[2, 1:2] <- c(a, sqrt(1 - a^2))
    L[3, ] <- c(b, c * sqrt(1 - b^2), sqrt((1 - b^2) * (1 - c^2)))
    L
  }
  total <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      for (k in seq_len(n)) {
        total <- total + wt[i] * wt[j] * wt[k] * sqrt(1 - x[j]^2) * exp(lkj_corr_cholesky_lpdf(l3(x[i], x[j], x[k]), eta))
      }
    }
  }
  expect_equal(total, 1, tolerance = 1e-8)
  # Every correlation of LKJ(eta) in P dimensions is 2 Beta(a, a) - 1 with a = eta - 1 + P / 2.
  P <- 10
  m <- 5000
  a <- eta - 1 + P / 2
  R <- withr::with_seed(4, replicate(m, tcrossprod(lkj_corr_cholesky_rng(P, eta))))
  r <- t(apply(R, 3, \(o) o[lower.tri(o)]))
  v <- 1 / (2 * a + 1)
  m4 <- 3 / ((2 * a + 1) * (2 * a + 3))
  expect_lt(max(abs(colMeans(r)) / sqrt(v / m)), 5)
  expect_lt(max(abs(colMeans(r^2) - v) / sqrt((m4 - v^2) / m)), 5)
  expect_equal(unique(round(diag(apply(R, 1:2, mean)), 12)), 1)
})
test_that("the reference refuses another form, an unbuilt switch, a missing parameter and an unreachable mutant", {
  d <- d_2024eve()
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  expect_named(pars, V2_CORE_PARS$name)
  expect_setequal(v2_mutants(d), V2_MUTANTS$mutant[is.na(V2_MUTANTS$switch)])
  expect_false(any(c("previous_w", "next_epoch_delta") %in% v2_mutants(utils::modifyList(d, list(n_closed = 1L)))))
  expect_error(v2_lp_ref(pars, utils::modifyList(d, list(walk_form = 1L))), "reference forms")
  expect_error(v2_lp_ref(pars, utils::modifyList(d, list(shock = 1L))), "shock")
  expect_error(v2_simulate(utils::modifyList(d, list(delta_form = 1L)), seed = 1), "reference forms")
  expect_error(v2_lp_ref(pars, d, "no_such_mutant"), "mutant")
  expect_error(v2_lp_ref(pars[-1], d), "theta1")
  expect_error(v2_lp_ref(utils::modifyList(pars, list(sigma = -pars$sigma)), d), "sigma")
})

# The epoch bias (step v2s-m1, §6.4, switch epoch_bias), on the cells of the Stan lp tests.

test_that("the epoch-bias reference: each of its mutants changes the density, and the identity under permutation holds", {
  cells <- v2_test_cells(cfg_m1())
  for (name in c("eve2024", "f180_2021")) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$epoch_bias), 1L)
    expect_true(all(c("house_walk_from_first_poll", "newcomer_mean_of_mu1") %in% v2_mutants(d)))
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      lp <- v2_lp_ref(s$pars, d)
      for (m in v2_mutants(d)) expect_gt(abs(v2_lp_ref(s$pars, d, m) - lp), 1e-6, label = paste(name, m))
      p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
      a <- v2_natural(s$pars, d)
      b <- v2_natural(p$pars, p$data)
      expect_equal(b$mu, a$mu[, rev(seq_len(d$P))], tolerance = 1e-12)
      expect_equal(b$gamma, a$gamma[, , rev(seq_len(d$P))], tolerance = 1e-12)
    }
  }
  # WHY: the reversed order puts Annað first, so its index must move with the parties.
  built <- v2_stan_data(info_2024eve(), utils::modifyList(cfg_m1(), list(order = "reversed")))$data
  expect_identical(built$annad_idx, 1L)
  d <- d_2024eve()
  expect_identical(permute_v2(v2_simulate(d, 1, FALSE)$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))$data$annad_idx, 1L)
})

test_that("the epoch-bias reference walks the industry bias with Annad's own scale, and each house from its first epoch", {
  d <- v2_test_cells(cfg_m1())$eve2024$data
  s <- v2_simulate(d, seed = 3, observe = FALSE)
  pars <- s$pars
  a <- v2_natural(pars, d)
  # Raising Annað's scale moves the industry bias's steps only through Annað's element.
  big <- utils::modifyList(pars, list(tau_mu_A = pars$tau_mu_A * 3))
  b <- v2_natural(big, d)
  steps <- function(nat) nat$mu[2, ] - nat$mu[1, ]
  annad <- d$annad_idx
  stay <- which(c(d$entry_epoch) == 0L & seq_len(d$P) != annad)
  expect_gt(abs(diff(steps(b)[c(annad, stay[1])]) - diff(steps(a)[c(annad, stay[1])])), 1e-6)
  expect_equal(diff(steps(b)[stay]), diff(steps(a)[stay]), tolerance = 1e-12)
  # A house's steps before or at its first epoch leave every gamma alone; a later one moves it.
  h <- which(d$house_first_epoch > 1L)[1]
  f <- d$house_first_epoch[h]
  early <- pars
  early$house_step_raw[h, , f - 1L] <- early$house_step_raw[h, , f - 1L] + 1
  expect_equal(v2_natural(early, d)$gamma, a$gamma, tolerance = 0)
  late <- pars
  late$house_step_raw[h, , f] <- late$house_step_raw[h, , f] + c(1, rep(0, d$P - 1))
  expect_gt(max(abs(v2_natural(late, d)$gamma[f + 1L, h, ] - a$gamma[f + 1L, h, ])), 1e-6)
  expect_equal(v2_natural(late, d)$gamma[seq_len(f), , ], a$gamma[seq_len(f), , ], tolerance = 0)
})

test_that("with every epoch scale near 0 the epoch-bias reference gives the core's biases", {
  d <- v2_test_cells(cfg_m1())$eve2024$data
  s <- v2_simulate(d, seed = 4, observe = FALSE)$pars
  tiny <- s
  for (n in c("tau_mu", "tau_mu_A", "tau_house")) tiny[[n]][] <- 1e-14
  d0 <- d
  d0$epoch_bias <- 0L
  core <- s
  core$mu_step_raw <- matrix(0, d$P, 0)
  core$house_step_raw <- array(0, c(d$H, d$P, 0))
  for (n in c("tau_mu", "tau_mu_A", "tau_house")) core[[n]] <- array(numeric(0), 0)
  a <- v2_natural(tiny, d)
  b <- v2_natural(core, d0)
  expect_equal(a$mu, b$mu, tolerance = 1e-12)
  expect_equal(a$gamma, b$gamma, tolerance = 1e-12)
})

test_that("each house's first epoch is that of its first national poll on the lp tests' cells", {
  # WHY: house_first_epoch also counts kjördæmi polls, but a house's deviation reaches the
  # likelihood only through a national poll, so the house walk's start is right only where
  # the two agree.
  for (cell in v2_test_cells(cfg_m1())) {
    d <- cell$data
    first_nat <- vapply(seq_len(d$H), \(h) min(c(d$grid_epoch)[c(d$grid_nat)[c(d$house_nat) == h]]), numeric(1))
    expect_identical(as.integer(first_nat), as.integer(c(d$house_first_epoch)))
  }
})
