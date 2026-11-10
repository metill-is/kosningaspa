box::use(
  R / harness / v2[v2_stan_data],
  R / harness / v2_reference[
    campaign_steps, chol_of_gram, dirmult_lpmf, dirmult_rng, fund_scales, lkj_corr_cholesky_lpdf, lkj_corr_cholesky_rng,
    M_e, permute_v2, V2_CORE_PARS, V2_MUTANTS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate, walk_rows,
    walk_scale
  ]
)

# The two real cells of the Stan lp tests, with stage1_stub()'s mu_f, so no stage-1 fit runs here.
real_cells <- function() v2_test_cells(cfg_v2(), stage1 = stage1_stub)[c("eve2024", "f180_2021")]

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
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
      lp <- v2_lp_ref(s$pars, d)
      expect_identical(d$P + 1L - c(p$data$sf_idx), c(d$sf_idx))
      # WHY: each lp (about -3e5 at a prior point on the real data) is a double rounded at
      # about 6e-11, so the identity holds to the rounding of the two lp values (4 eps |lp|)
      # and of their terms (1e-10). A tolerance relative to the Jacobian term (0.15 to 1 here)
      # sits below that floor, and passes or fails by how the last bit rounds.
      expect_lt(
        abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp),
        label = paste(name, seed)
      )
    }
  }
})
test_that("each mutant changes the density on both real cells", {
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    expect_setequal(v2_mutants(d), V2_MUTANTS$mutant)
    pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
    lp <- v2_lp_ref(pars, d)
    for (m in v2_mutants(d)) expect_gt(abs(v2_lp_ref(pars, d, m) - lp), 1e-6, label = paste(name, m))
  }
  expect_identical(names(V2_MUTANTS), c("mutant", "mistake", "min_closed"))
  expect_false(anyNA(V2_MUTANTS$mistake))
})
test_that("simulated data keep the design and replace the counts", {
  d <- d_2024eve()
  sim <- v2_simulate(d, seed = 1)$data
  design <- c(
    "P", "K", "H", "G", "n_epoch", "delta_days", "campaign", "grid_epoch", "exists", "w",
    "grid_nat", "house_nat", "n_rep_nat", "idx_nat", "grid_kd", "house_kd", "kd_kd", "grid_res", "N_open", "sf_idx"
  )
  expect_identical(sim[design], d[design])
  expect_identical(rowSums(sim$y_nat), rowSums(d$y_nat))
  expect_identical(apply(sim$y_res, 1:2, sum), apply(d$y_res, 1:2, sum))
  expect_true(all(sim$y_nat[d$y_nat == 0] == 0))
  expect_false(identical(sim$y_nat, d$y_nat))
})

test_that("the step leaving each closed election scales by the campaign multiplier, a step elsewhere off the campaign by sqrt(Delta) alone", {
  # §6.2 with R0's rule: a step is a campaign step if it lies inside a campaign or starts on
  # a closed election's date. Spec values written out here, not read from the module.
  tau <- 0.7
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    G <- d$G
    starts <- seq_len(G - 1L)
    leaving <- c(d$grid_res)
    expect_true(all(leaving < G), label = name)
    expect_true(all(c(d$campaign)[leaving] == 0L), label = paste(name, "no leaving step is inside a campaign already"))
    sc <- walk_scale(tau, d)
    dd <- c(d$delta_days)
    expect_equal(sc[leaving], sqrt(dd[leaving]) * (1 + tau), tolerance = 1e-15, label = name)
    camp <- which(c(d$campaign) == 1L)
    expect_gt(length(camp), 0)
    expect_equal(sc[camp], sqrt(dd[camp]) * (1 + tau), tolerance = 1e-15, label = name)
    off <- setdiff(starts, c(camp, leaving))
    expect_gt(length(off), 0)
    expect_equal(sc[off], sqrt(dd[off]), tolerance = 1e-15, label = name)
    expect_identical(which(campaign_steps(d) == 1L), sort(union(camp, leaving)), label = name)
    # The natural walk's steps carry those scales: at a point with the innovations of one step
    # set to 1, that step of theta moves by its scale times the factor's row sums.
    s <- v2_simulate(d, seed = 2, observe = FALSE)$pars
    s$tau_camp <- tau
    nat <- v2_natural(s, d)
    L <- chol_of_gram(walk_rows(s$sigma, matrix(s$L_Omega, d$P, d$P)))
    for (i in c(leaving[1], off[1])) {
      inc <- nat$theta[-1, i + 1L] - nat$theta[-1, i]
      expect_equal(inc, drop(L %*% s$z_walk[, i]) * sc[i], tolerance = 1e-10, label = paste(name, "step", i))
    }
    # The mutant without the rule changes the density.
    lp <- v2_lp_ref(s, d)
    expect_gt(abs(v2_lp_ref(s, d, "no_leaving_step") - lp), 1e-6, label = name)
  }
})

test_that("the bias is constant over epochs but at an entry, and every house's deviations are the same in each epoch", {
  # §6.4: mu_e changes only where a newcomer enters, and there only by the newcomer's entry
  # value and the re-centring, so the contrasts among the parties existing before it stay;
  # gamma_{e,h} - mu_e = C(sigma_gamma eps_h) in every epoch.
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    entry <- c(d$entry_epoch)
    s <- v2_simulate(d, seed = 3, observe = FALSE)$pars
    nat <- v2_natural(s, d)
    for (e in seq_len(d$n_epoch)[-1]) {
      before <- which(entry < e)
      if (!any(entry == e)) {
        expect_equal(nat$mu[e, ], nat$mu[e - 1, ], tolerance = 1e-14, label = paste(name, "epoch", e))
      } else {
        x <- nat$mu[e, before] - nat$mu[e - 1, before]
        expect_lt(max(abs(x - x[1])), 1e-14, label = paste(name, "entry epoch", e))
      }
    }
    for (h in seq_len(d$H)) {
      x <- s$sigma_gamma * s$house_raw[h, ]
      for (e in seq_len(d$n_epoch)) {
        expect_equal(nat$gamma[e, h, ] - nat$mu[e, ], x - mean(x), tolerance = 1e-14, label = paste(name, h, e))
      }
    }
    expect_equal(rowSums(nat$mu), rep(0, d$n_epoch), tolerance = 1e-14)
  }
})

test_that("delta_raw is one zeta, and every epoch's delta is M_e of s_delta0 times it with that epoch's weights", {
  # §6.7: delta_e = M_e(s_delta0 zeta), one zeta for every epoch, s_delta0 = exp(mu_sd + tau_sd r).
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    s <- v2_simulate(d, seed = 11, observe = FALSE)$pars
    expect_identical(dim(s$delta_raw), as.integer(c(1L, d$K, d$P)))
    nat <- v2_natural(s, d)
    s0 <- exp(s$mu_sd + s$tau_sd * s$s_delta0_raw)
    expect_equal(nat$s_delta0, s0, tolerance = 1e-15)
    X <- matrix(s$delta_raw[1, , ], d$K, d$P) %*% diag(s0)
    for (e in seq_len(d$n_epoch)) expect_equal(nat$delta[e, , ], M_e(X, d$w[e, ]), tolerance = 1e-12, label = paste(name, e))
  }
})

test_that("a newcomer enters at the existing parties' mean plus 0.2023 eta", {
  # §6.4: the mean runs over the parties existing before e_p (spec values written out here).
  checked <- 0L
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    entry <- c(d$entry_epoch)
    expect_gt(sum(entry > 0L), 0, label = name)
    s <- v2_simulate(d, seed = 5, observe = FALSE)$pars
    nat <- v2_natural(s, d)
    for (p in which(entry > 0L)) {
      ep <- entry[p]
      expect_equal(
        nat$mu[ep, p] - mean(nat$mu[ep, entry < ep]), 0.2023 * s$new_raw[match(p, which(entry > 0L))],
        tolerance = 1e-12, label = paste(name, "party", p, "entering at", ep)
      )
      checked <- checked + 1L
    }
    expect_gt(abs(v2_lp_ref(s, d, "newcomer_mean_over_all") - v2_lp_ref(s, d)), 1e-6, label = name)
  }
  expect_gt(checked, 0L)
})

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
  built <- v2_stan_data(info_2024eve(), utils::modifyList(cfg_v2(), list(order = "reversed")), stage1 = stage1_stub)$data
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
test_that("the reference refuses a missing or out-of-bounds parameter, an unreachable mutant and malformed closed-election dates", {
  d <- d_2024eve()
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  expect_named(pars, V2_CORE_PARS$name)
  expect_false(any(c("previous_w", "next_epoch_delta") %in% v2_mutants(utils::modifyList(d, list(n_closed = 1L)))))
  expect_error(v2_lp_ref(pars, d, "no_such_mutant"), "mutant")
  expect_error(v2_lp_ref(pars[-1], d), "theta1")
  expect_error(v2_lp_ref(utils::modifyList(pars, list(sigma = -pars$sigma)), d), "sigma")
  expect_error(v2_lp_ref(c(pars, list(tau_post = 1)), d), "outside the model: tau_post")
  expect_error(
    v2_lp_ref(pars, utils::modifyList(d, list(grid_res = array(c(c(d$grid_res)[-1], d$G))))),
    "data$grid_res must be n_closed grid dates before E",
    fixed = TRUE
  )
})

test_that("the fixed newcomer scale 0.2023 is the median of half-N(0, 0.3), in the reference and in Stan", {
  ref <- environment(v2_lp_ref)
  expect_identical(ref$S_NEW, 0.2023)
  expect_identical(ref$S_NEW, round(ref$S_NEW_SD * stats::qnorm(0.75), 4))
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  hit <- regmatches(stan, regexec("^ *real s_new_fixed = ([0-9.]+);", stan))
  hit <- hit[lengths(hit) > 0]
  expect_length(hit, 1)
  expect_identical(as.numeric(hit[[1]][2]), ref$S_NEW)
})

test_that("Stan declares the parameters V2_CORE_PARS names, with its types, bounds included, in its order", {
  # WHY: the Stan tests' free sizes and round trips cannot tell a bounded scalar from an
  # unbounded real; this compares the declarations as written.
  decl <- stan_block_decls(readLines(root("Stan", "forecast_v2.stan")), "parameters")
  expect_identical(decl$name, V2_CORE_PARS$name)
  expect_identical(decl$type, V2_CORE_PARS$type)
  d <- d_2024eve()
  expect_identical(names(v2_par_dims(d)), V2_CORE_PARS$name)
  expect_identical(as.integer(v2_par_dims(d)$delta_raw), as.integer(c(1L, d$K, d$P)))
})

# The fundamentals pseudo-observation (§6.5) written out at a prior point `pars`: x the log of
# the latent national share at E over S_f, centred; v each party's variance of a walk step,
# read off the walk's own factor (coordinates 2:P step by L_walk z and party 1 by minus their
# sum); V_w that variance over a stretch of weight_time - campaign_days days off the campaign
# and campaign_days on it.
fund_written_out <- function(pars, d) {
  nat <- v2_natural(pars, d)
  A <- which(d$exists[d$G, ] == 1L)
  share <- numeric(d$P)
  for (k in seq_len(d$K)) {
    eta <- nat$theta[A, d$G] + nat$delta[d$n_epoch, k, A]
    share[A] <- share[A] + d$w[d$n_epoch, k] * exp(eta) / sum(exp(eta))
  }
  sf <- c(d$sf_idx)
  x <- log(share[sf])
  L <- chol_of_gram(walk_rows(pars$sigma, matrix(pars$L_Omega, d$P, d$P)))
  v <- c(sum(colSums(L)^2), rowSums(L^2))[sf]
  V_w <- v * ((d$weight_time - d$campaign_days) + d$campaign_days * (1 + pars$tau_camp)^2)
  sum(stats::dnorm(c(d$mu_f), x - mean(x), sqrt(V_w * (1 - d$desired_weight) / d$desired_weight), log = TRUE))
}

test_that("the reference adds the fundamentals term of 6.5 written out", {
  ref <- environment(v2_lp_ref)
  for (name in names(real_cells())) {
    d <- real_cells()[[name]]$data
    moved <- d
    moved$mu_f <- array(c(d$mu_f) + seq_along(c(d$mu_f)) / 10)
    for (seed in 1:3) {
      pars <- v2_simulate(d, seed = seed, observe = FALSE)$pars
      p <- ref$core_pars(pars, d)
      expect_equal(ref$fund_lp(p, ref$natural(p, d), d), fund_written_out(pars, d), tolerance = 1e-10, label = paste(name, "the term"))
      # mu_f is read by that term alone, so the density moves with it as the term written out does.
      expect_equal(
        v2_lp_ref(pars, d) - v2_lp_ref(pars, moved), fund_written_out(pars, d) - fund_written_out(pars, moved),
        tolerance = 1e-8, label = paste(name, "the density's change")
      )
    }
  }
})

test_that("the reference refuses fundamentals data it cannot evaluate, naming the condition", {
  d <- d_2024eve()
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  expect_true(is.finite(v2_lp_ref(pars, d)))
  sf <- c(d$sf_idx)
  bad <- list(
    absent = list(exists = replace(d$exists, cbind(d$G, sf[2]), 0L)),
    mu_short = list(mu_f = array(c(d$mu_f)[-1])),
    mu_nonfinite = list(mu_f = array(replace(c(d$mu_f), 1, NaN))),
    weight_one = list(desired_weight = 1),
    time_short = list(weight_time = 30),
    repeated = list(sf_idx = array(replace(sf, 2, sf[1])))
  )
  says <- c(
    absent = "the fundamentals need every party of S_f (sf_idx) to exist at E",
    mu_short = "data$mu_f must be n_sf finite numbers",
    mu_nonfinite = "data$mu_f must be n_sf finite numbers",
    weight_one = "data$desired_weight must be one number in (0, 1)",
    time_short = "data$weight_time must be at least data$campaign_days, itself at least 1",
    repeated = "the fundamentals need at least two distinct parties of S_f (n_sf and sf_idx)"
  )
  for (n in names(bad)) {
    expect_error(v2_lp_ref(pars, utils::modifyList(d, bad[[n]])), says[[n]], fixed = TRUE, label = n)
  }
})

test_that("fund_scales(): tau_f gives the pseudo-observation the weight w against V_w, and each scale moves as 6.5 says", {
  d <- list(P = 4L, sf_idx = c(1L, 3L, 4L), weight_time = 180, campaign_days = 47L, desired_weight = 0.22)
  withr::with_seed(5, {
    for (i in 1:5) {
      sigma <- stats::runif(4, 0.005, 0.04)
      L <- lkj_corr_cholesky_rng(4, 2)
      tau <- stats::rexp(1)
      sc <- fund_scales(sigma, L, tau, d)
      C <- diag(4) - 1 / 4
      expect_equal(sc$v, diag(C %*% diag(sigma) %*% tcrossprod(L) %*% diag(sigma) %*% C)[c(1, 3, 4)], tolerance = 1e-12)
      expect_equal(sc$V_w, sc$v * (133 + 47 * (1 + tau)^2), tolerance = 1e-12)
      prec <- 1 / sc$tau_f^2
      expect_equal(prec / (prec + 1 / sc$V_w), rep(0.22, 3), tolerance = 1e-12)
      # At tau_camp = 0 the campaign days count as any other.
      expect_equal(fund_scales(sigma, L, 0, d)$V_w, sc$v * 180, tolerance = 1e-12)
    }
  })
  # As in Stan's model block, V_w counts the window's campaign days alone, with no step
  # leaving a closed election: the data's grid and grid_res are never read.
  real <- d_2024eve()
  expect_identical(
    fund_scales(rep(0.02, real$P), diag(real$P), 0.5, real),
    fund_scales(rep(0.02, real$P), diag(real$P), 0.5, utils::modifyList(real, list(grid_res = NULL, campaign = NULL)))
  )
})

test_that("v2_simulate() with observe redraws mu_f from N(x_c, tau_f), and leaves it alone without", {
  ref <- environment(v2_lp_ref)
  d <- d_2024eve()
  s <- v2_simulate(d, seed = 3)
  expect_identical(v2_simulate(d, seed = 3, observe = FALSE)$data$mu_f, d$mu_f)
  expect_identical(dim(s$data$mu_f), dim(d$mu_f))
  p <- ref$core_pars(s$pars, d)
  f <- ref$fund_normal(p, ref$natural(p, d), d)
  z <- (c(s$data$mu_f) - f$mean) / f$tau_f
  expect_true(all(is.finite(z)) && all(abs(z) < 6) && all(z != (c(d$mu_f) - f$mean) / f$tau_f))
})
