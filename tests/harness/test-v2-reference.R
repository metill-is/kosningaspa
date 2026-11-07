box::use(
  R / harness / v2[v2_stan_data],
  R / harness / v2_reference[
    chol_of_gram, dirmult_lpmf, dirmult_rng, fund_scales, lkj_corr_cholesky_lpdf, lkj_corr_cholesky_rng, M_e, permute_v2,
    V2_CORE_PARS, V2_MUTANTS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate, walk_rows, walk_scale
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
test_that("the reference refuses another form, fundamentals without S_f, a missing parameter, an unreachable mutant and a delta walk's malformed gaps", {
  d <- d_2024eve()
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  expect_named(pars, V2_CORE_PARS$name)
  expect_setequal(v2_mutants(d), V2_MUTANTS$mutant[is.na(V2_MUTANTS$switch)])
  expect_false(any(c("previous_w", "next_epoch_delta") %in% v2_mutants(utils::modifyList(d, list(n_closed = 1L)))))
  expect_error(v2_lp_ref(pars, utils::modifyList(d, list(walk_form = 1L))), "reference forms")
  # WHY: the m0 build has fundamentals = 0, so it carries no party of S_f to centre over.
  expect_error(
    v2_lp_ref(pars, utils::modifyList(d, list(fundamentals = 1L))),
    "data$fundamentals = 1 needs at least two distinct parties of S_f (n_sf and sf_idx)",
    fixed = TRUE
  )
  expect_error(v2_simulate(utils::modifyList(d, list(delta_form = 1L)), seed = 1), "reference forms")
  expect_error(v2_lp_ref(pars, d, "no_such_mutant"), "mutant")
  expect_error(v2_lp_ref(pars[-1], d), "theta1")
  expect_error(v2_lp_ref(utils::modifyList(pars, list(sigma = -pars$sigma)), d), "sigma")
  expect_error(v2_lp_ref(pars, utils::modifyList(d, list(epoch_bias = c(0L, 1L)))), "neither 0 nor 1")
  d4 <- v2_test_cells(cfg_m4())$eve2024$data
  expect_identical(as.integer(d4$delta_walk), 1L)
  pars4 <- v2_simulate(d4, seed = 1, observe = FALSE)$pars
  expect_error(
    v2_lp_ref(pars4, utils::modifyList(d4, list(delta_years = d4$delta_years[-1]))),
    "data$delta_years must be n_epoch non-negative numbers",
    fixed = TRUE
  )
  expect_error(
    v2_lp_ref(pars4, utils::modifyList(d4, list(delta_years_bar = 0))),
    "data$delta_years_bar must be one positive number",
    fixed = TRUE
  )
})

# The epoch bias (step v2s-m1, §6.4, switch epoch_bias), on the cells of the Stan lp tests.

test_that("the epoch-bias reference: each of its mutants changes the density, and the identity under permutation holds", {
  cells <- v2_test_cells(cfg_m1())
  for (name in c("eve2024", "f180_2021")) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$epoch_bias), 1L)
    expect_true(all(c(
      "house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart", "no_house_restart", "house_restart_absolute"
    ) %in% v2_mutants(d)))
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

test_that("with every epoch scale near 0 the epoch-bias reference gives the core's biases, and at 0 exactly them", {
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
  # WHY: at τ_house = 0 every existing party's drift is exactly 0, so a newcomer's level is its
  # σ_γ ε draw bit for bit. core_pars() refuses a zero scale, so the zero point reaches
  # natural() past it; the industry scales are 0 too, so μ, and with it γ, can be compared.
  ref <- environment(v2_lp_ref)
  zero <- ref$core_pars(s, d)
  for (n in c("tau_mu", "tau_mu_A", "tau_house")) zero[[n]] <- 0
  z <- ref$natural(zero, d)
  expect_identical(z$mu, b$mu)
  expect_identical(z$gamma, b$gamma)
})

test_that("a newcomer restarts at its entry epoch: its own steps before it never reach the likelihood, a step after it does", {
  # §6.4: at e_p a newcomer's μ and its level in each house restart instead of carrying
  # prior-only drift. Moving a pre-entry step changes the density only by that step's own
  # std_normal prior, and the entry value is the mean over the parties existing before e_p
  # plus m_new + s_new η (spec values written out here, not read from the module).
  m_new <- 0
  s_new <- 0.2023
  checked <- c(mu = 0L, house = 0L, after = 0L)
  for (name in c("eve2024", "f180_2021")) {
    d <- v2_test_cells(cfg_m1())[[name]]$data
    entry <- c(d$entry_epoch)
    nat_epochs <- function(h) c(d$grid_epoch)[c(d$grid_nat)[c(d$house_nat) == h]]
    s <- v2_simulate(d, seed = 3, observe = FALSE)$pars
    lp <- v2_lp_ref(s, d)
    tol <- 1e-10 + 4 * .Machine$double.eps * abs(lp)
    # The bar for a change the likelihood sees, as mutant_exercised() has it.
    moved <- 1e-6
    # The change in the density's likelihood part when parameter `n`'s elements `at` (an
    # index matrix) move by 1: the density's change less the moved elements' prior change.
    lik_change <- function(n, at, mutant = "none") {
      q <- s
      q[[n]][at] <- q[[n]][at] + 1
      v2_lp_ref(q, d, mutant) - v2_lp_ref(s, d, mutant) + 0.5 * sum(q[[n]][at]^2 - s[[n]][at]^2)
    }
    for (p in which(entry > 1L)) {
      ep <- entry[p]
      lbl <- paste(name, "party", p, "entering at", ep)
      pre <- cbind(p, seq_len(ep - 1L))
      expect_lt(abs(lik_change("mu_step_raw", pre)), tol, label = paste(lbl, "pre-entry industry steps"))
      expect_gt(abs(lik_change("mu_step_raw", pre, "no_mu_restart")), moved, label = paste(lbl, "no_mu_restart"))
      nat <- v2_natural(s, d)
      expect_equal(
        nat$mu[ep, p] - mean(nat$mu[ep, entry < ep]), m_new + s_new * s$new_raw[match(p, which(entry > 0L))],
        tolerance = 1e-12, label = paste(lbl, "entry value")
      )
      checked[["mu"]] <- checked[["mu"]] + 1L
      if (ep < d$n_epoch) {
        expect_gt(abs(lik_change("mu_step_raw", cbind(p, ep))), moved, label = paste(lbl, "industry step after entry"))
        checked[["after"]] <- checked[["after"]] + 1L
      }
      for (h in which(c(d$house_first_epoch) < ep)) {
        pre_h <- cbind(h, p, seq(d$house_first_epoch[h], ep - 1L))
        expect_lt(abs(lik_change("house_step_raw", pre_h)), tol, label = paste(lbl, "house", h, "pre-entry steps"))
        # A house polling after e_p shows the drift a missing restart would carry, and its next step.
        if (any(nat_epochs(h) > ep)) {
          expect_gt(abs(lik_change("house_step_raw", pre_h, "no_house_restart")), moved, label = paste(lbl, "house", h, "no_house_restart"))
          expect_gt(abs(lik_change("house_step_raw", cbind(h, p, ep))), moved, label = paste(lbl, "house", h, "step after entry"))
          checked[["house"]] <- checked[["house"]] + 1L
        }
      }
    }
  }
  expect_true(all(checked > 0L))
})

test_that("a newcomer's house level enters against the existing parties': a drift common to them before its entry never reaches the likelihood", {
  # §6.4's purpose for the house walk: a shift common to the levels of the parties existing
  # before e_p is flat in every likelihood row before e_p, so it must leave every row after e_p
  # unchanged too. The shift here moves house h's step of epoch e0, f_h < e0 <= e_p (the entry
  # epoch's own step comes before the restart), by 1 for every party existing at e0. A party
  # entering at e0 or later takes the shift on through the existing parties' mean drift, so,
  # unlike under the absolute restart, no house_raw element moves with it.
  checked <- c(common = 0L, absolute = 0L)
  for (name in c("eve2024", "f180_2021")) {
    d <- v2_test_cells(cfg_m1())[[name]]$data
    entry <- c(d$entry_epoch)
    row_epoch <- c(d$grid_epoch)[c(d$grid_nat)]
    row_S <- lapply(seq_len(d$N_nat), \(n) d$idx_nat[n, seq_len(d$n_rep_nat[n])])
    s <- v2_simulate(d, seed = 3, observe = FALSE)$pars
    lp <- c(none = v2_lp_ref(s, d), house_restart_absolute = v2_lp_ref(s, d, "house_restart_absolute"))
    tol <- 1e-10 + 4 * .Machine$double.eps * abs(lp[["none"]])
    # Every national poll row's house effects over its reported parties S, centred over S: all
    # that the likelihood reads of γ, the one quantity the house steps move.
    seen <- function(pars) {
      g <- v2_natural(pars, d)$gamma
      lapply(seq_len(d$N_nat), \(n) {
        x <- g[row_epoch[n], d$house_nat[n], row_S[[n]]]
        x - mean(x)
      })
    }
    base <- seen(s)
    # The change in the density's likelihood part: the density's change less the steps' prior change.
    lik_change <- function(q, mutant = "none") {
      v2_lp_ref(q, d, mutant) - lp[[mutant]] + 0.5 * sum(q$house_step_raw^2 - s$house_step_raw^2)
    }
    for (p in which(entry > 1L)) {
      ep <- entry[p]
      for (h in which(c(d$house_first_epoch) < ep)) {
        for (e0 in seq(d$house_first_epoch[h] + 1L, ep)) {
          lbl <- paste(name, "party", p, "entering at", ep, "house", h, "common step of epoch", e0)
          q <- s
          q$house_step_raw[h, entry < e0, e0 - 1L] <- q$house_step_raw[h, entry < e0, e0 - 1L] + 1
          change <- max(abs(unlist(Map(`-`, seen(q), base))))
          expect_lt(change, 1e-12, label = paste(lbl, "rows' centred house effects"))
          expect_lt(abs(lik_change(q)), tol, label = paste(lbl, "likelihood"))
          checked[["common"]] <- checked[["common"]] + 1L
          # Where house h polls p after its entry, the absolute restart lets the shift through.
          if (any(c(d$house_nat) == h & row_epoch >= ep & vapply(row_S, \(S) p %in% S, logical(1)))) {
            expect_gt(abs(lik_change(q, "house_restart_absolute")), 1e-6, label = paste(lbl, "house_restart_absolute"))
            checked[["absolute"]] <- checked[["absolute"]] + 1L
          }
        }
      }
    }
  }
  expect_true(all(checked > 0L))
})

test_that("a house's walk starts at its first national poll's epoch, even after an earlier kjordaemi poll of it", {
  box::use(R / harness / tables[epoch_of])
  info <- info_2024eve()
  house_of <- function(f) info$houses$house[match(f, info$houses$fyrirtaeki)]
  kd_epoch <- vapply(split(info$polls_kd$date, house_of(info$polls_kd$fyrirtaeki)), \(x) epoch_of(min(x)), integer(1))
  nat_house <- house_of(info$polls_nat$fyrirtaeki)
  nat_epoch <- epoch_of(info$polls_nat$date)
  # A house whose kjördæmi polls start in an epoch before some of its national polls.
  later <- vapply(names(kd_epoch), \(h) any(nat_house == h & nat_epoch > kd_epoch[[h]]), logical(1))
  h <- names(kd_epoch)[later][1]
  e_k <- kd_epoch[[h]]
  moved <- info
  moved$polls_nat <- info$polls_nat[!(nat_house == h & nat_epoch <= e_k), ]
  built <- v2_stan_data(moved, cfg_m1())
  i <- match(h, built$meta$house_levels)
  first <- epoch_of(min(moved$polls_nat$date[house_of(moved$polls_nat$fyrirtaeki) == h]))
  expect_gt(first, e_k)
  expect_identical(as.integer(built$data$house_first_epoch[i]), as.integer(first))
  # With no national poll at all the house never walks.
  none <- info
  none$polls_nat <- info$polls_nat[nat_house != h, ]
  built <- v2_stan_data(none, cfg_m1())
  expect_identical(as.integer(built$data$house_first_epoch[match(h, built$meta$house_levels)]), built$data$n_epoch)
})

test_that("on every forecast cell no house's kjordaemi poll comes before its national ones, so the walk's start is unchanged", {
  # WHY: the walk's start was the first poll of either kind before it was tied to the first
  # national poll; on the snapshot the two agree for every house of the ten cells that has a
  # national poll. The one house without, Maskína at 2021 f180 (kjördæmi polls in December
  # 2020 and January 2021, its first national poll on 19 July 2021), now starts at n_epoch:
  # a kjördæmi poll reads phi_h but never gamma, so its deviations never reach the
  # likelihood. A 2017 cell is built under a root with no freeze (no forecast).
  box::use(R / harness / constants[TARGETS])
  root <- withr::local_tempdir("v2-cells-root-")
  no_nat <- character(0)
  for (i in seq_len(nrow(TARGETS))) {
    b <- v2_stan_data(info_set(TARGETS$E[i], TARGETS$F[i], v2_inputs()), cfg_m1(), root = root)
    d <- b$data
    ge <- c(d$grid_epoch)
    for (h in seq_len(d$H)) {
      lbl <- paste(TARGETS$E[i], TARGETS$tag[i], b$meta$house_levels[h])
      nat <- ge[c(d$grid_nat)[c(d$house_nat) == h]]
      kd <- ge[c(d$grid_kd)[c(d$house_kd) == h]]
      if (length(nat) == 0) no_nat <- c(no_nat, lbl)
      start <- if (length(nat)) min(c(nat, kd)) else d$n_epoch
      expect_identical(as.integer(d$house_first_epoch[h]), as.integer(start), label = lbl)
    }
  }
  expect_identical(no_nat, "2021-09-25 f180 Maskína")
})

# The newcomer pool (step v2s-m2, §6.4, switch newcomer_pool): the newcomer contrast's mean
# m_new and scale s_new estimated, instead of fixed at 0 and 0.2023.

test_that("the fixed s_new is the median of the pooled s_new's half-normal prior, in the reference and in Stan", {
  # WHY: newcomer_pool = 0 holds s_new at the median of the prior newcomer_pool = 1 puts on it,
  # so the two literals move together or this fails.
  ref <- environment(v2_lp_ref)
  expect_identical(ref$S_NEW, round(ref$S_NEW_SD * stats::qnorm(0.75), 4))
  expect_identical(c(ref$M_NEW, ref$M_NEW_SD), c(0, 0.3))
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  literal <- function(pattern) {
    hit <- regmatches(stan, regexec(pattern, stan))
    hit <- hit[lengths(hit) > 0]
    expect_length(hit, 1)
    if (length(hit) == 1) as.numeric(hit[[1]][2]) else NA_real_
  }
  expect_identical(literal("^ *real s_new_fixed = ([0-9.]+);"), ref$S_NEW)
  expect_identical(literal("^ *real m_new_fixed = ([0-9.]+);"), ref$M_NEW)
  expect_identical(literal("^ *s_new ~ normal\\(0, ([0-9.]+)\\);"), ref$S_NEW_SD)
  expect_identical(literal("^ *m_new ~ normal\\(0, ([0-9.]+)\\);"), ref$M_NEW_SD)
})

test_that("Stan declares the parameters V2_CORE_PARS names, with its types, bounds included, in its order", {
  # WHY: the Stan tests' free sizes and round trips cannot tell a bounded scalar such as
  # real<lower=0> s_new from an unbounded real; this compares the declarations as written.
  decl <- stan_block_decls(readLines(root("Stan", "forecast_v2.stan")), "parameters")
  expect_identical(decl$name, V2_CORE_PARS$name)
  expect_identical(decl$type, V2_CORE_PARS$type)
  # §6.4: s_new is a scale, half-normal under newcomer_pool = 1.
  expect_identical(V2_CORE_PARS$type[V2_CORE_PARS$name == "s_new"], "array[newcomer_pool] real<lower=0>")
  # §6.6: s_bar_E is the shock's scale, half-normal under shock = 1.
  expect_identical(V2_CORE_PARS$type[V2_CORE_PARS$name == "s_bar_E"], "array[shock] real<lower=0>")
  # §6.7: the δ walk's scales are half-normal under delta_walk = 1, and delta_raw holds one ζ
  # per epoch then, one for every epoch with delta_walk = 0.
  expect_identical(V2_CORE_PARS$type[V2_CORE_PARS$name == "s_zeta_raw"], "vector<lower=0>[delta_walk * P]")
  expect_identical(V2_CORE_PARS$type[V2_CORE_PARS$name == "s_bar_zeta"], "array[delta_walk] real<lower=0>")
  expect_identical(
    V2_CORE_PARS$type[V2_CORE_PARS$name == "delta_raw"], "array[1 + delta_walk * (n_epoch - 1) - delta_form * n_closed] matrix[K, P]"
  )
  # §6.7 held centred (delta_form = 1): each closed election's coordinates delta_e[2:K, 2:P],
  # plain unconstrained matrices, and delta_raw then only the open epoch's zeta.
  expect_identical(V2_CORE_PARS$type[V2_CORE_PARS$name == "delta_coord"], "array[delta_form * n_closed] matrix[K - 1, P - 1]")
  expect_identical(utils::tail(V2_CORE_PARS$name, 1), "tau_house_drift")
})

test_that("v2_par_dims() refuses a delta_form other than 0 or 1, and delta_form = 1 without the delta walk", {
  # WHY: without the second refusal, delta_form = 1 with delta_walk = 0 would size delta_raw
  # at 1 - n_closed matrices instead of stopping: a static delta has no centred form.
  d <- list(
    P = 4L, K = 3L, G = 9L, H = 2L, J = 1L, n_closed = 2L, n_epoch = 3L, entry_epoch = c(0L, 0L, 0L, 2L),
    walk_form = 0L, epoch_bias = 0L, newcomer_pool = 0L, shock = 0L, delta_walk = 1L, delta_form = 1L, post_flex = 0L,
    bias_drift = 0L
  )
  expect_identical(
    lapply(v2_par_dims(d)[c("delta_raw", "delta_coord")], as.integer),
    list(delta_raw = c(1L, 3L, 4L), delta_coord = c(2L, 2L, 3L))
  )
  for (bad in list(2L, -1L, c(0L, 1L), integer(0))) {
    expect_error(
      v2_par_dims(utils::modifyList(d, list(delta_form = bad))), "v2 reference: data$delta_form must be 0 or 1",
      fixed = TRUE
    )
  }
  expect_error(
    v2_par_dims(utils::modifyList(d, list(delta_walk = 0L))), "v2 reference: data$delta_form = 1 needs delta_walk = 1",
    fixed = TRUE
  )
})

test_that("the newcomer-pool reference: its mutant changes the density on every cell, and the identity under permutation holds", {
  # WHY: every newcomer enters in an epoch of the window (C and F in the first), so the mean
  # over every party differs from the mean over the existing ones on each cell, 2017 eve's
  # prior points included; the epoch bias's mutants are the v2s-m1 test's.
  cells <- v2_test_cells(cfg_m2())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(c(d$epoch_bias, d$newcomer_pool)), c(1L, 1L))
    expect_true("newcomer_mean_over_all" %in% v2_mutants(d))
    expect_identical(lengths(v2_par_dims(d)[c("m_new", "s_new")]), c(m_new = 1L, s_new = 1L))
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      expect_length(s$pars$m_new, 1)
      lp <- v2_lp_ref(s$pars, d)
      expect_gt(abs(v2_lp_ref(s$pars, d, "newcomer_mean_over_all") - lp), 1e-6, label = name)
      p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
      expect_identical(p$pars[c("m_new", "s_new")], s$pars[c("m_new", "s_new")])
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
    }
  }
  # WHY: 2017 eve has one closed election, so min_closed = 1 is the fewest a cell can have.
  expect_identical(V2_MUTANTS$min_closed[V2_MUTANTS$mutant == "newcomer_mean_over_all"], 1L)
  expect_identical(as.integer(cells$eve2017$data$n_closed), 1L)
})

test_that("a newcomer enters at the existing parties' mean plus the point's m_new + s_new eta", {
  # §6.4: the mean runs over the parties existing before e_p (spec values written out here),
  # with the epoch bias on (v2s-m2) and off (newcomer_pool = 1 with epoch_bias = 0, as the
  # v2-noepoch ablation fits it, where μ takes no step).
  configs <- list(epoch_bias = cfg_m2(), no_epoch_bias = utils::modifyList(cfg_m2(), list(epoch_bias = 0L)))
  for (cfg_name in names(configs)) {
    for (name in c("eve2024", "f180_2021", "eve2017")) {
      d <- v2_test_cells(configs[[cfg_name]])[[name]]$data
      label <- paste(cfg_name, name)
      expect_identical(as.integer(c(d$epoch_bias, d$newcomer_pool)), c(as.integer(cfg_name == "epoch_bias"), 1L), label = label)
      if (cfg_name == "no_epoch_bias") {
        epoch_pars <- c("mu_step_raw", "house_step_raw", "tau_mu", "tau_mu_A", "tau_house")
        expect_true(all(vapply(v2_par_dims(d)[epoch_pars], prod, numeric(1)) == 0), label = label)
      }
      entry <- c(d$entry_epoch)
      expect_gt(sum(entry > 0L), 0, label = label)
      s <- v2_simulate(d, seed = 5, observe = FALSE)$pars
      s$m_new[] <- 0.4
      s$s_new[] <- 0.7
      nat <- v2_natural(s, d)
      for (p in which(entry > 0L)) {
        ep <- entry[p]
        expect_equal(
          nat$mu[ep, p] - mean(nat$mu[ep, entry < ep]), 0.4 + 0.7 * s$new_raw[match(p, which(entry > 0L))],
          tolerance = 1e-12, label = paste(label, "party", p, "entering at", ep)
        )
      }
      expect_gt(abs(v2_lp_ref(s, d, "newcomer_mean_over_all") - v2_lp_ref(s, d)), 1e-6, label = label)
    }
  }
})

test_that("at m_new = 0 and s_new = 0.2023 the pooled density is the fixed one plus the two priors", {
  for (name in c("eve2024", "eve2017")) {
    d <- v2_test_cells(cfg_m2())[[name]]$data
    d0 <- d
    d0$newcomer_pool <- 0L
    s <- v2_simulate(d, seed = 6, observe = FALSE)$pars
    s$m_new[] <- 0
    s$s_new[] <- 0.2023
    fixed <- s
    fixed$m_new <- array(numeric(0), 0)
    fixed$s_new <- array(numeric(0), 0)
    priors <- stats::dnorm(0, 0, 0.3, log = TRUE) + log(2) + stats::dnorm(0.2023, 0, 0.3, log = TRUE)
    lp <- v2_lp_ref(s, d)
    expect_lt(abs(lp - v2_lp_ref(fixed, d0) - priors), 1e-10 + 4 * .Machine$double.eps * abs(lp), label = name)
    expect_equal(v2_natural(s, d)$mu, v2_natural(fixed, d0)$mu, tolerance = 0)
    # Moving m_new shifts every newcomer's contrast with the existing parties, so the likelihood sees it.
    moved <- s
    moved$m_new[] <- 0.5
    expect_gt(abs(v2_lp_ref(moved, d) - lp - (stats::dnorm(0.5, 0, 0.3, log = TRUE) - stats::dnorm(0, 0, 0.3, log = TRUE))), 1e-6)
  }
})

# The election-day shock (step v2s-m3, §6.6, switch shock): eps_e = C(s_bar_E d_e z_e) in each
# closed election's kjordaemi softmax, d_e the row of d_shock of the epoch e closes.

test_that("the shock reference: its mutant changes the density on every cell, and the identity under permutation holds", {
  # WHY: no party's d is 1 unless its previous share is exactly 0.2, so every closed election,
  # 2017 eve's one included, tells d from 1.
  cells <- v2_test_cells(cfg_m3())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$shock), 1L)
    expect_true("shock_unscaled" %in% v2_mutants(d))
    expect_identical(
      lapply(v2_par_dims(d)[c("z_shock", "s_bar_E")], as.integer),
      list(z_shock = as.integer(c(d$P, d$n_closed)), s_bar_E = 1L)
    )
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      lp <- v2_lp_ref(s$pars, d)
      expect_gt(abs(v2_lp_ref(s$pars, d, "shock_unscaled") - lp), 1e-6, label = name)
      rp <- rev(seq_len(d$P))
      p <- permute_v2(s$pars, d, rp, rev(seq_len(d$K)))
      expect_equal(p$pars$z_shock, s$pars$z_shock[rp, , drop = FALSE], tolerance = 0)
      expect_identical(p$pars$s_bar_E, s$pars$s_bar_E)
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
      expect_equal(v2_natural(p$pars, p$data)$eps, v2_natural(s$pars, d)$eps[, rp, drop = FALSE], tolerance = 1e-12)
    }
  }
  expect_identical(V2_MUTANTS$min_closed[V2_MUTANTS$mutant == "shock_unscaled"], 1L)
  expect_identical(as.integer(cells$eve2017$data$n_closed), 1L)
})

test_that("each closed election's shock is C(s_bar_E d_e z_e) with its own row of d_shock, shared by its kjordaemi and reaching only its results", {
  # WHY: the reference's likelihood rows are internal to its module.
  ref <- environment(v2_lp_ref)
  for (name in c("eve2024", "f180_2021", "eve2017")) {
    d <- v2_test_cells(cfg_m3())[[name]]$data
    s <- v2_simulate(d, seed = 7, observe = FALSE)$pars
    s$s_bar_E[] <- 0.3
    eps <- v2_natural(s, d)$eps
    expect_identical(dim(eps), as.integer(c(d$n_closed, d$P)))
    for (e in seq_len(d$n_closed)) {
      x <- 0.3 * d$d_shock[e, ] * s$z_shock[, e]
      expect_equal(eps[e, ], x - mean(x), tolerance = 1e-14, label = paste(name, "election", e))
    }
    rows <- function(pars) {
      p <- ref$core_pars(pars, d)
      ref$dm_rows(p, ref$natural(p, d), d)
    }
    a <- rows(s)
    for (e in seq_len(d$n_closed)) {
      moved <- s
      moved$z_shock[, e] <- moved$z_shock[, e] + seq_len(d$P) / d$P
      b <- rows(moved)
      hit <- vapply(seq_along(a), \(i) !identical(a[[i]]$alpha, b[[i]]$alpha), logical(1))
      own <- vapply(a, \(r) r$field == "y_res" && r$at[1] == e, logical(1))
      expect_identical(hit, own, label = paste(name, "rows moved by election", e))
      # Every kjordaemi's log concentrations move by the same contrast: one shock per election.
      moves <- vapply(which(own), \(i) {
        x <- log(b[[i]]$alpha) - log(a[[i]]$alpha)
        x - mean(x)
      }, numeric(length(a[[which(own)[1]]]$alpha)))
      expect_lt(max(abs(moves - moves[, 1])), 1e-10, label = paste(name, "election", e))
      expect_gt(max(abs(moves[, 1])), 1e-3, label = paste(name, "election", e))
    }
  }
})

test_that("at z_shock = 0 the shock's density is the shock-free one plus its priors, and z_shock reaches the likelihood", {
  for (name in c("eve2024", "eve2017")) {
    d <- v2_test_cells(cfg_m3())[[name]]$data
    d0 <- d
    d0$shock <- 0L
    s <- v2_simulate(d, seed = 8, observe = FALSE)$pars
    s$z_shock[] <- 0
    off <- s
    off$z_shock <- matrix(0, d$P, 0)
    off$s_bar_E <- array(numeric(0), 0)
    priors <- d$P * d$n_closed * stats::dnorm(0, log = TRUE) + log(2) + stats::dnorm(s$s_bar_E, 0, d$prior_s_bar_E, log = TRUE)
    lp <- v2_lp_ref(s, d)
    expect_lt(abs(lp - v2_lp_ref(off, d0) - priors), 1e-10 + 4 * .Machine$double.eps * abs(lp), label = name)
    expect_equal(v2_natural(s, d)$eps, matrix(0, d$n_closed, d$P), tolerance = 0)
    expect_equal(v2_natural(off, d0)$eps, matrix(0, d$n_closed, d$P), tolerance = 0)
    moved <- s
    moved$z_shock[1, ] <- 1
    prior_move <- d$n_closed * (stats::dnorm(1, log = TRUE) - stats::dnorm(0, log = TRUE))
    expect_gt(abs(v2_lp_ref(moved, d) - lp - prior_move), 1e-6, label = name)
  }
})

# The δ walk (step v2s-m4, §6.7, switch delta_walk): δ_1 = M_1(s_δ0 ⊙ ζ_1) and δ_e =
# M_e(δ_{e−1} − c_{e−1} 1ᵀ + √(Δyears_e / Δ̄) s_ζ ⊙ ζ_e), with δ_{e−1} re-centred within each
# kjördæmi over the previous election's parties (c_{e−1}, their unweighted mean), a newcomer's
# column fresh at its entry epoch.

test_that("the delta-walk reference: its mutants change the density on every cell, and the identity under permutation holds", {
  # WHY: every cell has a step whose delta_years is not delta_years_bar (2017 eve's one step is
  # 1 year), which exercises delta_years_unscaled, and a newcomer entering after the first
  # epoch (M, and J), which exercises no_delta_restart and delta_entry_leak; so all three
  # mutants are exercised on each cell, 2017 eve's prior points included.
  cells <- v2_test_cells(cfg_m4())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$delta_walk), 1L)
    expect_true(all(c("delta_years_unscaled", "no_delta_restart", "delta_entry_leak") %in% v2_mutants(d)))
    expect_identical(
      lapply(v2_par_dims(d)[c("delta_raw", "s_zeta_raw", "s_bar_zeta")], as.integer),
      list(delta_raw = as.integer(c(d$n_epoch, d$K, d$P)), s_zeta_raw = as.integer(d$P), s_bar_zeta = 1L)
    )
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      expect_identical(dim(s$pars$delta_raw), as.integer(c(d$n_epoch, d$K, d$P)))
      lp <- v2_lp_ref(s$pars, d)
      for (m in c("delta_years_unscaled", "no_delta_restart", "delta_entry_leak")) {
        expect_gt(abs(v2_lp_ref(s$pars, d, m) - lp), 1e-6, label = paste(name, m))
      }
      rp <- rev(seq_len(d$P))
      rk <- rev(seq_len(d$K))
      p <- permute_v2(s$pars, d, rp, rk)
      expect_equal(p$pars$delta_raw, s$pars$delta_raw[, rk, rp, drop = FALSE], tolerance = 0)
      expect_equal(p$pars$s_zeta_raw, s$pars$s_zeta_raw[rp], tolerance = 0)
      expect_identical(p$pars$s_bar_zeta, s$pars$s_bar_zeta)
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
      expect_equal(v2_natural(p$pars, p$data)$delta, v2_natural(s$pars, d)$delta[, rk, rp, drop = FALSE], tolerance = 1e-12)
    }
  }
  walk_mutants <- V2_MUTANTS[V2_MUTANTS$switch %in% "delta_walk", ]
  expect_identical(walk_mutants$mutant, c("delta_years_unscaled", "no_delta_restart", "delta_entry_leak"))
  expect_identical(walk_mutants$min_closed, c(1L, 1L, 1L))
  expect_identical(as.integer(cells$eve2017$data$n_closed), 1L)
})

test_that("each epoch's delta is the walk of 6.7 written out, re-centred over the previous election's parties, a newcomer fresh at its entry epoch", {
  # 6.7: delta_1 = M_1(s_delta0 zeta_1); delta_e = M_e(delta_{e-1} - c 1' + sqrt(dyears_e / 4)
  # s_zeta zeta_e), with s_zeta = s_bar_zeta s_zeta_raw and c_k the unweighted mean of
  # delta_{e-1}[k, S] over the parties S with results at election e - 1, except that a
  # newcomer's column in its entry epoch e_p > 1 is s_delta0,p zeta_{e_p,.,p}. The open
  # epoch's dyears runs to E.
  for (name in c("eve2024", "f180_2021", "eve2017")) {
    d <- v2_test_cells(cfg_m4())[[name]]$data
    expect_identical(d$delta_years_bar, 4)
    entry <- c(d$entry_epoch)
    expect_true(any(entry > 1L), label = name)
    s <- v2_simulate(d, seed = 9, observe = FALSE)$pars
    s$s_bar_zeta[] <- 0.3
    nat <- v2_natural(s, d)
    s0 <- exp(s$mu_sd + s$tau_sd * s$s_delta0_raw)
    sz <- 0.3 * s$s_zeta_raw
    expect_equal(nat$s_zeta, sz, tolerance = 1e-15)
    zeta <- function(e) matrix(s$delta_raw[e, , ], d$K, d$P)
    expect_equal(nat$delta[1, , ], M_e(zeta(1) %*% diag(s0), d$w[1, ]), tolerance = 1e-12, label = paste(name, "epoch 1"))
    for (e in 2:d$n_epoch) {
      S <- d$idx_res[e - 1, seq_len(d$n_rep_res[e - 1])]
      c_k <- rowMeans(nat$delta[e - 1, , S])
      X <- nat$delta[e - 1, , ] - c_k + sqrt(d$delta_years[e] / 4) * zeta(e) %*% diag(sz)
      for (p in which(entry == e)) X[, p] <- s0[p] * zeta(e)[, p]
      expect_equal(nat$delta[e, , ], M_e(X, d$w[e, ]), tolerance = 1e-12, label = paste(name, "epoch", e))
    }
    # The walk carries a step forward: moving the coordinate party's (present from the
    # window's start) zeta_e in the first kjordaemi moves delta_e and every later epoch's, never
    # an earlier one's. A shift of a whole column would not move delta: M_e removes it.
    for (e in 2:d$n_epoch) {
      moved <- s
      moved$delta_raw[e, 1, 1] <- moved$delta_raw[e, 1, 1] + 1
      dm <- v2_natural(moved, d)$delta
      for (f in seq_len(d$n_epoch)) {
        changed <- max(abs(dm[f, , ] - nat$delta[f, , ])) > 1e-6
        expect_identical(changed, f >= e, label = paste(name, "zeta of epoch", e, "and delta of epoch", f))
      }
    }
  }
})

test_that("with every step's zeta at 0 the walk's scales reach only their half-normal priors", {
  for (name in c("eve2024", "eve2017")) {
    d <- v2_test_cells(cfg_m4())[[name]]$data
    s <- v2_simulate(d, seed = 10, observe = FALSE)$pars
    s$delta_raw[-1, , ] <- 0
    moved <- s
    moved$s_zeta_raw <- 2 * s$s_zeta_raw
    moved$s_bar_zeta[] <- 0.37
    expect_equal(v2_natural(moved, d)$delta, v2_natural(s, d)$delta, tolerance = 1e-14)
    prior <- sum(stats::dnorm(moved$s_zeta_raw, log = TRUE) - stats::dnorm(s$s_zeta_raw, log = TRUE)) +
      stats::dnorm(0.37, 0, d$prior_s_bar_zeta, log = TRUE) - stats::dnorm(s$s_bar_zeta, 0, d$prior_s_bar_zeta, log = TRUE)
    lp <- v2_lp_ref(s, d)
    expect_lt(abs(v2_lp_ref(moved, d) - lp - prior), 1e-9 + 8 * .Machine$double.eps * abs(lp), label = name)
    # A step's zeta reaches the likelihood beside its N(0, 1) prior.
    stepped <- s
    stepped$delta_raw[2, 1, 1] <- 1
    expect_gt(abs(v2_lp_ref(stepped, d) - lp - (stats::dnorm(1, log = TRUE) - stats::dnorm(0, log = TRUE))), 1e-6, label = name)
  }
})

test_that("with delta_walk = 0 delta_raw is one zeta, the walk's scales have size 0, and every epoch maps that one zeta", {
  d <- v2_test_cells(cfg_m3())$eve2024$data
  expect_identical(as.integer(d$delta_walk), 0L)
  dims <- v2_par_dims(d)
  expect_identical(lapply(dims[c("delta_raw", "s_zeta_raw", "s_bar_zeta")], as.integer), list(
    delta_raw = as.integer(c(1L, d$K, d$P)), s_zeta_raw = 0L, s_bar_zeta = 0L
  ))
  s <- v2_simulate(d, seed = 11, observe = FALSE)$pars
  expect_length(s$s_zeta_raw, 0)
  expect_length(s$s_bar_zeta, 0)
  nat <- v2_natural(s, d)
  expect_length(nat$s_zeta, 0)
  X <- matrix(s$delta_raw[1, , ], d$K, d$P) %*% diag(nat$s_delta0)
  for (e in seq_len(d$n_epoch)) expect_equal(nat$delta[e, , ], M_e(X, d$w[e, ]), tolerance = 1e-12)
  # The δ walk's draws come after every other parameter's, so the other parameters of a
  # delta_walk = 1 draw are the delta_walk = 0 draw's, and so is its first zeta.
  d1 <- d
  d1$delta_walk <- 1L
  s1 <- v2_simulate(d1, seed = 11, observe = FALSE)$pars
  others <- setdiff(names(s), c("delta_raw", "s_zeta_raw", "s_bar_zeta"))
  expect_identical(s1[others], s[others])
  expect_identical(s1$delta_raw[1, , ], s$delta_raw[1, , ])
})

test_that("on real data neither a newcomer's earlier zeta nor an offset shared by S_1 reaches the likelihood, which the walk without the re-centring lets through", {
  # WHY: before its entry epoch a newcomer is in no softmax, so its column of delta reaches the
  # likelihood only through the walk into its entry epoch. Re-centring delta_{e-1} over the
  # previous election's parties, which exclude it, keeps it out: moving one of its earlier zeta
  # changes the density by that zeta's N(0, 1) prior alone.
  d <- v2_test_cells(cfg_m4())$eve2024$data
  entry <- c(d$entry_epoch)
  late <- which(entry > 1L)
  expect_true(length(late) >= 2)
  s <- v2_simulate(d, seed = 9, observe = FALSE)$pars
  s$s_bar_zeta[] <- 0.3
  lp <- v2_lp_ref(s, d)
  lp_leak <- v2_lp_ref(s, d, "delta_entry_leak")
  tol <- lp_tol(lp)
  for (p in late) {
    for (e in seq_len(entry[p] - 1L)) {
      for (k in 1:2) {
        moved <- s
        z <- s$delta_raw[e, k, p]
        moved$delta_raw[e, k, p] <- z + 1
        prior <- -0.5 * ((z + 1)^2 - z^2)
        at <- sprintf("party %d (entry epoch %d), zeta of epoch %d, kjordaemi %d", p, entry[p], e, k)
        dlp <- v2_lp_ref(moved, d) - lp
        expect_lt(abs(dlp - prior), tol, label = at)
        expect_lt(abs(dlp - prior), 1e-10 + 8 * .Machine$double.eps * abs(lp), label = at)
        expect_gt(abs(v2_lp_ref(moved, d, "delta_entry_leak") - lp_leak - prior), 100 * tol, label = paste(at, "under delta_entry_leak"))
      }
    }
  }
  # An offset a_k shared by the parties S_1 with results at the first closed election, added to
  # their epoch-1 delta as a_k / s_delta0,q on zeta_1[k, q], cancels in every softmax over S_1
  # at epoch 1 and is re-centred away at epoch 2; the literal walk carries it into the entry
  # epoch's contrasts.
  a <- c(0.3, -0.2, 0.1, 0.25, -0.15, 0.05)
  for (name in c("eve2024", "f180_2021")) {
    d <- v2_test_cells(cfg_m4())[[name]]$data
    expect_identical(as.integer(d$K), length(a))
    s <- v2_simulate(d, seed = 9, observe = FALSE)$pars
    s$s_bar_zeta[] <- 0.3
    s0 <- v2_natural(s, d)$s_delta0
    S1 <- d$idx_res[1, seq_len(d$n_rep_res[1])]
    expect_true(any(c(d$entry_epoch) > 1L), label = name)
    moved <- s
    prior <- 0
    for (q in S1) {
      z <- s$delta_raw[1, , q]
      moved$delta_raw[1, , q] <- z + a / s0[q]
      prior <- prior - 0.5 * sum((z + a / s0[q])^2 - z^2)
    }
    lp <- v2_lp_ref(s, d)
    tol <- lp_tol(lp)
    dlp <- v2_lp_ref(moved, d) - lp
    expect_lt(abs(dlp - prior), tol, label = name)
    expect_lt(abs(dlp - prior), 1e-10 + 8 * .Machine$double.eps * abs(lp), label = name)
    leak <- v2_lp_ref(moved, d, "delta_entry_leak") - v2_lp_ref(s, d, "delta_entry_leak")
    expect_gt(abs(leak - prior), 100 * tol, label = paste(name, "under delta_entry_leak"))
  }
})

test_that("on a toy input neither a newcomer's earlier zeta nor an offset shared by the previous election's parties moves the entry epoch's contrasts", {
  # K = 4 kjordaemi, P = 5 parties, party 5 entering at epoch 2; S_1 = 1:4 have results at
  # election 1. Both perturbations are flat at epoch 1, where party 5 is in no softmax and a
  # row offset over S_1 cancels in every softmax over S_1. What a softmax over the parties
  # sees at the entry epoch is each kjordaemi's contrasts, here against party 1.
  ref <- environment(v2_lp_ref)
  K <- 4L
  P <- 5L
  data <- list(
    K = K, P = P, n_epoch = 2L, delta_walk = 1L, w = rbind(c(0.4, 0.3, 0.2, 0.1), c(0.35, 0.3, 0.2, 0.15)),
    delta_years = c(0, 4), delta_years_bar = 4, entry_epoch = c(0L, 0L, 0L, 0L, 2L),
    idx_res = matrix(c(1:4, 0L), 1), n_rep_res = 4L
  )
  zeta <- withr::with_seed(1, array(stats::rnorm(2 * K * P), c(2, K, P)))
  s0 <- c(0.3, 0.25, 0.35, 0.3, 0.3)
  sz <- c(0.2, 0.1, 0.3, 0.2, 0.25)
  seen <- function(d, e, S) d[e, , S] - d[e, , S[1]]
  pre <- zeta
  pre[1, 2, 5] <- pre[1, 2, 5] + 1
  offset <- zeta
  for (q in 1:4) offset[1, , q] <- offset[1, , q] + c(1, -2, 0.5, 3) / s0[q]
  for (leak in c(FALSE, TRUE)) {
    a <- ref$delta_epochs(zeta, s0, sz, data, entry_leak = leak)
    for (moved in list(pre, offset)) {
      b <- ref$delta_epochs(moved, s0, sz, data, entry_leak = leak)
      expect_lt(max(abs(seen(b, 1, 1:4) - seen(a, 1, 1:4))), 1e-12)
      change <- max(abs(seen(b, 2, 1:5) - seen(a, 2, 1:5)))
      if (leak) expect_gt(change, 1e-3) else expect_lt(change, 1e-12)
    }
  }
})

# The fundamentals pseudo-observation (step v2s-m5, §6.5, switch fundamentals), on the cells of
# the Stan lp tests with stage1_stub()'s mu_f, so no stage-1 fit runs here.

# The pseudo-observation's log density written out from §6.5 at a prior point `pars`: x the log
# of the shock-free latent national share at E over S_f, centred; v each party's variance of a
# walk step, read off the walk's own factor (coordinates 2:P step by L_walk z and party 1 by
# minus their sum); V_w that variance over a stretch of weight_time - campaign_days days off the
# campaign and campaign_days on it, through the walk's own step scales.
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
  stretch <- list(delta_days = c(d$weight_time - d$campaign_days, d$campaign_days), campaign = c(0L, 1L))
  V_w <- v * sum(walk_scale(pars$tau_camp, stretch)^2)
  sum(stats::dnorm(c(d$mu_f), x - mean(x), sqrt(V_w * (1 - d$desired_weight) / d$desired_weight), log = TRUE))
}

test_that("the fundamentals reference adds the term of 6.5 written out, and nothing with fundamentals = 0", {
  cells <- v2_test_cells(cfg_m5(), stage1 = stage1_stub)
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$fundamentals), 1L)
    d0 <- d
    d0$fundamentals <- 0L
    d4 <- v2_test_cells(cfg_m4())[[name]]$data
    for (seed in 1:3) {
      pars <- v2_simulate(d, seed = seed, observe = FALSE)$pars
      lp <- v2_lp_ref(pars, d)
      lp0 <- v2_lp_ref(pars, d0)
      expect_equal(lp - lp0, fund_written_out(pars, d), tolerance = 1e-8, label = paste(name, "the term"))
      # The switch off is v2s-m4 exactly: the same draw and the same density as its own build.
      expect_identical(v2_simulate(d4, seed = seed, observe = FALSE)$pars, pars)
      expect_identical(v2_lp_ref(pars, d4), lp0)
    }
  }
})

test_that("the fundamentals reference: its mutant changes the density on every cell, and the identity under permutation holds", {
  # WHY: mean(log pi[S_f]) is negative for any share vector, so the uncentred mean shifts every
  # party's, on every cell.
  cells <- v2_test_cells(cfg_m5(), stage1 = stage1_stub)
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_true("fund_uncentred" %in% v2_mutants(d), label = name)
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      lp <- v2_lp_ref(s$pars, d)
      expect_gt(abs(v2_lp_ref(s$pars, d, "fund_uncentred") - lp), 1e-6, label = name)
      p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
      expect_identical(d$P + 1L - c(p$data$sf_idx), c(d$sf_idx))
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp))
    }
  }
  fund <- V2_MUTANTS[V2_MUTANTS$switch %in% "fundamentals", ]
  expect_identical(fund$mutant, "fund_uncentred")
  expect_identical(fund$min_closed, 1L)
})

test_that("the fundamentals reference refuses data it cannot evaluate, naming the condition, and reads them only under the switch", {
  d <- v2_test_cells(cfg_m5(), stage1 = stage1_stub)$eve2024$data
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
    absent = "data$fundamentals = 1 needs every party of S_f (sf_idx) to exist at E",
    mu_short = "data$mu_f must be n_sf finite numbers",
    mu_nonfinite = "data$mu_f must be n_sf finite numbers",
    weight_one = "data$desired_weight must be one number in (0, 1)",
    time_short = "data$weight_time must be at least data$campaign_days, itself at least 1",
    repeated = "data$fundamentals = 1 needs at least two distinct parties of S_f (n_sf and sf_idx)"
  )
  for (n in names(bad)) {
    x <- utils::modifyList(d, bad[[n]])
    expect_error(v2_lp_ref(pars, x), says[[n]], fixed = TRUE, label = n)
    x$fundamentals <- 0L
    expect_no_error(v2_lp_ref(pars, x))
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
})

test_that("v2_simulate() with observe redraws mu_f after every count, from N(x_c, tau_f), and leaves it alone without", {
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_m5(), stage1 = stage1_stub)$eve2024$data
  d0 <- d
  d0$fundamentals <- 0L
  s <- v2_simulate(d, seed = 3)
  s0 <- v2_simulate(d0, seed = 3)
  for (f in c("y_nat", "y_kd", "y_res")) expect_identical(s$data[[f]], s0$data[[f]], label = f)
  expect_identical(s0$data$mu_f, d$mu_f)
  expect_identical(v2_simulate(d, seed = 3, observe = FALSE)$data$mu_f, d$mu_f)
  expect_identical(dim(s$data$mu_f), dim(d$mu_f))
  p <- ref$core_pars(s$pars, d)
  f <- ref$fund_normal(p, ref$natural(p, d), d)
  z <- (c(s$data$mu_f) - f$mean) / f$tau_f
  expect_true(all(is.finite(z)) && all(abs(z) < 6) && all(z != (c(d$mu_f) - f$mean) / f$tau_f))
})

# The post-election multiplier (step v2s-m6, §6.2, switch post_flex): each walk step in the
# post_days after a closed election scales by 1 + tau_post, as a campaign step by 1 + tau_camp,
# tau_post ~ exponential(1); V_w counts the fundamentals window's days in a post window.

test_that("the post-election reference: its mutant changes the density on every cell, and the identity under permutation holds", {
  cells <- v2_test_cells(cfg_m6(), stage1_stub)
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$post_flex), 1L, label = name)
    expect_true("post_unscaled" %in% v2_mutants(d), label = name)
    expect_identical(as.integer(v2_par_dims(d)$tau_post), 1L, label = name)
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      lp <- v2_lp_ref(s$pars, d)
      expect_gt(abs(v2_lp_ref(s$pars, d, "post_unscaled") - lp), 1e-6, label = name)
      rp <- rev(seq_len(d$P))
      p <- permute_v2(s$pars, d, rp, rev(seq_len(d$K)))
      expect_identical(p$pars$tau_post, s$pars$tau_post)
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp), label = name)
    }
  }
  expect_identical(V2_MUTANTS$switch[V2_MUTANTS$mutant == "post_unscaled"], "post_flex")
  expect_identical(V2_MUTANTS$min_closed[V2_MUTANTS$mutant == "post_unscaled"], 1L)
})

test_that("each step's scale is sqrt(Delta) (1 + tau_camp c + tau_post q), and with post_flex = 0 tau_post is never read", {
  d <- v2_test_cells(cfg_m6(), stage1_stub)$eve2024$data
  q <- c(d$post)
  expect_true(any(q == 1L) && any(q == 0L))
  want <- sqrt(c(d$delta_days)) * (1 + 0.7 * c(d$campaign) + 1.9 * q)
  expect_equal(walk_scale(0.7, d, 1.9), want, tolerance = 1e-15)
  d0 <- d
  d0$post_flex <- 0L
  expect_identical(walk_scale(0.7, d0, 1.9), walk_scale(0.7, d0))
  expect_equal(walk_scale(0.7, d0), sqrt(c(d$delta_days)) * (1 + 0.7 * c(d$campaign)), tolerance = 1e-15)
})

test_that("the multiplier rescales only the walk's post steps: its density is post_flex = 0's at the matching innovations, plus the z_walk change and exponential(1)", {
  for (name in c("eve2024", "eve2017")) {
    d <- v2_test_cells(cfg_m6(), stage1_stub)[[name]]$data
    d0 <- d
    d0$post_flex <- 0L
    s <- v2_simulate(d, seed = 11, observe = FALSE)$pars
    tp <- as.numeric(s$tau_post)
    q <- c(d$post) == 1L
    off <- s
    off$tau_post <- array(numeric(0), 0)
    off$z_walk[, q] <- s$z_walk[, q] * (1 + tp)
    expect_equal(v2_natural(off, d0)$theta, v2_natural(s, d)$theta, tolerance = 1e-12, label = name)
    want <- sum(stats::dnorm(s$z_walk, log = TRUE)) - sum(stats::dnorm(off$z_walk, log = TRUE)) + stats::dexp(tp, 1, log = TRUE)
    lp <- v2_lp_ref(s, d)
    expect_lt(abs(lp - v2_lp_ref(off, d0) - want), 1e-8 * max(1, abs(lp)), label = name)
  }
})

test_that("fund_scales() counts the fundamentals window's post days at (1 + tau_post)^2, and only with post_flex = 1", {
  d <- v2_test_cells(cfg_m6(), stage1_stub)$eve2024$data
  sigma <- seq(0.01, 0.03, length.out = d$P)
  L <- diag(d$P)
  base <- fund_scales(sigma, L, 0.8, d, 1.5)
  expect_identical(d$weight_post_days, 0)
  expect_equal(base$V_w, base$v * ((180 - 47) + 47 * 1.8^2), tolerance = 1e-14)
  d$weight_post_days <- 30
  sc <- fund_scales(sigma, L, 0.8, d, 1.5)
  expect_equal(sc$V_w, sc$v * ((180 - 47 - 30) + 30 * 2.5^2 + 47 * 1.8^2), tolerance = 1e-14)
  w <- d$desired_weight
  expect_equal((1 / sc$tau_f^2) / (1 / sc$tau_f^2 + 1 / sc$V_w), rep(w, length(sc$V_w)), tolerance = 1e-12)
  d$post_flex <- 0L
  expect_equal(fund_scales(sigma, L, 0.8, d, 1.5)$V_w, base$V_w, tolerance = 0)
})

test_that("the post-election reference refuses post flags in a campaign, malformed flags and too many post days in the fundamentals window", {
  d <- v2_test_cells(cfg_m6(), stage1_stub)$eve2024$data
  s <- v2_simulate(d, seed = 3, observe = FALSE)$pars
  both <- d
  both$post[which(c(d$campaign) == 1L)[1]] <- 1L
  expect_error(v2_lp_ref(s, both), "no step may be in both a campaign and a post window")
  short <- d
  short$post <- d$post[-1]
  expect_error(v2_lp_ref(s, short), "data\\$post must be G - 1 flags, each 0 or 1")
  many <- d
  many$weight_post_days <- d$weight_time - d$campaign_days + 1
  expect_error(v2_lp_ref(s, many), "data\\$weight_post_days must be one number in \\[0, weight_time - campaign_days\\]")
})

test_that("v2_simulate() with post_flex = 1 draws tau_post last, so every other parameter is post_flex = 0's draw", {
  d <- v2_test_cells(cfg_m6(), stage1_stub)$eve2024$data
  d0 <- d
  d0$post_flex <- 0L
  d0$post[] <- 0L
  a <- v2_simulate(d, seed = 21, observe = FALSE)$pars
  b <- v2_simulate(d0, seed = 21, observe = FALSE)$pars
  expect_identical(length(a$tau_post), 1L)
  expect_identical(length(b$tau_post), 0L)
  expect_gt(as.numeric(a$tau_post), 0)
  expect_identical(a[setdiff(names(a), "tau_post")], b[setdiff(names(b), "tau_post")])
  expect_identical(names(a), V2_CORE_PARS$name)
})

# The calendar drift (step v2s-m7, §6.4, switch bias_drift): in place of the epoch walk the
# industry bias walks on half-yearly knots and each house's deviations on yearly ones, with
# steps sqrt(years between knots) C(tau .* xi), and a poll's bias is interpolated between the
# knots around its date.

test_that("the drift reference: its mutants change the density wherever they apply, and the identity under permutation holds", {
  cells <- v2_test_cells(cfg_m7(), stage1_stub)
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(c(as.integer(d$bias_drift), as.integer(d$epoch_bias)), c(1L, 0L), label = name)
    drift <- c("drift_unscaled", "drift_left_knot", "drift_house_from_start", "drift_no_mu_restart")
    expect_true(all(drift %in% v2_mutants(d)), label = name)
    expect_false(any(c("house_walk_from_first_poll", "no_mu_restart") %in% v2_mutants(d)), label = name)
    dims <- v2_par_dims(d)
    expect_identical(dims$mu_drift_raw, c(d$P, as.integer(d$n_bk) - 1L), label = name)
    expect_identical(dims$house_drift_raw, c(d$H, d$P, as.integer(d$n_hk) - 1L), label = name)
    expect_identical(as.integer(dims[c("tau_drift", "tau_drift_A", "tau_house_drift", "tau_mu")]), c(1L, 1L, 1L, 0L), label = name)
    # WHY: the 2017 cell's houses all poll in the first house knot's interval, so nothing there
    # tells a house's first knot from knot 1.
    must <- if (all(c(d$house_first_hk) == 1L)) setdiff(drift, "drift_house_from_start") else drift
    for (seed in 1:2) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      lp <- v2_lp_ref(s$pars, d)
      for (mu in must) expect_gt(abs(v2_lp_ref(s$pars, d, mu) - lp), 1e-6, label = paste(name, mu))
      p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
      expect_identical(p$pars$tau_drift, s$pars$tau_drift)
      expect_lt(abs(v2_lp_ref(p$pars, p$data) - lp - attr(p, "ljac_L_Omega")), 1e-10 + 4 * .Machine$double.eps * abs(lp), label = name)
    }
  }
  expect_identical(V2_MUTANTS$switch[startsWith(V2_MUTANTS$mutant, "drift_")], rep("bias_drift", 4))
})

test_that("the drift's knots step by sqrt(years) C(tau .* xi), a newcomer entering at the existing parties' mean plus m_new + s_new eta, and a poll's bias interpolates the knots around its date", {
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_m7(), stage1_stub)$eve2024$data
  s <- v2_simulate(d, seed = 5, observe = FALSE)$pars
  p <- ref$core_pars(s, d)
  nat <- ref$natural(p, d)
  P <- d$P
  C <- function(x) x - mean(x)
  tau <- rep(p$tau_drift, P)
  tau[d$annad_idx] <- p$tau_drift_A
  entry <- c(d$entry_bk)
  m1 <- p$mu1
  q1 <- which(entry == 1L)
  m1[q1] <- mean(p$mu1[entry < 1L]) + p$m_new + p$s_new * p$new_raw[match(q1, which(entry > 0))]
  expect_true(length(q1) >= 1)
  expect_equal(nat$mu_knot[1, ], C(m1), tolerance = 1e-12)
  for (k in 2:d$n_bk) {
    step <- C(sqrt(0.5) * tau * p$mu_drift_raw[, k - 1])
    if (!any(entry == k)) {
      expect_equal(nat$mu_knot[k, ], C(nat$mu_knot[k - 1, ] + step), tolerance = 1e-12, label = k)
    } else {
      m <- nat$mu_knot[k - 1, ] + step
      q <- which(entry == k)
      before <- which(entry < k)
      new <- which(entry > 0)
      want <- m
      want[q] <- mean(m[before]) + p$m_new + p$s_new * p$new_raw[match(q, new)]
      expect_equal(nat$mu_knot[k, ], C(want), tolerance = 1e-12, label = k)
    }
  }
  for (n in c(1, d$N_nat %/% 2, d$N_nat)) {
    t <- d$grid_nat[n]
    h <- d$house_nat[n]
    lb <- d$bk_lo[t]
    lh <- d$hk_lo[t]
    want <- (1 - d$bk_a[t]) * nat$mu_knot[lb, ] + d$bk_a[t] * nat$mu_knot[lb + 1, ] +
      (1 - d$hk_a[t]) * nat$dev_knot[h, lh, ] + d$hk_a[t] * nat$dev_knot[h, lh + 1, ]
    expect_equal(nat$gamma_nat[n, ], want, tolerance = 1e-12, label = n)
  }
  # WHY: mu and gamma report the bias at each epoch's last date: the closed elections, then E.
  t <- d$grid_res[2]
  expect_equal(nat$mu[2, ], (1 - d$bk_a[t]) * nat$mu_knot[d$bk_lo[t], ] + d$bk_a[t] * nat$mu_knot[d$bk_lo[t] + 1, ], tolerance = 1e-12)
  expect_equal(nat$mu[d$n_epoch, ], nat$mu_knot[d$n_bk, ], tolerance = 1e-12)
})

test_that("a house's drift starts after its first knot, and a newcomer's level in a walking house restarts at sigma_gamma eps plus the existing parties' mean drift", {
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_m7(), stage1_stub)$eve2024$data
  p <- ref$core_pars(v2_simulate(d, seed = 6, observe = FALSE)$pars, d)
  nat <- ref$natural(p, d)
  h <- which.max(c(d$house_first_hk))
  f <- d$house_first_hk[h]
  expect_gt(f, 1L)
  base <- p$sigma_gamma * p$house_raw[h, ]
  for (k in seq_len(f)) expect_equal(nat$dev_knot[h, k, ], base - mean(base), tolerance = 1e-12, label = k)
  h1 <- which(c(d$house_first_hk) == 1L)[1]
  entry <- c(d$entry_hk)
  k <- max(entry)
  q <- which(entry == k)
  before <- which(entry < k)
  # WHY: the levels are known up to a common shift, which the centring removes; rebuild them.
  x <- p$sigma_gamma * p$house_raw[h1, ]
  for (j in 2:k) {
    x <- x + sqrt(1) * p$tau_house_drift * p$house_drift_raw[h1, , j - 1]
    if (any(entry == j)) {
      e <- which(entry == j)
      b <- which(entry < j)
      x[e] <- p$sigma_gamma * p$house_raw[h1, e] + mean(x[b] - p$sigma_gamma * p$house_raw[h1, b])
    }
  }
  expect_equal(nat$dev_knot[h1, k, ], x - mean(x), tolerance = 1e-12)
  expect_true(length(q) >= 1 && length(before) >= 1)
})

test_that("the drift reference refuses the epoch walk beside it and malformed knot data", {
  d <- v2_test_cells(cfg_m7(), stage1_stub)$eve2024$data
  s <- v2_simulate(d, seed = 3, observe = FALSE)$pars
  both <- d
  both$epoch_bias <- 1L
  expect_error(v2_lp_ref(s, both), "data\\$bias_drift = 1 replaces the epoch walk, so it needs epoch_bias = 0")
  lo <- d
  lo$bk_lo[1] <- d$n_bk
  expect_error(v2_lp_ref(s, lo), "data\\$bk_lo must be G knots in 1..n_bk - 1")
  a <- d
  a$hk_a[2] <- 1.5
  expect_error(v2_lp_ref(s, a), "data\\$hk_a must be G weights in \\[0, 1\\]")
  e <- d
  e$entry_bk[1] <- 1L
  expect_error(v2_lp_ref(s, e), "data\\$entry_bk must be P knots, positive exactly for the newcomers")
  f <- d
  f$house_first_hk[1] <- d$n_hk + 1L
  expect_error(v2_lp_ref(s, f), "data\\$house_first_hk must be H knots in 1..n_hk")
})

test_that("v2_simulate() with bias_drift = 1 draws the drift last, so every other parameter is bias_drift = 0's draw", {
  d <- v2_test_cells(cfg_m7(), stage1_stub)$eve2024$data
  d0 <- d
  d0$bias_drift <- 0L
  a <- v2_simulate(d, seed = 21, observe = FALSE)$pars
  b <- v2_simulate(d0, seed = 21, observe = FALSE)$pars
  drift <- c("mu_drift_raw", "house_drift_raw", "tau_drift", "tau_drift_A", "tau_house_drift")
  expect_identical(lengths(a[c("tau_drift", "tau_drift_A", "tau_house_drift")]), c(tau_drift = 1L, tau_drift_A = 1L, tau_house_drift = 1L))
  expect_identical(sum(lengths(b[drift])), 0L)
  expect_identical(a[setdiff(names(a), drift)], b[setdiff(names(b), drift)])
  expect_identical(names(a), V2_CORE_PARS$name)
  expect_identical(tail(V2_CORE_PARS$name, 5), drift)
})
