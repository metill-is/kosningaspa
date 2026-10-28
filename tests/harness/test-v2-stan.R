box::use(
  R / harness / v2_reference[
    dirmult_lpmf, permute_v2, V2_CORE_PARS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate
  ]
)

V2_STAN <- "Stan/forecast_v2.stan"

test_that("v2s-m0: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  cells <- v2_test_cells(cfg_m0())
  hit <- character(0)
  for (name in names(cells)) {
    # 2024 eve, 2021 f180: posterior + off; 2017 eve: prior only
    cell <- cells[[name]]
    m <- v2_methods(V2_STAN, cell$data)
    pts <- lp_points(m, cell$data, n_post = cell$n_post, n_off = cell$n_off)
    ok <- expect_lp_matches(m, cell$data, pts)
    ratios <- c()
    for (mu in v2_mutants(cell$data)) {
      if (mutant_exercised(cell$data, pts, mu)) {
        ratios[mu] <- expect_lp_rejects(m, cell$data, pts, mu)
        hit <- c(hit, mu)
      }
    }
    weakest <- if (length(ratios)) names(which.min(ratios)) else "none"
    unexercised <- setdiff(v2_mutants(cell$data), names(ratios))
    message(sprintf(
      "v2s-m0 %s: %d points, lp constant range %.3g (tol %.3g, ratio %.3g); weakest exercised mutant %s; range / (100 tol): %s; not exercised: %s",
      name, length(pts), ok$range, ok$tol, ok$range / ok$tol, weakest,
      paste(sprintf("%s %.3g", names(ratios), ratios), collapse = ", "),
      if (length(unexercised)) paste(unexercised, collapse = ", ") else "none"
    ))
  }
  expect_setequal(unique(hit), v2_mutants(cells$eve2024$data)) # every mutant is exercised on some cell
})
test_that("v2s-m0: positional-gather mutant is rejected", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  # 2024 eve: every poll before 2017-09-21 and the 2016 result omit M and J but report Annað
  cell <- v2_test_cells(cfg_m0())$eve2024
  m <- v2_methods(V2_STAN, cell$data)
  expect_lp_rejects(m, cell$data, lp_points(m, cell$data, cell$n_post, cell$n_off), "positional_gather")
})
test_that("v2s-m0: Stan's log density is permutation-invariant up to the L_Omega Jacobian", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  d <- v2_test_cells(cfg_m0())$eve2024$data
  rp <- rev(seq_len(d$P))
  rk <- rev(seq_len(d$K))
  m <- v2_methods(V2_STAN, d)
  m_p <- v2_methods(V2_STAN, permute_v2(v2_simulate(d, 1, FALSE)$pars, d, rp, rk)$data)
  ratio <- numeric(0)
  for (seed in 1:10) {
    s <- v2_simulate(d, seed = seed, observe = FALSE)
    p <- permute_v2(s$pars, d, rp, rk)
    lp <- m$log_prob(m$unconstrain_variables(s$pars), jacobian = FALSE)
    lp_p <- m_p$log_prob(m_p$unconstrain_variables(p$pars), jacobian = FALSE)
    # WHY: Ruling T3b. The identity is exact algebra; each lp (about -3e5) is a double that
    # Stan sums without compensation, so the floor is a rounding of lp, not of the Jacobian.
    err <- abs((lp_p - lp) - attr(p, "ljac_L_Omega"))
    tol <- 1e-9 + 64 * .Machine$double.eps * abs(lp)
    expect_lt(err, tol)
    ratio[seed] <- err / tol
  }
  message(sprintf("v2s-m0 permutation: worst error / tolerance %.3g (seed %d)", max(ratio), which.max(ratio)))
})
test_that("v2s-m0: bounded concentrations start at their init and stay below their caps", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  d <- v2_test_cells(cfg_m0())$eve2024$data
  m <- v2_methods(V2_STAN, d)
  b0 <- bounded_at(m, d, 0)
  expect_equal(b0$log_phi, rep(mean(log(rowSums(d$y_nat))), d$H))
  expect_equal(b0$log_phi_K, log(1000))
  expect_equal(b0$log_kappa, 0)
  for (u in c(-30, 30)) {
    b <- bounded_at(m, d, u)
    expect_true(all(b$log_phi < 20) && b$log_phi_K < 20 && b$log_kappa < 2)
  }
})
test_that("v2s-m0: v2_methods attaches methods to a 2017 cell without drawing a forecast", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  m <- v2_methods(V2_STAN, v2_test_cells(cfg_m0())$eve2017$data)
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% m$metadata()$stan_variables))
})

# The tests below go beyond the brief's: they pin the declarations against Ruling T3a, the
# shapes cmdstanr gives the methods, and the transformed parameters the extractor reads.

test_that("v2s-m0: the parameters are V2_CORE_PARS, in its order, with its dims and free sizes", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  for (cell in v2_test_cells(cfg_m0())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    expect_identical(names(v2_model(V2_STAN)$variables()$parameters), V2_CORE_PARS$name)
    dims <- v2_par_dims(d)
    sizes <- m$metadata()$stan_variable_sizes
    present <- names(dims)[vapply(dims, prod, numeric(1)) > 0]
    expect_identical(lapply(sizes[present], as.integer), lapply(dims[present], as.integer))
    # WHY: the free sizes pin the types: P - 1 for a sum-to-zero vector, P (P - 1) / 2 for L_Omega.
    P <- d$P
    free <- (P - 1) + (P - 1) * (d$G - 1) + P + P * (P - 1) / 2 + 1 + (P - 1) + d$H * P + 1 +
      sum(c(d$entry_epoch) > 0) + d$H + 4 + d$K * P + P + 2 + d$J * (P - 1)
    expect_identical(as.numeric(n_unc(m)), free)
    # WHY: cmdstanr returns a matrix as a matrix, but utils::relist() drops the dims of the
    # 1 x K x P delta_raw, which comes back flat in column-major order; the reference reads it so.
    s <- v2_simulate(d, seed = 1, observe = FALSE)
    pars <- stan_pars(m, m$unconstrain_variables(s$pars))
    expect_null(dim(pars$delta_raw))
    expect_equal(pars$delta_raw, c(s$pars$delta_raw), tolerance = 1e-12)
    expect_identical(dim(pars$nu), c(d$J, P))
    expect_equal(pars$nu, s$pars$nu, tolerance = 1e-12)
  }
})
test_that("v2s-m0: the transformed parameters are the reference's natural quantities", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  for (cell in v2_test_cells(cfg_m0())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      for (n in c("theta", "mu", "gamma", "delta", "s_delta0", "log_phi")) {
        expect_equal(array(tp[[n]], dim(as.array(ref[[n]]))), as.array(ref[[n]]), tolerance = 1e-10, label = n)
      }
      expect_equal(tp$log_phi_K, ref$log_phi_K, tolerance = 1e-12)
      expect_equal(tp$log_kappa, ref$log_kappa, tolerance = 1e-12)
    }
  }
})
test_that("v2s-m0: the generated quantities are the reference's at a prior point", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_m0())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    # The 2017 cell keeps its draw_forecast = 0; latent_path = 1 writes pi_latent everywhere.
    r <- gq_at(V2_STAN, utils::modifyList(d, list(latent_path = 1L)), v2_simulate(d, seed = 2, observe = FALSE)$pars)
    p <- ref$core_pars(r$pars, d)
    nat <- ref$natural(p, d)
    rows <- ref$dm_rows(p, nat, d)
    expect_equal(r$gq$log_lik, vapply(rows, \(x) dirmult_lpmf(ref$row_counts(d, x), x$alpha), numeric(1)), tolerance = 1e-10)
    rep <- r$gq$y_rep_res
    expect_equal(apply(rep, 1:2, sum), apply(d$y_res, 1:2, sum) + 0)
    for (e in seq_len(d$n_closed)) expect_true(all(rep[e, , -d$idx_res[e, seq_len(d$n_rep_res[e])]] == 0))
    # WHY: no forecast quantity is printed, even on failure, so these compare by isTRUE(all.equal()).
    pi <- vapply(seq_len(d$G), \(t) {
      e <- d$grid_epoch[t]
      A <- which(d$exists[t, ] == 1L)
      out <- numeric(d$P)
      for (k in seq_len(d$K)) out[A] <- out[A] + d$w[e, k] * ref$softmax(nat$theta[A, t] + nat$delta[e, k, A])
      out
    }, numeric(d$P))
    expect_true(isTRUE(all.equal(r$gq$pi_latent, pi, tolerance = 1e-10)))
    if (d$draw_forecast == 1L) {
      v <- r$gq$votes_kd
      expect_true(isTRUE(all.equal(rowSums(v), c(d$N_open))))
      expect_true(isTRUE(all.equal(c(r$gq$pi_nat_eday), colSums(v) / sum(d$N_open), tolerance = 1e-12)))
      expect_true(isTRUE(all.equal(c(r$gq$pi_latent_E), pi[, d$G], tolerance = 1e-10)))
    } else {
      expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r$gq)))
    }
  }
})
