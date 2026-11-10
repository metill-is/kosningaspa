box::use(
  R / harness / v2_reference[
    campaign_steps, dirmult_lpmf, permute_v2, V2_CORE_PARS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate
  ]
)

# WHY: a planted fault's RED runs these tests on a scratch copy of the Stan file, named by
# HARNESS_V2_STAN as an absolute path; unset, they test the repo's file and print nothing.
# Set, the file must exist, and it is announced with its sha256 so that every log, the
# queue's preflight.log included, records the switch (queue.sh refuses to start with it set).
V2_STAN <- local({
  f <- Sys.getenv("HARNESS_V2_STAN")
  if (!nzchar(f)) {
    return("Stan/forecast_v2.stan")
  }
  if (!startsWith(f, "/") || !file.exists(f)) {
    stop("HARNESS_V2_STAN must be the absolute path of an existing Stan file: ", f, call. = FALSE)
  }
  message("HARNESS_V2_STAN: the Stan tests run on ", f, " (sha256 ", digest::digest(file = f, algo = "sha256"), ")")
  f
})

# The lp comparison on the cells of v2_test_cells(cfg): on each cell Stan's log density minus
# the reference's is constant over the cell's points (2024 eve and 2021 f180: posterior and
# off-posterior points; 2017 eve: prior points only), each mutant of v2_mutants() that the
# cell exercises is rejected, and every mutant of the 2024-eve cell is exercised on some
# cell. Returns the mutants exercised, invisibly, with attribute `by_cell`: each cell's.
expect_cells_lp <- function(cfg) {
  cells <- v2_test_cells(cfg)
  hit <- character(0)
  by_cell <- list()
  for (name in names(cells)) {
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
    by_cell[[name]] <- names(ratios)
    weakest <- if (length(ratios)) names(which.min(ratios)) else "none"
    unexercised <- setdiff(v2_mutants(cell$data), names(ratios))
    by_kind <- paste(vapply(names(ok$kind), \(k) sprintf("%s %.3g", k, ok$kind[[k]][["range"]] / ok$kind[[k]][["tol"]]), ""), collapse = ", ")
    message(sprintf(
      "lp %s: %d points, lp constant range %.3g (tol %.3g, ratio %.3g; ratio by kind: %s); weakest exercised mutant %s; range / (100 tol): %s; not exercised: %s",
      name, length(pts), ok$range, ok$tol, ok$range / ok$tol, by_kind, weakest,
      paste(sprintf("%s %.3g", names(ratios), ratios), collapse = ", "),
      if (length(unexercised)) paste(unexercised, collapse = ", ") else "none"
    ))
  }
  expect_setequal(unique(hit), v2_mutants(cells$eve2024$data)) # every mutant is exercised on some cell
  invisible(structure(unique(hit), by_cell = by_cell))
}

# Prior points (v2_simulate() at seed + i) on `data` for the v2_methods() fit `m`, each with
# `tau_camp` set to one of `tau`, in the form lp_points() gives.
prior_points_at_tau <- function(m, data, tau, seed) {
  lapply(seq_along(tau), \(i) {
    pars <- v2_simulate(data, seed = seed + i, observe = FALSE)$pars
    pars$tau_camp <- tau[i]
    u <- m$unconstrain_variables(pars)
    list(u = u, pars = stan_pars(m, u), kind = "prior")
  })
}

test_that("the cells carry stage 1's mu_f over parties existing at E, and 2017 eve the placeholder's zeros", {
  skip_unless_slow()
  cells <- v2_test_cells(cfg_v2())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_true(d$n_sf >= 2L && length(d$sf_idx) == d$n_sf && length(d$mu_f) == d$n_sf, label = name)
    expect_true(all(d$exists[d$G, c(d$sf_idx)] == 1L), label = name)
  }
  expect_true(all(c(cells$eve2017$data$mu_f) == 0))
  # WHY: the mutant fund_uncentred and the term's slope in mu_f need values away from 0 and unequal.
  for (name in c("eve2024", "f180_2021")) {
    mf <- c(cells[[name]]$data$mu_f)
    expect_true(isTRUE(all(mf != 0) && !anyDuplicated(mf)), label = name)
  }
})

test_that("Stan's log density matches the reference up to a constant, and each mutant is rejected where it is exercised", {
  skip_unless_slow()
  hit <- expect_cells_lp(cfg_v2())
  by_cell <- attr(hit, "by_cell")
  expect_identical(names(by_cell), c("eve2024", "f180_2021", "eve2017"))
  # WHY: 2024 eve's polls before 2017-09-21 and its 2016 result omit M and J but report
  # Annad, so a positional gather reads the wrong parties there. Every cell has a step leaving
  # a closed election, and mean(log pi[S_f]) is negative for any share vector, so the leaving
  # step's multiplier and the uncentred mean differ on every cell, 2017 eve's (mu_f 0) too.
  expect_true("positional_gather" %in% by_cell$eve2024)
  for (name in names(by_cell)) {
    expect_true(all(c("no_leaving_step", "fund_uncentred") %in% by_cell[[name]]), label = name)
  }
})

test_that("the campaign multiplier scales the step leaving each closed election, with tau_camp away from 0", {
  skip_unless_slow()
  # WHY: the rule shows only where a leaving step is not a campaign step already, and only in
  # proportion to tau_camp; at points with tau_camp from 0.25 to 3 the no_leaving_step mutant
  # must be told apart from Stan on both cells with a forecast.
  tau <- c(0.25, 0.5, 1, 2, 3)
  cells <- v2_test_cells(cfg_v2())
  for (name in c("eve2024", "f180_2021")) {
    d <- cells[[name]]$data
    leaving <- as.integer(c(d$grid_res))
    expect_true(all(c(d$campaign)[leaving] == 0L), label = paste(name, "leaving steps outside the campaign flags"))
    expect_identical(which(campaign_steps(d) != c(d$campaign)), sort(leaving), label = name)
    m <- v2_methods(V2_STAN, d)
    pts <- prior_points_at_tau(m, d, tau, seed = 6100)
    ok <- expect_lp_matches(m, d, pts)
    ratio <- expect_lp_rejects(m, d, pts, "no_leaving_step")
    message(sprintf(
      "leaving step %s: %d prior points at tau_camp %s, lp constant range %.3g (tol %.3g); no_leaving_step range / (100 tol) %.3g",
      name, length(pts), paste(tau, collapse = "/"), ok$range, ok$tol, ratio
    ))
  }
})

test_that("with desired_weight 0.3, weight_time 150 and campaign_days 40, Stan's log density matches the reference, and the config's values would not", {
  skip_unless_slow()
  # WHY: the other tests run at the config's values, where literals in Stan would pass. Stan
  # reads campaign_days in the fundamentals term alone (the campaign flags are the builder's),
  # so the data can change it here without rebuilding.
  d <- v2_test_cells(cfg_v2())$eve2024$data
  cfg <- list(desired_weight = d$desired_weight, weight_time = d$weight_time, campaign_days = as.integer(d$campaign_days))
  new <- list(desired_weight = 0.3, weight_time = 150, campaign_days = 40L)
  for (n in names(new)) expect_false(isTRUE(all.equal(cfg[[n]], new[[n]])), label = paste(n, "differs from the config's"))
  d <- utils::modifyList(d, new)
  m <- v2_methods(V2_STAN, d)
  pts <- lp_points(m, d, n_post = 0, n_off = 0, n_prior = 10)
  ok <- expect_lp_matches(m, d, pts)
  slips <- vapply(names(cfg), \(n) lp_range(lp_gap(m, utils::modifyList(d, cfg[n]), pts)$gap), numeric(1))
  names(slips) <- paste0("config_", names(cfg))
  for (n in names(slips)) expect_gt(slips[[n]], 100 * ok$tol, label = n)
  message(sprintf(
    "fundamentals eve2024 at desired_weight 0.3, weight_time 150, campaign_days 40: %d prior points, lp constant range %.3g (tol %.3g); range / (100 tol): %s",
    length(pts), ok$range, ok$tol, paste(sprintf("%s %.3g", names(slips), slips / (100 * ok$tol)), collapse = ", ")
  ))
})

test_that("Stan's log density is permutation-invariant up to the L_Omega Jacobian", {
  skip_unless_slow()
  d <- v2_test_cells(cfg_v2())$eve2024$data
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
    # WHY: the identity is exact algebra, so only rounding separates the two sides; each lp
    # (about -3e5) is a double that Stan sums without compensation, so the floor scales with lp.
    err <- abs((lp_p - lp) - attr(p, "ljac_L_Omega"))
    tol <- 1e-9 + 64 * .Machine$double.eps * abs(lp)
    expect_lt(err, tol)
    ratio[seed] <- err / tol
  }
  message(sprintf("permutation: worst error / tolerance %.3g (seed %d)", max(ratio), which.max(ratio)))
})

test_that("bounded concentrations start at their init and stay below their caps", {
  skip_unless_slow()
  d <- v2_test_cells(cfg_v2())$eve2024$data
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

test_that("v2_methods attaches methods to a 2017 cell without drawing a forecast", {
  skip_unless_slow()
  m <- v2_methods(V2_STAN, v2_test_cells(cfg_v2())$eve2017$data)
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% m$metadata()$stan_variables))
})

# The tests below pin what the lp comparison cannot see: the declarations against
# V2_CORE_PARS, the shapes cmdstanr gives the methods, the transformed parameters the
# extractor reads, and the generated quantities.

test_that("the parameters are V2_CORE_PARS, in its order, with its dims and free sizes", {
  skip_unless_slow()
  for (cell in v2_test_cells(cfg_v2())) {
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

test_that("the transformed parameters are the reference's natural quantities", {
  skip_unless_slow()
  for (cell in v2_test_cells(cfg_v2())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      for (n in c("theta", "mu", "gamma", "delta", "s_delta0", "log_phi")) {
        dims <- dim(as.array(ref[[n]]))
        # WHY: array() recycles or truncates a vector of the wrong length without a word.
        expect_identical(length(tp[[n]]), as.integer(prod(dims)), label = paste("length of", n))
        expect_equal(array(tp[[n]], dims), as.array(ref[[n]]), tolerance = 1e-10, label = n)
      }
      expect_equal(tp$log_phi_K, ref$log_phi_K, tolerance = 1e-12)
      expect_equal(tp$log_kappa, ref$log_kappa, tolerance = 1e-12)
    }
  }
})

test_that("the generated quantities are the reference's at a prior point", {
  skip_unless_slow()
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_v2())
  # WHY: the forecast branch below runs only where draw_forecast is 1, so a builder that
  # stopped setting it would skip the branch without a failure.
  expect_identical(
    vapply(cells, \(cell) as.integer(cell$data$draw_forecast), integer(1)),
    c(eve2024 = 1L, f180_2021 = 1L, eve2017 = 0L)
  )
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$latent_path), 0L)
    # The 2017 cell keeps the data's flags (draw_forecast = 0, latent_path = 0); on the
    # others latent_path = 1 writes pi_latent on the whole grid.
    latent <- name != "eve2017"
    if (latent) d$latent_path <- 1L
    r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 2, observe = FALSE)$pars)
    p <- ref$core_pars(r$pars, d)
    nat <- ref$natural(p, d)
    rows <- ref$dm_rows(p, nat, d)
    expect_equal(r$gq$log_lik, vapply(rows, \(x) dirmult_lpmf(ref$row_counts(d, x), x$alpha), numeric(1)), tolerance = 1e-10)
    rep <- r$gq$y_rep_res
    expect_equal(apply(rep, 1:2, sum), apply(d$y_res, 1:2, sum) + 0)
    for (e in seq_len(d$n_closed)) expect_true(all(rep[e, , -d$idx_res[e, seq_len(d$n_rep_res[e])]] == 0))
    # WHY: no forecast quantity is printed, even on failure, so these compare by isTRUE(all.equal());
    # on the 2017 cell no latent share is computed at all.
    pi <- if (latent || d$draw_forecast == 1L) {
      vapply(seq_len(d$G), \(t) {
        e <- d$grid_epoch[t]
        A <- which(d$exists[t, ] == 1L)
        out <- numeric(d$P)
        for (k in seq_len(d$K)) out[A] <- out[A] + d$w[e, k] * ref$softmax(nat$theta[A, t] + nat$delta[e, k, A])
        out
      }, numeric(d$P))
    }
    if (latent) {
      expect_true(isTRUE(all.equal(r$gq$pi_latent, pi, tolerance = 1e-10)))
    } else {
      expect_false("pi_latent" %in% names(r$gq))
    }
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

test_that("no forecast draw precedes the replicates: draw_forecast leaves log_lik and y_rep_res unchanged", {
  skip_unless_slow()
  # WHY: a forecast draw gated on draw_forecast and placed before the replicates would move
  # y_rep_res's RNG stream against draw_forecast = 0. Nothing below prints a forecast draw.
  d <- v2_test_cells(cfg_v2())$eve2024$data
  expect_identical(as.integer(d$draw_forecast), 1L)
  pars <- v2_simulate(d, seed = 3, observe = FALSE)$pars
  r1 <- gq_at(V2_STAN, d, pars)
  d0 <- d
  d0$draw_forecast <- 0L
  r0 <- gq_at(V2_STAN, d0, pars)
  expect_true("votes_kd" %in% names(r1$gq))
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r0$gq)))
  expect_identical(r0$gq$log_lik, r1$gq$log_lik)
  expect_identical(r0$gq$y_rep_res, r1$gq$y_rep_res)
})

test_that("votes_kd is Dirichlet-multinomial with concentration phi_K at a prior point", {
  skip_unless_slow()
  # WHY: the point is a prior draw, so no poll enters it, and nothing below prints a draw: each
  # check is a scalar compared inside isTRUE().
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_v2())$eve2024$data
  expect_identical(as.integer(d$draw_forecast), 1L)
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  # log phi_K = cap - (cap - init) exp(u), cap 20 and init log(1000): u sets phi_K to 30
  pars$log_phi_K_u <- log((20 - log(30)) / (20 - log(1000)))
  # WHY: u = 0 puts every house's log phi at its init, the mean log national poll size, so no
  # house's phi lies near phi_K by the draw's chance; the houses do not enter votes_kd.
  pars$log_phi_u[] <- 0
  nat <- v2_natural(pars, d)
  phi_K <- nat$phi_K
  expect_equal(phi_K, 30, tolerance = 1e-10)
  # WHY: the inflation (N + phi) / (1 + phi) is nearly N / phi, so a house's phi (or 1) in place
  # of phi_K moves it by their ratio; at least 10 here, far outside the 10% tolerance below.
  expect_true(isTRUE(min(abs(log(nat$phi / phi_K))) > log(10)))
  n <- 4000
  v <- gq_draws(V2_STAN, d, pars, "votes_kd", n)
  expect_identical(dim(v), as.integer(c(n, d$K, d$P)))
  A <- which(d$exists[d$G, ] == 1L)
  e <- d$grid_epoch[d$G]
  N <- c(d$N_open)
  s <- r <- inflation <- z <- matrix(NA_real_, d$K, length(A))
  for (k in seq_len(d$K)) {
    s[k, ] <- ref$softmax(nat$theta[A, d$G] + nat$delta[e, k, A])
    for (j in seq_along(A)) {
      x <- v[, k, A[j]]
      r[k, j] <- stats::var(x) / (N[k] * s[k, j] * (1 - s[k, j]))
      inflation[k, j] <- (N[k] + phi_K) / (1 + phi_K)
      z[k, j] <- (mean(x) / N[k] - s[k, j]) / (stats::sd(x) / N[k] / sqrt(n))
    }
  }
  # WHY: a share near 0 makes the variance of its count, and so r, noisy in 4000 draws.
  big <- s > 0.02
  expect_true(isTRUE(sum(big) > d$K))
  expect_true(isTRUE(abs(stats::median(r[big]) / stats::median(inflation[big]) - 1) < 0.1))
  expect_true(isTRUE(max(abs(z[big])) < 5))
})

test_that("at a prior point the pseudo-observation's mean is the centred log of pi_latent_E over S_f", {
  skip_unless_slow()
  # WHY: spec 6.5's pi_nat,E is spec 6.8's latent share at E, which Stan writes as pi_latent_E; the
  # reference computes it apart, so this ties the term's mean to Stan's own theta_E, open-epoch
  # delta and weights.
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_v2())
  for (name in c("eve2024", "f180_2021")) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$draw_forecast), 1L)
    r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 4, observe = FALSE)$pars)
    p <- ref$core_pars(r$pars, d)
    nat <- ref$natural(p, d)
    x <- log(c(r$gq$pi_latent_E)[c(d$sf_idx)])
    expect_true(isTRUE(all.equal(ref$fund_normal(p, nat, d)$mean, x - mean(x), tolerance = 1e-10)), label = name)
    expect_true(isTRUE(all.equal(ref$fund_normal(p, nat, d, uncentred = TRUE)$mean, x, tolerance = 1e-10)), label = name)
    expect_false(isTRUE(all.equal(x - mean(x), x, tolerance = 1e-3)), label = name)
  }
})

test_that("transformed data refuses fundamentals it cannot evaluate, naming the condition", {
  skip_unless_slow()
  d <- v2_test_cells(cfg_v2())$eve2024$data
  # A party of S_f that does not exist at E: its latent share there is 0.
  absent <- d
  absent$exists[d$G, d$sf_idx[2]] <- 0L
  expect_error(
    v2_methods(V2_STAN, absent),
    paste0(
      "forecast_v2: the fundamentals need every party of S_f to exist at E (exists[G, sf_idx[i]] = 1), but sf_idx[2] = ",
      d$sf_idx[2], " does not"
    ),
    fixed = TRUE
  )
  one <- d
  one$n_sf <- 1L
  one$sf_idx <- array(d$sf_idx[1], 1)
  one$mu_f <- array(d$mu_f[1], 1)
  expect_error(
    v2_methods(V2_STAN, one),
    "forecast_v2: the fundamentals need at least two parties of S_f, since mu_f's mean is centred over them, but n_sf = 1",
    fixed = TRUE
  )
  # A party of S_f twice: mean(x) and the normal would count it twice.
  dup <- d
  dup$sf_idx[2] <- d$sf_idx[1]
  expect_error(
    v2_methods(V2_STAN, dup),
    paste0("forecast_v2: the fundamentals need the parties of S_f to be distinct, but sf_idx[1] = sf_idx[2] = ", d$sf_idx[1]),
    fixed = TRUE
  )
  # w at 1 gives tau_f = 0, and at 0 an infinite tau_f; the data block admits both.
  for (w in c(1, 0)) {
    expect_error(
      v2_methods(V2_STAN, utils::modifyList(d, list(desired_weight = w))),
      paste0("forecast_v2: the fundamentals need desired_weight in (0, 1), since tau_f = sqrt(V_w (1 - w) / w), but desired_weight = ", w),
      fixed = TRUE
    )
  }
  short <- d
  short$weight_time <- 30
  expect_error(
    v2_methods(V2_STAN, short),
    paste0(
      "forecast_v2: the fundamentals need weight_time to be at least campaign_days, since V_w counts weight_time - campaign_days days off the campaign, but weight_time = 30 and campaign_days = ",
      d$campaign_days
    ),
    fixed = TRUE
  )
})

test_that("the matrix likelihood's transformed data refuses a decreasing grid_epoch, and date G outside the open epoch", {
  # WHY: the likelihood reads each epoch's poll dates as one contiguous block of columns, which
  # holds only when grid_epoch never decreases and the last date is in the open epoch.
  skip_unless_slow()
  d <- v2_test_cells(cfg_v2())$eve2024$data
  t2 <- match(2L, d$grid_epoch)
  dec <- d
  dec$grid_epoch[t2 + 1] <- 1L
  expect_error(
    v2_methods(V2_STAN, dec),
    paste0("forecast_v2: the matrix likelihood needs grid_epoch non-decreasing, but grid_epoch[", t2 + 1, "] = 1 < grid_epoch[", t2, "] = 2"),
    fixed = TRUE
  )
  shut <- d
  shut$grid_epoch[shut$grid_epoch == d$n_epoch] <- d$n_epoch - 1L
  expect_error(
    v2_methods(V2_STAN, shut),
    paste0("forecast_v2: the matrix likelihood needs date G in the open epoch, but grid_epoch[G] = ", d$n_epoch - 1L, " and n_epoch = ", d$n_epoch),
    fixed = TRUE
  )
})
