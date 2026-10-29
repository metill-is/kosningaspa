box::use(
  R / harness / v2[v2_stan_data],
  R / harness / v2_reference[
    dirmult_lpmf, permute_v2, V2_CORE_PARS, v2_lp_ref, v2_mutants, v2_natural, v2_par_dims, v2_simulate
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

# A step's lp test (the step protocol's Step B) on the cells of v2_test_cells(cfg), whose
# walk and delta forms are the reference forms: on each cell Stan's log density minus the
# reference's is constant over the cell's points (2024 eve and 2021 f180: posterior and
# off-posterior points; 2017 eve: prior points only), each mutant of v2_mutants() that the
# cell exercises is rejected, and every mutant of the 2024-eve cell is exercised on some
# cell. Returns the mutants exercised, invisibly, with attribute `by_cell`: each cell's.
expect_step_lp <- function(step, cfg) {
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
      "%s %s: %d points, lp constant range %.3g (tol %.3g, ratio %.3g; ratio by kind: %s); weakest exercised mutant %s; range / (100 tol): %s; not exercised: %s",
      step, name, length(pts), ok$range, ok$tol, ok$range / ok$tol, by_kind, weakest,
      paste(sprintf("%s %.3g", names(ratios), ratios), collapse = ", "),
      if (length(unexercised)) paste(unexercised, collapse = ", ") else "none"
    ))
  }
  expect_setequal(unique(hit), v2_mutants(cells$eve2024$data)) # every mutant is exercised on some cell
  invisible(structure(unique(hit), by_cell = by_cell))
}

test_that("v2s-m0: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  expect_step_lp("v2s-m0", cfg_m0())
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
    # WHY: the identity is exact algebra, so only rounding separates the two sides; each lp
    # (about -3e5) is a double that Stan sums without compensation, so the floor scales with lp.
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
test_that("v2s-m0: v2_methods stops on each unbuilt switch with CmdStan's text naming it", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  d <- v2_test_cells(cfg_m0())$eve2024$data
  for (sw in c("fundamentals", "delta_form")) {
    d1 <- d
    d1[[sw]] <- 1L
    expect_error(v2_methods(V2_STAN, d1), paste0("forecast_v2: switch ", sw, " = 1 is not built yet"), fixed = TRUE)
  }
})
test_that("v2s-m0: votes_kd is Dirichlet-multinomial with concentration phi_K at a prior point", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  # WHY: the point is a prior draw, so no poll enters it, and nothing below prints a draw: each
  # check is a scalar compared inside isTRUE().
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_m0())$eve2024$data
  expect_identical(as.integer(d$draw_forecast), 1L)
  pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  # log phi_K = cap - (cap - init) exp(u), cap 20 and init log(1000): u sets phi_K to 30
  pars$log_phi_K_u <- log((20 - log(30)) / (20 - log(1000)))
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

# The tests below pin what the lp comparison cannot see: the declarations against
# V2_CORE_PARS, the shapes cmdstanr gives the methods, the transformed parameters the
# extractor reads, and the generated quantities.

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
test_that("v2s-m0: the generated quantities are the reference's at a prior point", {
  skip_unless_slow()
  skip_unless_step("v2s-m0")
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_m0())
  # WHY: the forecast branch below runs only where draw_forecast is 1, so a builder that
  # stopped setting it would skip the branch without a failure.
  expect_identical(
    vapply(cells, \(cell) as.integer(cell$data$draw_forecast), integer(1)),
    c(eve2024 = 1L, f180_2021 = 1L, eve2017 = 0L)
  )
  for (name in names(cells)) {
    d <- cells[[name]]$data
    expect_identical(as.integer(d$latent_path), 0L)
    # The 2017 cell keeps the data's switches (draw_forecast = 0, latent_path = 0); on the
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

# The cell's data with the anchored walk (walk_form = 1, step v2s-p1); nothing else in the
# data depends on the switch.
p1_data <- function(d) {
  d$walk_form <- 1L
  d
}

# A point of the anchored walk on `d1` (p1_data()): a prior draw of the reference walk's
# parameters at `seed`, its anchor_w and z_walk replaced by N(0, 1) draws of the anchored
# walk's sizes; named and ordered as v2_par_dims(d1).
p1_point <- function(d1, seed) {
  d0 <- d1
  d0$walk_form <- 0L
  pars <- v2_simulate(d0, seed = seed, observe = FALSE)$pars
  dims <- v2_par_dims(d1)
  withr::with_seed(seed, {
    pars$anchor_w <- array(stats::rnorm(prod(dims$anchor_w)), dims$anchor_w)
    pars$z_walk <- array(stats::rnorm(prod(dims$z_walk)), dims$z_walk)
  })
  pars[names(dims)]
}

test_that("v2s-p1: transformed data refuses an anchored walk it cannot build, naming the condition", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  d <- p1_data(v2_test_cells(cfg_m0())$eve2024$data)
  g <- as.integer(c(d$grid_res))
  d1 <- d
  d1$grid_res[1] <- 1L
  expect_error(v2_methods(V2_STAN, d1), "forecast_v2: walk_form = 1 needs every grid_res in 2..G, but grid_res[1] = 1", fixed = TRUE)
  d1 <- d
  d1$grid_res[2] <- g[1]
  expect_error(
    v2_methods(V2_STAN, d1),
    paste0("forecast_v2: walk_form = 1 needs grid_res strictly increasing, but grid_res[2] = ", g[1], " follows ", g[1]),
    fixed = TRUE
  )
  d1 <- d
  d1$y_res[1, , 1] <- 0L
  expect_error(
    v2_methods(V2_STAN, d1),
    "forecast_v2: walk_form = 1 needs the coordinate party (index 1) to have national votes at every closed election, but it has none at closed election 1",
    fixed = TRUE
  )
  # WHY: party 1 missing from the result's parties while its votes stay is the other half of the condition.
  d1 <- d
  rest <- setdiff(as.integer(d$idx_res[1, seq_len(d$n_rep_res[1])]), 1L)
  expect_gte(length(rest), 2L)
  d1$idx_res[1, ] <- c(rest, rep(0L, d$P - length(rest)))
  d1$n_rep_res[1] <- length(rest)
  expect_error(
    v2_methods(V2_STAN, d1),
    "forecast_v2: walk_form = 1 needs the coordinate party (index 1) to have national votes at every closed election, but it has none at closed election 1",
    fixed = TRUE
  )
})

test_that("v2s-p1: the anchored walk's parameters have v2_par_dims()' sizes and the reference walk's free size", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  for (cell in v2_test_cells(cfg_m0())) {
    d <- p1_data(cell$data)
    m <- v2_methods(V2_STAN, d)
    dims <- v2_par_dims(d)
    expect_identical(names(dims), V2_CORE_PARS$name)
    expect_identical(as.integer(dims$anchor_w), as.integer(c(d$n_closed, d$P - 1)))
    expect_identical(as.integer(dims$z_walk), as.integer(c(d$P - 1, d$G - 1 - d$n_closed)))
    sizes <- m$metadata()$stan_variable_sizes
    present <- names(dims)[vapply(dims, prod, numeric(1)) > 0]
    expect_true("anchor_w" %in% present)
    expect_identical(lapply(sizes[present], as.integer), lapply(dims[present], as.integer))
    # WHY: the anchors take n_closed of z_walk's columns, so the free size is the reference walk's.
    expect_identical(n_unc(m), n_unc(v2_methods(V2_STAN, cell$data)))
  }
})

test_that("v2s-p1: with the present levels at 0 each anchor is at its result, whatever the absent parties do", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  softmax <- function(x) exp(x - max(x)) / sum(exp(x - max(x)))
  for (cell in v2_test_cells(cfg_m0())[c("eve2024", "f180_2021")]) {
    d <- p1_data(cell$data)
    m <- v2_methods(V2_STAN, d)
    P <- d$P
    theta_at <- function(pars) {
      tp <- m$constrain_variables(m$unconstrain_variables(pars), transformed_parameters = TRUE, generated_quantities = FALSE)
      matrix(tp$theta, P, d$G)
    }
    idx <- matrix(as.integer(d$idx_res), d$n_closed, P)
    present <- lapply(seq_len(d$n_closed), \(j) idx[j, seq_len(d$n_rep_res[j])])
    pars <- p1_point(d, seed = 1)
    for (j in seq_len(d$n_closed)) pars$anchor_w[j, setdiff(present[[j]], 1L) - 1L] <- 0
    moved <- pars
    for (j in seq_len(d$n_closed)) {
      absent <- setdiff(seq_len(P - 1), present[[j]] - 1L)
      moved$anchor_w[j, absent] <- pars$anchor_w[j, absent] + 1
    }
    th0 <- theta_at(pars)
    th1 <- theta_at(moved)
    for (j in seq_len(d$n_closed)) {
      s <- present[[j]]
      a <- as.integer(d$grid_res[j])
      votes <- colSums(matrix(d$y_res[j, , ], d$K, P))[s]
      expect_equal(softmax(th0[s, a]), votes / sum(votes), tolerance = 1e-12, label = paste("anchor", j))
      expect_equal(softmax(th1[s, a]), votes / sum(votes), tolerance = 1e-12, label = paste("moved anchor", j))
      if (length(s) < P) expect_gt(max(abs(th1[-s, a] - th0[-s, a])), 1e-6, label = paste("absent parties at anchor", j))
    }
  }
})

# Step v2s-m1, the epoch bias (§6.4, switch epoch_bias): the industry bias walks across
# epochs, each house's deviations walk after its first epoch, and a newcomer restarts at its
# entry epoch.
EPOCH_PARS <- c("mu_step_raw", "house_step_raw", "tau_mu", "tau_mu_A", "tau_house")

test_that("v2s-m1: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  hit <- expect_step_lp("v2s-m1", cfg_m1())
  expect_true(all(c("house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart", "no_house_restart") %in% hit))
})

test_that("v2s-m1: with Annad first and prior_tau_mu at 0.37, Stan's log density matches the reference", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  # WHY: the other cells put Annað last and run at §6.10's prior_tau_mu = 0.1, where Stan
  # scaling party P instead of annad_idx, or a literal 0.1 for the data scale, would pass.
  cfg <- utils::modifyList(cfg_m1(), list(order = "reversed", walk_form = 0L, delta_form = 0L, prior_tau_mu = 0.37))
  d <- v2_stan_data(info_2024eve(), cfg)$data
  expect_identical(d$annad_idx, 1L)
  expect_identical(d$prior_tau_mu, 0.37)
  m <- v2_methods(V2_STAN, d)
  pts <- lp_points(m, d, n_post = 0, n_off = 0, n_prior = 10)
  ok <- expect_lp_matches(m, d, pts)
  # Each slip, planted in the reference's data, is told apart from Stan on these points.
  last <- d
  last$annad_idx <- d$P
  literal <- d
  literal$prior_tau_mu <- 0.1
  slips <- c(annad_last = lp_range(lp_gap(m, last, pts)$gap), literal_0.1 = lp_range(lp_gap(m, literal, pts)$gap))
  for (n in names(slips)) expect_gt(slips[[n]], 100 * ok$tol, label = n)
  message(sprintf(
    "v2s-m1 reversed eve2024 at prior_tau_mu 0.37: %d prior points, lp constant range %.3g (tol %.3g); range / (100 tol): %s",
    length(pts), ok$range, ok$tol, paste(sprintf("%s %.3g", names(slips), slips / (100 * ok$tol)), collapse = ", ")
  ))
})

test_that("v2s-m1: the epoch bias's parameters have v2_par_dims()' sizes, and size 0 with epoch_bias = 0", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  for (cell in v2_test_cells(cfg_m1())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    expect_identical(names(v2_model(V2_STAN)$variables()$parameters), V2_CORE_PARS$name)
    dims <- v2_par_dims(d)
    n_step <- d$n_epoch - 1L
    expect_identical(lapply(dims[EPOCH_PARS], as.integer), list(
      mu_step_raw = c(d$P, n_step), house_step_raw = c(d$H, d$P, n_step), tau_mu = 1L, tau_mu_A = 1L, tau_house = 1L
    ))
    sizes <- m$metadata()$stan_variable_sizes
    expect_identical(lapply(sizes[EPOCH_PARS], as.integer), lapply(dims[EPOCH_PARS], as.integer))
    d0 <- d
    d0$epoch_bias <- 0L
    expect_true(all(vapply(v2_par_dims(d0)[EPOCH_PARS], prod, numeric(1)) == 0))
    # WHY: every new parameter is unconstrained but the three scales, each one free element.
    expect_identical(as.numeric(n_unc(m) - n_unc(v2_methods(V2_STAN, d0))), as.numeric((d$H + 1) * d$P * n_step + 3))
    # WHY: utils::relist() drops the dims of the H x P x (n_epoch - 1) house_step_raw, which
    # comes back flat in column-major order; the reference reads it so.
    s <- v2_simulate(d, seed = 1, observe = FALSE)
    pars <- stan_pars(m, m$unconstrain_variables(s$pars))
    expect_equal(as.numeric(pars$house_step_raw), c(s$pars$house_step_raw), tolerance = 1e-12)
    expect_equal(as.numeric(pars$mu_step_raw), c(s$pars$mu_step_raw), tolerance = 1e-12)
    expect_equal(as.numeric(pars$tau_mu_A), c(s$pars$tau_mu_A), tolerance = 1e-12)
  }
})

test_that("v2s-m1: the industry and house biases are the reference's natural quantities", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  for (cell in v2_test_cells(cfg_m1())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      for (n in c("mu", "gamma")) {
        dims <- dim(as.array(ref[[n]]))
        expect_identical(length(tp[[n]]), as.integer(prod(dims)), label = paste("length of", n))
        expect_equal(array(tp[[n]], dims), as.array(ref[[n]]), tolerance = 1e-10, label = n)
      }
    }
  }
})

test_that("v2s-m1: the log-likelihood rows are the reference's at a prior point", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  # The forecast is switched off, so the fixed_param run computes none.
  ref <- environment(v2_lp_ref)
  d <- v2_test_cells(cfg_m1())$eve2024$data
  d$draw_forecast <- 0L
  r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 2, observe = FALSE)$pars)
  p <- ref$core_pars(r$pars, d)
  rows <- ref$dm_rows(p, ref$natural(p, d), d)
  expect_equal(r$gq$log_lik, vapply(rows, \(x) dirmult_lpmf(ref$row_counts(d, x), x$alpha), numeric(1)), tolerance = 1e-10)
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r$gq)))
})

test_that("v2s-m1: a fit_values() point keeps the epoch bias's scales as one-element arrays, so CmdStan reads it back", {
  skip_unless_slow()
  skip_unless_step("v2s-m1")
  # WHY: write_stan_json() writes a plain length-1 numeric as a scalar, which CmdStan's init
  # reader refuses for an array[1]; the forecast is off, so the runs compute none.
  d <- v2_test_cells(cfg_m1())$eve2024$data
  d$draw_forecast <- 0L
  r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 2, observe = FALSE)$pars)
  json <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(r$pars, json)
  back <- jsonlite::read_json(json)
  for (n in c("tau_mu", "tau_mu_A", "tau_house")) {
    expect_true(is.list(back[[n]]) && length(back[[n]]) == 1L, label = paste(n, "is a one-element JSON array"))
  }
  again <- gq_at(V2_STAN, d, r$pars)
  for (n in c("tau_mu", "tau_mu_A", "tau_house")) expect_equal(c(again$pars[[n]]), c(r$pars[[n]]), tolerance = 1e-12, label = n)
})

# Step v2s-m2, the newcomer pool (§6.4, switch newcomer_pool): the mean m_new and scale s_new
# of a newcomer's bias contrast m_new + s_new eta are estimated, on v2s-m1's epoch bias.
POOL_PARS <- c("m_new", "s_new")

test_that("v2s-m2: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m2")
  hit <- expect_step_lp("v2s-m2", cfg_m2())
  expect_true("newcomer_mean_over_all" %in% hit)
  expect_true(all(c("house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart", "no_house_restart") %in% hit))
})

test_that("v2s-m2: with the pool on and the epoch bias off, Stan's log density matches the reference", {
  skip_unless_slow()
  skip_unless_step("v2s-m2")
  # WHY: the v2-noepoch ablation fits newcomer_pool = 1 with epoch_bias = 0, where μ takes no
  # step and the epoch bias's parameters, size 0, sit before m_new and s_new.
  cfg <- utils::modifyList(cfg_m2(), list(epoch_bias = 0L, walk_form = 0L, delta_form = 0L))
  d <- v2_stan_data(info_2024eve(), cfg)$data
  expect_identical(as.integer(c(d$epoch_bias, d$newcomer_pool)), c(0L, 1L))
  m <- v2_methods(V2_STAN, d)
  pts <- lp_points(m, d, n_post = 0, n_off = 0, n_prior = 10)
  ok <- expect_lp_matches(m, d, pts)
  expect_true(mutant_exercised(d, pts, "newcomer_mean_over_all"))
  ratio <- expect_lp_rejects(m, d, pts, "newcomer_mean_over_all")
  message(sprintf(
    "v2s-m2 eve2024 with epoch_bias = 0: %d prior points, lp constant range %.3g (tol %.3g); range / (100 tol): newcomer_mean_over_all %.3g",
    length(pts), ok$range, ok$tol, ratio
  ))
})

test_that("v2s-m2: m_new and s_new have v2_par_dims()' sizes, and size 0 with newcomer_pool = 0", {
  skip_unless_slow()
  skip_unless_step("v2s-m2")
  for (cell in v2_test_cells(cfg_m2())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    expect_identical(names(v2_model(V2_STAN)$variables()$parameters), V2_CORE_PARS$name)
    dims <- v2_par_dims(d)
    expect_identical(lapply(dims[POOL_PARS], as.integer), list(m_new = 1L, s_new = 1L))
    sizes <- m$metadata()$stan_variable_sizes
    expect_identical(lapply(sizes[POOL_PARS], as.integer), lapply(dims[POOL_PARS], as.integer))
    d0 <- d
    d0$newcomer_pool <- 0L
    expect_true(all(vapply(v2_par_dims(d0)[POOL_PARS], prod, numeric(1)) == 0))
    # WHY: m_new and s_new add one free element each.
    expect_identical(as.numeric(n_unc(m) - n_unc(v2_methods(V2_STAN, d0))), 2)
    s <- v2_simulate(d, seed = 1, observe = FALSE)
    pars <- stan_pars(m, m$unconstrain_variables(s$pars))
    for (n in POOL_PARS) expect_equal(as.numeric(pars[[n]]), c(s$pars[[n]]), tolerance = 1e-12, label = n)
  }
})

test_that("v2s-m2: the industry and house biases are the reference's natural quantities, at m_new and s_new away from 0 and 0.2023", {
  skip_unless_slow()
  skip_unless_step("v2s-m2")
  for (cell in v2_test_cells(cfg_m2())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      # WHY: a Stan file that read the newcomer_pool = 0 constants would agree with the
      # reference wherever the point sits near them.
      s$pars$m_new[] <- c(-0.5, 0.3, 0.8)[seed]
      s$pars$s_new[] <- c(0.05, 0.6, 1.1)[seed]
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      for (n in c("mu", "gamma")) {
        dims <- dim(as.array(ref[[n]]))
        expect_identical(length(tp[[n]]), as.integer(prod(dims)), label = paste("length of", n))
        expect_equal(array(tp[[n]], dims), as.array(ref[[n]]), tolerance = 1e-10, label = n)
      }
    }
  }
})

test_that("v2s-m2: a fit_values() point keeps m_new and s_new as one-element arrays, so CmdStan reads it back", {
  skip_unless_slow()
  skip_unless_step("v2s-m2")
  # WHY: write_stan_json() writes a plain length-1 numeric as a scalar, which CmdStan's init
  # reader refuses for an array[1]; the forecast is off, so the runs compute none.
  d <- v2_test_cells(cfg_m2())$eve2024$data
  d$draw_forecast <- 0L
  r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 2, observe = FALSE)$pars)
  json <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(r$pars, json)
  back <- jsonlite::read_json(json)
  for (n in POOL_PARS) expect_true(is.list(back[[n]]) && length(back[[n]]) == 1L, label = paste(n, "is a one-element JSON array"))
  again <- gq_at(V2_STAN, d, r$pars)
  for (n in POOL_PARS) expect_equal(c(again$pars[[n]]), c(r$pars[[n]]), tolerance = 1e-12, label = n)
})

# Step v2s-m3, the election-day shock (§6.6, switch shock): eps_e = C(s_bar_E d_e z_e) in each
# closed election's kjordaemi softmax, d_e the row of d_shock of the epoch e closes, and in the
# forecast a fresh eps_E with the open epoch's row in votes_kd (and so pi_nat_eday) but not in
# pi_latent_E; on v2s-m2's newcomer pool.
SHOCK_PARS <- c("z_shock", "s_bar_E")

test_that("v2s-m3: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  hit <- expect_step_lp("v2s-m3", cfg_m3())
  expect_true("shock_unscaled" %in% hit)
  expect_true(all(c(
    "newcomer_mean_over_all", "house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart", "no_house_restart"
  ) %in% hit))
})

test_that("v2s-m3: with prior_s_bar_E at 0.37, Stan's log density matches the reference, each closed shock on its own row of d_shock", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  # WHY: the other cells run at §6.10's prior_s_bar_E = 0.15, where a literal 0.15 in Stan would
  # pass; the row slips show that these points tell Stan's row of d_shock from its neighbours.
  cfg <- utils::modifyList(cfg_m3(), list(walk_form = 0L, delta_form = 0L, prior_s_bar_E = 0.37))
  d <- v2_stan_data(info_2024eve(), cfg)$data
  expect_identical(d$prior_s_bar_E, 0.37)
  m <- v2_methods(V2_STAN, d)
  pts <- lp_points(m, d, n_post = 0, n_off = 0, n_prior = 10)
  ok <- expect_lp_matches(m, d, pts)
  closed <- seq_len(d$n_closed)
  literal <- d
  literal$prior_s_bar_E <- 0.15
  next_row <- d
  next_row$d_shock[closed, ] <- d$d_shock[closed + 1L, ]
  open_row <- d
  open_row$d_shock[closed, ] <- d$d_shock[rep(d$n_epoch, d$n_closed), ]
  slips <- c(
    literal_0.15 = lp_range(lp_gap(m, literal, pts)$gap), next_row = lp_range(lp_gap(m, next_row, pts)$gap),
    open_row = lp_range(lp_gap(m, open_row, pts)$gap)
  )
  for (n in names(slips)) expect_gt(slips[[n]], 100 * ok$tol, label = n)
  message(sprintf(
    "v2s-m3 eve2024 at prior_s_bar_E 0.37: %d prior points, lp constant range %.3g (tol %.3g); range / (100 tol): %s",
    length(pts), ok$range, ok$tol, paste(sprintf("%s %.3g", names(slips), slips / (100 * ok$tol)), collapse = ", ")
  ))
})

test_that("v2s-m3: z_shock and s_bar_E have v2_par_dims()' sizes, and size 0 with shock = 0", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  for (cell in v2_test_cells(cfg_m3())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    expect_identical(names(v2_model(V2_STAN)$variables()$parameters), V2_CORE_PARS$name)
    dims <- v2_par_dims(d)
    expect_identical(lapply(dims[SHOCK_PARS], as.integer), list(z_shock = as.integer(c(d$P, d$n_closed)), s_bar_E = 1L))
    sizes <- m$metadata()$stan_variable_sizes
    expect_identical(lapply(sizes[SHOCK_PARS], as.integer), lapply(dims[SHOCK_PARS], as.integer))
    d0 <- d
    d0$shock <- 0L
    expect_true(all(vapply(v2_par_dims(d0)[SHOCK_PARS], prod, numeric(1)) == 0))
    # WHY: z_shock adds P free elements per closed election, s_bar_E one.
    expect_identical(as.numeric(n_unc(m) - n_unc(v2_methods(V2_STAN, d0))), as.numeric(d$P * d$n_closed + 1))
    s <- v2_simulate(d, seed = 1, observe = FALSE)
    pars <- stan_pars(m, m$unconstrain_variables(s$pars))
    for (n in SHOCK_PARS) expect_equal(as.numeric(pars[[n]]), c(s$pars[[n]]), tolerance = 1e-12, label = n)
  }
})

test_that("v2s-m3: the closed elections' shocks are the reference's, and 0 with shock = 0", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  for (cell in v2_test_cells(cfg_m3())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      expect_identical(length(tp$eps), length(ref$eps))
      expect_equal(array(tp$eps, dim(ref$eps)), ref$eps, tolerance = 1e-10)
      expect_gt(max(abs(ref$eps)), 1e-6)
    }
    d0 <- d
    d0$shock <- 0L
    m0 <- v2_methods(V2_STAN, d0)
    s0 <- v2_simulate(d0, seed = 1, observe = FALSE)
    tp0 <- m0$constrain_variables(m0$unconstrain_variables(s0$pars), transformed_parameters = TRUE, generated_quantities = FALSE)
    expect_identical(length(tp0$eps), as.integer(d$n_closed * d$P))
    expect_true(all(tp0$eps == 0))
  }
})

test_that("v2s-m3: at a prior point the results' rows carry their shocks, pi_latent_E is the shock-free latent share and pi_nat_eday aggregates votes_kd", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  # No forecast quantity is printed, even on failure: those compare by isTRUE(all.equal()).
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_m3())
  expect_identical(
    vapply(cells, \(cell) as.integer(cell$data$draw_forecast), integer(1)),
    c(eve2024 = 1L, f180_2021 = 1L, eve2017 = 0L)
  )
  for (name in names(cells)) {
    d <- cells[[name]]$data
    pars <- v2_simulate(d, seed = 2, observe = FALSE)$pars
    pars$s_bar_E[] <- 0.3
    r <- gq_at(V2_STAN, d, pars)
    p <- ref$core_pars(r$pars, d)
    nat <- ref$natural(p, d)
    expect_gt(max(abs(nat$eps)), 1e-3)
    rows <- ref$dm_rows(p, nat, d)
    expect_equal(r$gq$log_lik, vapply(rows, \(x) dirmult_lpmf(ref$row_counts(d, x), x$alpha), numeric(1)), tolerance = 1e-10)
    expect_equal(apply(r$gq$y_rep_res, 1:2, sum), apply(d$y_res, 1:2, sum) + 0)
    if (d$draw_forecast == 1L) {
      A <- which(d$exists[d$G, ] == 1L)
      latent <- numeric(d$P)
      for (k in seq_len(d$K)) {
        latent[A] <- latent[A] + d$w[d$n_epoch, k] * ref$softmax(nat$theta[A, d$G] + nat$delta[d$n_epoch, k, A])
      }
      v <- r$gq$votes_kd
      expect_true(isTRUE(all.equal(rowSums(v), c(d$N_open))))
      expect_true(isTRUE(all.equal(c(r$gq$pi_nat_eday), colSums(v) / sum(d$N_open), tolerance = 1e-12)))
      expect_true(isTRUE(all.equal(c(r$gq$pi_latent_E), latent, tolerance = 1e-10)))
    } else {
      expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r$gq)))
    }
  }
})

test_that("v2s-m3: with z_shock = 0 the results' rows are shock = 0's, and no forecast draw gated on draw_forecast precedes the replicates", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  # WHY: the last check detects a forecast draw gated on draw_forecast and placed before the
  # replicates, which would move y_rep_res's RNG stream against draw_forecast = 0. It cannot
  # see a draw guarded only by if (shock) before the results loop, nor a draw outside
  # if (draw_forecast): both runs make it. Runs at shock = 1 and 0 cannot compare their
  # replicates draw for draw either: a fixed_param run's initialisation draws one uniform per
  # unconstrained parameter even when every init is given, and the two are P n_closed + 1
  # parameters apart. That shock = 0 draws exactly what the previous step's file drew is
  # checked once and recorded in harness/v2/steps.md's v2s-m3 section, not here; that
  # votes_kd carries the shock, the next test checks. Nothing below prints a forecast draw.
  d <- v2_test_cells(cfg_m3())$eve2024$data
  expect_identical(as.integer(d$draw_forecast), 1L)
  pars <- v2_simulate(d, seed = 3, observe = FALSE)$pars
  pars$z_shock[] <- 0
  d0 <- d
  d0$shock <- 0L
  p0 <- pars
  p0$z_shock <- matrix(0, d$P, 0)
  p0$s_bar_E <- array(numeric(0), 0)
  r1 <- gq_at(V2_STAN, d, pars)
  r0 <- gq_at(V2_STAN, d0, p0)
  expect_identical(r1$gq$log_lik, r0$gq$log_lik)
  d1 <- d
  d1$draw_forecast <- 0L
  r2 <- gq_at(V2_STAN, d1, pars)
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r2$gq)))
  expect_identical(r2$gq$y_rep_res, r1$gq$y_rep_res)
})

test_that("v2s-m3: votes_kd carries a fresh shock on the open epoch's row of d_shock, one per draw for every kjordaemi", {
  skip_unless_slow()
  skip_unless_step("v2s-m3")
  # WHY: at a prior draw with the walk and delta at 0, every party's latent log-ratio at E is
  # 0, and at phi_K = exp(19.5) the cell noise is about the multinomial's. So a draw's log
  # count ratio of party p to party q, averaged over the kjordaemi, is eps_E's contrast
  # s_bar_E (d_p z_p - d_q z_q) up to noise far below it, with mean 0 and variance
  # s_bar_E^2 (d_p^2 + d_q^2). The closed elections' shocks are set far from 0 (z = +-2,
  # alternating over the parties, its sign flipped from one election to the next) and must
  # not reach votes_kd: each contrast's mean stays at the shock-free centre, 0. Nothing below
  # prints a draw: each check is a scalar compared inside isTRUE(), and a failing centre
  # check reports only its largest |z|.
  d <- v2_test_cells(cfg_m3())$eve2024$data
  A <- which(d$exists[d$G, ] == 1L)
  expect_true(isTRUE(length(A) >= 3))
  pars <- v2_simulate(d, seed = 4, observe = FALSE)$pars
  pars$theta1[] <- 0
  pars$z_walk[] <- 0
  pars$delta_raw[] <- 0
  pars$z_shock[] <- vapply(seq_len(d$n_closed), \(e) 2 * (-1)^(seq_len(d$P) + e), numeric(d$P))
  # log phi_K = cap - (cap - init) exp(u), cap 20 and init log(1000): u sets log phi_K to 19.5
  pars$log_phi_K_u <- log((20 - 19.5) / (20 - log(1000)))
  s_bar <- 0.2
  pars$s_bar_E[] <- s_bar
  n <- 4000
  v <- gq_draws(V2_STAN, d, pars, "votes_kd", n)
  expect_identical(dim(v), as.integer(c(n, d$K, d$P)))
  expect_true(isTRUE(all(v[, , A] > 0)))
  lv <- log(v)
  q <- A[1]
  contrast <- function(p) rowMeans(lv[, , p] - lv[, , q])
  rows <- list(open = d$d_shock[d$n_epoch, ], last_closed = d$d_shock[d$n_closed, ], unscaled = rep(1, d$P))
  ratio <- vapply(rows, \(dd) vapply(A[-1], \(p) stats::var(contrast(p)) / (s_bar^2 * (dd[p]^2 + dd[q]^2)), numeric(1)), numeric(length(A) - 1))
  expect_true(isTRUE(max(abs(ratio[, "open"] - 1)) < 0.15))
  # WHY: the test can tell the open row from the last closed election's and from d = 1.
  expect_true(isTRUE(max(abs(ratio[, "last_closed"] - 1)) > 0.3))
  expect_true(isTRUE(max(abs(ratio[, "unscaled"] - 1)) > 0.3))
  # One shock per draw, shared by the kjordaemi: a contrast moves together in two of them.
  k_cor <- stats::cor(lv[, 1, A[2]] - lv[, 1, q], lv[, 2, A[2]] - lv[, 2, q])
  expect_true(isTRUE(k_cor > 0.9))
  # The centre: each contrast's mean is the shock-free centre, 0, within 4 sd / sqrt(n). The
  # last closed election's shock alone would move some contrast by more than 20 of those.
  se <- vapply(A[-1], \(p) stats::sd(contrast(p)) / sqrt(n), numeric(1))
  centre_z <- abs(vapply(A[-1], \(p) mean(contrast(p)), numeric(1))) / se
  last <- s_bar * d$d_shock[d$n_closed, ] * pars$z_shock[, d$n_closed]
  expect_true(isTRUE(max(abs(last[A[-1]] - last[q]) / se) > 20))
  expect_true(isTRUE(max(centre_z) < 4), info = sprintf("largest |z| of a contrast's mean: %.1f", max(centre_z)))
})

# Step v2s-m4, the delta walk (§6.7, switch delta_walk): delta_1 = M_1(s_delta0 zeta_1) and
# delta_e = M_e(delta_{e-1} - c_{e-1} 1' + sqrt(dyears_e / dyears_bar) s_zeta zeta_e), with
# delta_{e-1} re-centred within each kjordaemi over the previous election's parties, a
# newcomer's column fresh at its entry epoch; on v2s-m3's shock.
WALK_PARS <- c("delta_raw", "s_zeta_raw", "s_bar_zeta")

test_that("v2s-m4: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  hit <- expect_step_lp("v2s-m4", cfg_m4())
  # WHY: every cell has a step whose gap is not dyears_bar, 2017 eve's one step (a year) too,
  # and a newcomer entering after its first epoch, so the re-centring shows on each.
  by_cell <- attr(hit, "by_cell")
  expect_identical(names(by_cell), c("eve2024", "f180_2021", "eve2017"))
  for (name in names(by_cell)) {
    expect_true("delta_years_unscaled" %in% by_cell[[name]], label = name)
    expect_true("delta_entry_leak" %in% by_cell[[name]], label = name)
  }
  expect_true("no_delta_restart" %in% hit)
  expect_true(all(c(
    "shock_unscaled", "newcomer_mean_over_all", "house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart",
    "no_house_restart"
  ) %in% hit))
})

test_that("v2s-m4: with delta_years_bar at 2.5 and prior_s_bar_zeta at 0.37, Stan's log density matches the reference, each step on its own gap", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  # WHY: the other cells run at delta_years_bar = 4 and §6.10's prior_s_bar_zeta = 0.2, where a
  # literal 4 or 0.2 in Stan would pass; the gap slip shows that these points tell Stan's
  # delta_years[e] from the previous epoch's.
  cfg <- utils::modifyList(cfg_m4(), list(walk_form = 0L, delta_form = 0L, delta_years_bar = 2.5, prior_s_bar_zeta = 0.37))
  d <- v2_stan_data(info_2024eve(), cfg)$data
  expect_identical(d$delta_years_bar, 2.5)
  expect_identical(d$prior_s_bar_zeta, 0.37)
  m <- v2_methods(V2_STAN, d)
  pts <- lp_points(m, d, n_post = 0, n_off = 0, n_prior = 10)
  ok <- expect_lp_matches(m, d, pts)
  literal_bar <- d
  literal_bar$delta_years_bar <- 4
  literal_prior <- d
  literal_prior$prior_s_bar_zeta <- 0.2
  prev_years <- d
  prev_years$delta_years[-1] <- d$delta_years[-d$n_epoch]
  slips <- c(
    literal_bar_4 = lp_range(lp_gap(m, literal_bar, pts)$gap), literal_0.2 = lp_range(lp_gap(m, literal_prior, pts)$gap),
    prev_years = lp_range(lp_gap(m, prev_years, pts)$gap)
  )
  for (n in names(slips)) expect_gt(slips[[n]], 100 * ok$tol, label = n)
  message(sprintf(
    "v2s-m4 eve2024 at delta_years_bar 2.5, prior_s_bar_zeta 0.37: %d prior points, lp constant range %.3g (tol %.3g); range / (100 tol): %s",
    length(pts), ok$range, ok$tol, paste(sprintf("%s %.3g", names(slips), slips / (100 * ok$tol)), collapse = ", ")
  ))
})

test_that("v2s-m4: delta_raw, s_zeta_raw and s_bar_zeta have v2_par_dims()' sizes, and delta_walk = 0's with it off", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  for (cell in v2_test_cells(cfg_m4())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    expect_identical(names(v2_model(V2_STAN)$variables()$parameters), V2_CORE_PARS$name)
    dims <- v2_par_dims(d)
    expect_identical(lapply(dims[WALK_PARS], as.integer), list(
      delta_raw = as.integer(c(d$n_epoch, d$K, d$P)), s_zeta_raw = as.integer(d$P), s_bar_zeta = 1L
    ))
    sizes <- m$metadata()$stan_variable_sizes
    expect_identical(lapply(sizes[WALK_PARS], as.integer), lapply(dims[WALK_PARS], as.integer))
    d0 <- d
    d0$delta_walk <- 0L
    expect_identical(lapply(v2_par_dims(d0)[WALK_PARS], as.integer), list(
      delta_raw = as.integer(c(1L, d$K, d$P)), s_zeta_raw = 0L, s_bar_zeta = 0L
    ))
    # WHY: the walk adds the (n_epoch - 1) K P free elements of zeta_2..zeta_n_epoch, P of
    # s_zeta_raw and one of s_bar_zeta.
    expect_identical(as.numeric(n_unc(m) - n_unc(v2_methods(V2_STAN, d0))), as.numeric((d$n_epoch - 1) * d$K * d$P + d$P + 1))
    s <- v2_simulate(d, seed = 1, observe = FALSE)
    pars <- stan_pars(m, m$unconstrain_variables(s$pars))
    for (n in WALK_PARS) expect_equal(as.numeric(pars[[n]]), c(s$pars[[n]]), tolerance = 1e-12, label = n)
  }
})

test_that("v2s-m4: each epoch's delta and s_zeta are the reference's, and static with delta_walk = 0", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  for (cell in v2_test_cells(cfg_m4())) {
    d <- cell$data
    m <- v2_methods(V2_STAN, d)
    for (seed in 1:3) {
      s <- v2_simulate(d, seed = seed, observe = FALSE)
      # WHY: near s_bar_zeta = 0 a Stan file that kept delta static would agree with the walk.
      s$pars$s_bar_zeta[] <- c(0.1, 0.4, 0.9)[seed]
      u <- m$unconstrain_variables(s$pars)
      tp <- m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
      ref <- v2_natural(stan_pars(m, u), d)
      for (n in c("delta", "s_zeta")) {
        dims <- dim(as.array(ref[[n]]))
        expect_identical(length(tp[[n]]), as.integer(prod(dims)), label = paste("length of", n))
        expect_equal(array(tp[[n]], dims), as.array(ref[[n]]), tolerance = 1e-10, label = n)
      }
      expect_gt(max(abs(ref$delta[d$n_epoch, , ] - ref$delta[1, , ])), 1e-3)
    }
    d0 <- d
    d0$delta_walk <- 0L
    m0 <- v2_methods(V2_STAN, d0)
    u0 <- m0$unconstrain_variables(v2_simulate(d0, seed = 1, observe = FALSE)$pars)
    tp0 <- m0$constrain_variables(u0, transformed_parameters = TRUE, generated_quantities = FALSE)
    ref0 <- v2_natural(stan_pars(m0, u0), d0)
    expect_length(tp0$s_zeta, 0)
    expect_identical(length(tp0$delta), as.integer(prod(dim(ref0$delta))))
    expect_equal(array(tp0$delta, dim(ref0$delta)), ref0$delta, tolerance = 1e-10)
  }
})

test_that("v2s-m4: at a prior point the log-likelihood rows are the reference's and pi_latent_E reads the walked delta of the open epoch", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  # WHY: the reference's likelihood rows are internal to its module; they are the oracle here.
  # No forecast quantity is printed, even on failure: those compare by isTRUE(all.equal()).
  ref <- environment(v2_lp_ref)
  cells <- v2_test_cells(cfg_m4())
  for (name in names(cells)) {
    d <- cells[[name]]$data
    pars <- v2_simulate(d, seed = 2, observe = FALSE)$pars
    pars$s_bar_zeta[] <- 0.5
    r <- gq_at(V2_STAN, d, pars)
    p <- ref$core_pars(r$pars, d)
    nat <- ref$natural(p, d)
    rows <- ref$dm_rows(p, nat, d)
    expect_equal(r$gq$log_lik, vapply(rows, \(x) dirmult_lpmf(ref$row_counts(d, x), x$alpha), numeric(1)), tolerance = 1e-10)
    # The power check: the walked delta of the open epoch is not the static one.
    static <- ref$M_e(matrix(p$delta_raw[1, , ], d$K, d$P) %*% diag(nat$s_delta0), d$w[d$n_epoch, ])
    expect_true(isTRUE(max(abs(nat$delta[d$n_epoch, , ] - static)) > 1e-2), label = name)
    if (d$draw_forecast == 1L) {
      A <- which(d$exists[d$G, ] == 1L)
      latent <- numeric(d$P)
      latent_static <- numeric(d$P)
      for (k in seq_len(d$K)) {
        latent[A] <- latent[A] + d$w[d$n_epoch, k] * ref$softmax(nat$theta[A, d$G] + nat$delta[d$n_epoch, k, A])
        latent_static[A] <- latent_static[A] + d$w[d$n_epoch, k] * ref$softmax(nat$theta[A, d$G] + static[k, A])
      }
      expect_true(isTRUE(all.equal(c(r$gq$pi_latent_E), latent, tolerance = 1e-10)))
      expect_false(isTRUE(all.equal(c(r$gq$pi_latent_E), latent_static, tolerance = 1e-6)))
    } else {
      expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% names(r$gq)))
    }
  }
})

test_that("v2s-m4: a fit_values() point keeps s_bar_zeta as a one-element array, so CmdStan reads it back", {
  skip_unless_slow()
  skip_unless_step("v2s-m4")
  # WHY: write_stan_json() writes a plain length-1 numeric as a scalar, which CmdStan's init
  # reader refuses for an array[1]; the forecast is off, so the runs compute none.
  d <- v2_test_cells(cfg_m4())$eve2024$data
  d$draw_forecast <- 0L
  r <- gq_at(V2_STAN, d, v2_simulate(d, seed = 2, observe = FALSE)$pars)
  json <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(r$pars, json)
  back <- jsonlite::read_json(json)
  expect_true(is.list(back$s_bar_zeta) && length(back$s_bar_zeta) == 1L, label = "s_bar_zeta is a one-element JSON array")
  expect_identical(dim(r$pars$delta_raw), as.integer(c(d$n_epoch, d$K, d$P)))
  again <- gq_at(V2_STAN, d, r$pars)
  for (n in WALK_PARS) expect_equal(c(again$pars[[n]]), c(r$pars[[n]]), tolerance = 1e-12, label = n)
})
