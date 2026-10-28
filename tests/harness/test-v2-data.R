box::use(
  R / harness / constants[ELECTIONS],
  R / harness / info_set[info_set],
  R / harness / tables[grid_dates, valid_votes],
  R / harness / truth[national_shares, read_truth],
  R / harness / v2[read_v2_config, v2_stan_data, V2_SWITCHES]
)

test_that("the grid holds every observation date, F and E, strictly increasing", {
  info <- info_2024eve()
  g <- grid_dates(info)
  expect_true(all(diff(g) > 0))
  expect_equal(tail(g, 2), as.Date(c("2024-11-29", "2024-11-30")))
  expect_true(all(c(info$polls_nat$date, info$polls_kd$date, as.Date(c("2016-10-29", "2017-10-28", "2021-09-25"))) %in% g))
  expect_true((as.Date("2024-11-30") - 47) %in% g)
})
test_that("a 180-day grid ends at F, the campaign start and E; a 47-day grid has F as the campaign start", {
  skip_without_snapshot()
  expect_equal(tail(grid_dates(info_set(as.Date("2024-11-30"), as.Date("2024-06-03"))), 3), as.Date(c("2024-06-03", "2024-10-14", "2024-11-30")))
  expect_equal(tail(grid_dates(info_set(as.Date("2021-09-25"), as.Date("2021-08-09"))), 2), as.Date(c("2021-08-09", "2021-09-25")))
})
test_that("epochs and weights follow the closing election", {
  d <- d_2024eve()
  i21 <- d$grid_res[3]
  expect_equal(d$grid_epoch[i21], 3L)
  expect_equal(d$w[4, ], d$w[3, ])
  expect_equal(unname(d$w[3, ]), unname(valid_votes(read_truth(), as.Date("2021-09-25"))$w))
})
test_that("2021 f180 keeps J with no kjordaemi row, entering in the open epoch", {
  skip_without_snapshot()
  b <- v2_stan_data(info_set(as.Date("2021-09-25"), as.Date("2021-03-29")), cfg_m0())
  j <- match("S\u00f3s\u00edalistaflokkurinn", b$meta$party_order)
  expect_false(is.na(j))
  expect_false(any(b$data$y_kd[, j] > 0))
  expect_equal(b$data$entry_epoch[j], b$data$n_epoch)
})
test_that("the 2017 cells have nine parties, one closed epoch, M entering in the open one, and no forecast", {
  skip_without_snapshot()
  for (Fc in c("2017-10-14", "2017-10-27")) {
    b <- v2_stan_data(info_set(as.Date("2017-10-28"), as.Date(Fc)), modifyList(cfg_m0(), list(fundamentals = 1L)),
      stage1 = \(...) stop("stage 1 called"), root = withr::local_tempdir()
    )
    expect_equal(b$data$P, 9L)
    expect_equal(b$data$n_closed, 1L)
    expect_equal(b$data$n_epoch, 2L)
    expect_equal(b$data$entry_epoch[match("Mi\u00f0flokkurinn", b$meta$party_order)], 2L)
    expect_equal(b$data$draw_forecast, 0L)
    expect_identical(b$meta$stage1_sha256, "placeholder")
  }
})
test_that("partial polls and results are gathered by identity", {
  # polls before 2017-09-21 omit M and J (positions 8, 9) but report Annað (10); so does the 2016 result
  d <- d_2024eve()
  for (i in seq_len(d$N_nat)) expect_equal(d$idx_nat[i, seq_len(d$n_rep_nat[i])], which(d$y_nat[i, ] > 0))
  expect_equal(d$idx_res[1, seq_len(d$n_rep_res[1])], which(colSums(d$y_res[1, , ]) > 0))
  expect_true(any(d$n_rep_nat < d$P & d$y_nat[, d$P] > 0))
})
test_that("the guards fire", {
  info <- info_2024eve()
  cfg <- cfg_m0()
  expect_error(v2_stan_data(info, cfg, party_order = j_first(info)), "v2 input: coordinate party")
  expect_error(v2_stan_data(info, cfg, party_order = head(info$parties, -1)), "v2 input: party_order is not a permutation")
  expect_error(v2_stan_data(zero_votes_in_one_kd(info), cfg), "v2 input: .* no votes in")
  expect_error(v2_stan_data(duplicate_kd_row(info), cfg), "v2 input: kj.rd.mi poll .* twice")
  expect_error(v2_stan_data(no_national_polls(info), cfg), "v2 input: no national poll")
  expect_error(v2_stan_data(poll_after_F(info), cfg), "v2 input: observation after F")
  expect_error(read_v2_config(config_with(order = "backwards")), "order")
})

test_that("the m0 config has every switch off, the default orders and Decision 1's values, and its keys are checked", {
  cfg <- cfg_m0()
  expect_identical(unlist(cfg[V2_SWITCHES]), stats::setNames(integer(7), V2_SWITCHES))
  expect_identical(cfg$order, "default")
  expect_identical(cfg$prior_mu_phi, log(1000))
  expect_identical(cfg[c("prior_sd_mu1", "prior_rate_sigma_gamma")], list(prior_sd_mu1 = 1, prior_rate_sigma_gamma = 1))
  expect_identical(cfg$stage1, list(seed = 20261028L, iter_warmup = 1000L, iter_sampling = 1000L))
  expect_false(any(grepl("[^\\x01-\\x7f]", readLines(V2_M0_CONFIG), perl = TRUE)))
  expect_identical(read_v2_config(config_with()), cfg)
  expect_error(read_v2_config(config_with(shock = NULL)), "missing key\\(s\\): shock")
  expect_error(read_v2_config(config_with(extra = 1)), "unknown key\\(s\\): extra")
  expect_error(read_v2_config(config_with(shock = 2)), "shock must be 0 or 1")
  expect_error(read_v2_config(config_with(stage1 = list(iter_sampling = NULL))), "stage1: missing key")
})
test_that("the data fields come in the contract's order, with integer counts and indices and the declared dims", {
  d <- d_2024eve()
  expect_identical(names(d), c(
    "P", "K", "H", "G", "n_epoch", "delta_days", "campaign", "grid_epoch", "exists", "w", "delta_years",
    "N_nat", "grid_nat", "house_nat", "y_nat", "n_rep_nat", "idx_nat",
    "J", "N_kd", "poll_kd", "kd_kd", "grid_kd", "house_kd", "y_kd", "n_rep_kd", "idx_kd",
    "n_closed", "grid_res", "y_res", "n_rep_res", "idx_res",
    "house_first_epoch", "entry_epoch",
    "d_shock", "N_open", "draw_forecast", "latent_path",
    "n_sf", "sf_idx", "mu_f", "desired_weight", "weight_time", "campaign_days", "delta_years_bar",
    "prior_tau_mu", "prior_s_bar_E", "prior_s_bar_zeta", "prior_sd_log_kappa", "prior_mu_phi", "prior_sd_mu1",
    "prior_rate_sigma_gamma", V2_SWITCHES
  ))
  reals <- c("delta_days", "w", "delta_years", "d_shock", "mu_f", "desired_weight", "weight_time", "delta_years_bar")
  reals <- c(reals, grep("^prior_", names(d), value = TRUE))
  for (f in setdiff(names(d), reals)) expect_true(is.integer(d[[f]]), label = f)
  for (f in reals) expect_true(is.double(d[[f]]), label = f)
  expect_identical(lengths(d[c("delta_days", "campaign")]), c(delta_days = d$G - 1L, campaign = d$G - 1L))
  expect_identical(dim(d$exists), c(d$G, d$P))
  expect_identical(dim(d$w), c(d$n_epoch, d$K))
  expect_identical(dim(d$d_shock), c(d$n_epoch, d$P))
  expect_identical(dim(d$y_nat), c(d$N_nat, d$P))
  expect_identical(dim(d$idx_nat), c(d$N_nat, d$P))
  expect_identical(dim(d$y_kd), c(d$N_kd, d$P))
  expect_identical(dim(d$idx_kd), c(d$N_kd, d$P))
  expect_identical(dim(d$y_res), c(d$n_closed, d$K, d$P))
  expect_identical(dim(d$idx_res), c(d$n_closed, d$P))
  expect_identical(c(d$n_epoch, d$n_closed, d$draw_forecast, d$latent_path, d$n_sf), c(4L, 3L, 1L, 0L, 0L))
  expect_equal(rowSums(d$w), rep(1, d$n_epoch))
  expect_true(all(d$poll_kd == cummax(d$poll_kd)) && identical(sort(unique(d$poll_kd)), seq_len(d$J)))
  expect_true(all(d$house_first_epoch == cummax(d$house_first_epoch)))
})
test_that("the shock scaling reads the previous election's national share, with the floor for a party with none", {
  d <- d_2024eve()
  info <- info_2024eve()
  expect_equal(d$d_shock[1, 6], sqrt(10)) # Viðreisn: no 2013 result
  expect_equal(d$d_shock[3, 9], sqrt(10)) # Sósíalistaflokkurinn: no 2017 result
  s13 <- info$prev_shares$share[info$prev_shares$election == ELECTIONS[1]][1]
  expect_equal(d$d_shock[1, 1], sqrt(min(0.2 / s13, 10)))
  s21 <- national_shares(read_truth(), "2021-09-25")[[1]]
  expect_equal(d$d_shock[4, 1], sqrt(min(0.2 / s21, 10)))
})
test_that("the reversed order reverses parties and kjordaemi alike, so Anna\u00f0 is the coordinate party", {
  info <- info_2024eve()
  d <- d_2024eve()
  b <- v2_stan_data(info, modifyList(cfg_m0(), list(order = "reversed")))
  rp <- rev(seq_len(d$P))
  rk <- rev(seq_len(d$K))
  expect_identical(b$meta$party_order, rev(info$parties))
  expect_identical(b$meta$coordinate_party, "Anna\u00f0")
  expect_identical(b$data$y_nat, d$y_nat[, rp])
  expect_identical(b$data$y_res, d$y_res[, rk, rp, drop = FALSE])
  expect_identical(b$data$w, d$w[, rk])
  expect_identical(b$data$N_open, d$N_open[rk])
  expect_identical(b$data$d_shock, d$d_shock[, rp])
  expect_identical(b$data$entry_epoch, d$entry_epoch[rp])
})
test_that("a 2017 cell under a sealed freeze draws a forecast and calls stage 1", {
  skip_without_snapshot()
  info <- info_set(as.Date("2017-10-28"), as.Date("2017-10-27"), v2_inputs())
  S_f <- info$fundamentals$S_f
  stage1 <- \(info, cfg, root) list(mu_f = stats::setNames(seq_along(S_f) / 10, S_f), sha256 = "stub", diagnostics = list())
  b <- v2_stan_data(info, modifyList(cfg_m0(), list(fundamentals = 1L)), stage1 = stage1, root = frozen_root())
  expect_identical(b$data$draw_forecast, 1L)
  expect_identical(b$meta$stage1_sha256, "stub")
  expect_identical(b$meta$party_order[b$data$sf_idx], S_f)
  expect_identical(b$data$mu_f, seq_along(S_f) / 10)
})
