box::use(
  R / harness / constants[ELECTIONS],
  R / harness / info_set[info_set],
  R / harness / tables[grid_dates, valid_votes],
  R / harness / truth[read_truth],
  R / harness / v2[read_v2_config, v2_stan_data]
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
test_that("2021 f180 keeps J with or without a kjordaemi row, entering in the open epoch", {
  skip_without_snapshot()
  J <- "Sósíalistaflokkurinn"
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"))
  # MMR's January 2020 and Maskína's December 2020 and January 2021 kjördæmi polls print J.
  # Zeroing J's rows rebuilds the cell as it was before they were admitted.
  none <- info
  none$polls_kd$n[none$polls_kd$flokkur == J] <- 0
  for (x in list(list(info = info, rows = TRUE), list(info = none, rows = FALSE))) {
    b <- v2_stan_data(x$info, cfg_v2(), stage1 = stage1_stub)
    j <- match(J, b$meta$party_order)
    expect_false(is.na(j))
    expect_identical(any(b$data$y_kd[, j] > 0), x$rows)
    expect_equal(b$data$entry_epoch[j], b$data$n_epoch)
    expect_identical(b$meta$closed_elections, c("2016-10-29", "2017-10-28"))
  }
})
test_that("the 2017 cells have nine parties, one closed epoch, M entering in the open one, and no forecast", {
  skip_without_snapshot()
  for (Fc in c("2017-10-14", "2017-10-27")) {
    b <- v2_stan_data(info_set(as.Date("2017-10-28"), as.Date(Fc)), cfg_v2(),
      stage1 = \(...) stop("stage 1 called"), root = withr::local_tempdir()
    )
    expect_equal(b$data$P, 9L)
    expect_equal(b$data$n_closed, 1L)
    expect_equal(b$data$n_epoch, 2L)
    expect_equal(b$data$entry_epoch[match("Mi\u00f0flokkurinn", b$meta$party_order)], 2L)
    expect_equal(b$data$draw_forecast, 0L)
    expect_identical(b$meta$stage1_sha256, "placeholder")
    expect_identical(b$meta$closed_elections, "2016-10-29")
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
  cfg <- cfg_v2()
  expect_error(v2_stan_data(info, cfg, party_order = j_first(info)), "v2 input: coordinate party")
  expect_error(v2_stan_data(info, cfg, party_order = head(info$parties, -1)), "v2 input: party_order is not a permutation")
  expect_error(v2_stan_data(zero_votes_in_one_kd(info), cfg), "v2 input: .* no votes in")
  expect_error(v2_stan_data(duplicate_kd_row(info), cfg), "v2 input: kj.rd.mi poll .* twice")
  expect_error(v2_stan_data(no_national_polls(info), cfg), "v2 input: no national poll")
  expect_error(v2_stan_data(poll_after_F(info), cfg), "v2 input: observation after F")
  expect_error(read_v2_config(config_with(order = "backwards")), "order")
})

test_that("V2's config has the default orders and Decision 1's values, and its keys are checked", {
  cfg <- cfg_v2()
  expect_named(cfg, c(
    "campaign_days", "desired_weight", "weight_time", "order", "prior_sd_log_kappa", "prior_mu_phi", "prior_sd_mu1",
    "prior_rate_sigma_gamma", "stage1"
  ))
  expect_identical(cfg[c("campaign_days", "desired_weight", "weight_time")], list(campaign_days = 47L, desired_weight = 0.22, weight_time = 180))
  expect_identical(cfg$order, "default")
  expect_identical(cfg$prior_mu_phi, log(1000))
  expect_identical(cfg[c("prior_sd_mu1", "prior_rate_sigma_gamma")], list(prior_sd_mu1 = 1, prior_rate_sigma_gamma = 1))
  expect_identical(cfg$stage1, list(seed = 20261028L, iter_warmup = 1000L, iter_sampling = 1000L))
  expect_false(any(grepl("[^\\x01-\\x7f]", readLines(V2_CONFIG), perl = TRUE)))
  expect_identical(read_v2_config(config_with()), cfg)
  expect_error(read_v2_config(config_with(order = NULL)), "missing key\\(s\\): order")
  expect_error(read_v2_config(config_with(extra = 1)), "unknown key\\(s\\): extra")
  # WHY: the keys of the removed components are refused, so an old config cannot run as if they were on.
  for (k in c("post_days", "delta_years_bar", "epoch_bias", "shock", "fundamentals", "prior_tau_mu", "prior_s_bar_E")) {
    expect_error(read_v2_config(do.call(config_with, stats::setNames(list(0), k))), paste0("unknown key\\(s\\): ", k), label = k)
  }
  expect_error(read_v2_config(config_with(stage1 = list(iter_sampling = NULL))), "stage1: missing key")
})
test_that("the data fields come in the contract's order, with integer counts and indices and the declared dims", {
  d <- d_2024eve()
  expect_identical(names(d), c(
    "P", "K", "H", "G", "n_epoch", "delta_days", "campaign", "grid_epoch", "exists", "w",
    "N_nat", "grid_nat", "house_nat", "y_nat", "n_rep_nat", "idx_nat",
    "J", "N_kd", "poll_kd", "kd_kd", "grid_kd", "house_kd", "y_kd", "n_rep_kd", "idx_kd",
    "n_closed", "grid_res", "y_res", "n_rep_res", "idx_res",
    "entry_epoch", "N_open", "draw_forecast", "latent_path",
    "n_sf", "sf_idx", "mu_f", "desired_weight", "weight_time", "campaign_days",
    "prior_sd_log_kappa", "prior_mu_phi", "prior_sd_mu1", "prior_rate_sigma_gamma"
  ))
  expect_identical(names(d), stan_block_decls(readLines(root("Stan", "forecast_v2.stan")), "data")$name)
  reals <- c("delta_days", "w", "mu_f", "desired_weight", "weight_time")
  reals <- c(reals, grep("^prior_", names(d), value = TRUE))
  for (f in setdiff(names(d), reals)) expect_true(is.integer(d[[f]]), label = f)
  for (f in reals) expect_true(is.double(d[[f]]), label = f)
  expect_identical(lengths(d[c("delta_days", "campaign")]), c(delta_days = d$G - 1L, campaign = d$G - 1L))
  expect_identical(dim(d$exists), c(d$G, d$P))
  expect_identical(dim(d$w), c(d$n_epoch, d$K))
  expect_identical(dim(d$y_nat), c(d$N_nat, d$P))
  expect_identical(dim(d$idx_nat), c(d$N_nat, d$P))
  expect_identical(dim(d$y_kd), c(d$N_kd, d$P))
  expect_identical(dim(d$idx_kd), c(d$N_kd, d$P))
  expect_identical(dim(d$y_res), c(d$n_closed, d$K, d$P))
  expect_identical(dim(d$idx_res), c(d$n_closed, d$P))
  expect_identical(c(d$n_epoch, d$n_closed, d$draw_forecast, d$latent_path), c(4L, 3L, 1L, 0L))
  expect_identical(d$n_sf, length(info_2024eve()$fundamentals$S_f))
  expect_equal(rowSums(d$w), rep(1, d$n_epoch))
  expect_true(all(d$poll_kd == cummax(d$poll_kd)) && identical(sort(unique(c(d$poll_kd))), seq_len(d$J)))
})
test_that("the reversed order reverses parties and kjordaemi alike, so Anna\u00f0 is the coordinate party", {
  info <- info_2024eve()
  d <- d_2024eve()
  b <- v2_stan_data(info, modifyList(cfg_v2(), list(order = "reversed")), stage1 = stage1_stub)
  rp <- rev(seq_len(d$P))
  rk <- rev(seq_len(d$K))
  expect_identical(b$meta$party_order, rev(info$parties))
  expect_identical(b$meta$coordinate_party, "Anna\u00f0")
  expect_identical(b$data$y_nat, d$y_nat[, rp])
  expect_identical(b$data$y_res, d$y_res[, rk, rp, drop = FALSE])
  expect_identical(b$data$w, d$w[, rk])
  expect_identical(b$data$N_open, d$N_open[rk])
  expect_identical(b$data$entry_epoch, d$entry_epoch[rp])
})
test_that("a 2017 cell under a sealed freeze draws a forecast and calls stage 1", {
  skip_without_snapshot()
  info <- info_set(as.Date("2017-10-28"), as.Date("2017-10-27"), v2_inputs())
  S_f <- info$fundamentals$S_f
  stage1 <- \(info, cfg, root) list(mu_f = stats::setNames(seq_along(S_f) / 10, S_f), sha256 = "stub", diagnostics = list())
  b <- v2_stan_data(info, cfg_v2(), stage1 = stage1, root = frozen_root())
  expect_identical(b$data$draw_forecast, 1L)
  expect_identical(b$meta$stage1_sha256, "stub")
  expect_identical(b$meta$party_order[b$data$sf_idx], S_f)
  expect_identical(c(b$data$mu_f), seq_along(S_f) / 10)
})
test_that("every one-dimensional field is a JSON array, even of length 1, as CmdStan reads it", {
  skip_without_snapshot()
  vectors <- c(
    "delta_days", "campaign", "grid_epoch", "grid_nat", "house_nat", "n_rep_nat",
    "poll_kd", "kd_kd", "grid_kd", "house_kd", "n_rep_kd", "grid_res", "n_rep_res",
    "entry_epoch", "N_open", "sf_idx", "mu_f"
  )
  info <- info_set(as.Date("2017-10-28"), as.Date("2017-10-14"), v2_inputs())
  b <- v2_stan_data(info, cfg_v2(), root = withr::local_tempdir())
  expect_identical(lengths(b$data[c("grid_res", "n_rep_res")]), c(grid_res = 1L, n_rep_res = 1L))
  path <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(b$data, path)
  j <- jsonlite::read_json(path)
  for (f in c("grid_res", "n_rep_res")) {
    expect_type(j[[f]], "list")
    expect_length(j[[f]], 1L)
  }
  for (f in vectors) {
    expect_type(j[[f]], "list")
    expect_length(j[[f]], length(b$data[[f]]))
  }
  expect_identical(unlist(j$grid_res), c(b$data$grid_res))
})
test_that("the campaign flag covers exactly the steps within 47 days before an election, past or target", {
  skip_without_snapshot()
  els <- ELECTIONS
  for (cell in list(c("2024-11-30", "2024-11-29", "4"), c("2017-10-28", "2017-10-27", "2"))) {
    info <- info_set(as.Date(cell[1]), as.Date(cell[2]), v2_inputs())
    d <- v2_stan_data(info, cfg_v2(), stage1 = stage1_stub, root = withr::local_tempdir())$data
    g <- grid_dates(info)
    expect_identical(length(g), d$G)
    expect_equal(sum(d$campaign * d$delta_days), 47 * as.numeric(cell[3]))
    E <- as.Date(cell[1])
    inside <- vapply(seq_len(d$G - 1), \(i) any(g[i] >= els[els <= E] - 47 & g[i + 1] <= els[els <= E]), logical(1))
    expect_identical(c(d$campaign) == 1L, inside)
  }
  d <- d_2024eve()
  g <- grid_dates(info_2024eve())
  start <- match(as.Date("2024-10-14"), g)
  expect_identical(c(d$campaign[start - 1], d$campaign[start], d$campaign[d$G - 1]), c(0L, 1L, 1L))
  e21 <- match(as.Date("2021-09-25"), g)
  expect_identical(c(d$campaign[e21 - 1], d$campaign[e21]), c(1L, 0L))
})
test_that("a party exists from its entry date on, and throughout if present at the window's start", {
  skip_without_snapshot()
  entry <- readr::read_csv(root("harness", "entry_table.csv"), col_types = readr::cols(
    flokkur = "c", first_nonzero = "D", entry_epoch = "i", newcomer = "l"
  ))
  for (cell in list(c("2024-11-30", "2024-11-29"), c("2021-09-25", "2021-03-29"))) {
    info <- info_set(as.Date(cell[1]), as.Date(cell[2]), v2_inputs())
    b <- v2_stan_data(info, cfg_v2(), stage1 = stage1_stub)
    g <- grid_dates(info)
    for (p in seq_len(b$data$P)) {
      row <- entry[entry$flokkur == b$meta$party_order[p], ]
      expect_identical(nrow(row), 1L)
      want <- if (row$newcomer) as.integer(g >= row$first_nonzero) else rep(1L, length(g))
      expect_identical(b$data$exists[, p], want, label = b$meta$party_order[p])
    }
    expect_true(any(b$data$exists == 0L))
  }
})
