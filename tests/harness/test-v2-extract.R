box::use(
  R / harness / v2[v2_extract, v2_variables, V2_SWITCHES, V2_PARAM_VARIABLES, V2_PARAM_SWITCH]
)

K <- 2L
P <- 3L
CLOSED <- c("2016-10-29", "2017-10-28")
N_CLOSED <- length(CLOSED)
SWITCHES <- stats::setNames(rep(0L, length(V2_SWITCHES)), V2_SWITCHES)
META <- list(
  party_order = c("A", "X", "Y"),
  kjordaemi_levels = c("K1", "K2"),
  closed_elections = CLOSED,
  switches = SWITCHES
)
# The core's parameters (no switch on), in param_draws order.
CORE <- c(
  "sigma", "L_Omega", "tau_camp", "mu_phi", "sigma_phi", "log_phi", "log_phi_K", "log_kappa", "sigma_gamma", "mu",
  "mu_sd", "tau_sd", "s_delta0"
)
SCALARS <- c(
  "tau_camp", "mu_phi", "sigma_phi", "log_phi_K", "log_kappa", "sigma_gamma", "mu_sd", "tau_sd", "s_bar_E", "s_bar_zeta"
)
# The epoch bias's scales and the newcomer pool's mean and scale, each declared
# array[<switch>] real, so a fit writes tau_mu[1] and m_new[1].
ONE_ELEMENT <- c("tau_mu", "tau_mu_A", "tau_house", "m_new", "s_new")
MATRICES <- c("L_Omega", "mu", "anchor_w")
DRAW <- seq_len(16)

# Element names of a parameter in Stan's (column-major) order.
elements <- function(v) {
  if (v %in% SCALARS) {
    v
  } else if (v %in% ONE_ELEMENT) {
    paste0(v, "[1]")
  } else if (v %in% MATRICES) {
    paste0(v, c("[1,1]", "[2,1]", "[1,2]"))
  } else {
    paste0(v, c("[1]", "[2]"))
  }
}
columns_of <- function(vars) unlist(lapply(vars, elements))

# A draws_df of 4 chains x 4 iterations over `n_k` kjordaemi, `n_p` parties and `n_e` closed
# elections: votes_kd[k,p] = 1000k + p and y_rep_res[e,k,p] = 100000 draw + 10000e + 100k + p;
# pi_nat_eday and pi_latent_E; and each element of `params`, each varying by draw. The columns
# come shuffled, so a label taken by position is wrong.
synthetic_draws <- function(params = CORE, drop = character(0), change = list(), n_k = K, n_p = P, n_e = N_CLOSED) {
  cols <- list()
  for (p in seq_len(n_p)) for (k in seq_len(n_k)) cols[[sprintf("votes_kd[%d,%d]", k, p)]] <- rep(1000 * k + p, 16)
  for (p in seq_len(n_p)) cols[[sprintf("pi_nat_eday[%d]", p)]] <- (DRAW + p) / 100
  for (p in seq_len(n_p)) cols[[sprintf("pi_latent_E[%d]", p)]] <- (DRAW + 10 * p) / 1000
  for (p in seq_len(n_p)) {
    for (k in seq_len(n_k)) {
      for (e in seq_len(n_e)) cols[[sprintf("y_rep_res[%d,%d,%d]", e, k, p)]] <- 100000 * DRAW + 10000 * e + 100 * k + p
    }
  }
  pc <- columns_of(params)
  for (i in seq_along(pc)) cols[[pc[i]]] <- i + DRAW / 1000
  cols[names(change)] <- change
  cols <- cols[setdiff(names(cols), drop)]
  cols <- cols[withr::with_seed(20261029, sample(length(cols)))]
  x <- array(unlist(cols, use.names = FALSE), dim = c(4, 4, length(cols)), dimnames = list(NULL, NULL, names(cols)))
  posterior::as_draws_df(posterior::as_draws_array(x))
}

# Every element name of `variable` among the columns of synthetic_draws(params).
all_of <- function(variable, params = CORE) {
  vars <- posterior::variables(synthetic_draws(params))
  vars[sub("\\[.*$", "", vars) == variable]
}

extracted <- function(draws = synthetic_draws(), meta = META, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  paths <- v2_extract(draws, meta, dir)
  list(dir = dir, paths = paths, read = lapply(paths, arrow::read_parquet))
}

classes <- function(df) vapply(df, \(x) class(x)[1], character(1))

test_that("V2_PARAM_VARIABLES lists the core's and the switched parameters in param_draws column order, and each switch adds its own", {
  expect_identical(V2_PARAM_VARIABLES, c(
    "sigma", "L_Omega", "tau_camp", "mu_phi", "sigma_phi", "log_phi", "log_phi_K", "log_kappa", "sigma_gamma", "mu",
    "tau_mu", "tau_mu_A", "tau_house", "m_new", "s_new", "s_bar_E", "mu_sd", "tau_sd", "s_delta0", "s_bar_zeta", "s_zeta",
    "anchor_w"
  ))
  expect_identical(V2_PARAM_SWITCH, c(
    tau_mu = "epoch_bias", tau_mu_A = "epoch_bias", tau_house = "epoch_bias", m_new = "newcomer_pool",
    s_new = "newcomer_pool", s_bar_E = "shock", s_bar_zeta = "delta_walk", s_zeta = "delta_walk", anchor_w = "walk_form"
  ))
  expect_identical(setdiff(V2_PARAM_VARIABLES, names(V2_PARAM_SWITCH)), CORE)
})

test_that("v2_extract labels kjordaemi and parties by the Stan indices, never by column position", {
  x <- extracted()
  kd <- x$read$eday_kjordaemi
  expect_equal(nrow(kd), 16 * K * P)
  k2y <- kd[kd$kjordaemi == "K2" & kd$flokkur == "Y", ]
  expect_identical(sort(k2y$.draw), seq_len(16L))
  expect_true(all(k2y$votes == 2003L))
  k <- match(kd$kjordaemi, META$kjordaemi_levels)
  p <- match(kd$flokkur, META$party_order)
  expect_false(anyNA(k) || anyNA(p))
  expect_identical(kd$votes, as.integer(1000 * k + p))
  expect_identical(nrow(dplyr::distinct(kd, .draw, kjordaemi, flokkur)), nrow(kd))
})

test_that("with 10 kjordaemi and 11 parties, the labels follow the indices' numeric order, not their names' sort order", {
  # WHY: a sort of the element names puts votes_kd[10,1] between votes_kd[1,1] and votes_kd[2,1], so it would mislabel here.
  wide <- utils::modifyList(META, list(
    party_order = c("D", "B", "S", "V", "C", "P", "M", "F", "J", "O", "A"),
    kjordaemi_levels = sprintf("K%02d", c(5, 3, 9, 1, 10, 2, 8, 4, 7, 6))
  ))
  x <- extracted(synthetic_draws(n_k = 10L, n_p = 11L), wide)
  kd <- x$read$eday_kjordaemi
  expect_equal(nrow(kd), 16 * 10 * 11)
  k <- match(kd$kjordaemi, wide$kjordaemi_levels)
  p <- match(kd$flokkur, wide$party_order)
  expect_false(anyNA(k) || anyNA(p))
  expect_identical(kd$votes, as.integer(1000 * k + p))
  expect_true(all(kd$votes[kd$kjordaemi == "K06" & kd$flokkur == "A"] == 10011L))
  nat <- x$read$eday_national
  expect_identical(nat$share, (nat$.draw + match(nat$flokkur, wide$party_order)) / 100)
  lat <- x$read$latent_national
  expect_identical(lat$share, (lat$.draw + 10 * match(lat$flokkur, wide$party_order)) / 1000)
  ppc <- x$read$ppc_results
  expect_equal(nrow(ppc), 16 * N_CLOSED * 10 * 11)
  e <- match(format(ppc$election), CLOSED)
  expect_identical(
    ppc$votes,
    as.integer(100000 * ppc$.draw + 10000 * e + 100 * match(ppc$kjordaemi, wide$kjordaemi_levels) + match(ppc$flokkur, wide$party_order))
  )
})

test_that("the five parquets have exactly the listed columns and types, and .draw runs 1..16", {
  x <- extracted()
  expect_named(x$paths, c("eday_kjordaemi", "eday_national", "latent_national", "param_draws", "ppc_results"))
  expect_identical(unname(x$paths), file.path(x$dir, paste0(names(x$paths), ".parquet")))
  ids <- c(.chain = "integer", .iteration = "integer", .draw = "integer")
  expect_identical(classes(x$read$eday_kjordaemi), c(ids, kjordaemi = "character", flokkur = "character", votes = "integer"))
  expect_identical(classes(x$read$eday_national), c(ids, flokkur = "character", share = "numeric"))
  expect_identical(classes(x$read$latent_national), c(ids, flokkur = "character", share = "numeric"))
  pc <- columns_of(CORE)
  expect_identical(classes(x$read$param_draws), c(ids, stats::setNames(rep("numeric", length(pc)), pc)))
  expect_identical(
    classes(x$read$ppc_results),
    c(ids, election = "Date", kjordaemi = "character", flokkur = "character", votes = "integer")
  )
  for (f in names(x$read)) {
    expect_identical(sort(unique(x$read[[f]]$.draw)), seq_len(16L), label = f)
    expect_identical(sort(unique(x$read[[f]]$.chain)), seq_len(4L), label = f)
    expect_identical(sort(unique(x$read[[f]]$.iteration)), seq_len(4L), label = f)
  }
})

test_that("eday_national is pi_nat_eday and latent_national is pi_latent_E", {
  x <- extracted()
  nat <- x$read$eday_national
  expect_equal(nrow(nat), 16 * P)
  p <- match(nat$flokkur, META$party_order)
  expect_false(anyNA(p))
  expect_identical(nat$share, (nat$.draw + p) / 100)
  lat <- x$read$latent_national
  expect_equal(nrow(lat), 16 * P)
  expect_identical(lat$share, (lat$.draw + 10 * match(lat$flokkur, META$party_order)) / 1000)
  expect_identical(nat$.chain, as.integer((nat$.draw - 1L) %/% 4L + 1L))
  expect_identical(nat$.iteration, as.integer((nat$.draw - 1L) %% 4L + 1L))
})

test_that("ppc_results labels each closed election, kjordaemi and party by its Stan indices", {
  x <- extracted()
  ppc <- x$read$ppc_results
  expect_equal(nrow(ppc), 16 * N_CLOSED * K * P)
  e <- match(format(ppc$election), CLOSED)
  k <- match(ppc$kjordaemi, META$kjordaemi_levels)
  p <- match(ppc$flokkur, META$party_order)
  expect_false(anyNA(e) || anyNA(k) || anyNA(p))
  expect_identical(ppc$votes, as.integer(100000 * ppc$.draw + 10000 * e + 100 * k + p))
  expect_identical(nrow(dplyr::distinct(ppc, .draw, election, kjordaemi, flokkur)), nrow(ppc))
})

test_that("param_draws holds the parameters the meta's switches put in the model, in Stan order", {
  pd <- extracted()$read$param_draws
  pc <- columns_of(CORE)
  expect_identical(names(pd), c(".chain", ".iteration", ".draw", pc))
  expect_identical(pd$.draw, seq_len(16L))
  for (i in seq_along(pc)) expect_identical(pd[[pc[i]]], i + DRAW / 1000, label = pc[i])

  walk <- utils::modifyList(META, list(switches = replace(SWITCHES, "walk_form", 1L)))
  with_anchor <- c(CORE, "anchor_w")
  pd <- extracted(synthetic_draws(with_anchor), walk)$read$param_draws
  expect_identical(names(pd), c(".chain", ".iteration", ".draw", columns_of(with_anchor)))
  epoch <- utils::modifyList(META, list(switches = replace(SWITCHES, "epoch_bias", 1L)))
  with_epoch <- append(CORE, c("tau_mu", "tau_mu_A", "tau_house"), after = match("mu", CORE))
  pd <- extracted(synthetic_draws(with_epoch), epoch)$read$param_draws
  expect_identical(names(pd), c(".chain", ".iteration", ".draw", columns_of(with_epoch)))
  expect_true(all(c("tau_mu[1]", "tau_mu_A[1]", "tau_house[1]") %in% names(pd)))
  pool <- utils::modifyList(META, list(switches = replace(SWITCHES, c("epoch_bias", "newcomer_pool"), 1L)))
  with_pool <- append(with_epoch, c("m_new", "s_new"), after = match("tau_house", with_epoch))
  pd <- extracted(synthetic_draws(with_pool), pool)$read$param_draws
  expect_identical(names(pd), c(".chain", ".iteration", ".draw", columns_of(with_pool)))
  expect_true(all(c("m_new[1]", "s_new[1]") %in% names(pd)))
})

test_that("v2_extract reads every variable v2_variables() names for the meta's switches, and no other", {
  vars <- v2_variables(as.list(SWITCHES))
  expect_identical(vars, c("votes_kd", "pi_nat_eday", "pi_latent_E", "y_rep_res", CORE))
  for (v in vars) {
    dir <- withr::local_tempdir()
    expect_error(v2_extract(synthetic_draws(drop = all_of(v)), META, dir), paste("no", v), label = v)
    expect_length(list.files(dir), 0)
  }
  extra <- synthetic_draws(c(CORE, "anchor_w", "tau_mu"))
  expect_named(extracted(extra)$read$param_draws, c(".chain", ".iteration", ".draw", columns_of(CORE)))
})

test_that("v2_extract stops on a missing cell, a non-whole count, a missing parameter or meta field, and writes nothing", {
  dir <- withr::local_tempdir()
  fails <- function(draws, meta = META, what) {
    expect_error(v2_extract(draws, meta, dir), what)
    expect_length(list.files(dir), 0)
  }
  fails(synthetic_draws(drop = "votes_kd[2,1]"), what = "v2_extract: votes_kd")
  fails(synthetic_draws(change = list(`votes_kd[1,2]` = rep(1.5, 16))), what = "whole")
  fails(synthetic_draws(drop = "y_rep_res[2,2,3]"), what = "y_rep_res")
  fails(synthetic_draws(change = list(`y_rep_res[1,1,1]` = rep(0.5, 16))), what = "whole")
  fails(synthetic_draws(drop = "pi_latent_E[3]"), what = "pi_latent_E")
  fails(synthetic_draws(drop = "tau_sd"), what = "tau_sd")
  walk <- utils::modifyList(META, list(switches = replace(SWITCHES, "walk_form", 1L)))
  fails(synthetic_draws(), walk, "no anchor_w")
  for (f in c("party_order", "kjordaemi_levels", "closed_elections", "switches")) {
    fails(synthetic_draws(), META[setdiff(names(META), f)], paste("lacks", f))
  }
  fails(synthetic_draws(), utils::modifyList(META, list(closed_elections = CLOSED[1])), "y_rep_res")
  fails(synthetic_draws(), utils::modifyList(META, list(switches = SWITCHES[-1])), "switches")
})
