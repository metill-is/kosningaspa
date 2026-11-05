# v2_checks.R (the Phase 2 plan's Task 16), run as a script on synthetic run directories: made-up
# meta.json, param_draws.parquet and ppc_results.parquet, never a fit's draws, with the committed
# official counts of harness/truth/. Its counts, correlation and ESS are compared with values
# computed here by hand. The run directories hold no eday_* or latent_* parquet, so a script that
# read one would stop.
box::use(
  R / harness / constants[KJORDAEMI, POLL_PARTIES],
  R / harness / runs[run_id],
  R / harness / truth[fold_truth, read_truth]
)

EVE <- c("2024-11-30", "2024-11-29")
F180 <- c("2021-09-25", "2021-03-29")
CLOSED_EVE <- c("2016-10-29", "2017-10-28", "2021-09-25")
CLOSED_F180 <- c("2016-10-29", "2017-10-28")
CHAINS <- 4L
WARMUP <- 300L
ITERS <- 100L
DRAWS <- CHAINS * ITERS
HOUSES <- c("House A", "House B", "House C")
KD <- KJORDAEMI$kjordaemi
PARTIES <- POLL_PARTIES
# WHY: a forecast-like field of meta.json, which the checks must not print.
DECOY <- 0.4242

# v2_checks.R run from the project root; its output, with the exit status as attribute "status"
# when it is not 0.
checks <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/v2_checks.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

cells <- function(row) trimws(strsplit(sub("^\\|(.*)\\|$", "\\1", row), "|", fixed = TRUE)[[1]])

# The rows of the markdown table under the heading line `heading`, header first.
table_after <- function(text, heading) {
  start <- match(heading, text)
  rows <- which(startsWith(text, "|") & seq_along(text) > start)
  rows <- rows[seq_len(min(c(which(diff(rows) != 1), length(rows))))]
  lapply(text[rows[-2]], cells)
}

# DRAWS standard normal draws, chain by chain, each chain an AR(1) series with autocorrelation rho.
ar1 <- function(rho) {
  unlist(lapply(seq_len(CHAINS), \(chain) {
    x <- stats::rnorm(ITERS)
    for (t in 2:ITERS) x[t] <- rho * x[t - 1] + sqrt(1 - rho^2) * x[t]
    x
  }))
}

# The made-up parameters of one run, in draw order (chain by chain): per draw, the walk scales
# sigma and a random correlation matrix Omega over the ten parties, three houses' log phi, log
# phi_K, log kappa and s_bar_zeta. Each sigma and each entry of the matrix whose Gram matrix
# gives Omega mixes at its own rate, so the entries' ESS differ and the min names one of them.
# With `near`, some draws sit within 2 of a cap (20, 20, 2): log_phi[1] on 4 draws (and at exactly
# cap - 2 on a fifth, which is not within), log_phi[2] on 12, log_phi_K on 8 (and exactly at 18
# on one), log_kappa on 16 (and exactly at 0 on one).
make_params <- function(seed, near = FALSE) {
  P <- length(PARTIES)
  p <- withr::with_seed(seed, {
    sigma <- exp(-4 + 0.4 * vapply(stats::runif(P, 0, 0.9), ar1, numeric(DRAWS)))
    z <- vapply(stats::runif((P + 3) * P, 0, 0.95), ar1, numeric(DRAWS))
    omega <- lapply(seq_len(DRAWS), \(i) stats::cov2cor(crossprod(matrix(z[i, ], P + 3, P))))
    log_phi_K <- stats::rnorm(DRAWS, 8, 1)
    list(
      sigma = sigma, omega = omega, log_phi = matrix(stats::runif(DRAWS * 3, 5, 15), DRAWS, 3), log_phi_K = log_phi_K,
      log_kappa = stats::runif(DRAWS, -1.5, -0.5), s_bar_zeta = abs(0.1 + 0.02 * log_phi_K + stats::rnorm(DRAWS, 0, 0.03))
    )
  })
  if (near) {
    p$log_phi[1:4, 1] <- 18.5
    p$log_phi[5, 1] <- 18
    p$log_phi[6:17, 2] <- 19.9
    p$log_phi_K[18:25] <- 18.25
    p$log_phi_K[26] <- 18
    p$log_kappa[27:42] <- 0.5
    p$log_kappa[43] <- 0
  }
  p
}

# param_draws.parquet of `p`, as v2_extract() writes it: .chain, .iteration, .draw, then sigma,
# L_Omega (the Cholesky factor of Omega, column-major), other parameters, the log
# concentrations and s_bar_zeta[1]. With `reverse`, in the reversed party order (sigma reversed,
# L_Omega the factor of Omega with rows and columns reversed), as v2-perm's; with `shuffle`, its
# rows and columns in a random order, so that only names and .chain and .iteration place them.
param_table <- function(p, reverse = FALSE, shuffle = FALSE) {
  P <- ncol(p$sigma)
  o <- if (reverse) rev(seq_len(P)) else seq_len(P)
  pd <- list(.chain = rep(seq_len(CHAINS), each = ITERS), .iteration = rep(seq_len(ITERS), CHAINS), .draw = seq_len(DRAWS))
  for (j in seq_len(P)) pd[[sprintf("sigma[%d]", j)]] <- p$sigma[, o[j]]
  L <- lapply(p$omega, \(om) t(chol(om[o, o])))
  for (j in seq_len(P)) for (i in seq_len(P)) pd[[sprintf("L_Omega[%d,%d]", i, j)]] <- vapply(L, \(l) l[i, j], numeric(1))
  pd$tau_camp <- rep(0.5, DRAWS)
  for (h in seq_along(HOUSES)) pd[[sprintf("log_phi[%d]", h)]] <- p$log_phi[, h]
  pd$log_phi_K <- p$log_phi_K
  pd$log_kappa <- p$log_kappa
  pd[["s_bar_zeta[1]"]] <- p$s_bar_zeta
  pd <- tibble::as_tibble(pd)
  if (shuffle) pd <- withr::with_seed(5, pd[sample(nrow(pd)), sample(ncol(pd))])
  pd
}

# The key of a planted cell of ppc_results: election, kjördæmi (index into KD) and party (index
# into PARTIES).
planted <- function(election, k, p) paste(election, KD[k], PARTIES[p])

# ppc_results.parquet of one run: for each closed election, kjördæmi and party, DRAWS replicates of
# the official count y (read by name from the folded official table): half at y - 1, half at y + 1,
# except a planted cell's c(below, tie, above) counts at y - 1, y and y + 1; 0 for a party with no
# official count at that election. With `reverse`, the parties and kjördæmi in reversed order, and
# with `shuffle`, the rows in a random order.
ppc_table <- function(closed, plant = list(), reverse = FALSE, shuffle = FALSE) {
  folded <- fold_truth(read_truth())
  parties <- if (reverse) rev(PARTIES) else PARTIES
  kd <- if (reverse) rev(KD) else KD
  grid <- expand.grid(flokkur = parties, kjordaemi = kd, election = closed, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), \(i) {
    g <- grid[i, ]
    e <- as.Date(g$election)
    y <- folded$votes[folded$election == e & folded$kjordaemi == g$kjordaemi & folded$flokkur == g$flokkur]
    stopifnot(length(y) == 1)
    national <- sum(folded$votes[folded$election == e & folded$flokkur == g$flokkur])
    split <- plant[[paste(g$election, g$kjordaemi, g$flokkur)]]
    if (is.null(split)) split <- c(DRAWS / 2L, 0L, DRAWS / 2L)
    stopifnot(sum(split) == DRAWS)
    reps <- if (national == 0) rep(0L, DRAWS) else rep(c(y - 1L, y, y + 1L), split)
    tibble::tibble(
      .chain = rep(seq_len(CHAINS), each = ITERS), .iteration = rep(seq_len(ITERS), CHAINS), .draw = seq_len(DRAWS),
      election = e, kjordaemi = g$kjordaemi, flokkur = g$flokkur, votes = as.integer(reps)
    )
  })
  ppc <- dplyr::bind_rows(rows)
  if (shuffle) ppc <- withr::with_seed(6, ppc[sample(nrow(ppc)), ])
  ppc
}

# A finished V2-family run under `results`: its meta.json (passed through `edit_meta`), its
# param_draws.parquet and its ppc_results.parquet (each passed through its `edit_` function).
fake_run <- function(results, arm, cell, seed, closed, params, plant = list(), minutes = 1, reverse = FALSE,
                     shuffle = FALSE, edit_meta = identity, edit_pd = identity, edit_ppc = identity) {
  dir <- file.path(results, run_id(arm, cell[1], cell[2], seed))
  dir.create(dir, recursive = TRUE)
  meta <- list(
    arm = arm, role = if (arm == "v2") "candidate" else "check", E = cell[1], F = cell[2], id = basename(dir),
    seed = seed, iter_warmup = WARMUP, iter_sampling = ITERS, chains = CHAINS, max_treedepth = 12L, minutes = minutes,
    party_order = I(if (reverse) rev(PARTIES) else PARTIES), kjordaemi_levels = I(if (reverse) rev(KD) else KD),
    house_levels = I(HOUSES), closed_elections = I(closed), decoy_median_share = DECOY
  )
  jsonlite::write_json(edit_meta(meta), file.path(dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "string")
  arrow::write_parquet(edit_pd(param_table(params, reverse, shuffle)), file.path(dir, "param_draws.parquet"))
  arrow::write_parquet(edit_ppc(ppc_table(closed, plant, reverse, shuffle)), file.path(dir, "ppc_results.parquet"))
  invisible(dir)
}

# By hand, from the parameters themselves (Omega, not its factor): per draw C Sigma C' with an
# explicit C = I - 11'/P, and Omega's off-diagonal entries, all P^2 and P(P - 1) of them; each
# one's bulk ESS over the chains, and the min, rounded down as the report rounds.
flat_by_hand <- function(p) {
  P <- ncol(p$sigma)
  C <- diag(P) - matrix(1 / P, P, P)
  csc <- t(vapply(seq_len(DRAWS), \(i) {
    S <- diag(p$sigma[i, ]) %*% p$omega[[i]] %*% diag(p$sigma[i, ])
    c(C %*% S %*% t(C))
  }, numeric(P * P)))
  off <- t(vapply(seq_len(DRAWS), \(i) p$omega[[i]][row(diag(P)) != col(diag(P))], numeric(P * (P - 1))))
  ess <- function(m) apply(m, 2, \(x) posterior::ess_bulk(matrix(x, ITERS, CHAINS)))
  c(sprintf("%.0f", floor(round(min(ess(csc)), 6))), sprintf("%.0f", floor(round(min(ess(off)), 6))))
}

pearson <- function(x, y) sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2))

# The planted PPC cells of the 2024 eve runs, as (below, tie, above) counts of the 400 draws: 2016
# has three cells outside [0.025, 0.975] (mid-PIT 1, 0, and 0.0125: 10 ties, no draw below) and two
# exactly on its edge (0.025: 10 draws below; 0.025: 20 ties); 2017 has one outside (0.9875: 390
# below, 10 ties) and two on the edge (0.975); 2021 none (0.975, and 0.5 from 400 ties).
PLANT_EVE <- stats::setNames(
  list(
    c(400L, 0L, 0L), c(0L, 0L, 400L), c(10L, 0L, 390L), c(0L, 10L, 390L), c(0L, 20L, 380L),
    c(390L, 0L, 10L), c(390L, 10L, 0L), c(380L, 20L, 0L),
    c(380L, 20L, 0L), c(0L, 400L, 0L)
  ),
  c(
    planted("2016-10-29", 4, 1), planted("2016-10-29", 2, 5), planted("2016-10-29", 5, 2), planted("2016-10-29", 3, 6),
    planted("2016-10-29", 6, 10),
    planted("2017-10-28", 1, 3), planted("2017-10-28", 4, 7), planted("2017-10-28", 5, 8),
    planted("2021-09-25", 4, 9), planted("2021-09-25", 2, 4)
  )
)
# The 2021 f180 run's: two outside in 2016 (mid-PIT 0 and 1), none in 2017 (0.05).
PLANT_F180 <- stats::setNames(
  list(c(0L, 0L, 400L), c(400L, 0L, 0L), c(20L, 0L, 380L)),
  c(planted("2016-10-29", 3, 1), planted("2016-10-29", 6, 2), planted("2017-10-28", 1, 8))
)

HEADINGS <- c(
  "Arm", "Cell (E, F, tag)", "Seed", "Iterations (warmup + sampling)", "Minutes", "Minutes projected to 1000 + 2000",
  "Over 120 projected minutes (spec \u00a76.9)", "Corr(log \u03c6_K, s\u0304_\u03b6)",
  "Draws with log \u03c6_h > 18 (cap 20 \u2212 2), max over houses", "Draws with log \u03c6_K > 18 (cap 20 \u2212 2)",
  "Draws with log \u03ba > 0 (cap 2 \u2212 2)", "Min bulk ESS over the entries of C\u03a3C\u1d40",
  "Min bulk ESS over the off-diagonals of \u03a9 = L_Omega L_Omega\u1d40"
)
PPC_HEADINGS <- c("Arm", "Cell (E, F, tag)", "Seed", "Closed election", "Cells with PIT outside [0.025, 0.975]", "Cells")

test_that("the Tier 0 run list holds V2's and v2-perm's fits on 2024 eve and 2021 f180", {
  lines <- trimws(readLines(root("harness", "runs", "tier0-v2.txt")))
  expect_identical(lines[nzchar(lines) & !startsWith(lines, "#")], c(
    "A v2 2024-11-30 2024-11-29 1201", "B v2 2021-09-25 2021-03-29 1202",
    "A v2-perm 2024-11-30 2024-11-29 1201", "B v2-perm 2021-09-25 2021-03-29 1202"
  ))
})

test_that("the V2 checks report the hand-computed counts, correlation and ESS, by name, and no forecast", {
  results <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  eve <- make_params(11, near = TRUE)
  f180 <- make_params(12)
  fake_run(results, "v2", EVE, 1201L, CLOSED_EVE, eve, PLANT_EVE, minutes = 12.4)
  fake_run(results, "v2", F180, 1202L, CLOSED_F180, f180, PLANT_F180, minutes = 10)
  # WHY: v2-perm's parameters and replicates are V2's in the reversed orders, its rows and columns shuffled, so its row repeats V2's only if every value is read by name.
  fake_run(results, "v2-perm", EVE, 1201L, CLOSED_EVE, eve, PLANT_EVE, minutes = 17, reverse = TRUE, shuffle = TRUE)
  runs <- file.path(out_dir, "runs.txt")
  writeLines(c(
    "# lane arm E F seed",
    "A v2 2024-11-30 2024-11-29 1201",
    "B v2 2021-09-25 2021-03-29 1202",
    "A v2-perm 2024-11-30 2024-11-29 1201"
  ), runs)
  out <- file.path(out_dir, "v2_checks.md")
  tier0 <- root("harness", "tier0")
  before <- tools::md5sum(list.files(tier0, full.names = TRUE))

  shown <- checks(c(runs, out))
  expect_null(attr(shown, "status"), info = paste(shown, collapse = "\n"))
  # WHY: 17 minutes at 300 + 100 iterations project to 17 x 3000 / 400 = 127.5 minutes.
  expect_match(shown, "FLAG v2-perm_2024-11-30_2024-11-29_s1201: projected to 127.5 minutes at 1000 + 2000, over 120",
    fixed = TRUE, all = FALSE
  )
  expect_length(grep("^FLAG", shown), 1)
  text <- readLines(out, encoding = "UTF-8")

  main <- table_after(text, "## Parameters and compute")
  expect_identical(main[[1]], HEADINGS)
  expect_length(main, 4)
  flat_eve <- flat_by_hand(eve)
  flat_f180 <- flat_by_hand(f180)
  corr_eve <- sprintf("%.3f", pearson(eve$log_phi_K, eve$s_bar_zeta))
  corr_f180 <- sprintf("%.3f", pearson(f180$log_phi_K, f180$s_bar_zeta))
  near_eve <- c("12 of 400 (3.0%) in log_phi[2] (House B)", "8 of 400 (2.0%)", "16 of 400 (4.0%)")
  expect_identical(main[[2]], c(
    "v2", "2024-11-30, 2024-11-29, eve", "1201", "300 + 100", "12.4", "93.0", "no", corr_eve, near_eve, flat_eve
  ))
  expect_identical(main[[3]], c(
    "v2", "2021-09-25, 2021-03-29, f180", "1202", "300 + 100", "10.0", "75.0", "no", corr_f180,
    rep("0 of 400 (0.0%)", 3), flat_f180
  ))
  expect_identical(main[[4]], c(
    "v2-perm", "2024-11-30, 2024-11-29, eve", "1201", "300 + 100", "17.0", "127.5", "yes", corr_eve, near_eve, flat_eve
  ))

  ppc <- table_after(text, "## Past elections: in-sample PIT under `y_rep_res`")
  expect_identical(ppc[[1]], PPC_HEADINGS)
  # WHY: 2016 had 8 parties with official counts (no M, no J), 2017 9 (no J) and 2021 all 10, each in 6 kjördæmi.
  expect_identical(do.call(rbind, ppc[-1]), rbind(
    c("v2", "2024-11-30, 2024-11-29, eve", "1201", "2016-10-29", "3", "48"),
    c("v2", "2024-11-30, 2024-11-29, eve", "1201", "2017-10-28", "1", "54"),
    c("v2", "2024-11-30, 2024-11-29, eve", "1201", "2021-09-25", "0", "60"),
    c("v2", "2021-09-25, 2021-03-29, f180", "1202", "2016-10-29", "2", "48"),
    c("v2", "2021-09-25, 2021-03-29, f180", "1202", "2017-10-28", "0", "54"),
    c("v2-perm", "2024-11-30, 2024-11-29, eve", "1201", "2016-10-29", "3", "48"),
    c("v2-perm", "2024-11-30, 2024-11-29, eve", "1201", "2017-10-28", "1", "54"),
    c("v2-perm", "2024-11-30, 2024-11-29, eve", "1201", "2021-09-25", "0", "60")
  ))

  own <- gsub(out_dir, "", text, fixed = TRUE)
  expect_true(any(grepl("- Run list: `/runs.txt`", own, fixed = TRUE)))
  # WHY: no column, header or row label may be a forecast quantity, so the whole report is checked for the words.
  expect_false(any(grepl("\\b(share|votes|seats)\\b|eday|latent|pi_", own, ignore.case = TRUE)))
  expect_false(any(grepl("0\\.424|42\\.4|4242", own)))
  expect_identical(tools::md5sum(list.files(tier0, full.names = TRUE)), before)
})

test_that("the V2 checks stop, writing nothing, on a run they cannot read", {
  results <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  runs <- file.path(out_dir, "runs.txt")
  out <- file.path(out_dir, "v2_checks.md")
  writeLines(c("A v2 2021-09-25 2021-03-29 1202", "A v2 2024-11-30 2024-11-29 1201"), runs)
  p <- make_params(13)
  # WHY: a readable run comes first, so a stop on the second shows that nothing is written in part.
  fake_run(results, "v2", F180, 1202L, CLOSED_F180, p)
  second <- file.path(results, run_id("v2", EVE[1], EVE[2], 1201L))
  fails_with <- function(message, args = c(runs, out), ...) {
    unlink(second, recursive = TRUE)
    if (length(list(...))) fake_run(results, "v2", EVE, 1201L, CLOSED_EVE, p, ...)
    unlink(out)
    shown <- checks(args)
    expect_identical(attr(shown, "status"), 1L, label = message)
    expect_match(shown, message, fixed = TRUE, all = FALSE)
    expect_false(file.exists(out))
  }

  fails_with("usage: Rscript R/harness/v2_checks.R <run_list> [out]", args = character(0))
  fails_with("v2_checks: no meta.json")
  fails_with("is not of the run list's run v2_2024-11-30_2024-11-29_s1201", edit_meta = \(m) utils::modifyList(m, list(seed = 1299L)))
  fails_with("v2_checks: meta.json: minutes is not a number", edit_meta = \(m) m[names(m) != "minutes"])
  fails_with("v2_checks: param_draws.parquet lacks L_Omega[3,2]", edit_pd = \(pd) pd[names(pd) != "L_Omega[3,2]"])
  fails_with("v2_checks: param_draws.parquet holds 399 draws, not chains x iter_sampling = 400", edit_pd = \(pd) pd[-1, ])
  fails_with(
    "v2_checks: ppc_results.parquet's elections are not meta.json's closed elections",
    edit_meta = \(m) utils::modifyList(m, list(closed_elections = I(CLOSED_EVE[1:2])))
  )
  fails_with("v2_checks: ppc_results.parquet does not hold 400 draws of each cell", edit_ppc = \(ppc) ppc[-1, ])
  # WHY: Miðflokkurinn (PARTIES[8]) had no list in 2016, so its replicates there must be 0.
  fails_with("a replicate count for a party with no official count", edit_ppc = \(ppc) {
    hit <- which(ppc$election == as.Date("2016-10-29") & ppc$flokkur == PARTIES[8])[1]
    ppc$votes[hit] <- 5L
    ppc
  })
  writeLines("A v2 2017-10-28 2017-10-27 1201", runs)
  fails_with("v2_checks: 2017 is not checked before the freeze")
})
