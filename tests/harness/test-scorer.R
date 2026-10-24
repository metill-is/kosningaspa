box::use(
  R / harness / scorer[score_cell, read_run_meta],
  R / harness / tables[read_coalitions, formed_coalition],
  R / harness / scores[wide_draws, kd_to_nat, national_scores, party_scores, kd_scores, batch_of, batch_se],
  R / harness / scores[seat_draws, seat_scores],
  R / election_utils[layout_for]
)

E24 <- as.Date("2024-11-30")
F24 <- as.Date("2024-11-29")
SHARES <- c(0.1, 0.45, 0.25, 0.2)
SE_SCORES <- c("crps10", "is90", "dss10", "mdss", "d2", "mae8", "cov50", "cov90")

# WHY: jittered draws: constant ones make the alr covariance singular and break batch_se().
fake_run <- function(E, F, env = parent.frame()) {
  E <- as.Date(E)
  parties <- c("Annað", formed_coalition(E, read_coalitions()))
  kd <- layout_for(E)$kjordaemi
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_seed(20261024)
  eday_kd <- tidyr::expand_grid(.chain = 1:4, .iteration = 1:40, kjordaemi = kd, flokkur = parties) |>
    dplyr::mutate(
      .draw = (.chain - 1L) * 40L + .iteration,
      votes = as.integer(round(20000 * SHARES[match(flokkur, parties)] * exp(stats::rnorm(dplyr::n(), 0, 0.1))))
    ) |>
    dplyr::select(.chain, .iteration, .draw, kjordaemi, flokkur, votes)
  eday_nat <- kd_to_nat(eday_kd) |>
    dplyr::mutate(share = share * exp(stats::rnorm(dplyr::n(), 0, 0.03))) |>
    dplyr::mutate(share = share / sum(share), .by = .draw) |>
    dplyr::select(.chain, .iteration, .draw, flokkur, share)
  arrow::write_parquet(eday_kd, file.path(dir, "eday_kjordaemi.parquet"))
  arrow::write_parquet(eday_nat, file.path(dir, "eday_national.parquet"))
  meta <- list(
    arm = "r0", E = format(E), F = format(as.Date(F)), seed = 1101L,
    party_order = parties, kjordaemi_levels = kd,
    n_pred = 200000, n_pred_k = as.list(stats::setNames(c(30000, 32000, 70000, 30000, 28000, 20000), kd)),
    coalition_event = list(parties = parties[3:4], dropped = character(0)),
    diagnostics = list(rhat_max = 1.004)
  )
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  dir
}

fixed_truth <- function(parties, kd) {
  list(
    nat = stats::setNames(c(0.12, 0.42, 0.25, 0.21), parties),
    kd = tidyr::expand_grid(kjordaemi = kd, flokkur = parties) |>
      dplyr::mutate(votes = 1000L, share = rep(c(0.12, 0.42, 0.25, 0.21), length(kd))),
    seats = stats::setNames(c(0L, 30L, 18L, 15L), parties)
  )
}

test_that("score_cell refuses every target cell while the repo has no freeze, before reading any result", {
  expect_false(file.exists(root("harness", "FREEZE.md")))
  for (i in seq_len(nrow(TARGETS))) {
    calls <- 0L
    reader <- function(...) {
      calls <<- calls + 1L
      stop("truth read")
    }
    expect_error(score_cell(fake_run(TARGETS$E[i], TARGETS$F[i]), truth = reader), "no freeze")
    expect_equal(calls, 0L)
  }
})

test_that("score_cell refuses a cell outside TARGETS, even under a freeze", {
  calls <- 0L
  reader <- function(...) {
    calls <<- calls + 1L
    stop("truth read")
  }
  run <- fake_run(E24, "2024-09-01")
  expect_error(score_cell(run, truth = reader, root = frozen_root()), "not a forecast cell")
  expect_equal(calls, 0L)
})

test_that("score_cell refuses a 2017 cell under a committed decision: Tier C scores it", {
  fr <- frozen_root(decision = TRUE)
  for (F in c("2017-10-14", "2017-10-27")) {
    calls <- 0L
    reader <- function(...) {
      calls <<- calls + 1L
      stop("truth read")
    }
    expect_error(
      score_cell(fake_run("2017-10-28", F), truth = reader, root = fr),
      "2017 cells are scored by the Tier C runbook, which writes harness/CONFIRM.md", fixed = TRUE
    )
    expect_equal(calls, 0L)
  }
  expect_error(score_cell(fake_run("2017-10-28", "2017-10-27"), truth = stop, root = frozen_root()), "2017 is locked")
})

test_that("read_run_meta gives n_pred_k as a named numeric and party_order as a character vector", {
  run <- fake_run(E24, F24)
  meta <- read_run_meta(run)
  kd <- layout_for(E24)$kjordaemi
  expect_identical(meta$n_pred_k, stats::setNames(c(30000, 32000, 70000, 30000, 28000, 20000), kd))
  expect_identical(meta$party_order, c("Annað", "Samfylkingin", "Viðreisn", "Flokkur Fólksins"))
  expect_identical(meta$coalition_event$parties, c("Viðreisn", "Flokkur Fólksins"))
  expect_identical(meta$E, E24)
  expect_identical(meta$F, F24)
  expect_identical(meta$n_pred, 200000)
  expect_identical(meta$arm, "r0")
  m <- jsonlite::read_json(file.path(run, "meta.json"))
  m$n_pred_k <- NULL
  m$arm <- NULL
  jsonlite::write_json(m, file.path(run, "meta.json"), auto_unbox = TRUE)
  expect_error(read_run_meta(run), "meta.json lacks: arm, n_pred_k", fixed = TRUE)
  rewrite <- function(field, value) {
    m <- jsonlite::read_json(file.path(fake_run(E24, F24), "meta.json"))
    m[[field]] <- value
    run <- withr::local_tempdir(.local_envir = parent.frame())
    jsonlite::write_json(m, file.path(run, "meta.json"), auto_unbox = TRUE)
    run
  }
  no_parties <- rewrite("coalition_event", list(dropped = "A"))
  expect_error(read_run_meta(no_parties), "lacks: coalition_event$parties", fixed = TRUE)
  not_run <- rewrite("coalition_event", list(parties = "Píratar"))
  expect_error(read_run_meta(not_run), "coalition_event$parties does not parse", fixed = TRUE)
  expect_error(read_run_meta(rewrite("n_pred_k", list(1, 2))), "n_pred_k does not parse", fixed = TRUE)
  expect_error(read_run_meta(rewrite("E", "2024-13-30")), "E does not parse", fixed = TRUE)
})

test_that("score_cell scores the kjördæmi aggregate and reports the eday reading beside it", {
  fr <- frozen_root()
  run <- fake_run(E24, F24)
  meta <- read_run_meta(run)
  tr <- fixed_truth(meta$party_order, layout_for(E24)$kjordaemi)
  calls <- list()
  reader <- function(E, parties) {
    calls[[length(calls) + 1L]] <<- list(E = E, parties = parties)
    tr
  }
  s <- score_cell(run, truth = reader, root = fr)
  expect_length(calls, 1)
  expect_identical(as.Date(calls[[1]]$E), E24)
  expect_identical(calls[[1]]$parties, meta$party_order)
  expect_named(s, c("cell", "national", "gap_pp", "per_party", "kjordaemi", "seats", "seats_per_party"))
  expect_identical(s$cell, tibble::tibble(arm = "r0", E = E24, F = F24, tag = "eve"))

  eday_kd <- arrow::read_parquet(file.path(run, "eday_kjordaemi.parquet"))
  X <- wide_draws(kd_to_nat(eday_kd), "share")
  Xn <- wide_draws(arrow::read_parquet(file.path(run, "eday_national.parquet")), "share")
  zero_kd <- 0.5 / sum(meta$n_pred_k)
  direct <- national_scores(X, tr$nat, zero_kd)
  expect_identical(s$national$reading, c("kd_aggregate", "eday_national"))
  expect_equal(s$national$zero_share, c(zero_kd, 0.5 / 200000))
  expect_equal(s$national[1, names(direct)], direct, tolerance = 1e-12)
  expect_equal(s$national[2, names(direct)], national_scores(Xn, tr$nat, 0.5 / 200000), tolerance = 1e-12)

  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  expect_length(unique(block), 16)
  se <- batch_se(\(x) unlist(national_scores(x, tr$nat, zero_kd)[SE_SCORES]), X, block)
  se_cols <- paste0("se_", SE_SCORES)
  expect_equal(unlist(s$national[1, se_cols]), stats::setNames(se, se_cols), tolerance = 1e-12)
  expect_true(all(is.finite(unlist(s$national[2, paste0("se_", SE_SCORES)]))))
  expect_true(all(unlist(s$national[, paste0("se_", c("crps10", "d2"))]) > 0))

  expect_equal(s$gap_pp, 100 * max(abs(colMeans(X) - colMeans(Xn)[colnames(X)])), tolerance = 1e-12)
  expect_gt(s$gap_pp, 0)

  ps <- party_scores(X, tr$nat)
  expect_equal(s$per_party[names(ps)], ps, tolerance = 1e-12)
  pit_se <- batch_se(\(x) stats::setNames(party_scores(x, tr$nat)$pit, colnames(x)), X, block)
  expect_equal(s$per_party$pit_se, unname(pit_se[ps$flokkur]), tolerance = 1e-12)

  expect_equal(s$kjordaemi, kd_scores(eday_kd, tr$kd), tolerance = 1e-12)

  seats <- seat_draws(eday_kd, layout_for(E24), ineligible = "Annað")
  expect_true(all(seats$seats[seats$flokkur == "Annað"] == 0))
  direct_seats <- seat_scores(
    seats, tr$seats,
    coalition = c("Viðreisn", "Flokkur Fólksins"), formed = c("Samfylkingin", "Viðreisn", "Flokkur Fólksins")
  )
  expect_equal(s$seats, direct_seats$summary, tolerance = 1e-12)
  expect_equal(s$seats_per_party, direct_seats$per_party, tolerance = 1e-12)
  expect_gt(s$seats$p_majority, 0.05)
  expect_lt(s$seats$p_majority, 0.95)
  expect_equal(s$seats$p_formed, 1)
})

test_that("score_cell stops on a forecast party with no list at E before it reads a draw", {
  fr <- frozen_root()
  run <- fake_run(E24, F24)
  meta <- read_run_meta(run)
  tr <- fixed_truth(meta$party_order, layout_for(E24)$kjordaemi)
  tr$nat[c("Viðreisn", "Flokkur Fólksins")] <- c(0, 0)
  calls <- 0L
  reader <- function(...) {
    calls <<- calls + 1L
    tr
  }
  file.remove(file.path(run, c("eday_kjordaemi.parquet", "eday_national.parquet")))
  expect_error(score_cell(run, truth = reader, root = fr), "no list at E: Viðreisn, Flokkur Fólksins", fixed = TRUE)
  expect_equal(calls, 1L)
})

test_that("score_cell refuses kjördæmi draws whose .draw restarts in each chain", {
  fr <- frozen_root()
  run <- fake_run(E24, F24)
  meta <- read_run_meta(run)
  path <- file.path(run, "eday_kjordaemi.parquet")
  eday_kd <- arrow::read_parquet(path)
  eday_kd$.draw <- eday_kd$.iteration
  arrow::write_parquet(eday_kd, path)
  reader <- function(...) fixed_truth(meta$party_order, layout_for(E24)$kjordaemi)
  expect_error(score_cell(run, truth = reader, root = fr), "repeats a (.draw, kjordaemi, flokkur) cell", fixed = TRUE)
})
