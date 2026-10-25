box::use(
  R / harness / scorer[score_cell, read_run_meta],
  R / harness / truth[cell_truth],
  R / harness / tables[read_coalitions, formed_coalition, coalition_event],
  R / harness / scores[wide_draws, kd_to_nat, national_scores, party_scores, kd_scores, batch_of, batch_se],
  R / harness / scores[seat_draws, seat_scores],
  R / election_utils[layout_for]
)

E24 <- as.Date("2024-11-30")
F24 <- as.Date("2024-11-29")
D_B <- c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn")
# WHY: shares that put the 2024-11-29 event, D + B, near a majority of the seats, so the Brier score and its SE vary.
NOMINAL <- stats::setNames(c(0.255, 0.205, 0.195, 0.08, 0.06, 0.15, 0.12, 0.1, 0.05, 0.05), POLL_PARTIES)
SE_SCORES <- c("crps10", "is90", "dss10", "mdss", "d2", "mae8", "cov50", "cov90")

# A finished run of the cell (E, F) whose parties are Annað, the coalition that formed after E and the Brier
# event: by default the table's event at F, as the fit records it, or `event` in its place.
# WHY: jittered draws: constant ones make the alr covariance singular and break batch_se().
fake_run <- function(E, F, env = parent.frame(), event = NULL) {
  E <- as.Date(E)
  co <- read_coalitions()
  table_event <- coalition_event(F, co)
  parties <- c("Annað", intersect(POLL_PARTIES, c(formed_coalition(E, co), table_event$parties, event)))
  kd <- layout_for(E)$kjordaemi
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_seed(20261024)
  eday_kd <- tidyr::expand_grid(.chain = 1:4, .iteration = 1:40, kjordaemi = kd, flokkur = parties) |>
    dplyr::mutate(
      .draw = (.chain - 1L) * 40L + .iteration,
      votes = as.integer(round(20000 * NOMINAL[flokkur] * exp(stats::rnorm(dplyr::n(), 0, 0.1))))
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
    coalition_event = list(parties = if (is.null(event)) table_event$parties else event, dropped = table_event$dropped),
    diagnostics = list(rhat_max = 1.004)
  )
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  dir
}

# The 2024 result of the fake run's parties: shares the same in every kjördæmi; D + B short of a majority.
fixed_truth <- function(parties, kd) {
  nat <- stats::setNames(c(0.04, 0.25, 0.2, 0.21, 0.16, 0.14), c("Annað", D_B, "Samfylkingin", "Viðreisn", "Flokkur Fólksins"))
  seats <- stats::setNames(c(0L, 17L, 13L, 15L, 10L, 8L), names(nat))
  stopifnot(setequal(parties, names(nat)))
  list(
    nat = nat[parties],
    kd = tidyr::expand_grid(kjordaemi = kd, flokkur = parties) |>
      dplyr::mutate(votes = 1000L, share = unname(nat[flokkur])),
    seats = seats[parties]
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
  expect_identical(meta$party_order, c("Annað", D_B, "Samfylkingin", "Viðreisn", "Flokkur Fólksins"))
  expect_identical(meta$coalition_event$parties, D_B)
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

  direct_kd <- kd_scores(eday_kd, tr$kd)
  expect_named(s$kjordaemi, c(names(direct_kd), "se_kd_crps", "se_kd_cov90"))
  expect_equal(s$kjordaemi[names(direct_kd)], direct_kd, tolerance = 1e-12)
  # WHY: by hand: each long row's own block, the same 16 as the draws' (four per chain).
  kd_block <- batch_of(eday_kd$.chain, eday_kd$.iteration)
  expect_identical(sort(unique(kd_block)), 1:16)
  kd_se <- batch_se(
    \(x) unlist(kd_scores(eday_kd[x[, 1], ], tr$kd)[c("kd_crps", "kd_cov90")]), matrix(seq_len(nrow(eday_kd))), kd_block
  )
  se <- unlist(s$kjordaemi[c("se_kd_crps", "se_kd_cov90")])
  expect_equal(unname(se), unname(kd_se[c("kd_crps", "kd_cov90")]), tolerance = 1e-12)
  expect_true(all(is.finite(se) & se > 0), info = paste(names(se), se, collapse = "; "))

  seats <- seat_draws(eday_kd, layout_for(E24), ineligible = "Annað")
  expect_true(all(seats$seats[seats$flokkur == "Annað"] == 0))
  formed <- c("Samfylkingin", "Viðreisn", "Flokkur Fólksins")
  direct_seats <- seat_scores(seats, tr$seats, coalition = D_B, formed = formed)
  expect_named(s$seats, c(names(direct_seats$summary), "se_seat_crps", "se_seat_cov90", "se_brier"))
  expect_equal(s$seats[names(direct_seats$summary)], direct_seats$summary, tolerance = 1e-12)
  expect_equal(s$seats_per_party, direct_seats$per_party, tolerance = 1e-12)
  expect_gt(s$seats$p_majority, 0.05)
  expect_lt(s$seats$p_majority, 0.95)
  seat_block <- batch_of(seats$.chain, seats$.iteration)
  expect_identical(sort(unique(seat_block)), 1:16)
  seat_se <- batch_se(
    \(x) unlist(seat_scores(seats[x[, 1], ], tr$seats, coalition = D_B)$summary[c("seat_crps", "seat_cov90", "brier")]),
    matrix(seq_len(nrow(seats))), seat_block
  )
  se <- unlist(s$seats[c("se_seat_crps", "se_seat_cov90", "se_brier")])
  expect_equal(unname(se), unname(seat_se[c("seat_crps", "seat_cov90", "brier")]), tolerance = 1e-12)
  expect_true(all(is.finite(se) & se > 0), info = paste(names(se), se, collapse = "; "))
})

test_that("score_cell stops when the Brier event in meta.json is not the table's, before it reads a result", {
  fr <- frozen_root()
  reader <- function(...) {
    calls <<- calls + 1L
    stop("truth read")
  }
  calls <- 0L
  run <- fake_run(E24, F24, event = c(D_B, "Vinstri Græn"))
  expect_error(score_cell(run, truth = reader, root = fr), paste("coalition event changed since the fit: refit", basename(run)),
    fixed = TRUE
  )
  expect_equal(calls, 0L)
  expect_error(score_cell(fake_run(E24, F24, event = rev(D_B)), truth = reader, root = fr), "truth read", fixed = TRUE)
  expect_equal(calls, 1L)
})

test_that("score_cell with the default result reader refuses a root other than the project's", {
  root_msg <- "the default result reader reads this project's results, so root must be project_root(), not "
  fr <- frozen_root()
  expect_error(score_cell(fake_run(E24, F24), root = fr), paste0(root_msg, fr), fixed = TRUE)
  expect_error(score_cell(fake_run(E24, F24), truth = cell_truth, root = fr), paste0(root_msg, fr), fixed = TRUE)
  expect_error(score_cell(fake_run(E24, F24), root = root()), "no freeze")
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
