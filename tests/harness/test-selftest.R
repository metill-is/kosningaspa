box::use(
  R / harness / selftest[SELFTEST_FILES, selftest_file, selftest_present],
  R / harness / inputs[verify_snapshot],
  R / harness / constants[harness_path],
  R / harness / truth[cell_truth],
  R / harness / scores[crps_draws, wide_draws, national_scores, batch_of, batch_se, mae_se, NAMED8],
  R / harness / scores[kd_scores, seat_draws, seat_scores],
  R / election_utils[seats_tibble]
)

# WHY: the phase's one scoring of a known result (spec §5.6, §8). It re-scores draws the
# overdisp study already scored, so no harness arm meets a result before the freeze.
# The reference values are that study's published scores for base_eve, read on 2026-10-24.
ZERO_SHARE <- 0.5 / 196292
E <- "2024-11-30"
COALITION <- c("Samfylkingin", "Viðreisn", "Flokkur Fólksins")
NATIONAL_REF <- c(
  crps10 = 0.70877364221101147, is90 = 6.352848993229478, dss10 = 2.4305189899030442,
  mdss = -0.87010115800367061, d2 = 36.537669694218991, es = 2.6166490644078086,
  mae8 = 1.1389019529791662, mae_all = 0.93983520886613281, w90 = 3.050931775110548,
  cov90 = 6, zero_cells_replaced = 0
)
SE_REF <- c(
  crps10 = 0.0051017190249985113, is90 = 0.073069598173392494, dss10 = 0.034319142943985859,
  mdss = 0.90728689419264363, d2 = 0.94081239456516885
)
KD_REF <- c(kd_mae_named = 1.3773025672446, kd_crps = 0.9089015855884458, kd_w90 = 3.1471739157676528, kd_cov90 = 37)
SEAT_REF <- c(seat_crps = 4.0283096250000145, seat_abs_err = 5, seat_w90 = 2.8, seat_cov90 = 10, p_majority = 0.839875)
SEAT_CRPS_SE <- 0.04525820426471517

skip_without_selftest <- function() {
  skip_if_not(selftest_present(), "no scorer self-test inputs: run selftest_inputs()")
}
draws <- function() dplyr::rename(arrow::read_parquet(selftest_file("eday_national")), share = value)
kd_draws <- function() arrow::read_parquet(selftest_file("constituency_predictions"))

test_that("the self-test manifest names its two files by their path in the study's folder", {
  m <- readr::read_csv(harness_path("selftest_manifest.csv"), col_types = readr::cols(.default = "c"))
  expect_equal(m$name, names(SELFTEST_FILES))
  expect_equal(m$source, unname(SELFTEST_FILES))
  expect_equal(m$file, basename(SELFTEST_FILES))
  expect_true(all(grepl("^[0-9a-f]{64}$", m$sha256)))
  expect_false(any(grepl("^/|~", m$source)))
})

test_that("the self-test inputs match their manifest", {
  skip_without_selftest()
  expect_true(verify_snapshot(harness_path("selftest"), harness_path("selftest_manifest.csv")))
})

test_that("the scorer reproduces base_eve's national scores and their SEs", {
  skip_without_selftest()
  X <- wide_draws(draws(), "share")
  expect_equal(dim(X), c(8000L, 10L))
  y <- cell_truth(E, colnames(X))$nat
  s <- national_scores(X, y, ZERO_SHARE)
  for (k in names(NATIONAL_REF)) expect_equal(s[[k]], NATIONAL_REF[[k]], tolerance = 1e-9, label = k)
  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  expect_equal(length(unique(block)), 16)
  se <- batch_se(\(x) unlist(national_scores(x, y, ZERO_SHARE)[names(SE_REF)]), X, block)
  for (k in names(SE_REF)) expect_equal(se[[k]], SE_REF[[k]], tolerance = 1e-9, label = paste("se", k))
  expect_equal(mae_se(X, y, intersect(NAMED8, colnames(X))), 0.0082496541290049616, tolerance = 1e-9, label = "se mae8")
  expect_equal(mae_se(X, y, colnames(X)), 0.0067659892340684201, tolerance = 1e-9, label = "se mae_all")
})

test_that("the scorer reproduces base_eve's kjördæmi scores", {
  skip_without_selftest()
  skip_unless_slow()
  cp <- kd_draws()
  s <- kd_scores(cp, cell_truth(E, unique(cp$flokkur))$kd)
  expect_equal(s$kd_cells, 60L)
  for (k in names(KD_REF)) expect_equal(s[[k]], KD_REF[[k]], tolerance = 1e-9, label = k)
})

test_that("the scorer reproduces base_eve's seat scores", {
  skip_without_selftest()
  skip_unless_slow()
  cp <- kd_draws()
  seats <- seat_draws(cp, seats_tibble(), ineligible = character(0))
  y <- cell_truth(E, unique(cp$flokkur))$seats
  # WHY: the study's p_gov32 is the majority of the coalition that formed, which is this call's coalition
  s <- seat_scores(seats, y, coalition = COALITION)$summary
  for (k in names(SEAT_REF)) expect_equal(s[[k]], SEAT_REF[[k]], tolerance = 1e-9, label = k)
  X <- wide_draws(seats, "seats")
  expect_equal(nrow(X), 8000L)
  expect_true(all(rowSums(X) == 63))
  yx <- y[colnames(X)]
  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  se <- batch_se(\(x) c(seat_crps = sum(vapply(seq_along(yx), \(j) crps_draws(x[, j], yx[[j]]), 0))), X, block)
  expect_equal(se[["seat_crps"]], SEAT_CRPS_SE, tolerance = 1e-9, label = "se seat_crps")
})
