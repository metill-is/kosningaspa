# Scoring a 2016 cell: Flokkur Folksins, which had no admitted poll at 2016-05-02, folded into
# Annad for that cell, and the 2016 seat scores, which Annad's official seats make not comparable;
# and how the report prints a cell's MAE8, fold and seat errors.
box::use(
  R / harness / truth[cell_truth, read_truth],
  R / harness / scorer[score_cell, fold_parties],
  R / harness / score_report[adoption_criteria, cell_section, mae_row],
  R / harness / tables[read_coalitions, coalition_event, formed_coalition],
  R / harness / scores[wide_draws, kd_to_nat, national_scores, seat_draws, seat_scores, NAMED8],
  R / harness / runs[run_id],
  R / election_utils[layout_for]
)

E16 <- as.Date("2016-10-29")
E24 <- as.Date("2024-11-30")
ANNAD <- "Anna\u00f0"
FF <- "Flokkur F\u00f3lksins"
stopifnot(identical(POLL_PARTIES[c(7, 10)], c(FF, ANNAD)))
# WHY: the parties of the 2016-05-02 cell, which has no Flokkur Folksins, and of the later 2016 cells.
SEVEN <- POLL_PARTIES[c(1:6, 10)]
EIGHT <- POLL_PARTIES[c(1:7, 10)]
NOMINAL <- stats::setNames(c(0.29, 0.115, 0.057, 0.159, 0.145, 0.105, 0.035, 0.03, 0.02, 0.094), POLL_PARTIES)
N_PRED_K <- c(30000, 32000, 70000, 30000, 28000, 20000)
SCORE_NAMES <- c("cell", "national", "gap_pp", "draws", "per_party", "kjordaemi", "seats", "seats_per_party")

# A finished run of `arm` on the cell (E, F) forecasting `parties`, with the coalition event the table
# records at F: in `results` under its queue id (seed 1101), or in a temporary directory.
# WHY: jittered draws: constant ones make the alr covariance singular and break batch_se().
fake_run <- function(E, F, parties, arm = "r0", results = NULL, env = parent.frame()) {
  E <- as.Date(E)
  F <- as.Date(F)
  event <- coalition_event(F, read_coalitions())
  kd <- layout_for(E)$kjordaemi
  dir <- if (is.null(results)) withr::local_tempdir(.local_envir = env) else file.path(results, run_id(arm, E, F, 1101L))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::local_seed(if (arm == "r0") 20261111 else 20261112)
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
    arm = arm, E = format(E), F = format(F), seed = 1101L, iter_warmup = 40L, iter_sampling = 40L,
    party_order = parties, kjordaemi_levels = kd,
    n_pred = 200000, n_pred_k = as.list(stats::setNames(N_PRED_K, kd)),
    coalition_event = list(parties = event$parties, dropped = I(event$dropped)),
    gate = list(pass = TRUE, reasons = I(character(0)))
  )
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  dir
}

test_that("cell_truth folds Flokkur F\u00f3lksins into Anna\u00f0 for the seven parties of the 2016-05-02 cell, keeping every vote and seat (votes dropped from the denominator, or a party's votes lost)", {
  ct <- cell_truth(E16, SEVEN, fold = FF)
  full <- cell_truth(E16, EIGHT)
  expect_named(ct$nat, SEVEN)
  expect_equal(sum(ct$nat), 1)
  expect_equal(ct$nat[[ANNAD]], full$nat[[ANNAD]] + full$nat[[FF]])
  expect_gt(full$nat[[FF]], 0.035)
  expect_equal(ct$nat[setdiff(SEVEN, ANNAD)], full$nat[setdiff(SEVEN, ANNAD)])
  expect_equal(nrow(ct$kd), 6 * 7)
  expect_equal(sum(ct$kd$votes), 189648)
  expect_equal(unname(c(tapply(ct$kd$share, ct$kd$kjordaemi, sum))), rep(1, 6))
  both <- full$kd[full$kd$flokkur %in% c(ANNAD, FF), ]
  annad <- ct$kd[ct$kd$flokkur == ANNAD, ]
  expect_equal(annad$votes, unname(c(tapply(both$votes, both$kjordaemi, sum))[annad$kjordaemi]))
  expect_identical(sum(ct$seats), 63L)
  expect_identical(ct$seats[[ANNAD]], full$seats[[ANNAD]] + full$seats[[FF]])
  expect_identical(ct$seats[[ANNAD]], 4L)
})

test_that("without a fold, cell_truth refuses the seven-party 2016 result as before (a fold switched on by default)", {
  expect_error(cell_truth(E16, SEVEN), paste0("truth has votes for parties the run does not forecast: ", FF), fixed = TRUE)
  expect_error(cell_truth(E16, SEVEN, fold = character(0)), "truth has votes for parties the run does not forecast", fixed = TRUE)
})

test_that("cell_truth refuses a fold of a party the run forecasts, of a party with no votes at E, or into a run with no Anna\u00f0 (a fold that hides a real mismatch)", {
  expect_error(cell_truth(E16, EIGHT, fold = FF), paste0("cannot fold parties the run forecasts: ", FF), fixed = TRUE)
  expect_error(
    cell_truth(E16, SEVEN, fold = c(FF, POLL_PARTIES[8])),
    paste0("cannot fold parties with no votes at 2016-10-29: ", POLL_PARTIES[8]),
    fixed = TRUE
  )
  expect_error(
    cell_truth(E16, setdiff(SEVEN, ANNAD), fold = FF),
    paste0("cannot fold into ", ANNAD, ", which the run does not forecast: ", FF),
    fixed = TRUE
  )
  expect_error(cell_truth(E16, SEVEN, fold = "Bj\u00f6rt framt\u00ed\u00f0"), "not poll parties: Bj\u00f6rt framt\u00ed\u00f0", fixed = TRUE)
})

test_that("fold_parties names a party with votes at E only when the cell has none of it, and reads the cell only when the run leaves out such a party (a fold of a party the cell has, which would hide a run that drops it)", {
  truth <- read_truth()
  never <- function(E, F) stop("cell read")
  expect_identical(fold_parties(E24, as.Date("2024-11-29"), POLL_PARTIES, truth, never), character(0))
  expect_identical(fold_parties(E16, as.Date("2016-10-28"), EIGHT, truth, never), character(0))
  expect_identical(fold_parties(E16, as.Date("2016-05-02"), SEVEN, truth, \(E, F) SEVEN), FF)
  expect_identical(fold_parties(E16, as.Date("2016-10-28"), SEVEN, truth, \(E, F) EIGHT), character(0))
  expect_identical(fold_parties(E16, as.Date("2016-05-02"), setdiff(SEVEN, POLL_PARTIES[6]), truth, \(E, F) SEVEN), FF)
  expect_error(
    fold_parties(E16, as.Date("2016-05-02"), SEVEN, truth, \(E, F) NULL),
    paste0("truth has votes for parties the run does not forecast: ", FF),
    fixed = TRUE
  )
})

test_that("fold_parties reads each 2016 cell's parties from the snapshot: Flokkur F\u00f3lksins is folded at 2016-05-02 and at no later 2016 cell (a fold read from the wrong cell)", {
  skip_without_snapshot()
  truth <- read_truth()
  expect_identical(fold_parties(E16, as.Date("2016-05-02"), SEVEN, truth), FF)
  for (F in c("2016-09-12", "2016-10-15", "2016-10-28")) {
    expect_identical(fold_parties(E16, as.Date(F), SEVEN, truth), character(0), label = F)
  }
})

test_that("score_cell scores a made-up 2016-05-02 run of the seven parties against the folded result and marks its seat scores not comparable (a 2016 seat score passed off as comparable)", {
  skip_without_snapshot()
  run <- fake_run(E16, "2016-05-02", SEVEN)
  s <- score_cell(run, root = root())
  expect_named(s, c(SCORE_NAMES, "seats_not_comparable"))
  expect_identical(s$cell$tag, "f180")
  folded <- cell_truth(E16, SEVEN, fold = FF)
  expect_equal(s$per_party$actual[match(SEVEN, s$per_party$flokkur)], unname(folded$nat[SEVEN]))
  expect_true(all(is.finite(s$national$crps10)))
  expect_true(all(is.finite(unlist(s$seats[c("seat_crps", "seat_abs_err", "brier", "p_formed")]))))
  expect_identical(nrow(s$seats_per_party), 7L)
  expect_true(all(s$seats_per_party$median[s$seats_per_party$flokkur == ANNAD] == 0))
  expect_match(s$seats_not_comparable, paste(ANNAD, "holds 4 official seats at E"), fixed = TRUE)
})

test_that("score_cell refuses a made-up 2016-10-28 run that leaves out Flokkur F\u00f3lksins, a party of that cell, before it reads a draw (a run that silently drops a party scored as though folded)", {
  skip_without_snapshot()
  run <- fake_run(E16, "2016-10-28", SEVEN)
  file.remove(file.path(run, c("eday_kjordaemi.parquet", "eday_national.parquet")))
  expect_error(
    score_cell(run, root = root()),
    paste0("truth has votes for parties the run does not forecast: ", FF),
    fixed = TRUE
  )
})

test_that("score_cell scores a made-up 2024 run against the result cell_truth gives with no fold, and adds no seat flag (a fold or flag reaching a cell with nothing to fold)", {
  F24 <- as.Date("2024-11-29")
  run <- fake_run(E24, F24, POLL_PARTIES)
  s <- score_cell(run, root = root())
  expect_named(s, SCORE_NAMES)
  tr <- cell_truth(E24, POLL_PARTIES)
  eday_kd <- arrow::read_parquet(file.path(run, "eday_kjordaemi.parquet"))
  direct <- national_scores(wide_draws(kd_to_nat(eday_kd), "share"), tr$nat, 0.5 / sum(N_PRED_K))
  expect_equal(s$national[1, names(direct)], direct, tolerance = 1e-12)
  co <- read_coalitions()
  seats <- seat_draws(eday_kd, layout_for(E24), ineligible = ANNAD)
  direct_seats <- seat_scores(seats, tr$seats, coalition = coalition_event(F24, co)$parties, formed = formed_coalition(E24, co))
  expect_equal(s$seats[names(direct_seats$summary)], direct_seats$summary, tolerance = 1e-12)
})

test_that("adoption_criteria leaves seat CRPS out of criterion 4 when either run's seat scores are not comparable, and reads it otherwise (a non-comparable seat score deciding criterion 4)", {
  base <- list(
    crps10 = 1.0, se_crps10 = 0.01, mdss = -8, se_mdss = 0.1, d2 = 8, se_d2 = 0.2, n_parties = 10,
    kd_crps = 1.5, se_kd_crps = 0.01, seat_crps_pp = 1.0, se_seat_crps_pp = 0.01
  )
  seat_loss <- utils::modifyList(base, list(seat_crps_pp = 1.5, mdss = -9))
  expect_false(adoption_criteria(base, seat_loss)[["regional"]])
  expect_false(adoption_criteria(c(base, seats_comparable = TRUE), c(seat_loss, seats_comparable = TRUE))[["regional"]])
  expect_true(adoption_criteria(c(base, seats_comparable = FALSE), c(seat_loss, seats_comparable = FALSE))[["regional"]])
  expect_true(adoption_criteria(base, c(seat_loss, seats_comparable = FALSE))[["regional"]])
  kd_loss <- utils::modifyList(seat_loss, list(kd_crps = 1.7))
  expect_false(adoption_criteria(c(base, seats_comparable = FALSE), c(kd_loss, seats_comparable = FALSE))[["regional"]])
})

# score_report.R run from the project root on the runs in `results`; its output lines, with the exit
# status as attribute "status" when it is not 0.
report <- function(results, run_list, out) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/score_report.R", "r0", "v2", run_list, out),
    stdout = TRUE, stderr = TRUE, env = c("LC_ALL=en_US.UTF-8", paste0("HARNESS_RESULTS=", results))
  )))
}

# The lines of a report from the heading that starts with `heading` to the next heading.
section <- function(text, heading) {
  start <- which(startsWith(text, heading))
  stopifnot(length(start) == 1)
  after <- which(startsWith(text, "## ") & seq_along(text) > start)
  text[start:(if (length(after)) min(after) - 1L else length(text))]
}

test_that("the report marks a 2016 cell's seat rows not comparable and leaves them out of criterion 4, and writes a 2024 cell's section as it would alone (a non-comparable seat score judging two arms, or the mark leaking into another cell)", {
  skip_without_snapshot()
  results <- withr::local_tempdir()
  for (arm in c("r0", "v2")) {
    fake_run(E16, "2016-05-02", SEVEN, arm, results)
    fake_run(E24, "2024-11-29", POLL_PARTIES, arm, results)
  }
  cells24 <- c("A r0 2024-11-30 2024-11-29 1101", "B v2 2024-11-30 2024-11-29 1101")
  writeLines(c("A r0 2016-10-29 2016-05-02 1101", "B v2 2016-10-29 2016-05-02 1101", cells24), file.path(results, "both.txt"))
  writeLines(cells24, file.path(results, "only24.txt"))
  for (list in c("both", "only24")) {
    out <- report(results, file.path(results, paste0(list, ".txt")), file.path(results, paste0(list, ".md")))
    expect_null(attr(out, "status"), label = list)
  }
  text <- readLines(file.path(results, "both.md"), encoding = "UTF-8")

  s16 <- section(text, "## 2016, f180")
  seat_rows <- s16[grepl("^\\| (Seat |Brier |P\\(majority )", s16)]
  expect_length(seat_rows, 5)
  expect_true(all(grepl(" (not comparable) |", seat_rows, fixed = TRUE)))
  expect_true(all(endsWith(seat_rows, "| not comparable |")))
  expect_false(any(grepl("not comparable", setdiff(s16[startsWith(s16, "| ")], seat_rows), fixed = TRUE)))
  expect_length(grep("^4\\. No regional loss: .*; seat CRPS per party left out, not comparable: (yes|no)\\.$", s16), 1)
  expect_length(grep(paste("^Seat scores not comparable on this cell:", ANNAD, "holds 4 official seats at E"), s16), 1)
  rows16 <- text[startsWith(text, "| 2016 f180 |")]
  expect_length(rows16, 2)
  expect_length(grep("[0-9] \\(not comparable\\) \\| [0-9]+ \\(not comparable\\) \\|$", rows16), 1)

  s24 <- section(text, "## 2024, eve")
  expect_identical(s24, section(readLines(file.path(results, "only24.md"), encoding = "UTF-8"), "## 2024, eve"))
  expect_false(any(grepl("not comparable", c(s24, text[startsWith(text, "| 2024 eve |")]), fixed = TRUE)))
  expect_length(grep("^4\\. No regional loss: .*; seat CRPS per party -?[0-9.]+ \\((tie|better|worse)\\), at most 0.05 worse or a tie: (yes|no)\\.$", s24), 1)
})

# score_cell() of a made-up run of `arm` on the cell (E, F) forecasting `parties`, scored once per suite run.
scored <- function(E, F, parties, arm) {
  skip_without_snapshot()
  cached(paste("score16", E, F, arm, length(parties)), score_cell(fake_run(E, F, parties, arm), root = root()))
}

# The sections of three made-up cells, built once per suite run: 2016-05-02 with both arms on the
# seven parties, the same cell with V2 on eight, and 2024 eve with both arms on every party.
sections <- function() {
  r0_16 <- scored(E16, "2016-05-02", SEVEN, "r0")
  v2_16 <- scored(E16, "2016-05-02", SEVEN, "v2")
  v2_16_8 <- scored(E16, "2016-05-02", EIGHT, "v2")
  r0_24 <- scored(E24, "2024-11-29", POLL_PARTIES, "r0")
  v2_24 <- scored(E24, "2024-11-29", POLL_PARTIES, "v2")
  cached("score16_sections", list(
    f180 = cell_section(v2_16$cell, "r0", "v2", r0_16, v2_16),
    mixed = cell_section(v2_16_8$cell, "r0", "v2", r0_16, v2_16_8),
    eve24 = cell_section(v2_24$cell, "r0", "v2", r0_24, v2_24)
  ))
}

# The MAE8 row of a cell section.
mae8_row <- function(lines) {
  r <- lines[startsWith(lines, "| MAE8, pp ")]
  stopifnot(length(r) == 1)
  r
}

test_that("each cell's MAE8 row gives the number of named parties its mean runs over, the named parties each run forecasts: 6 on the seven-party 2016-05-02 cell, 7 with Flokkur F\u00f3lksins, 8 in 2024, and each arm's number where they differ (a row that claims the eight named parties on a cell that scores fewer)", {
  r0_16 <- scored(E16, "2016-05-02", SEVEN, "r0")
  v2_16_8 <- scored(E16, "2016-05-02", EIGHT, "v2")
  r0_24 <- scored(E24, "2024-11-29", POLL_PARTIES, "r0")
  for (s in list(r0_16, v2_16_8, r0_24)) {
    named <- s$per_party$flokkur %in% NAMED8
    nat <- s$national[s$national$reading == "kd_aggregate", ]
    expect_equal(nat$mae8, 100 * mean(s$per_party$abs_err[named]))
  }
  expect_identical(vapply(list(r0_16, v2_16_8, r0_24), \(s) sum(s$per_party$flokkur %in% NAMED8), 0L), c(6L, 7L, 8L))
  expect_true(startsWith(mae8_row(sections()$f180), "| MAE8, pp (over the 6 named parties scored) |"))
  expect_true(startsWith(mae8_row(sections()$eve24), "| MAE8, pp (over the 8 named parties scored) |"))
  expect_true(startsWith(mae8_row(sections()$mixed), "| MAE8, pp (over the named parties scored: 6 for r0, 7 for v2) |"))
})

test_that("a cell section names the parties scored inside Anna\u00f0 in one sentence just before the seat note, names the arm when only one arm folds, and says nothing on a cell with no fold (Anna\u00f0's 2016-05-02 result, which holds Flokkur F\u00f3lksins's votes, shown with no word of it)", {
  tail <- paste0(" is scored inside ", ANNAD, ", having had no admitted poll by F, so ", ANNAD, "'s result includes its share.")
  s <- sections()$f180
  at <- which(s == paste0("On this cell ", FF, tail))
  expect_length(at, 1)
  expect_length(grep("scored inside", s, fixed = TRUE), 1)
  expect_identical(s[at + 1], "")
  expect_true(startsWith(s[at + 2], "Seat scores not comparable on this cell: "))
  expect_identical(grep("scored inside", sections()$mixed, fixed = TRUE, value = TRUE), paste0("On this cell, in r0's scores, ", FF, tail))
  expect_false(any(grepl("scored inside", sections()$eve24, fixed = TRUE)))
})

test_that("a summed seat error prints with one decimal where it is not whole, so the printed values subtract to the printed difference, in the cell table and the MAE table, marked on a 2016 cell (12.5, 13 and their difference 0.5 printed as 12, 13 and 0)", {
  r0_24 <- scored(E24, "2024-11-29", POLL_PARTIES, "r0")
  v2_24 <- scored(E24, "2024-11-29", POLL_PARTIES, "v2")
  seat_row <- function(r0, v2) {
    grep("^\\| Seat absolute error", cell_section(v2$cell, "r0", "v2", r0, v2), value = TRUE)
  }
  cases <- list(
    list(c(12.5, 13), c("12.5", "13", "0.5")), list(c(12.5, 15), c("12.5", "15", "2.5")),
    list(c(12, 15), c("12", "15", "3")), list(c(13, 12.5), c("13", "12.5", "-0.5"))
  )
  for (case in cases) {
    r0_24$seats$seat_abs_err <- case[[1]][1]
    v2_24$seats$seat_abs_err <- case[[1]][2]
    shown <- case[[2]]
    expect_identical(
      seat_row(r0_24, v2_24),
      paste0("| Seat absolute error, summed over parties | ", shown[1], " | ", shown[2], " | ", shown[3], " |  | reported |")
    )
    expect_true(endsWith(mae_row("2024 eve", r0_24, v2_24), paste0("| ", shown[1], " | ", shown[2], " |")))
  }
  r0_16 <- scored(E16, "2016-05-02", SEVEN, "r0")
  v2_16 <- scored(E16, "2016-05-02", SEVEN, "v2")
  r0_16$seats$seat_abs_err <- 20.5
  v2_16$seats$seat_abs_err <- 21
  expect_identical(
    seat_row(r0_16, v2_16),
    "| Seat absolute error, summed over parties (not comparable) | 20.5 | 21 | 0.5 |  | not comparable |"
  )
  expect_true(endsWith(mae_row("2016 f180", r0_16, v2_16), "| 20.5 (not comparable) | 21 (not comparable) |"))
})

test_that("the report's MAE paragraph gives no fixed number of named parties and each cell's MAE8 row gives its own, the 2016-05-02 section says Flokkur F\u00f3lksins is scored inside Anna\u00f0, and the report cites a README paragraph that exists (MAE8 described as over the eight named parties on a cell that scores six, a fold left unsaid, or a citation of a renamed section)", {
  skip_without_snapshot()
  results <- withr::local_tempdir()
  for (arm in c("r0", "v2")) {
    fake_run(E16, "2016-05-02", SEVEN, arm, results)
    fake_run(E24, "2024-11-29", POLL_PARTIES, arm, results)
  }
  runs <- c("A r0 2016-10-29 2016-05-02 1101", "B v2 2016-10-29 2016-05-02 1101", "A r0 2024-11-30 2024-11-29 1101", "B v2 2024-11-30 2024-11-29 1101")
  writeLines(runs, file.path(results, "runs.txt"))
  out <- report(results, file.path(results, "runs.txt"), file.path(results, "runs.md"))
  expect_null(attr(out, "status"))
  text <- readLines(file.path(results, "runs.md"), encoding = "UTF-8")
  mae <- section(text, "## MAE")
  para <- paste(mae[2:(which(startsWith(mae, "| Cell |")) - 1)], collapse = " ")
  expect_false(grepl("over the eight named parties", para, fixed = TRUE))
  expect_match(para, "the number of parties can differ from cell to cell; the MAE8 row of each cell's section gives it.", fixed = TRUE)
  s16 <- section(text, "## 2016, f180")
  expect_true(startsWith(mae8_row(s16), "| MAE8, pp (over the 6 named parties scored) |"))
  expect_true(startsWith(mae8_row(section(text, "## 2024, eve")), "| MAE8, pp (over the 8 named parties scored) |"))
  expect_length(grep(paste0("On this cell ", FF, " is scored inside ", ANNAD), s16, fixed = TRUE), 1)
  expect_false(any(grepl("scored inside", section(text, "## 2024, eve"), fixed = TRUE)))
  expect_length(grep("(`harness/README.md`, Tests: the lean check for a model change)", text, fixed = TRUE), 1)
  readme <- readLines(root("harness", "README.md"), encoding = "UTF-8")
  tests_at <- which(readme == "## Tests")
  expect_length(tests_at, 1)
  next_at <- min(which(startsWith(readme, "## ") & seq_along(readme) > tests_at))
  expect_length(grep("^\\*\\*The lean check for a model change\\*\\*", readme[tests_at:next_at]), 1)
})
