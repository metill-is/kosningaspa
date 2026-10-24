box::use(
  R / harness / runs[
    EXTRACT_VERSION, SAMPLER, results_root, run_id, run_dir, sha256_file, stan_data_sha, write_frozen_inputs,
    expected_meta, meta_mismatch, retire_run, summarise_diagnostics, diagnostics_gate, diag_line
  ]
)

EXPECTED_KEYS <- c(
  "stan_sha256", "config_sha256", "data_sha256", "seed", "iter_warmup", "iter_sampling",
  "chains", "max_treedepth", "init", "extract_version"
)

# A Stan file, a config file and the expected meta of a run that reads them.
meta_fixture <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  stan <- file.path(dir, "model.stan")
  config <- file.path(dir, "config.json")
  writeLines("parameters { real x; } model { x ~ std_normal(); }", stan)
  writeLines('{"prior": 1}', config)
  list(
    stan = stan, config = config,
    expected = expected_meta("r0", 1101, 200, 200, stan, config, strrep("a", 64))
  )
}

# 4 chains x 1000 iterations of iid normal draws for `vars`, the chain `shift_chain` of the
# first variable shifted by `shift` sd.
normal_draws <- function(vars, shift = 0, shift_chain = 4L) {
  x <- array(stats::rnorm(1000 * 4 * length(vars)), dim = c(1000, 4, length(vars)))
  x[, shift_chain, 1] <- x[, shift_chain, 1] + shift
  dimnames(x) <- list(NULL, NULL, vars)
  posterior::as_draws_array(x)
}

clean_sampler <- function() {
  list(num_divergent = rep(0L, 4), num_max_treedepth = rep(0L, 4), ebfmi = c(0.9, 1.1, 0.8, 1), max_depth = rep(9L, 4))
}

test_that("the sampler settings and the extract version are the harness's (spec section 5.8)", {
  expect_identical(EXTRACT_VERSION, 1L)
  expect_identical(SAMPLER, list(chains = 4L, parallel_chains = 4L, init = 0, max_treedepth = 12L))
})

test_that("results_root() is results/harness under the project root unless HARNESS_RESULTS is set", {
  withr::local_envvar(HARNESS_RESULTS = NA)
  expect_identical(results_root(), root("results", "harness"))
  withr::local_envvar(HARNESS_RESULTS = "")
  expect_identical(results_root(), root("results", "harness"))
  tmp <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = tmp)
  expect_identical(results_root(), tmp)
  expect_identical(run_dir("r0_x"), file.path(tmp, "r0_x"))
})

test_that("run_id() joins the arm, E, F and the seed", {
  expect_identical(run_id("r0", as.Date("2024-11-30"), as.Date("2024-11-29"), 1101), "r0_2024-11-30_2024-11-29_s1101")
  expect_identical(run_id("r0", "2021-09-25", "2021-03-29", 1102L), "r0_2021-09-25_2021-03-29_s1102")
  expect_error(run_id("r0", "2024-11-30", "2024-11-29", 1.5), "seed")
  expect_error(run_id("r0", "2024-11-30", "2024-11-29", 2^31), "seed")
  expect_error(run_id("r0/..", "2024-11-30", "2024-11-29", 1), "arm")
  expect_error(run_id("r0", "2024-13-30", "2024-11-29", 1))
})

test_that("write_frozen_inputs() writes the Stan data and the inputs meta read-only, once", {
  dir <- file.path(withr::local_tempdir(), "r0_run")
  stan_data <- list(N = 3L, y = c(0.25, 1, 2))
  inputs_meta <- list(arm = "r0", E = "2024-11-30", party_order = c("A", "B"))
  sha <- write_frozen_inputs(dir, stan_data, inputs_meta)
  data_json <- file.path(dir, "inputs", "stan_data.json")
  meta_json <- file.path(dir, "inputs", "inputs.json")
  expect_true(all(file.exists(c(data_json, meta_json))))
  expect_identical(unname(file.access(c(data_json, meta_json), 2)), c(-1L, -1L))
  expect_identical(sha, digest::digest(file = data_json, algo = "sha256"))
  expect_identical(sha, sha256_file(data_json))
  expect_identical(jsonlite::read_json(data_json, simplifyVector = TRUE), list(N = 3L, y = c(0.25, 1, 2)))
  expect_identical(jsonlite::read_json(meta_json, simplifyVector = TRUE), inputs_meta)
  expect_error(write_frozen_inputs(dir, stan_data, inputs_meta), "inputs")
  expect_identical(sha256_file(data_json), sha)
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "inputs")
})

test_that("a failed write_frozen_inputs() leaves no inputs/ behind, so a retry succeeds", {
  dir <- file.path(withr::local_tempdir(), "r0_run")
  expect_error(write_frozen_inputs(dir, list(N = 1L, bad = "text"), list(arm = "r0")))
  expect_false(file.exists(file.path(dir, "inputs")))
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), character(0))
  sha <- write_frozen_inputs(dir, list(N = 1L), list(arm = "r0"))
  expect_identical(sha, sha256_file(file.path(dir, "inputs", "stan_data.json")))
})

test_that("stan_data_sha() hashes the data as write_stan_json() writes them, so changed data are stale", {
  d1 <- list(N = 3L, y = c(0.25, 1, 2))
  d2 <- list(N = 3L, y = c(0.25, 1, 2.5))
  path <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(d1, path)
  expect_identical(stan_data_sha(d1), sha256_file(path))
  expect_identical(stan_data_sha(d1), write_frozen_inputs(file.path(withr::local_tempdir(), "r"), d1, list()))
  expect_false(identical(stan_data_sha(d1), stan_data_sha(d2)))
  fx <- meta_fixture()
  frozen <- expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, stan_data_sha(d1))
  fresh <- expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, stan_data_sha(d2))
  expect_identical(meta_mismatch(frozen, fresh), "data_sha256")
  expect_identical(meta_mismatch(frozen, expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, stan_data_sha(d1))), character(0))
})

test_that("expected_meta() holds the shas, the seed, the iterations and the sampler settings", {
  fx <- meta_fixture()
  e <- fx$expected
  expect_identical(names(e), EXPECTED_KEYS)
  expect_identical(e$stan_sha256, digest::digest(file = fx$stan, algo = "sha256"))
  expect_identical(e$config_sha256, digest::digest(file = fx$config, algo = "sha256"))
  expect_identical(e$data_sha256, strrep("a", 64))
  expect_identical(e[c("seed", "iter_warmup", "iter_sampling")], list(seed = 1101L, iter_warmup = 200L, iter_sampling = 200L))
  expect_identical(e[c("chains", "max_treedepth", "init")], SAMPLER[c("chains", "max_treedepth", "init")])
  expect_identical(e$extract_version, EXTRACT_VERSION)
  expect_error(expected_meta("r0/x", 1101, 200, 200, fx$stan, fx$config, strrep("a", 64)), "arm")
  expect_error(expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, "abc"), "data_sha")
  expect_error(expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, strrep("A", 64)), "data_sha")
  expect_error(expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, c(strrep("a", 64), strrep("b", 64))), "data_sha")
})

test_that("meta_mismatch() names the keys that differ or are missing", {
  e <- meta_fixture()$expected
  expect_identical(meta_mismatch(e, e), character(0))
  other <- e
  other$data_sha256 <- strrep("b", 64)
  expect_identical(meta_mismatch(other, e), "data_sha256")
  expect_identical(meta_mismatch(e[setdiff(names(e), "seed")], e), "seed")
  other <- e
  other$iter_sampling <- 2000L
  other$seed <- NULL
  expect_identical(meta_mismatch(other, e), c("seed", "iter_sampling"))
  extra <- c(e, list(arm = "r0", minutes = 3.5, gate = list(pass = TRUE, reasons = list())))
  expect_identical(meta_mismatch(extra, e), character(0))
  other <- e
  other$seed <- "1101"
  expect_identical(meta_mismatch(other, e), "seed")
  other$seed <- NA
  expect_identical(meta_mismatch(other, e), "seed")
  expect_identical(meta_mismatch(NULL, e), EXPECTED_KEYS)
})

test_that("meta_mismatch() survives a jsonlite round trip of the expected meta", {
  e <- meta_fixture()$expected
  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(c(e, list(arm = "r0", minutes = 1.25)), path, auto_unbox = TRUE, digits = NA)
  expect_identical(meta_mismatch(jsonlite::read_json(path), e), character(0))
  expect_identical(meta_mismatch(jsonlite::read_json(path, simplifyVector = TRUE), e), character(0))
})

test_that("a changed Stan file, config or data makes the finished run stale", {
  fx <- meta_fixture()
  old <- fx$expected
  cat("// edited\n", file = fx$stan, append = TRUE)
  now <- expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, strrep("a", 64))
  expect_identical(meta_mismatch(old, now), "stan_sha256")
  writeLines('{"prior": 2}', fx$config)
  now <- expected_meta("r0", 1101, 200, 200, fx$stan, fx$config, strrep("c", 64))
  expect_identical(meta_mismatch(old, now), c("stan_sha256", "config_sha256", "data_sha256"))
})

test_that("retire_run() moves a run under _stale/ and leaves nothing at the old path", {
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  id <- "r0_2024-11-30_2024-11-29_s1101"
  dir <- run_dir(id)
  dir.create(file.path(dir, "inputs"), recursive = TRUE)
  writeLines("{}", file.path(dir, "meta.json"))
  new <- retire_run(dir)
  expect_identical(dirname(new), file.path(results_root(), "_stale"))
  expect_match(basename(new), paste0("^", id, "_[0-9]{14}$"))
  expect_false(file.exists(dir))
  expect_true(file.exists(file.path(new, "meta.json")))
  expect_true(dir.exists(file.path(new, "inputs")))
  expect_error(retire_run(dir), "no run")
})

test_that("retire_run() refuses to overwrite a run retired in the same second", {
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  id <- "r0_2024-11-30_2024-11-29_s1101"
  dir <- run_dir(id)
  dir.create(dir, recursive = TRUE)
  writeLines("new", file.path(dir, "meta.json"))
  # WHY: retire_run() reads the clock itself, so every second it could land on is taken.
  seconds <- format(Sys.time() + 0:30, "%Y%m%d%H%M%S")
  for (s in seconds) dir.create(file.path(results_root(), "_stale", paste0(id, "_", s)), recursive = TRUE)
  expect_error(retire_run(dir), "already retired this second")
  expect_identical(readLines(file.path(dir, "meta.json")), "new")
})

test_that("summarise_diagnostics() reads R-hat and ESS over parameters and scored quantities together", {
  withr::local_seed(20261024)
  d <- summarise_diagnostics(normal_draws(c("a", "b[1]", "b[2]")), normal_draws(c("s[1]", "s[2]")), clean_sampler())
  expect_named(d, c(
    "max_rhat", "max_rhat_scored", "min_ess_bulk", "min_ess_tail", "min_ess_bulk_scored",
    "min_ess_tail_scored", "na_scored", "divergent", "treedepth_hits", "max_depth", "min_ebfmi"
  ))
  expect_identical(d$na_scored, 0L)
  expect_lt(d$max_rhat, 1.01)
  expect_gte(d$min_ess_bulk_scored, 400)
  expect_identical(d[c("divergent", "treedepth_hits", "max_depth")], list(divergent = 0L, treedepth_hits = 0L, max_depth = 9L))
  expect_identical(d$min_ebfmi, 0.8)
  expect_identical(diagnostics_gate(d), list(pass = TRUE, reasons = character(0)))
})

test_that("a parameter chain shifted by 3 sd fails the gate on R-hat", {
  withr::local_seed(20261024)
  params <- normal_draws(c("a", "b[1]"), shift = 3)
  scored <- normal_draws(c("s[1]", "s[2]"))
  d <- summarise_diagnostics(params, scored, clean_sampler())
  expect_gt(d$max_rhat, 1.01)
  expect_lt(d$max_rhat_scored, 1.01)
  expect_identical(d$max_rhat, max(posterior::rhat(posterior::extract_variable_matrix(params, "a")), d$max_rhat_scored))
  g <- diagnostics_gate(d)
  expect_false(g$pass)
  expect_length(g$reasons, 1)
  expect_match(g$reasons, "R-hat")
  expect_match(g$reasons, sprintf("%.4f", d$max_rhat), fixed = TRUE)
})

test_that("summarise_diagnostics() sums divergences and treedepth hits and keeps the deepest tree", {
  withr::local_seed(1)
  sampler <- list(num_divergent = c(0L, 1L, 0L, 0L), num_max_treedepth = c(2L, 0L, 3L, 0L), ebfmi = c(0.4, 0.3, 0.5, 0.6), max_depth = c(11L, 12L, 10L, 11L))
  d <- summarise_diagnostics(normal_draws("a"), normal_draws("s"), sampler)
  expect_identical(d[c("divergent", "treedepth_hits", "max_depth")], list(divergent = 1L, treedepth_hits = 5L, max_depth = 12L))
  expect_identical(d$min_ebfmi, 0.3)
  g <- diagnostics_gate(d)
  expect_false(g$pass)
  expect_identical(g$reasons, "divergent 1 > 0")
  expect_error(summarise_diagnostics(normal_draws("a"), normal_draws("s"), sampler[1:3]), "max_depth")
})

NA_REASON <- "scored quantities with NA R-hat or ESS: "

test_that("a constant scored quantity fails the gate", {
  withr::local_seed(2)
  scored <- normal_draws(c("s[1]", "s[2]", "s[3]"))
  scored[, , "s[2]"] <- 0.5
  scored[, , "s[3]"] <- 0
  d <- summarise_diagnostics(normal_draws("a"), scored, clean_sampler())
  expect_identical(d$na_scored, 2L)
  expect_true(is.na(d$min_ess_bulk_scored))
  g <- diagnostics_gate(d)
  expect_false(g$pass)
  expect_true(paste0(NA_REASON, "2") %in% g$reasons)
})

test_that("one NA draw in a scored quantity fails the gate", {
  withr::local_seed(3)
  scored <- normal_draws(c("s[1]", "s[2]"))
  scored[17, 2, "s[1]"] <- NA
  d <- summarise_diagnostics(normal_draws("a"), scored, clean_sampler())
  expect_identical(d$na_scored, 1L)
  g <- diagnostics_gate(d)
  expect_false(g$pass)
  expect_true(paste0(NA_REASON, "1") %in% g$reasons)
})

test_that("a constant parameter is left out and the gate still passes", {
  withr::local_seed(4)
  params <- normal_draws(c("a", "L[1,1]", "L[1,2]"))
  params[, , "L[1,1]"] <- 1
  params[, , "L[1,2]"] <- 0
  scored <- normal_draws(c("s[1]", "s[2]"))
  d <- summarise_diagnostics(params, scored, clean_sampler())
  expect_identical(d$na_scored, 0L)
  expect_false(anyNA(unlist(d)))
  expect_identical(d$max_rhat, max(posterior::summarise_draws(posterior::bind_draws(params, scored), "rhat")$rhat, na.rm = TRUE))
  expect_identical(diagnostics_gate(d), list(pass = TRUE, reasons = character(0)))
})

test_that("a scored bulk ESS of 399 fails the gate and 400 passes it", {
  d <- list(
    max_rhat = 1.004, max_rhat_scored = 1.002, min_ess_bulk = 380, min_ess_tail = 350,
    min_ess_bulk_scored = 399, min_ess_tail_scored = 500, divergent = 0L, treedepth_hits = 0L,
    max_depth = 11L, min_ebfmi = 0.7, na_scored = 0L
  )
  g <- diagnostics_gate(d)
  expect_false(g$pass)
  expect_identical(g$reasons, "min scored bulk ESS 399 < 400")
  d$min_ess_bulk_scored <- 400
  expect_identical(diagnostics_gate(d), list(pass = TRUE, reasons = character(0)))
  d$max_rhat <- 1.01
  expect_true(diagnostics_gate(d)$pass)
  d$max_rhat <- NA_real_
  expect_identical(diagnostics_gate(d)$reasons, "max R-hat NA > 1.01")
  d$max_rhat <- NULL
  d["max_rhat"] <- list(NULL)
  expect_identical(diagnostics_gate(d)$reasons, "max R-hat NA > 1.01")
  d$max_rhat <- 1.0234
  d$divergent <- 2L
  d$min_ess_bulk_scored <- 120.4
  expect_identical(
    diagnostics_gate(d, rhat_max = 1.02, ess_min = 200)$reasons,
    c("max R-hat 1.0234 > 1.02", "divergent 2 > 0", "min scored bulk ESS 120 < 200")
  )
  expect_error(diagnostics_gate(d[setdiff(names(d), "divergent")]), "divergent")
  expect_error(diagnostics_gate(d[setdiff(names(d), "na_scored")]), "na_scored")
  d$max_rhat <- 1.004
  d$divergent <- 0L
  d$min_ess_bulk_scored <- 500
  d$na_scored <- 3L
  expect_identical(diagnostics_gate(d), list(pass = FALSE, reasons = paste0(NA_REASON, "3")))
  d["na_scored"] <- list(NULL)
  expect_identical(diagnostics_gate(d)$reasons, paste0(NA_REASON, "NA"))
})

test_that("a gate reason never reads as passing its own rule", {
  d <- list(max_rhat = 1.01004, divergent = 0L, min_ess_bulk_scored = 500, na_scored = 0L)
  g <- diagnostics_gate(d)
  expect_identical(g$reasons, "max R-hat 1.01004 > 1.01")
  shown <- as.numeric(sub("max R-hat ([^ ]+) > .*", "\\1", g$reasons))
  expect_gt(shown, 1.01)
  d$max_rhat <- 1.01 + 2 * .Machine$double.eps
  shown <- as.numeric(sub("max R-hat ([^ ]+) > .*", "\\1", diagnostics_gate(d)$reasons))
  expect_gt(shown, 1.01)
  d$max_rhat <- 1.0234
  expect_identical(diagnostics_gate(d)$reasons, "max R-hat 1.0234 > 1.01")
  d$max_rhat <- 1.004
  d$min_ess_bulk_scored <- 399.6
  expect_identical(diagnostics_gate(d)$reasons, "min scored bulk ESS 399 < 400")
  d$min_ess_bulk_scored <- 399.99999
  expect_identical(diagnostics_gate(d)$reasons, "min scored bulk ESS 399 < 400")
})

test_that("diag_line() is the fixed DIAG format", {
  d <- list(
    max_rhat = 1.00731, max_rhat_scored = 1.002, min_ess_bulk = 812.6, min_ess_tail = 640.2,
    min_ess_bulk_scored = 1500, min_ess_tail_scored = 900, divergent = 0L, treedepth_hits = 3L,
    max_depth = 12L, min_ebfmi = 0.61249, na_scored = 0L
  )
  expect_identical(
    diag_line("r0_2024-11-30_2024-11-29_s1101", 42.36, d, list(pass = TRUE, reasons = character(0))),
    paste(
      "DIAG run=r0_2024-11-30_2024-11-29_s1101 minutes=42.4 max_rhat=1.0073 min_ess_bulk=813",
      "min_ess_tail=640 divergent=0 treedepth_hits=3 max_depth=12 min_ebfmi=0.612 gate=PASS"
    )
  )
  expect_match(diag_line("x", 1, d, list(pass = FALSE, reasons = "divergent 1 > 0")), " gate=FAIL$")
  expect_error(diag_line("x", 1, d[setdiff(names(d), "max_depth")], list(pass = TRUE)), "max_depth")
})
