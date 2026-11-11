box::use(
  R / harness / arms[ARMS, arm],
  R / harness / freeze[is_frozen],
  R / harness / info_set[info_set, read_inputs],
  R / harness / runs[expected_meta, meta_mismatch, results_root, run_id, sha256_file],
  R / harness / v2[v2_stan_data],
  R / harness / warm[SIG_FIGS, WARM_BURN_IN, warm_inits, warm_mismatch, warm_reference, write_warm_record]
)

STANDARD_KEYS <- c(
  "stan_sha256", "config_sha256", "data_sha256", "seed", "iter_warmup", "iter_sampling",
  "chains", "max_treedepth", "init", "extract_version"
)
E <- "2024-11-30"
F <- "2024-11-29"
DATA_SHA <- strrep("a", 64)
# A model's parameters as model$variables()$parameters gives them, and their sizes in a fit
# where `empty` has size zero, so it has no column in the CSVs.
PARAMS <- list(
  theta = list(dimensions = 1), one = list(dimensions = 1), s = list(dimensions = 0), nu = list(dimensions = 2),
  empty = list(dimensions = 2)
)
SIZES <- list(theta = 3, one = 1, s = 1, nu = c(2, 3))
COLUMNS <- c("theta[1]", "theta[2]", "theta[3]", "one[1]", "s", "nu[1,1]", "nu[2,1]", "nu[1,2]", "nu[2,2]", "nu[1,3]", "nu[2,3]")
STEPS <- c(0.11, 0.12, 0.13, 0.14)
METRICS <- lapply(1:4, \(k) c(1, 2, 3) * k)

# 3 iterations x 4 chains of COLUMNS, the value at iteration i, chain position c and column k
# being 1000 c + 100 i + k, so a value says where it was taken from.
fixture_draws <- function() {
  x <- array(0, dim = c(3, 4, length(COLUMNS)), dimnames = list(NULL, NULL, COLUMNS))
  for (i in 1:3) for (c in 1:4) x[i, c, ] <- 1000 * c + 100 * i + seq_along(COLUMNS)
  posterior::as_draws_array(x)
}
value <- function(c, i, column) 1000 * c + 100 * i + match(column, COLUMNS)

# A Stan file and a config file in a temporary directory, and the expected meta of the warm run
# v2-w11 at seed 12 that reads them.
model_files <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  stan <- file.path(dir, "model.stan")
  config <- file.path(dir, "config.json")
  writeLines("parameters { real x; } model { x ~ std_normal(); }", stan)
  writeLines('{"prior": 1}', config)
  list(stan = stan, config = config, expected = expected_meta("v2-w11", 12, WARM_BURN_IN, 10, stan, config, DATA_SHA))
}

# Writes a finished standard run of v2 at `seed` on the cell (E, F) under HARNESS_RESULTS:
# its meta.json, from the expected meta of `files` at 30 + 10 with `extra` fields set, and,
# when `record` is TRUE, its warm-start record. Returns the run directory.
reference_run <- function(files, seed = 11, record = TRUE, extra = list(), data_sha = DATA_SHA) {
  id <- run_id("v2", E, F, seed)
  dir <- file.path(results_root(), id)
  dir.create(dir, recursive = TRUE)
  if (record) write_warm_record(dir, warm_inits(fixture_draws(), 1:4, PARAMS, SIZES), STEPS, METRICS, "empty")
  meta <- utils::modifyList(c(list(arm = "v2", E = E, F = F, id = id), expected_meta("v2", seed, 30, 10, files$stan, files$config, data_sha)), extra)
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
  dir
}

refused <- function(expected, seed = 11, parameters = names(PARAMS), E_ = E, F_ = F) {
  tryCatch(
    {
      warm_reference("v2", E_, F_, seed, expected, parameters)
      "not refused"
    },
    error = function(e) conditionMessage(e)
  )
}

test_that("warm_inits() takes each chain's last iteration and pairs it with that chain's id, not with the draws' order", {
  # WHY: an init taken from the first iteration, or from another chain, would start a chain away from its own metric and step size.
  inits <- warm_inits(fixture_draws(), 1:4, PARAMS, SIZES)
  expect_named(inits, c("1", "2", "3", "4"))
  for (c in 1:4) {
    expect_named(inits[[c]], c("theta", "one", "s", "nu"))
    expect_identical(inits[[c]]$s, value(c, 3, "s"))
    expect_identical(inits[[c]]$theta, array(value(c, 3, c("theta[1]", "theta[2]", "theta[3]")), 3))
    expect_identical(dim(inits[[c]]$nu), c(2L, 3L))
    expect_identical(inits[[c]]$nu[2, 1], value(c, 3, "nu[2,1]"))
    expect_identical(inits[[c]]$nu[1, 3], value(c, 3, "nu[1,3]"))
  }
  shuffled <- warm_inits(fixture_draws(), c(3L, 1L, 4L, 2L), PARAMS, SIZES)
  expect_named(shuffled, c("3", "1", "4", "2"))
  expect_identical(shuffled[["3"]]$s, value(1, 3, "s"))
  expect_identical(shuffled[["2"]]$s, value(4, 3, "s"))
  expect_error(warm_inits(fixture_draws(), 1:3, PARAMS, SIZES), "chain_ids")
  expect_error(warm_inits(fixture_draws(), c(1L, 1L, 2L, 3L), PARAMS, SIZES), "chain_ids")
  expect_error(warm_inits(fixture_draws(), 1:4, c(PARAMS, list(z = list(dimensions = 1))), c(SIZES, list(z = 2))), "draws lack z")
})

test_that("the record writes a one-element vector as [x], leaves out and lists a zero-size parameter, and is read-only", {
  # WHY: CmdStan refuses a scalar for a vector[1] init, and [] for a zero-size parameter of two dimensions.
  dir <- withr::local_tempdir()
  paths <- write_warm_record(dir, warm_inits(fixture_draws(), 1:4, PARAMS, SIZES), STEPS, METRICS, "empty")
  expect_identical(basename(paths), c(sprintf("init-%d.json", 1:4), "adaptation.json"))
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "warm_start")
  expect_identical(unname(file.access(paths, 2)), rep(-1L, 5))
  text <- paste(readLines(paths[2]), collapse = "\n")
  expect_match(text, sprintf('"one": \\[%d\\]', value(2, 3, "one[1]")))
  expect_match(text, sprintf('"s": %d[,\n]', value(2, 3, "s")))
  expect_false(grepl('"empty"', text, fixed = TRUE))
  rec <- jsonlite::read_json(paths[5], simplifyVector = TRUE)
  expect_named(rec, c("chain_ids", "step_size", "inv_metric", "zero_size", "init", "init_sha256"))
  expect_identical(rec$chain_ids, 1:4)
  expect_identical(rec$step_size, STEPS)
  expect_equal(unname(rec$inv_metric), do.call(rbind, METRICS))
  expect_identical(rec$zero_size, "empty")
  expect_identical(rec$init_sha256, unname(vapply(paths[1:4], sha256_file, character(1))))
  expect_error(write_warm_record(dir, warm_inits(fixture_draws(), 1:4, PARAMS, SIZES), STEPS, METRICS), "exists")

  other <- withr::local_tempdir()
  bad <- warm_inits(fixture_draws(), 1:4, PARAMS, SIZES)
  bad[["3"]]$s <- NA_real_
  expect_error(write_warm_record(other, bad, STEPS, METRICS), "NA")
  expect_identical(list.files(other, all.files = TRUE, no.. = TRUE), character(0))
  expect_error(write_warm_record(other, bad, c(STEPS[1:3], 0), METRICS), "step_size")
  expect_error(write_warm_record(other, bad, STEPS, METRICS[1:3]), "inv_metric")
})

test_that("warm_reference() returns a current standard run's record, and its sha256 is that of adaptation.json", {
  # WHY: a warm_from_sha256 taken from anything but adaptation.json, the one file that covers the step sizes, metrics and init hashes, or init paths not paired with chains 1 to 4, would tie a warm run to an adaptation it did not use.
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  files <- model_files()
  dir <- reference_run(files)
  ref <- warm_reference("v2", E, F, 11, files$expected, names(PARAMS))
  expect_identical(ref$id, run_id("v2", E, F, 11))
  expect_identical(ref$chain_ids, 1:4)
  expect_identical(ref$inits, normalizePath(file.path(dir, "warm_start", sprintf("init-%d.json", 1:4))))
  expect_identical(ref$step_size, STEPS)
  expect_identical(ref$inv_metric, METRICS)
  expect_identical(ref$sha256, sha256_file(file.path(dir, "warm_start", "adaptation.json")))
})

test_that("warm_reference() refuses each unusable reference with its own message", {
  # WHY: a warm run from a missing, unfinished, stale or warm reference, or at its seed, would reuse an adaptation that is not this model's on these data, or none.
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  files <- model_files()
  expect_match(refused(files$expected, seed = 12), "seed 12 is the seed of its reference v2_2024-11-30_2024-11-29_s12", fixed = TRUE)
  expect_match(refused(files$expected), "reference v2_2024-11-30_2024-11-29_s11 is not finished (no meta.json)", fixed = TRUE)
  dir.create(file.path(results_root(), run_id("v2", E, F, 11)))
  expect_match(refused(files$expected), "is not finished (no meta.json)", fixed = TRUE)

  reference_run(files, seed = 21, extra = list(warm_from = run_id("v2", E, F, 1)))
  expect_match(refused(files$expected, seed = 21), "reference v2_2024-11-30_2024-11-29_s21 is itself a warm run", fixed = TRUE)

  reference_run(files, seed = 22, record = FALSE)
  expect_match(refused(files$expected, seed = 22), "reference v2_2024-11-30_2024-11-29_s22 has no warm-start record; retire and refit it", fixed = TRUE)

  reference_run(files, seed = 23)
  expect_identical(refused(files$expected, seed = 23), "not refused")
  changed_data <- expected_meta("v2-w23", 12, WARM_BURN_IN, 10, files$stan, files$config, strrep("b", 64))
  expect_match(refused(changed_data, seed = 23), "reference v2_2024-11-30_2024-11-29_s23 is stale (data_sha256); refit it first", fixed = TRUE)
  stan2 <- file.path(dirname(files$stan), "model2.stan")
  writeLines("parameters { real y; } model { y ~ std_normal(); }", stan2)
  expect_match(refused(expected_meta("v2-w23", 12, WARM_BURN_IN, 10, stan2, files$config, DATA_SHA), seed = 23), "is stale (stan_sha256)", fixed = TRUE)
  config2 <- file.path(dirname(files$config), "config2.json")
  writeLines('{"prior": 2}', config2)
  expect_match(refused(expected_meta("v2-w23", 12, WARM_BURN_IN, 10, files$stan, config2, DATA_SHA), seed = 23), "is stale (config_sha256)", fixed = TRUE)
  reference_run(files, seed = 24, extra = list(init = "warm"))
  expect_match(refused(files$expected, seed = 24), "is stale (init)", fixed = TRUE)
})

test_that("warm_reference() refuses a record whose inits do not name the model's parameters, or do not hash as recorded", {
  # WHY: CmdStan drops an init name the model lacks and starts a parameter the init lacks at random, both silently.
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  files <- model_files()
  dir <- reference_run(files)
  renamed <- sub("^theta$", "theta1", names(PARAMS))
  expect_match(refused(files$expected, parameters = renamed), "warm-start record init-1.json lacks theta1; retire and refit it", fixed = TRUE)
  expect_match(refused(files$expected, parameters = setdiff(names(PARAMS), "nu")), "init-1.json names what the model does not declare: nu", fixed = TRUE)
  expect_match(refused(files$expected, parameters = setdiff(names(PARAMS), "empty")), "names what the model does not declare: empty", fixed = TRUE)
  init2 <- file.path(dir, "warm_start", "init-2.json")
  Sys.chmod(init2, "0644")
  cat(" ", file = init2, append = TRUE)
  expect_match(refused(files$expected), "warm-start record does not match init-2.json; retire and refit it", fixed = TRUE)
})

test_that("warm_reference() refuses a 2017 cell before the freeze before it reads anything", {
  # WHY: a warm run is a fit, and 2017 is not fitted before the freeze; with no reference present, any other check would say so first.
  skip_if(is_frozen(), "the freeze is sealed")
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  files <- model_files()
  expect_match(refused(files$expected, E_ = "2017-10-28", F_ = "2017-10-27"), "warm run: 2017 is not fitted before the freeze", fixed = TRUE)
  expect_length(list.files(results_root(), all.files = TRUE, no.. = TRUE), 0)
})

test_that("expected_meta() without warm is today's list, so standard runs still SKIP, and a refitted reference makes a warm run STALE", {
  # WHY: a key leaking into the standard expected meta would make every finished run stale; a warm run must not outlive its reference's refit.
  files <- model_files()
  standard <- expected_meta("v2", 12, 100, 10, files$stan, files$config, DATA_SHA)
  expect_named(standard, STANDARD_KEYS)
  expect_identical(expected_meta("v2", 12, 100, 10, files$stan, files$config, DATA_SHA, warm = NULL), standard)
  from <- run_id("v2", E, F, 11)
  warm <- expected_meta("v2-w11", 12, 100, 10, files$stan, files$config, DATA_SHA, warm = list(from = from, sha256 = strrep("c", 64)))
  expect_named(warm, c(STANDARD_KEYS, "warm_from", "warm_from_sha256", "adapt_engaged"))
  expect_identical(warm[c("init", "warm_from", "warm_from_sha256", "adapt_engaged")], list(init = "warm", warm_from = from, warm_from_sha256 = strrep("c", 64), adapt_engaged = FALSE))
  expect_identical(warm[setdiff(STANDARD_KEYS, "init")], standard[setdiff(STANDARD_KEYS, "init")])
  expect_identical(attr(warm, "also_accepted"), attr(standard, "also_accepted"))

  back <- function(x) {
    path <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(x, path, auto_unbox = TRUE, digits = NA)
    jsonlite::read_json(path)
  }
  expect_identical(meta_mismatch(back(standard), standard), character(0))
  expect_identical(meta_mismatch(back(warm), warm), character(0))
  expect_identical(meta_mismatch(back(standard), warm), c("init", "warm_from", "warm_from_sha256", "adapt_engaged"))
  refitted <- expected_meta("v2-w11", 12, 100, 10, files$stan, files$config, DATA_SHA, warm = list(from = from, sha256 = strrep("d", 64)))
  expect_identical(meta_mismatch(back(warm), refitted), "warm_from_sha256")
  raw_config <- back(warm)
  raw_config$config_sha256 <- sha256_file(files$config)
  expect_identical(meta_mismatch(raw_config, warm), character(0))
  for (bad in list(list(from = from), list(from = from, sha256 = "abc"), list(from = c(from, from), sha256 = strrep("c", 64)), "x")) {
    expect_error(expected_meta("v2-w11", 12, 100, 10, files$stan, files$config, DATA_SHA, warm = bad), "warm must be")
  }
})

test_that("arm() resolves <base>-w<ref_seed> to the base arm's entry with the warm list, and nothing else", {
  # WHY: a warm name that resolved to the wrong arm, or a malformed one that resolved at all, would fit another model than its reference's.
  w <- arm("v2-w2702")
  expect_identical(w[names(ARMS$v2)], ARMS$v2)
  expect_identical(w$warm, list(base = "v2", ref_seed = 2702L))
  expect_identical(arm("r0-w0")$warm, list(base = "r0", ref_seed = 0L))
  expect_null(arm("v2")$warm)
  for (bad in c("v2-w", "r9-w1", "v2s-m0", "v2-w2702-w1", "v2-w07", "-w1", "v2-w4294967295", "v2-w12345678901")) {
    expect_error(arm(bad), paste0("unknown arm: ", bad), fixed = TRUE)
  }
})

test_that("warm_mismatch() names each chain whose step size or metric is not its reference's, pairing chains by id", {
  # WHY: a warm fit that re-engaged adaptation, or read another chain's adaptation, would pass as a warm run.
  ref <- list(chain_ids = 1:4, step_size = STEPS, inv_metric = METRICS)
  stepsizes <- function(steps) {
    x <- array(rep(steps, each = 5), dim = c(5, 4, 1), dimnames = list(NULL, NULL, "stepsize__"))
    posterior::as_draws_array(x)
  }
  metric <- stats::setNames(METRICS, 1:4)
  expect_identical(warm_mismatch(ref, 1:4, stepsizes(STEPS), metric), character(0))
  expect_identical(warm_mismatch(ref, 1:4, stepsizes(STEPS * (1 + 4e-6)), lapply(metric, \(m) m * (1 - 4e-6))), character(0))
  expect_length(warm_mismatch(ref, 1:4, stepsizes(STEPS * (1 + 1e-4)), lapply(metric, \(m) m * (1 - 1e-4))), 8)
  expect_identical(warm_mismatch(ref, 1:4, stepsizes(replace(STEPS, 2, 0.2)), metric), "chain 2 sampled at step size 0.2, not the reference's 0.12")
  expect_identical(warm_mismatch(ref, c(2L, 1L, 3L, 4L), stepsizes(STEPS[c(2, 1, 3, 4)]), metric), character(0))
  expect_length(warm_mismatch(ref, 1:4, stepsizes(STEPS[c(2, 1, 3, 4)]), metric), 2)
  expect_identical(warm_mismatch(ref, 1:4, stepsizes(STEPS), replace(metric, "3", list(c(3, 6, 9.1)))), "chain 3's inverse metric is not the reference's")
  expect_identical(warm_mismatch(ref, 1:4, stepsizes(STEPS), metric[1:3]), "chain 4's inverse metric is not the reference's")
  expect_identical(warm_mismatch(ref, c(1L, 2L, 3L, 5L), stepsizes(STEPS), metric), "chain 5 is not a chain of the reference")
})

run_fit_cell <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/fit_cell.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

test_that("a v2 fit writes its record before meta.json; a warm run from it starts every chain, samples at its step sizes, SKIPs, and turns STALE when the reference is refitted", {
  # WHY: CSVs deleted before the record, a sum-to-zero init refused at 8 significant figures, adaptation re-engaged, or a warm run surviving its reference's refit would each pass unseen.
  skip_unless_slow()
  skip_without_snapshot()
  E <- "2021-09-25"
  F <- "2021-03-29"
  a <- arm("v2")
  # WHY: stage 1 is fitted once into the project's results cache, then copied to where the runs look for it.
  v2_stan_data(info_set(as.Date(E), as.Date(F), read_inputs()), a$read_config(root(a$config)))
  results <- withr::local_tempdir()
  expect_true(file.copy(file.path(results_root(), "stage1"), results, recursive = TRUE))
  withr::local_envvar(HARNESS_RESULTS = results, HARNESS_KEEP_CMDSTAN = NA)
  ref_id <- run_id("v2", E, F, 11)
  warm_id <- run_id("v2-w11", E, F, 12)
  ref_dir <- file.path(results, ref_id)
  warm_dir <- file.path(results, warm_id)
  record <- file.path(ref_dir, "warm_start")
  shown <- function(out) paste(utils::tail(out, 30), collapse = "\n")

  out <- run_fit_cell(c("v2", E, F, "11", "30", "10"))
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste("DONE", ref_dir) %in% out)
  expect_setequal(list.files(record), c(sprintf("init-%d.json", 1:4), "adaptation.json"))
  expect_identical(unname(file.access(list.files(record, full.names = TRUE), 2)), rep(-1L, 5))
  expect_false(dir.exists(file.path(ref_dir, "cmdstan")))
  expect_identical(list.files(ref_dir, pattern = "\\.csv$", recursive = TRUE), "key_diagnostics.csv")
  expect_true(file.mtime(file.path(record, "adaptation.json")) <= file.mtime(file.path(ref_dir, "meta.json")))
  ref_meta <- jsonlite::read_json(file.path(ref_dir, "meta.json"))
  expect_null(ref_meta$warm_from)
  expect_identical(ref_meta$sig_figs, SIG_FIGS)
  expect_identical(vapply(ref_meta$chain_seconds, \(x) x$chain_id, integer(1)), 1:4)
  expect_true(all(vapply(ref_meta$chain_seconds, \(x) x$warmup > 0 && x$sampling > 0, logical(1))))
  rec <- jsonlite::read_json(file.path(record, "adaptation.json"), simplifyVector = TRUE)
  sha <- sha256_file(file.path(record, "adaptation.json"))

  out <- withr::with_envvar(c(HARNESS_KEEP_CMDSTAN = "1"), run_fit_cell(c("v2-w11", E, F, "12", "400", "10")))
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste("DONE", warm_dir) %in% out)
  expect_true(paste0("warm run from ", ref_id, ": 100 burn-in iterations with adaptation off; iter_warmup 400 is not used") %in% out)
  meta <- jsonlite::read_json(file.path(warm_dir, "meta.json"))
  expect_identical(
    meta[c("iter_warmup", "init", "warm_from", "warm_from_sha256", "adapt_engaged")],
    list(iter_warmup = WARM_BURN_IN, init = "warm", warm_from = ref_id, warm_from_sha256 = sha, adapt_engaged = FALSE)
  )
  expect_false(dir.exists(file.path(warm_dir, "warm_start")))
  csvs <- list.files(file.path(warm_dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  expect_length(csvs, 4)
  steps <- cmdstanr::read_cmdstan_csv(csvs, variables = "", sampler_diagnostics = "stepsize__")
  used <- unclass(steps$post_warmup_sampler_diagnostics)[, , "stepsize__"]
  for (j in seq_along(steps$metadata$id)) {
    expect_equal(unique(used[, j]), rec$step_size[match(steps$metadata$id[j], rec$chain_ids)], tolerance = 1e-12)
  }

  out <- run_fit_cell(c("v2-w11", E, F, "12", "400", "10"))
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste0("SKIP ", warm_id, " (meta.json matches)") %in% out)

  out <- run_fit_cell(c("v2", E, F, "11", "20", "10"))
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste("STALE", ref_id, "iter_warmup") %in% out)
  expect_false(identical(sha256_file(file.path(record, "adaptation.json")), sha))
  out <- run_fit_cell(c("v2-w11", E, F, "12", "400", "10"))
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste("STALE", warm_id, "warm_from_sha256") %in% out)
  expect_true(paste("DONE", warm_dir) %in% out)
})
