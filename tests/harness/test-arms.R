box::use(
  R / harness / arms[ARMS, arm, v2_arm],
  R / harness / v2[read_v2_config, v2_stan_data, v2_extract, V2_VARIABLES]
)

V2_STAN <- "Stan/forecast_v2.stan"
V2_GQ <- c("votes_kd", "pi_nat_eday", "pi_latent_E", "y_rep_res")
ENTRY <- c("stan_file", "config", "read_config", "build", "extract", "variables")

test_that("the arms are r0, v2 and v2-perm, each entry with the same fields", {
  expect_named(ARMS, c("r0", "v2", "v2-perm"))
  for (n in names(ARMS)) expect_named(ARMS[[n]], ENTRY, label = n)
  expect_identical(arm("r0")$stan_file, "Stan/polling_and_fundamentals_kjordaemi_r0.stan")
  expect_identical(c(arm("v2")$stan_file, arm("v2-perm")$stan_file), c(V2_STAN, V2_STAN))
  expect_error(arm("v2s-m0"), "unknown arm: v2s-m0")
})

test_that("v2_arm() builds a V2-family entry, and v2 and v2-perm are each one", {
  a <- v2_arm("harness/configs/v2.json")
  expect_named(a, ENTRY)
  expect_identical(a$stan_file, V2_STAN)
  expect_identical(a$config, "harness/configs/v2.json")
  expect_identical(a$read_config, read_v2_config)
  expect_identical(a$build, v2_stan_data)
  expect_identical(a$extract, v2_extract)
  expect_identical(a$variables, V2_VARIABLES)
  expect_identical(arm("v2"), a)
  expect_identical(arm("v2-perm"), v2_arm("harness/configs/v2-perm.json"))
  expect_error(v2_arm("configs/v2.json"), "harness/configs/")
  expect_error(v2_arm(c("harness/configs/v2.json", "harness/configs/v2-perm.json")), "harness/configs/")
})

test_that("every arm's config parses with its reader, and every V2 config file with read_v2_config()", {
  for (n in names(ARMS)) {
    a <- ARMS[[n]]
    expect_true(file.exists(root(a$config)), label = n)
    expect_true(file.exists(root(a$stan_file)), label = n)
    expect_type(a$read_config(root(a$config)), "list")
  }
  files <- Sys.glob(root("harness", "configs", "v2*.json"))
  expect_setequal(basename(files), c("v2.json", "v2-perm.json"))
  for (f in files) expect_type(read_v2_config(f), "list")
})

test_that("adapt_delta is the one optional V2 config key: absent it is not in the config, and it must lie in (0, 1)", {
  path <- root(arm("v2")$config)
  expect_false("adapt_delta" %in% names(read_v2_config(path)))
  cfg <- jsonlite::read_json(path)
  tmp <- withr::local_tempfile(fileext = ".json")
  for (bad in list(1, 0, -0.5, "0.9", c(0.9, 0.95))) {
    jsonlite::write_json(c(cfg, list(adapt_delta = bad)), tmp, auto_unbox = TRUE, digits = NA)
    expect_error(read_v2_config(tmp), "adapt_delta must be one number in \\(0, 1\\)", label = paste(format(bad), collapse = " "))
  }
  jsonlite::write_json(c(cfg, list(adapt_delta = 0.9)), tmp, auto_unbox = TRUE, digits = NA)
  expect_identical(read_v2_config(tmp)$adapt_delta, 0.9)
  jsonlite::write_json(c(cfg, list(adapt_deltas = 0.9)), tmp, auto_unbox = TRUE, digits = NA)
  expect_error(read_v2_config(tmp), "unknown key\\(s\\): adapt_deltas")
})

test_that("v2-perm's config is V2's with order = \"reversed\" and nothing else", {
  v2 <- jsonlite::read_json(root(arm("v2")$config))
  perm <- jsonlite::read_json(root(arm("v2-perm")$config))
  expect_identical(arm("v2-perm")$config, "harness/configs/v2-perm.json")
  expect_identical(v2$order, "default")
  expect_identical(perm$order, "reversed")
  expect_identical(names(perm), names(v2))
  differs <- names(v2)[!vapply(names(v2), \(k) identical(perm[[k]], v2[[k]]), logical(1))]
  expect_identical(differs, "order")
  expect_identical(read_v2_config(root(arm("v2-perm")$config))$order, "reversed")
})

test_that("every variable of V2_VARIABLES is declared in Stan/forecast_v2.stan, the forecast and replicates as generated quantities", {
  skip_unless_slow()
  v <- cmdstanr::cmdstan_model(root(V2_STAN), compile = FALSE)$variables()
  declared <- c(names(v$parameters), names(v$transformed_parameters), names(v$generated_quantities))
  expect_true(all(V2_VARIABLES %in% declared), info = paste(setdiff(V2_VARIABLES, declared), collapse = ", "))
  expect_true(all(V2_GQ %in% names(v$generated_quantities)))
})
