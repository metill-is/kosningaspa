box::use(
  R / harness / arms[ARMS, ARM_ROLES, arm, v2_arm],
  R / harness / v2[read_v2_config, v2_stan_data, v2_extract, v2_variables, V2_SWITCHES]
)

V2_STAN <- "Stan/forecast_v2.stan"
V2_GQ <- c("votes_kd", "pi_nat_eday", "pi_latent_E", "y_rep_res")
CORE <- c(
  "sigma", "L_Omega", "tau_camp", "mu_phi", "sigma_phi", "log_phi", "log_phi_K", "log_kappa", "sigma_gamma", "mu",
  "mu_sd", "tau_sd", "s_delta0"
)

# The role of each arm of the plan's arms table, by name.
table_role <- function(name) {
  if (name == "r0") {
    return("reference")
  }
  if (name == "v2") {
    return("candidate")
  }
  if (name == "v2-perm") {
    return("check")
  }
  if (startsWith(name, "v2s-")) {
    return("dev")
  }
  if (startsWith(name, "v2-")) {
    return("ablation")
  }
  NA_character_
}

v2_family <- function() names(ARMS)[vapply(ARMS, \(a) identical(a$stan_file, V2_STAN), logical(1))]

# The m0 config with the named switches set.
switched <- function(...) utils::modifyList(cfg_m0(), list(...))

test_that("every arm has one role of ARM_ROLES, and the v2 family's are those of the arms table", {
  expect_identical(ARM_ROLES, c("reference", "candidate", "ablation", "check", "dev"))
  for (n in names(ARMS)) {
    r <- ARMS[[n]]$role
    expect_true(is.character(r) && length(r) == 1 && nzchar(r), label = n)
    expect_true(r %in% ARM_ROLES, label = n)
  }
  expect_identical(ARMS$r0$role, "reference")
  expect_true("v2s-m0" %in% v2_family())
  for (n in v2_family()) expect_identical(ARMS[[n]]$role, table_role(n), label = n)
})

test_that("v2_arm() builds a V2-family entry, and every v2-family arm is one", {
  a <- v2_arm("harness/configs/steps/v2s-m0.json", "dev")
  expect_named(a, c("stan_file", "config", "read_config", "build", "extract", "variables", "role"))
  expect_identical(a$stan_file, V2_STAN)
  expect_identical(a$config, "harness/configs/steps/v2s-m0.json")
  expect_identical(a$read_config, read_v2_config)
  expect_identical(a$build, v2_stan_data)
  expect_identical(a$extract, v2_extract)
  expect_identical(a$variables, v2_variables)
  expect_identical(a$role, "dev")
  expect_identical(arm("v2s-m0"), a)
  for (n in v2_family()) expect_identical(ARMS[[n]], v2_arm(ARMS[[n]]$config, ARMS[[n]]$role), label = n)
  expect_error(v2_arm("harness/configs/steps/v2s-m0.json", "devv"), "role")
  expect_error(v2_arm("harness/configs/steps/v2s-m0.json", c("dev", "check")), "role")
  expect_error(v2_arm("configs/v2s-m0.json", "dev"), "harness/configs/")
})

test_that("every arm's config parses with its reader, every v2-family config file with read_v2_config()", {
  for (n in names(ARMS)) {
    a <- ARMS[[n]]
    expect_true(file.exists(root(a$config)), label = n)
    expect_true(file.exists(root(a$stan_file)), label = n)
    expect_type(a$read_config(root(a$config)), "list")
  }
  files <- c(Sys.glob(root("harness", "configs", "v2*.json")), Sys.glob(root("harness", "configs", "steps", "v2s-*.json")))
  expect_true(root("harness", "configs", "steps", "v2s-m0.json") %in% files)
  for (f in files) expect_type(read_v2_config(f), "list")
  registered <- vapply(ARMS[v2_family()], \(a) root(a$config), character(1))
  expect_true(all(registered %in% files))
})

test_that("v2_variables() names the forecast, the replicates and the parameters each switch puts in the model", {
  expect_identical(v2_variables(cfg_m0()), c(V2_GQ, CORE))
  expect_false("tau_mu" %in% v2_variables(cfg_m0()))
  expect_identical(
    v2_variables(switched(epoch_bias = 1L)),
    c(V2_GQ, append(CORE, c("tau_mu", "tau_mu_A", "tau_house"), after = match("mu", CORE)))
  )
  expect_identical(
    v2_variables(switched(newcomer_pool = 1L)), c(V2_GQ, append(CORE, c("m_new", "s_new"), after = match("mu", CORE)))
  )
  expect_identical(v2_variables(switched(shock = 1L)), c(V2_GQ, append(CORE, "s_bar_E", after = match("mu", CORE))))
  expect_identical(v2_variables(switched(delta_walk = 1L)), c(V2_GQ, CORE, "s_bar_zeta", "s_zeta"))
  expect_identical(v2_variables(switched(walk_form = 1L)), c(V2_GQ, CORE, "anchor_w"))
  expect_identical(v2_variables(switched(fundamentals = 1L, delta_form = 1L)), c(V2_GQ, CORE))
  all_on <- do.call(switched, as.list(stats::setNames(rep(1L, length(V2_SWITCHES)), V2_SWITCHES)))
  expect_length(v2_variables(all_on), length(V2_GQ) + 22)
  expect_error(v2_variables(cfg_m0()[setdiff(names(cfg_m0()), "shock")]), "switches")
  expect_error(v2_variables(switched(shock = 2L)), "switches")
})

test_that("every variable v2_variables() names for v2s-m0 is declared in Stan/forecast_v2.stan", {
  skip_unless_slow()
  v <- cmdstanr::cmdstan_model(root(V2_STAN), compile = FALSE)$variables()
  declared <- c(names(v$parameters), names(v$transformed_parameters), names(v$generated_quantities))
  vars <- v2_variables(read_v2_config(root(arm("v2s-m0")$config)))
  expect_true(all(vars %in% declared), info = paste(setdiff(vars, declared), collapse = ", "))
  expect_true(all(V2_GQ %in% names(v$generated_quantities)))
})

test_that("v2s-p1's config is v2s-m0's with walk_form = 1 and nothing else", {
  m0 <- jsonlite::read_json(root("harness", "configs", "steps", "v2s-m0.json"))
  p1 <- jsonlite::read_json(root(arm("v2s-p1")$config))
  expect_identical(m0$walk_form, 0L)
  expect_identical(p1$walk_form, 1L)
  expect_identical(names(p1), names(m0))
  expect_identical(utils::modifyList(p1, list(walk_form = 0L)), m0)
  expect_identical(arm("v2s-p1")$role, "dev")
})
