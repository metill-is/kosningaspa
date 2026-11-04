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

# Each ablation of V2 and the one switch it turns off (the Phase 2 plan's ablation table);
# V2-staticdelta keeps delta_form = 0, which V2 already holds, so it differs in delta_walk alone.
V2_ABLATIONS <- c(
  `v2-noepoch` = "epoch_bias", `v2-nonew` = "newcomer_pool", `v2-noshock` = "shock", `v2-staticdelta` = "delta_walk",
  `v2-nofund` = "fundamentals"
)

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
  expect_identical(v2_variables(switched(delta_form = 1L)), c(V2_GQ, CORE, "delta_coord"))
  expect_identical(v2_variables(switched(fundamentals = 1L)), c(V2_GQ, CORE))
  all_on <- do.call(switched, as.list(stats::setNames(rep(1L, length(V2_SWITCHES)), V2_SWITCHES)))
  expect_length(v2_variables(all_on), length(V2_GQ) + 23)
  expect_error(v2_variables(cfg_m0()[setdiff(names(cfg_m0()), "shock")]), "switches")
  expect_error(v2_variables(switched(shock = 2L)), "switches")
})

test_that("every variable v2_variables() names for v2s-m0, v2s-m1, v2s-m2, v2s-m3, v2s-m4, v2s-p2, v2s-m5, v2 and its ablations is declared in Stan/forecast_v2.stan", {
  skip_unless_slow()
  v <- cmdstanr::cmdstan_model(root(V2_STAN), compile = FALSE)$variables()
  declared <- c(names(v$parameters), names(v$transformed_parameters), names(v$generated_quantities))
  arms <- c("v2s-m0", "v2s-m1", "v2s-m2", "v2s-m3", "v2s-m4", "v2s-p2", "v2s-m5", "v2", names(V2_ABLATIONS))
  for (a in arms) {
    vars <- v2_variables(read_v2_config(root(arm(a)$config)))
    expect_true(all(vars %in% declared), info = paste(a, paste(setdiff(vars, declared), collapse = ", ")))
  }
  expect_true(all(c(
    "tau_mu", "tau_mu_A", "tau_house", "m_new", "s_new", "z_shock", "s_bar_E", "s_zeta_raw", "s_bar_zeta", "delta_coord"
  ) %in% names(v$parameters)))
  expect_true("s_zeta" %in% names(v$transformed_parameters))
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

test_that("v2s-m1's config is v2s-m0's with epoch_bias = 1 and nothing else", {
  m0 <- jsonlite::read_json(root("harness", "configs", "steps", "v2s-m0.json"))
  m1 <- jsonlite::read_json(root(arm("v2s-m1")$config))
  expect_identical(m0$epoch_bias, 0L)
  expect_identical(m1$epoch_bias, 1L)
  expect_identical(names(m1), names(m0))
  expect_identical(utils::modifyList(m1, list(epoch_bias = 0L)), m0)
  expect_identical(arm("v2s-m1")$role, "dev")
})

test_that("v2s-m2's config is v2s-m1's with newcomer_pool = 1 and nothing else", {
  m1 <- jsonlite::read_json(root("harness", "configs", "steps", "v2s-m1.json"))
  m2 <- jsonlite::read_json(root(arm("v2s-m2")$config))
  expect_identical(m1$newcomer_pool, 0L)
  expect_identical(m2$newcomer_pool, 1L)
  expect_identical(names(m2), names(m1))
  expect_identical(utils::modifyList(m2, list(newcomer_pool = 0L)), m1)
  expect_identical(arm("v2s-m2")$role, "dev")
})

test_that("v2s-m3's config is v2s-m2's with shock = 1 and nothing else", {
  m2 <- jsonlite::read_json(root("harness", "configs", "steps", "v2s-m2.json"))
  m3 <- jsonlite::read_json(root(arm("v2s-m3")$config))
  expect_identical(m2$shock, 0L)
  expect_identical(m3$shock, 1L)
  expect_identical(names(m3), names(m2))
  expect_identical(utils::modifyList(m3, list(shock = 0L)), m2)
  expect_identical(arm("v2s-m3")$role, "dev")
})

test_that("v2s-m4's config is v2s-m3's with delta_walk = 1 and nothing else", {
  m3 <- jsonlite::read_json(root("harness", "configs", "steps", "v2s-m3.json"))
  m4 <- jsonlite::read_json(root(arm("v2s-m4")$config))
  expect_identical(m3$delta_walk, 0L)
  expect_identical(m4$delta_walk, 1L)
  expect_identical(c(m4$walk_form, m4$delta_form), c(0L, 0L))
  expect_identical(names(m4), names(m3))
  expect_identical(utils::modifyList(m4, list(delta_walk = 0L)), m3)
  expect_identical(arm("v2s-m4")$role, "dev")
})

test_that("v2s-p2's config is v2s-m4's with delta_form = 1 and nothing else", {
  m4 <- jsonlite::read_json(root(arm("v2s-m4")$config))
  p2 <- jsonlite::read_json(root(arm("v2s-p2")$config))
  expect_identical(m4$delta_form, 0L)
  expect_identical(p2$delta_form, 1L)
  expect_identical(c(p2$walk_form, p2$delta_walk), c(0L, 1L))
  expect_identical(names(p2), names(m4))
  expect_identical(utils::modifyList(p2, list(delta_form = 0L)), m4)
  expect_identical(arm("v2s-p2")$role, "dev")
})

test_that("while P1's verdict stands (KEEP v2s-m0: the anchored walk is not adopted), every step config but v2s-p1 sets walk_form = 0, and so do V2 and its ablations", {
  # WHY: the steps after P1 build on the reference walk; a re-ruling that adopts the anchored
  # walk changes this test and the later step configs together.
  files <- c(Sys.glob(root("harness", "configs", "steps", "v2s-*.json")), Sys.glob(root("harness", "configs", "v2*.json")))
  steps <- c("v2s-m1.json", "v2s-m2.json", "v2s-m3.json", "v2s-m4.json", "v2s-p2.json", "v2s-m5.json")
  expect_true(all(root("harness", "configs", "steps", steps) %in% files))
  expect_true(all(root("harness", "configs", c("v2.json", paste0(names(V2_ABLATIONS), ".json"))) %in% files))
  for (f in files) {
    want <- if (basename(f) == "v2s-p1.json") 1L else 0L
    expect_identical(jsonlite::read_json(f)$walk_form, want, label = basename(f))
  }
})

test_that("while P2's verdict stands (KEEP v2s-m4: the centred delta is not adopted), every step config but v2s-p2 sets delta_form = 0, and so do V2 and its ablations", {
  # WHY: the steps after P2 build on the non-centred delta walk; a re-ruling that adopts the
  # centred form changes this test and v2s-m5, V2 and the ablations together.
  files <- c(Sys.glob(root("harness", "configs", "steps", "v2s-*.json")), Sys.glob(root("harness", "configs", "v2*.json")))
  expect_true(all(root("harness", "configs", c("steps/v2s-m5.json", "v2.json", paste0(names(V2_ABLATIONS), ".json"))) %in% files))
  for (f in files) {
    want <- if (basename(f) == "v2s-p2.json") 1L else 0L
    expect_identical(jsonlite::read_json(f)$delta_form, want, label = basename(f))
  }
})

test_that("v2s-m5's config is v2s-m4's with fundamentals = 1 and nothing else", {
  m4 <- jsonlite::read_json(root(arm("v2s-m4")$config))
  m5 <- jsonlite::read_json(root(arm("v2s-m5")$config))
  expect_identical(m4$fundamentals, 0L)
  expect_identical(m5$fundamentals, 1L)
  expect_identical(c(m5$walk_form, m5$delta_form), c(0L, 0L))
  expect_identical(names(m5), names(m4))
  expect_identical(utils::modifyList(m5, list(fundamentals = 0L)), m4)
  expect_identical(arm("v2s-m5")$role, "dev")
})

test_that("V2's config is v2s-m5's, every component on and both forms at the adopted 0, and it is the candidate", {
  m5 <- jsonlite::read_json(root(arm("v2s-m5")$config))
  v2 <- jsonlite::read_json(root(arm("v2")$config))
  expect_identical(arm("v2")$config, "harness/configs/v2.json")
  expect_identical(v2, m5)
  expect_identical(unlist(v2[c("epoch_bias", "newcomer_pool", "shock", "delta_walk", "fundamentals")]), c(
    epoch_bias = 1L, newcomer_pool = 1L, shock = 1L, delta_walk = 1L, fundamentals = 1L
  ))
  expect_identical(c(v2$walk_form, v2$delta_form), c(0L, 0L))
  expect_identical(arm("v2")$role, "candidate")
})

test_that("each ablation's config differs from V2's in exactly its one switch, set to 0, and is an ablation", {
  v2 <- jsonlite::read_json(root("harness", "configs", "v2.json"))
  registered <- names(ARMS)[vapply(ARMS, \(a) identical(a$role, "ablation"), logical(1))]
  expect_setequal(registered, names(V2_ABLATIONS))
  for (n in names(V2_ABLATIONS)) {
    a <- arm(n)
    expect_identical(a$config, file.path("harness", "configs", paste0(n, ".json")), label = n)
    expect_identical(a$role, "ablation", label = n)
    cfg <- jsonlite::read_json(root(a$config))
    expect_identical(names(cfg), names(v2), label = n)
    differs <- names(v2)[!vapply(names(v2), \(k) identical(cfg[[k]], v2[[k]]), logical(1))]
    expect_identical(differs, V2_ABLATIONS[[n]], label = n)
    expect_identical(cfg[[V2_ABLATIONS[[n]]]], 0L, label = n)
    expect_identical(v2[[V2_ABLATIONS[[n]]]], 1L, label = n)
  }
})
