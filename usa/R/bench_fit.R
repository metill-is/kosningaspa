# Fits a Stan model on a saved Stan data JSON and times it, for the runtime benchmark of
# the US feasibility study (usa/README.md).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=<file> data=<json> out=<dir>
#     [chains=4] [warmup=1000] [sampling=1000] [seed=4100] [max_treedepth=12] [init=0|pathfinder|smart]
#     [init_buffer=75] [window=25] [term_buffer=50]
#   LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=<file> data=<json> csv=<dir of an earlier run>
#
# Writes CmdStan CSVs, results.json and fit.rds to <out> and prints one DIAG line. R-hat
# and ESS run over the parameters and the forecast quantities the model declares among
# KEY, constant columns left out. ms per gradient is the chains' summed time over their
# summed leapfrog steps, warmup included. init=pathfinder starts the chains from Pathfinder
# draws (its time is reported apart and included in wall_min). init=smart starts every
# chain near plausible US values, sized from the data (smart_init() below), with a small
# jitter per chain. init_buffer, window and term_buffer are CmdStan's warmup adaptation
# windows: from a smart init, a short init_buffer reaches the first metric estimate sooner,
# where a unit metric would spend 75 iterations at the tree-depth cap.
# csv= summarises the CmdStan CSVs of an earlier run without sampling (wall time is then the
# longest chain's).

library(cmdstanr)
library(posterior)

opt <- list(
  stan = NULL, data = NULL, out = NULL, chains = "4", warmup = "1000", sampling = "1000",
  seed = "4100", max_treedepth = "12", init = "0", init_buffer = "75", window = "25", term_buffer = "50"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
if (!is.null(opt$csv) && is.null(opt$out)) opt$out <- opt$csv
stopifnot(!is.null(opt$stan), !is.null(opt$data), !is.null(opt$out))
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

KEY <- c(
  "sigma", "Omega", "L_corr", "phi", "mu_phi", "sigma_phi", "mu_gamma", "sigma_gamma", "tau_mu",
  "tau_house", "beta", "pi_smooth", "pi_elec", "theta", "sigma_race", "pi_race_d",
  "seats_house_d", "seats_senate_d"
)

# WHY: CmdStan's precompiled header goes stale when Apple clang updates; compiling
# without it leaves ~/.cmdstan untouched
if (is.null(opt$csv)) {
  model <- cmdstan_model(opt$stan, cpp_options = list(PRECOMPILED_HEADERS = "false"))
}
max_td <- as.integer(opt$max_treedepth)
# Walk volatility 0.004 a day, log phi 7, small bias and race scales, everything else 0.
# Fields a model does not declare are ignored by CmdStan, so this serves v5, v0 and v1.
smart_init <- function(data_file) {
  d <- jsonlite::read_json(data_file, simplifyVector = TRUE)
  m <- length(unique(d$date_n[d$house_n == 1]))
  P <- d$P
  H <- d$H
  E <- d$E
  tau <- if (d$estimate_tau == 1) array(0.02, 1) else numeric(0)
  # WHY: CmdStan reads an empty matrix as a 1-D [] and rejects it for a declared [0, P]
  # parameter; leaving zero-size parameters out of the init is equivalent
  function(chain_id) {
    init <- list(
      L_Omega = diag(P), L_corr = diag(P - 1), anchor_w = matrix(0, m, P - 1), z_walk = matrix(0, P - 1, d$D - m),
      log_sigma = log(0.004) + rnorm(if (grepl("us_midterm_v[2-9]", opt$stan)) P - 1 else P, 0, 0.1), log_phi_u = rep(0, H - 1), log_phi = rep(7, H - 1),
      mu_phi = 7 + rnorm(1, 0, 0.3), sigma_phi = 0.5, mu_gamma0 = rep(0, P),
      gamma_free = matrix(0, H - 1, P), sigma_gamma = 0.05, z_mu = matrix(0, E - 1, P),
      z_house = array(0, c(E - 1, H - 1, P)), tau_mu_est = tau, tau_house_est = tau,
      z_elec = rep(0, P - 1), theta = matrix(0, max(d$Q, 1), P), sigma_race = 0.1,
      z_race = matrix(0, max(d$R, 0), P)
    )
    init[vapply(init, length, integer(1)) > 0]
  }
}

pathfinder_min <- 0
if (is.null(opt$csv)) {
  t0 <- Sys.time()
  init <- 0
  if (opt$init == "smart") init <- smart_init(opt$data)
  if (opt$init == "pathfinder") {
    pf <- model$pathfinder(data = opt$data, seed = as.integer(opt$seed), init = 0, num_paths = 4)
    pathfinder_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    init <- pf
  }
  fit <- model$sample(
    data = opt$data,
    chains = as.integer(opt$chains),
    parallel_chains = as.integer(opt$chains),
    iter_warmup = as.integer(opt$warmup),
    iter_sampling = as.integer(opt$sampling),
    seed = as.integer(opt$seed),
    init = init,
    max_treedepth = max_td,
    init_buffer = as.integer(opt$init_buffer),
    window = as.integer(opt$window),
    term_buffer = as.integer(opt$term_buffer),
    save_warmup = TRUE,
    refresh = 200,
    output_dir = opt$out
  )
  wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
} else {
  fit <- as_cmdstan_fit(list.files(opt$csv, "\\.csv$", full.names = TRUE))
  wall_min <- max(fit$time()$chains$total) / 60
}

# WHY: a variable declared with size 0 (no races) has no column, and summary() rejects it
vars <- intersect(KEY, unique(sub("\\[.*$", "", fit$metadata()$variables)))
smry <- fit$summary(vars, "rhat", "ess_bulk", "ess_tail", sd = sd)
smry <- smry[is.finite(smry$sd) & smry$sd > 0, ]

all_diag <- fit$sampler_diagnostics(inc_warmup = TRUE, format = "df")
n_warm <- fit$metadata()$iter_warmup
post <- all_diag[all_diag$.iteration > n_warm, ]
times <- fit$time()$chains
grad_total <- sum(all_diag$n_leapfrog__)
chain_sec <- sum(times$total)
res <- list(
  stan = basename(opt$stan),
  data = opt$data,
  chains = fit$num_chains(),
  warmup = n_warm,
  sampling = fit$metadata()$iter_sampling,
  wall_min = wall_min,
  init = opt$init,
  pathfinder_min = pathfinder_min,
  chain_min = times$total / 60,
  ms_per_gradient = 1000 * chain_sec / grad_total,
  treedepth_table = as.list(table(post$treedepth__)),
  mean_leapfrog = mean(post$n_leapfrog__),
  stepsize = unique(round(post$stepsize__, 5)),
  divergent = sum(post$divergent__),
  treedepth_hits = sum(post$treedepth__ >= max_td),
  ebfmi = as.numeric(fit$diagnostic_summary(quiet = TRUE)$ebfmi),
  n_key = nrow(smry),
  max_rhat = max(smry$rhat, na.rm = TRUE),
  min_ess_bulk = min(smry$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(smry$ess_tail, na.rm = TRUE),
  worst_rhat = smry$variable[which.max(smry$rhat)],
  worst_ess = smry$variable[which.min(smry$ess_bulk)],
  ess_per_1000_grad = 1000 * min(smry$ess_bulk, na.rm = TRUE) / grad_total
)
jsonlite::write_json(res, file.path(opt$out, "results.json"),
  auto_unbox = TRUE, digits = NA, pretty = TRUE, na = "null"
)
fit$save_object(file.path(opt$out, "fit.rds"))
cat(sprintf(
  "DIAG stan=%s wall_min=%.1f ms_per_grad=%.3f mean_leapfrog=%.0f max_depth=%d div=%d cap_hits=%d max_rhat=%.3f min_ess_bulk=%.0f (%s) ess_per_1000_grad=%.3f\n",
  res$stan, res$wall_min, res$ms_per_gradient, res$mean_leapfrog,
  max(post$treedepth__), res$divergent, res$treedepth_hits, res$max_rhat,
  res$min_ess_bulk, res$worst_ess, res$ess_per_1000_grad
))
