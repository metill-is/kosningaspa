# Fits a Stan model on a saved Stan data JSON and times it, for the runtime benchmark of
# the US feasibility study (usa/README.md).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=<file> data=<json> out=<dir>
#     [chains=4] [warmup=1000] [sampling=1000] [seed=4100] [max_treedepth=12]
#
# Writes CmdStan CSVs, results.json and fit.rds to <out> and prints one DIAG line. R-hat
# and ESS run over the parameters and the forecast quantities the model declares among
# KEY, constant columns left out. ms per gradient is the chains' summed time over their
# summed leapfrog steps, warmup included.

library(cmdstanr)
library(posterior)

opt <- list(
  stan = NULL, data = NULL, out = NULL, chains = "4", warmup = "1000", sampling = "1000",
  seed = "4100", max_treedepth = "12"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$stan), !is.null(opt$data), !is.null(opt$out))
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

KEY <- c(
  "sigma", "Omega", "phi", "mu_phi", "sigma_phi", "mu_gamma", "sigma_gamma", "tau_mu",
  "tau_house", "beta", "pi_smooth", "pi_elec", "theta", "sigma_race", "pi_race_d",
  "seats_house_d", "seats_senate_d"
)

# WHY: CmdStan's precompiled header goes stale when Apple clang updates; compiling
# without it leaves ~/.cmdstan untouched
model <- cmdstan_model(opt$stan, cpp_options = list(PRECOMPILED_HEADERS = "false"))
max_td <- as.integer(opt$max_treedepth)
t0 <- Sys.time()
fit <- model$sample(
  data = opt$data,
  chains = as.integer(opt$chains),
  parallel_chains = as.integer(opt$chains),
  iter_warmup = as.integer(opt$warmup),
  iter_sampling = as.integer(opt$sampling),
  seed = as.integer(opt$seed),
  init = 0,
  max_treedepth = max_td,
  save_warmup = TRUE,
  refresh = 200,
  output_dir = opt$out
)
wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

declared <- names(model$variables()$parameters)
declared <- c(
  declared, names(model$variables()$transformed_parameters),
  names(model$variables()$generated_quantities)
)
vars <- intersect(KEY, declared)
smry <- fit$summary(vars, "rhat", "ess_bulk", "ess_tail", sd = sd)
smry <- smry[is.finite(smry$sd) & smry$sd > 0, ]

all_diag <- fit$sampler_diagnostics(inc_warmup = TRUE, format = "df")
n_warm <- as.integer(opt$warmup)
post <- all_diag[all_diag$.iteration > n_warm, ]
times <- fit$time()$chains
grad_total <- sum(all_diag$n_leapfrog__)
chain_sec <- sum(times$total)
res <- list(
  stan = basename(opt$stan),
  data = opt$data,
  chains = as.integer(opt$chains),
  warmup = n_warm,
  sampling = as.integer(opt$sampling),
  wall_min = wall_min,
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
