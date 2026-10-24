# Checks that the centred constituency effects and log volatilities leave the
# posterior unchanged. Old model: delta = sigma_delta * delta_raw and
# log sigma = mu_log_sigma + tau_log_sigma * log_sigma_raw. New model: delta_free
# and log_sigma are the parameters. Their unnormalised log densities must satisfy
#   lp_new(T(x)) - (lp_old(x) - (P-1) log tau_log_sigma - (K-1) sum_p log sigma_delta[p]) = constant,
# where the subtracted terms are log |det dT/dx|. Points: n posterior draws of the
# old model, plus n_off draws pushed off the posterior in every changed coordinate.
# The new point is computed in double precision from the CSV-rounded old point, so
# the constant should hold to floating-point error, not just to CSV rounding.
# The log density never reads generated quantities, so both models also run theirs at
# the posterior points with one seed; their RNG streams match, so every generated
# quantity (delta_pred, the y_rep draws) must agree.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence.R --old-stan=<old.stan> \
#     --new-stan=<new.stan> --data-json=<file> --draws-dir=<old fit's CSV dir> \
#     --out=<csv> [--n=40] [--n-off=15]
# Prints EQUIV PASS or EQUIV FAIL; exits with status 1 on FAIL. Specific to the two
# renames below (delta_raw -> delta_free, log_sigma_raw -> log_sigma).
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
data_json <- arg("data-json")
out_csv <- arg("out")
n_post <- as.integer(arg("n", "40"))
n_off <- as.integer(arg("n-off", "15"))
dat <- jsonlite::read_json(data_json, simplifyVector = TRUE)
P1 <- dat$P - 1
K1 <- dat$K - 1

methods_fit <- function(stan_file) {
  # WHY: cmdstanr attaches model methods only when it compiles, never to an existing binary.
  mod <- cmdstan_model(stan_file, compile_model_methods = TRUE, force_recompile = TRUE)
  fit <- mod$sample(
    data = data_json, chains = 1, iter_warmup = 0, iter_sampling = 1, fixed_param = TRUE,
    init = 0, seed = 1, output_dir = tempdir(), refresh = 0, show_messages = FALSE
  )
  fit$init_model_methods()
  list(mod = mod, fit = fit, pars = names(mod$variables()$parameters))
}
old <- methods_fit(arg("old-stan"))
new <- methods_fit(arg("new-stan"))
renamed_old <- c("delta_raw", "log_sigma_raw")
renamed_new <- c("delta_free", "log_sigma")
stopifnot(
  all(renamed_old %in% old$pars), all(renamed_new %in% new$pars),
  setequal(setdiff(old$pars, renamed_old), setdiff(new$pars, renamed_new))
)

files <- list.files(arg("draws-dir"), pattern = "\\.csv$", full.names = TRUE)
# WHY: zero-size parameters are declared but never written to the CSVs.
old$pars <- intersect(old$pars, read_cmdstan_csv(files, variables = "", sampler_diagnostics = "")$metadata$stan_variables)
M <- as_draws_matrix(read_cmdstan_csv(files, variables = old$pars, sampler_diagnostics = "")$post_warmup_draws)
cols <- function(fmt, ...) do.call(sprintf, c(list(fmt), expand.grid(...)))
delta_raw_cols <- cols("delta_raw[%d,%d]", 1:P1, 1:K1)
log_sigma_raw_cols <- sprintf("log_sigma_raw[%d]", 1:P1)
sigma_delta_cols <- sprintf("sigma_delta[%d]", 1:P1)

idx <- unique(round(seq(1, nrow(M), length.out = n_post)))
X <- unclass(as.matrix(M))[idx, , drop = FALSE]
set.seed(20261016)
X_off <- X[sample(nrow(X), n_off, replace = TRUE), , drop = FALSE]
X_off[, "tau_log_sigma"] <- X_off[, "tau_log_sigma"] * exp(rnorm(n_off, 0, 0.5))
X_off[, "mu_log_sigma"] <- X_off[, "mu_log_sigma"] + rnorm(n_off, 0, 0.5)
X_off[, sigma_delta_cols] <- X_off[, sigma_delta_cols] * exp(matrix(rnorm(n_off * P1, 0, 0.5), n_off))
X_off[, delta_raw_cols] <- X_off[, delta_raw_cols] + rnorm(n_off * length(delta_raw_cols))
X_off[, log_sigma_raw_cols] <- X_off[, log_sigma_raw_cols] + rnorm(n_off * P1, 0, 0.5)
X <- rbind(X, X_off)
point <- rep(c("posterior", "off-posterior"), c(length(idx), n_off))

delta_free <- X[, delta_raw_cols] * X[, sprintf("sigma_delta[%d]", rep(1:P1, K1))]
colnames(delta_free) <- cols("delta_free[%d,%d]", 1:P1, 1:K1)
log_sigma <- X[, "mu_log_sigma"] + X[, "tau_log_sigma"] * X[, log_sigma_raw_cols]
colnames(log_sigma) <- sprintf("log_sigma[%d]", 1:P1)
X_new <- cbind(X[, setdiff(colnames(X), c(delta_raw_cols, log_sigma_raw_cols))], delta_free, log_sigma)

as_draws <- function(x) as_draws_matrix(x)
u_old <- old$fit$unconstrain_draws(draws = as_draws(X), format = "draws_matrix")
u_new <- new$fit$unconstrain_draws(draws = as_draws(X_new), format = "draws_matrix")
eq <- data.frame(t(vapply(seq_len(nrow(X)), function(i) {
  # WHY: far enough off the posterior a softmax can underflow to an exact 0, which
  # Stan rejects; such points are dropped and counted, not evaluated.
  lp <- function(m, u) tryCatch(m$fit$log_prob(as.numeric(u)), error = function(e) NA_real_)
  lp_old <- lp(old, u_old[i, ])
  lp_new <- lp(new, u_new[i, ])
  log_jac_sigma <- P1 * log(X[i, "tau_log_sigma"])
  log_jac_delta <- K1 * sum(log(X[i, sigma_delta_cols]))
  log_jac <- log_jac_sigma + log_jac_delta
  c(
    lp_old = lp_old, lp_new = lp_new, log_jac_sigma = log_jac_sigma, log_jac_delta = log_jac_delta,
    log_jac = log_jac, const = lp_new - (lp_old - log_jac)
  )
}, numeric(6))))
eq$point <- point
rejected <- !is.finite(eq$const)
# WHY: the two models are one density, so Stan must reject a point in both or in neither.
one_sided <- sum(xor(is.finite(eq$lp_old), is.finite(eq$lp_new)))
cat(sprintf("points rejected by Stan (dropped): %d posterior, %d off-posterior; by one model only: %d\n",
  sum(rejected & point == "posterior"), sum(rejected & point != "posterior"), one_sided))
stopifnot(one_sided == 0, !any(rejected & point == "posterior"), sum(!rejected & point != "posterior") >= 5)
eq <- eq[!rejected, ]
write.csv(eq, out_csv, row.names = FALSE)

report <- function(d, label) {
  sprintf(
    "%s: n=%d, constant sd %.2e, range %.2e, mean %.3e; lp_new spans %.4g",
    label, nrow(d), sd(d$const), diff(range(d$const)), mean(d$const), diff(range(d$lp_new))
  )
}
cat(report(eq[eq$point == "posterior", ], "posterior"), "\n")
cat(report(eq[eq$point == "off-posterior", ], "off-posterior"), "\n")
# WHY: a wrong log-Jacobian term moves the constant by O(1) between points, so the
# tolerance only has to allow for floating-point error, about machine epsilon times |lp|.
tol <- 1e-12 * max(1, abs(eq$lp_new))
# Negative controls: each Jacobian term left out in turn must break the constant.
controls <- c(
  without_sigma_term = diff(range(eq$const - eq$log_jac_sigma)),
  without_delta_term = diff(range(eq$const - eq$log_jac_delta)),
  without_jacobian = diff(range(eq$const - eq$log_jac))
)
cat(sprintf("negative control %s: constant range %.3g (%s)\n", names(controls), controls,
  ifelse(controls > tol, "fails, as it must", "PASSES: the check cannot see this term")), sep = "")

post <- point == "posterior"
gq_at <- function(m, x) {
  # WHY: standalone generated quantities match parameter columns by position, in the
  # model's declaration order; X_new has delta_free and log_sigma at the end.
  base <- sub("\\[.*$", "", colnames(x))
  ord <- unlist(lapply(m$pars, function(v) which(base == v)))
  stopifnot(length(ord) == ncol(x))
  x <- x[, ord, drop = FALSE]
  gq <- m$mod$generate_quantities(
    fitted_params = as_draws_array(as_draws(x)), data = data_json, seed = 1, sig_figs = 18,
    output_dir = tempdir()
  )
  unclass(as_draws_matrix(gq$draws()))
}
gq_old <- gq_at(old, X[post, , drop = FALSE])
gq_new <- gq_at(new, X_new[post, , drop = FALSE])
stopifnot(identical(colnames(gq_old), colnames(gq_new)), any(startsWith(colnames(gq_old), "delta_pred[")))
gq_rel <- abs(gq_new - gq_old) / pmax(1, abs(gq_old))
delta_pred_cols <- startsWith(colnames(gq_old), "delta_pred[")
cat(sprintf(
  "generated quantities at %d posterior points: %d values, %d differ, max relative difference %.2e; delta_pred spans %.3g\n",
  sum(post), length(gq_rel), sum(gq_new != gq_old), max(gq_rel), diff(range(gq_old[, delta_pred_cols]))
))
gq_pass <- max(gq_rel) < 1e-9

pass <- diff(range(eq$const)) < tol && all(controls > tol) && gq_pass
cat(sprintf(
  "EQUIV %s: constant range %.2e over all %d points (tolerance %.1e); generated quantities max relative difference %.2e\n",
  if (pass) "PASS" else "FAIL", diff(range(eq$const)), nrow(eq), tol, max(gq_rel)
))
if (!pass) quit(status = 1)
