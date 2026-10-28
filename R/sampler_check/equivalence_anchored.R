# Checks that the anchored, partially centred parameterisation leaves the posterior
# unchanged, apart from its bound on log phi. Old model: backward non-centred walk
# (z_beta_raw, z_beta0), non-centred house effects (gamma_raw) and log overdispersion
# (log_phi), centred delta_free and log_sigma. New model: election-date anchors (anchor_w)
# with Brownian bridges (z_walk), partially centred delta_t and log_sigma_t, house effects
# partially centred (gamma_t) or non-centred as before (gamma_raw), and log phi bounded
# above (log_phi_u). Each old point maps to a new one through the natural
# quantities (the whole walk path beta, gamma, delta_free, log sigma, log phi), and
#   (lp_new - ld_new) - (lp_old - unused - ld_old) = constant,
# where ld is each model's log |d natural / d parameters|. The old model's first walk
# column, z_beta_raw[, 1], is never used: unused is its std_normal term, which the new model
# lacks. Each negative control drops one log-Jacobian term (or unused) and must then fail.
# The bound conditions on log phi < phi_cap, so the map is exact for every point below it;
# points at or above it are outside the claim, so they are counted and excluded.
# Points: n posterior draws of the old model plus n_off pushed off the posterior in the
# scales the new weights depend on. Both models then run their generated quantities at
# the posterior points with one seed; those must agree.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence_anchored.R --old-stan=<old.stan> \
#     --new-stan=<new.stan> --data-json=<file> --draws-dir=<old fit's CSV dir> \
#     --out=<csv> [--n=40] [--n-off=15]
# Prints EQUIV PASS or EQUIV FAIL; exits with status 1 on FAIL.
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
d <- jsonlite::read_json(data_json, simplifyVector = TRUE)
P1 <- d$P - 1
K1 <- d$K - 1
H1 <- d$H - 1
T_ <- d$D + d$pred_y_time_diff

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
stopifnot(
  c("z_beta_raw", "gamma_raw", "log_phi") %in% old$pars,
  c("anchor_w", "z_walk", "delta_t", "log_sigma_t", "log_phi_u") %in% new$pars,
  sum(c("gamma_t", "gamma_raw") %in% new$pars) == 1
)
# WHY: non-centred house effects are partial centring with weight 0, so one map covers both.
g_par <- if ("gamma_t" %in% new$pars) "gamma_t" else "gamma_raw"

# Anchors and present parties from the data, as the new model's transformed data has them
el <- which(d$house_n == 1)
anchors <- sort(unique(d$date_n[el]))
m <- length(anchors)
present <- lapply(anchors, \(a) {
  rows <- el[d$date_n[el] == a]
  p <- unlist(lapply(rows, \(n) d$party_index_n[n, seq_len(d$n_parties_n[n])]))
  sort(unique(p[p > 1] - 1))
})
w_off <- matrix(0, m, P1)
for (n in el) {
  j <- match(d$date_n[n], anchors)
  cols <- d$party_index_n[n, seq_len(d$n_parties_n[n])]
  cols <- cols[cols > 1]
  r <- log((d$y_n[n, cols] + 0.5) / (d$y_n[n, 1] + 0.5))
  w_off[j, cols - 1] <- r - sum(r) / (length(r) + 1)
}
zcol <- integer(T_)
zcol[setdiff(seq_len(T_ - 1), anchors)] <- seq_len(T_ - 1 - m)

files <- list.files(arg("draws-dir"), pattern = "\\.csv$", full.names = TRUE)
old$pars <- intersect(old$pars, read_cmdstan_csv(files, variables = "", sampler_diagnostics = "")$metadata$stan_variables)
M <- as_draws_matrix(read_cmdstan_csv(files, variables = old$pars, sampler_diagnostics = "")$post_warmup_draws)
idx <- unique(round(seq(1, nrow(M), length.out = n_post)))
X <- unclass(as.matrix(M))[idx, , drop = FALSE]
set.seed(20261027)
X_off <- X[sample(nrow(X), n_off, replace = TRUE), , drop = FALSE]
scale_cols <- grep("^(sigma_delta|sigma_gamma)\\[|^(tau_log_sigma|sigma_phi|tau_stjornarslit)$", colnames(X_off))
X_off[, scale_cols] <- X_off[, scale_cols] * exp(matrix(rnorm(n_off * length(scale_cols), 0, 0.5), n_off))
X <- rbind(X, X_off)
point <- rep(c("posterior", "off-posterior"), c(length(idx), n_off))

ldmvn_chol <- function(L) sum(log(diag(L)))
# One old point (a row of X) to the old parameter list, the new one and both log-Jacobians
map_point <- function(x) {
  po <- old$fit$constrain_variables(old$fit$unconstrain_draws(draws = as_draws_matrix(t(x)), format = "draws_matrix")[1, ],
    transformed_parameters = FALSE, generated_quantities = FALSE
  )
  co <- old$fit$constrain_variables(old$fit$unconstrain_variables(po), transformed_parameters = TRUE, generated_quantities = FALSE)
  beta <- co$beta
  sigma <- exp(po$log_sigma)
  L <- diag(sigma) %*% po$L_Omega
  ldL <- sum(log(diag(L)))
  log_phi <- po$mu_phi + po$sigma_phi * po$log_phi
  gam <- po$mu_gamma + po$sigma_gamma * po$gamma_raw
  # the new model's weights, step variances and bound depend only on the scales and data
  shared <- po[setdiff(names(po), c("z_beta_raw", "gamma_raw", "log_phi", "delta_free", "log_sigma"))]
  probe <- c(shared, list(
    anchor_w = matrix(0, m, P1), z_walk = matrix(0, P1, T_ - 1 - m),
    delta_t = matrix(0, P1, K1), log_sigma_t = rep(0, P1), log_phi_u = rep(0, H1)
  ))
  probe[[g_par]] <- matrix(0, P1 - 1, H1)
  cp <- new$fit$constrain_variables(new$fit$unconstrain_variables(probe), transformed_parameters = TRUE, generated_quantities = FALSE)
  phi_init <- cp$log_phi[1]
  probe$log_phi_u <- rep(-60, H1)
  phi_cap <- new$fit$constrain_variables(new$fit$unconstrain_variables(probe),
    transformed_parameters = TRUE,
    generated_quantities = FALSE
  )$log_phi[1]
  v <- cp$v
  cv <- cp$cv
  lam_s <- cp$lambda_sigma
  lam_d <- cp$lambda_delta
  lam_g <- if (g_par == "gamma_t") cp$lambda_gamma else matrix(0, P1 - 1, H1)
  if (any(log_phi >= phi_cap)) {
    return(list(outside = TRUE))
  }

  # walk: anchors last to first, then bridges and the pre-anchor stretch
  anchor_w <- matrix(0, m, P1)
  ld_anchor <- 0
  for (j in seq_len(m)) {
    a <- anchors[j]
    b <- if (j < m) anchors[j + 1] else T_
    pr <- present[[j]]
    ab <- setdiff(seq_len(P1), pr)
    if (length(ab)) {
      sc <- sqrt(cv[b] - cv[a])
      L_aa <- t(chol((L %*% t(L))[ab, ab, drop = FALSE]))
      anchor_w[j, ab] <- solve(L_aa, (beta[ab, a] - beta[ab, b]) / sc)
      anchor_w[j, pr] <- beta[pr, a] + sum(beta[ab, a]) / (length(pr) + 1) - w_off[j, pr]
      ld_anchor <- ld_anchor + length(ab) * log(sc) + ldmvn_chol(L_aa)
    } else {
      anchor_w[j, ] <- beta[, a] - w_off[j, ]
    }
  }
  z_walk <- matrix(NA_real_, P1, T_ - 1 - m)
  ld_walk <- 0
  for (j in seq_len(m)) {
    a <- anchors[j]
    b <- if (j < m) anchors[j + 1] else T_
    if (b - a >= 2) {
      for (t in (a + 1):(b - 1)) {
        R <- cv[b] - cv[t - 1]
        s <- sqrt(v[t] * (cv[b] - cv[t]) / R)
        z_walk[, zcol[t]] <- solve(L, (beta[, t] - beta[, t - 1] - (v[t] / R) * (beta[, b] - beta[, t - 1])) / s)
        ld_walk <- ld_walk + P1 * log(s) + ldL
      }
    }
  }
  a1 <- if (m > 0) anchors[1] else T_
  if (a1 >= 2) {
    for (t in (a1 - 1):1) {
      z_walk[, zcol[t]] <- solve(L, (beta[, t] - beta[, t + 1]) / sqrt(v[t + 1]))
      ld_walk <- ld_walk + P1 * 0.5 * log(v[t + 1]) + ldL
    }
  }
  stopifnot(!anyNA(z_walk))

  sg <- matrix(po$sigma_gamma, P1 - 1, H1)
  mg <- matrix(po$mu_gamma, P1 - 1, H1)
  sd_ <- matrix(po$sigma_delta, P1, K1)
  pn <- c(shared, list(
    anchor_w = anchor_w, z_walk = z_walk,
    delta_t = po$delta_free / sd_^(1 - lam_d),
    log_sigma_t = lam_s * po$mu_log_sigma + (po$log_sigma - po$mu_log_sigma) / po$tau_log_sigma^(1 - lam_s),
    log_phi_u = log((phi_cap - log_phi) / (phi_cap - phi_init))
  ))
  pn[[g_par]] <- lam_g * mg + (gam - mg) / sg^(1 - lam_g)
  # log |d natural / d parameters|, term by term so that each negative control drops one
  ld_beta0 <- sum(log(co$tau_f * sigma))
  jac <- list(
    old_walk = ld_beta0 + (T_ - 1) * ldL + P1 * 0.5 * sum(log(v[2:T_])),
    old_gamma = H1 * sum(log(po$sigma_gamma)),
    old_sigma_phi = H1 * log(po$sigma_phi),
    new_beta0 = ld_beta0,
    new_anchor = ld_anchor,
    new_bridge = ld_walk,
    new_gamma = sum((1 - lam_g) * log(sg)),
    new_delta = sum((1 - lam_d) * log(sd_)),
    new_log_sigma = sum((1 - lam_s) * log(po$tau_log_sigma)),
    new_phi = sum(log(phi_cap - phi_init) + pn$log_phi_u)
  )
  list(outside = FALSE, po = po, pn = pn, beta = beta, jac = jac, unused = -0.5 * sum(po$z_beta_raw[, 1]^2))
}

EQ_COLS <- c(
  "lp_old", "lp_new", "path_err", "unused", "const", "outside", "old_walk", "old_gamma", "old_sigma_phi",
  "new_beta0", "new_anchor", "new_bridge", "new_gamma", "new_delta", "new_log_sigma", "new_phi"
)
eq <- data.frame(t(vapply(seq_len(nrow(X)), function(i) {
  # WHY: a point the map cannot handle (an error from constrain or unconstrain, or a
  # Cholesky factor far off the posterior) is dropped and counted; so is a log density
  # Stan rejects, such as a softmax that underflows to an exact 0, which lp() returns as NA.
  # A point with log phi at or above the bound is outside the claim: excluded and counted.
  r <- tryCatch(map_point(X[i, ]), error = function(e) NULL)
  if (is.null(r) || r$outside) {
    return(setNames(c(rep(NA_real_, 5), as.numeric(isTRUE(r$outside)), rep(NA_real_, 10)), EQ_COLS))
  }
  un <- new$fit$unconstrain_variables(r$pn)
  cn <- new$fit$constrain_variables(un, transformed_parameters = TRUE, generated_quantities = FALSE)
  lp <- function(mm, u) tryCatch(mm$fit$log_prob(u, jacobian = TRUE), error = function(e) NA_real_)
  lp_old <- lp(old, old$fit$unconstrain_variables(r$po))
  lp_new <- lp(new, un)
  j <- unlist(r$jac)
  ld_new <- sum(j[startsWith(names(j), "new_")])
  ld_old <- sum(j[startsWith(names(j), "old_")])
  row <- c(
    lp_old = lp_old, lp_new = lp_new, path_err = max(abs(cn$beta - r$beta)), unused = r$unused,
    const = (lp_new - ld_new) - (lp_old - r$unused - ld_old), outside = 0, j
  )
  stopifnot(identical(names(row), EQ_COLS))
  row
}, setNames(numeric(length(EQ_COLS)), EQ_COLS))))
eq$point <- point
outside <- eq$outside == 1
rejected <- !outside & !is.finite(eq$const)
one_sided <- sum(xor(is.finite(eq$lp_old), is.finite(eq$lp_new)))
cat(sprintf(
  "points outside the bound (log phi >= phi_cap, excluded): %d posterior, %d off-posterior\n",
  sum(outside & point == "posterior"), sum(outside & point != "posterior")
))
cat(sprintf(
  "points rejected (dropped): %d posterior, %d off-posterior; by one model only: %d\n",
  sum(rejected & point == "posterior"), sum(rejected & point != "posterior"), one_sided
))
keep <- !outside & !rejected
stopifnot(
  one_sided == 0, !any(rejected & point == "posterior"),
  sum(keep & point == "posterior") >= 5, sum(keep & point != "posterior") >= 5
)
eq <- eq[keep, ]
write.csv(eq, out_csv, row.names = FALSE)

report <- function(x, label) {
  sprintf(
    "%s: n=%d, constant sd %.2e, range %.2e, mean %.3e; lp_new spans %.4g; max path error %.2e",
    label, nrow(x), sd(x$const), diff(range(x$const)), mean(x$const), diff(range(x$lp_new)), max(x$path_err)
  )
}
cat(report(eq[eq$point == "posterior", ], "posterior"), "\n")
cat(report(eq[eq$point == "off-posterior", ], "off-posterior"), "\n")
# WHY: a wrong log-Jacobian term moves the constant by O(1) between points, so the
# tolerance only has to allow for floating-point error, about machine epsilon times |lp|.
tol <- 1e-12 * max(1, abs(eq$lp_new))
# Each control drops one term from its own side; new_beta0 is left out, as it is also in
# old_walk and dropping it there too would cancel.
terms <- setdiff(grep("^(new|old)_", names(eq), value = TRUE), "new_beta0")
controls <- c(
  vapply(terms, \(t) diff(range(eq$const + if (startsWith(t, "new_")) eq[[t]] else -eq[[t]])), numeric(1)),
  unused_column = diff(range(eq$const - eq$unused))
)
names(controls) <- paste0("without_", names(controls))
cat(sprintf(
  "negative control %s: constant range %.3g (%s)\n", names(controls), controls,
  ifelse(controls > tol, "fails, as it must", "PASSES: the check cannot see this term")
), sep = "")

post <- which(point == "posterior" & keep)
flatten <- function(lst, pars) {
  # WHY: standalone generated quantities match parameter columns by position, in the
  # model's declaration order and Stan's column-major element order.
  unlist(lapply(pars, function(v) {
    x <- lst[[v]]
    dims <- dim(x) %||% length(x)
    nm <- if (length(dims) == 1 && length(x) == 1 && is.null(dim(x))) {
      v
    } else {
      paste0(v, "[", do.call(paste, c(expand.grid(lapply(dims, seq_len)), sep = ",")), "]")
    }
    setNames(as.vector(x), nm)
  }))
}
gq_at <- function(mm, rows) {
  x <- do.call(rbind, rows)
  gq <- mm$mod$generate_quantities(
    fitted_params = as_draws_array(as_draws_matrix(x)), data = data_json, seed = 1, sig_figs = 18,
    output_dir = tempdir()
  )
  unclass(as_draws_matrix(gq$draws()))
}
maps <- lapply(post, \(i) map_point(X[i, ]))
gq_old <- gq_at(old, lapply(maps, \(r) flatten(r$po, old$pars)))
gq_new <- gq_at(new, lapply(maps, \(r) flatten(r$pn, new$pars)))
common <- intersect(colnames(gq_old), colnames(gq_new))
stopifnot(length(common) > 0, any(startsWith(common, "delta_pred[")))
gq_rel <- abs(gq_new[, common] - gq_old[, common]) / pmax(1, abs(gq_old[, common]))
cat(sprintf(
  "generated quantities at %d posterior points: %d values in %d shared columns (old %d, new %d), %d differ, max relative difference %.2e\n",
  length(post), length(gq_rel), length(common), ncol(gq_old), ncol(gq_new), sum(gq_new[, common] != gq_old[, common]), max(gq_rel)
))
gq_pass <- max(gq_rel) < 1e-9

pass <- diff(range(eq$const)) < tol && all(controls > tol) && gq_pass && max(eq$path_err) < 1e-8
cat(sprintf(
  "EQUIV %s: constant range %.2e over all %d points (tolerance %.1e); max path error %.2e; generated quantities max relative difference %.2e\n",
  if (pass) "PASS" else "FAIL", diff(range(eq$const)), nrow(eq), tol, max(eq$path_err), max(gq_rel)
))
if (!pass) quit(status = 1)
