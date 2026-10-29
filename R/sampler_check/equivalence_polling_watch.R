# Checks that the anchored parameterisation of Stan/polling_watch_v4.stan, or of
# Stan/polling_watch_v5_epoch.stan, leaves the posterior unchanged, apart from its bound on
# log phi. The bias parameters (v5's epoch shocks and their scales too) are the same in the
# old and new model and are copied by name. Old model (master before
# 28 Oct 2026): forward walk from beta0 with non-centred innovations z_step_raw, centred
# onto the sum-to-zero subspace, and non-centred log overdispersion (log_phi). New model:
# the walk in the levels of the parties other than a reference party, with election-date
# anchors (anchor_w), Brownian bridges and the stretches before the first and after the
# last anchor (z_walk), and log phi centred and bounded above (log_phi_u). Each old point
# maps to a new one through the natural quantities (the whole path beta and log phi), and
#   (lp_new - ld_new) - (lp_old - unused - ld_old) = constant,
# where ld is each model's log |d natural / d parameters|. Each old step z_step_raw[, t] has
# one direction the centring discards, along n = M^-1 1 with M = diag(sigma) L_Omega:
# unused is its std_normal term, which the new model lacks. Each negative control drops one
# log-Jacobian term (or unused) and must then fail; a term that is 0 at every point (no
# absent party at an anchor, say) has nothing to control and is reported as such. The
# bound conditions on log phi < phi_cap, so points at or above it are counted and
# excluded. Points: n posterior draws of the old model plus n_off pushed off the posterior
# in the scales the map depends on. Both models then run their generated quantities at the
# posterior points with one seed; those must agree.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence_polling_watch.R --old-stan=<old.stan> \
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
P <- d$P
P1 <- P - 1
H1 <- d$H - 1
D <- d$D

methods_fit <- function(stan_file) {
  # WHY: cmdstanr attaches model methods only when it compiles, never to an existing binary.
  # It writes the binary beside the .stan file, over the old one in place, so compile a
  # private copy: a fit running the original binary is left alone.
  dir <- tempfile("equivalence_")
  dir.create(dir)
  stan_copy <- file.path(dir, basename(stan_file))
  stopifnot(file.copy(stan_file, stan_copy))
  mod <- cmdstan_model(stan_copy, compile_model_methods = TRUE, force_recompile = TRUE)
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
  c("beta0", "z_step_raw", "log_phi") %in% old$pars,
  c("anchor_w", "z_walk", "log_phi_u") %in% new$pars
)

# Anchors, the reference party and each anchor's present parties, as the new model's
# transformed data has them
el <- which(d$house_n == 1)
anchors <- sort(unique(d$date_n[el]))
m <- length(anchors)
reported <- lapply(anchors, \(a) {
  rows <- el[d$date_n[el] == a]
  sort(unique(unlist(lapply(rows, \(n) d$party_index_n[n, seq_len(d$n_parties_n[n])]))))
})
ref <- max(Reduce(intersect, reported))
oth <- setdiff(seq_len(P), ref)
present <- lapply(reported, \(r) which(oth %in% r))
w_off <- matrix(0, m, P1)
for (n in el) {
  j <- match(d$date_n[n], anchors)
  cols <- setdiff(d$party_index_n[n, seq_len(d$n_parties_n[n])], ref)
  r <- log((d$y_n[n, cols] + 0.5) / (d$y_n[n, ref] + 0.5))
  w_off[j, match(cols, oth)] <- r - sum(r) / (length(r) + 1)
}
v <- c(0, d$time_diff)
cv <- cumsum(v)
zcol <- integer(D)
zcol[setdiff(seq_len(D), anchors)] <- seq_len(D - m)
cat(sprintf(
  "D=%d, %d anchors at dates %s; reference party %d; absent parties per anchor: %s\n",
  D, m, paste(anchors, collapse = " "), ref,
  paste(vapply(present, \(p) P1 - length(p), numeric(1)), collapse = " ")
))

files <- list.files(arg("draws-dir"), pattern = "\\.csv$", full.names = TRUE)
old$pars <- intersect(old$pars, read_cmdstan_csv(files, variables = "", sampler_diagnostics = "")$metadata$stan_variables)
M <- as_draws_matrix(read_cmdstan_csv(files, variables = old$pars, sampler_diagnostics = "")$post_warmup_draws)
idx <- unique(round(seq(1, nrow(M), length.out = n_post)))
X <- unclass(as.matrix(M))[idx, , drop = FALSE]
set.seed(20261028)
X_off <- X[sample(nrow(X), n_off, replace = TRUE), , drop = FALSE]
log_cols <- grep("^log_sigma\\[", colnames(X_off))
X_off[, log_cols] <- X_off[, log_cols] + matrix(rnorm(n_off * length(log_cols), 0, 0.5), n_off)
scale_cols <- grep("^(sigma_gamma|sigma_phi|tau_mu_est\\[1\\]|tau_house_est\\[1\\])$", colnames(X_off))
X_off[, scale_cols] <- X_off[, scale_cols] * exp(matrix(rnorm(n_off * length(scale_cols), 0, 0.5), n_off))
X <- rbind(X, X_off)
point <- rep(c("posterior", "off-posterior"), c(length(idx), n_off))
# WHY: the CSVs keep 6 significant figures, so a sum_to_zero_vector draw sums to ~1e-7, and
# unconstraining it throws (Stan checks the sum to 1e-8). Both models are then evaluated
# at the projected point, which leaves the constant unchanged.
stz_cols <- grep("^(beta0|mu_gamma0?|gamma_free|z_mu|z_house)\\[", colnames(X), value = TRUE)
stz_groups <- split(stz_cols, sub(",?[0-9]+\\]$", "", stz_cols))
for (g in stz_groups) X[, g] <- X[, g] - rowMeans(X[, g, drop = FALSE])
# beta0, the industry bias and H - 1 house deviations, plus v5's shocks between E epochs
# WHY: from the model, not the data file, which may carry v5's epochs into a v4 check
E <- if ("E" %in% names(new$mod$variables()$data)) d$E else 1
stopifnot(length(stz_groups) == 2 + H1 + (E - 1) * (1 + H1))

# One old point (a row of X) to the old parameter list, the new one and both log-Jacobians
map_point <- function(x) {
  po <- old$fit$constrain_variables(old$fit$unconstrain_draws(draws = as_draws_matrix(t(x)), format = "draws_matrix")[1, ],
    transformed_parameters = FALSE, generated_quantities = FALSE
  )
  co <- old$fit$constrain_variables(old$fit$unconstrain_variables(po), transformed_parameters = TRUE, generated_quantities = FALSE)
  beta <- co$beta
  b <- t(beta[, oth, drop = FALSE])
  Ms <- diag(exp(po$log_sigma)) %*% po$L_Omega
  log_phi <- po$mu_phi + po$sigma_phi * po$log_phi
  shared <- po[setdiff(names(po), c("beta0", "z_step_raw", "log_phi"))]
  probe <- c(shared, list(anchor_w = matrix(0, m, P1), z_walk = matrix(0, P1, D - m), log_phi_u = rep(0, H1)))
  cp <- new$fit$constrain_variables(new$fit$unconstrain_variables(probe), transformed_parameters = TRUE, generated_quantities = FALSE)
  phi_init <- cp$log_phi[1]
  probe$log_phi_u <- rep(-60, H1)
  phi_cap <- new$fit$constrain_variables(new$fit$unconstrain_variables(probe),
    transformed_parameters = TRUE,
    generated_quantities = FALSE
  )$log_phi[1]
  L <- cp$L_walk
  if (any(log_phi >= phi_cap)) {
    return(list(outside = TRUE))
  }
  ldL <- sum(log(diag(L)))
  Sig <- L %*% t(L)

  # walk: the root anchor, earlier anchors back to front, then bridges and the stretches
  anchor_w <- matrix(0, m, P1)
  ld_anchor <- 0
  for (j in m:1) {
    a <- anchors[j]
    pr <- present[[j]]
    ab <- setdiff(seq_len(P1), pr)
    if (length(ab)) {
      if (j < m) {
        nx <- anchors[j + 1]
        sc <- sqrt(cv[nx] - cv[a])
        L_aa <- t(chol(Sig[ab, ab, drop = FALSE]))
        anchor_w[j, ab] <- solve(L_aa, (b[ab, a] - b[ab, nx]) / sc)
        ld_anchor <- ld_anchor + length(ab) * log(sc) + sum(log(diag(L_aa)))
      } else {
        anchor_w[j, ab] <- b[ab, a]
      }
      anchor_w[j, pr] <- b[pr, a] + sum(b[ab, a]) / (length(pr) + 1) - w_off[j, pr]
    } else {
      anchor_w[j, ] <- b[, a] - w_off[j, ]
    }
  }
  z_walk <- matrix(NA_real_, P1, D - m)
  ld_walk <- 0
  if (m >= 2) {
    for (j in seq_len(m - 1)) {
      a <- anchors[j]
      nx <- anchors[j + 1]
      if (nx - a >= 2) {
        for (t in (a + 1):(nx - 1)) {
          R <- cv[nx] - cv[t - 1]
          s <- sqrt(v[t] * (cv[nx] - cv[t]) / R)
          z_walk[, zcol[t]] <- solve(L, (b[, t] - b[, t - 1] - (v[t] / R) * (b[, nx] - b[, t - 1])) / s)
          ld_walk <- ld_walk + P1 * log(s) + ldL
        }
      }
    }
  }
  if (anchors[1] >= 2) {
    for (t in (anchors[1] - 1):1) {
      z_walk[, zcol[t]] <- solve(L, (b[, t] - b[, t + 1]) / sqrt(v[t + 1]))
      ld_walk <- ld_walk + P1 * 0.5 * log(v[t + 1]) + ldL
    }
  }
  if (anchors[m] < D) {
    for (t in (anchors[m] + 1):D) {
      z_walk[, zcol[t]] <- solve(L, (b[, t] - b[, t - 1]) / sqrt(v[t]))
      ld_walk <- ld_walk + P1 * 0.5 * log(v[t]) + ldL
    }
  }
  stopifnot(!anyNA(z_walk))

  # The old steps: b[, t] - b[, t - 1] = sqrt(dt) A z_t with A = (C M)[oth, ], A A' = L L'
  A <- (Ms - matrix(colMeans(Ms), P, P, byrow = TRUE))[oth, , drop = FALSE]
  n_dir <- solve(Ms, rep(1, P))
  n_dir <- n_dir / sqrt(sum(n_dir^2))
  pn <- c(shared, list(anchor_w = anchor_w, z_walk = z_walk, log_phi_u = log((phi_cap - log_phi) / (phi_cap - phi_init))))
  # log |d natural / d parameters|, term by term so that each negative control drops one
  jac <- list(
    old_walk = (D - 1) * ldL + P1 * 0.5 * sum(log(v[2:D])),
    old_phi = H1 * log(po$sigma_phi),
    new_anchor = ld_anchor,
    new_walk = ld_walk,
    new_phi = sum(log(phi_cap - phi_init) + pn$log_phi_u)
  )
  list(
    outside = FALSE, po = po, pn = pn, beta = beta, jac = jac,
    unused = -0.5 * sum(crossprod(n_dir, po$z_step_raw)^2),
    factor_err = max(abs(A %*% t(A) - Sig)), null_err = max(abs(A %*% n_dir))
  )
}

EQ_COLS <- c(
  "lp_old", "lp_new", "path_err", "factor_err", "null_err", "unused", "const", "outside",
  "old_walk", "old_phi", "new_anchor", "new_walk", "new_phi"
)
eq <- data.frame(t(vapply(seq_len(nrow(X)), function(i) {
  # WHY: an exception thrown inside the model methods corrupts R's heap when they are built
  # with g++ (CXX17 in ~/.R/Makevars) against a clang-built R, and R then crashes at a later
  # regex. So nothing here catches one and carries on: any error from a model method stops
  # the run. A point with log phi at or above the bound is outside the claim: map_point()
  # excludes and counts it before the new model's methods see its log phi (they see only
  # its fixed probe values).
  r <- map_point(X[i, ])
  if (r$outside) {
    return(setNames(c(rep(NA_real_, 7), 1, rep(NA_real_, 5)), EQ_COLS))
  }
  un <- new$fit$unconstrain_variables(r$pn)
  cn <- new$fit$constrain_variables(un, transformed_parameters = TRUE, generated_quantities = FALSE)
  lp <- function(mm, u) mm$fit$log_prob(u, jacobian = TRUE)
  lp_old <- lp(old, old$fit$unconstrain_variables(r$po))
  lp_new <- lp(new, un)
  j <- unlist(r$jac)
  ld_new <- sum(j[startsWith(names(j), "new_")])
  ld_old <- sum(j[startsWith(names(j), "old_")])
  row <- c(
    lp_old = lp_old, lp_new = lp_new, path_err = max(abs(cn$beta - r$beta)),
    factor_err = r$factor_err, null_err = r$null_err, unused = r$unused,
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
    "%s: n=%d, constant sd %.2e, range %.2e, mean %.3e; lp_new spans %.4g; max path error %.2e; max factor error %.2e; max null error %.2e",
    label, nrow(x), sd(x$const), diff(range(x$const)), mean(x$const), diff(range(x$lp_new)),
    max(x$path_err), max(x$factor_err), max(x$null_err)
  )
}
cat(report(eq[eq$point == "posterior", ], "posterior"), "\n")
cat(report(eq[eq$point == "off-posterior", ], "off-posterior"), "\n")
# WHY: a wrong log-Jacobian term moves the constant by O(1) between points, so the
# tolerance only has to allow for floating-point error, about machine epsilon times |lp|.
tol <- 1e-12 * max(1, abs(eq$lp_new))
terms <- grep("^(new|old)_", names(eq), value = TRUE)
controls <- c(
  vapply(terms, \(t) diff(range(eq$const + if (startsWith(t, "new_")) eq[[t]] else -eq[[t]])), numeric(1)),
  unused_direction = diff(range(eq$const - eq$unused))
)
names(controls) <- paste0("without_", names(controls))
silent <- c(vapply(terms, \(t) all(eq[[t]] == 0), logical(1)), unused_direction = FALSE)
cat(sprintf(
  "negative control %s: constant range %.3g (%s)\n", names(controls), controls,
  ifelse(silent, "term is 0 at every point: nothing to control",
    ifelse(controls > tol, "fails, as it must", "PASSES: the check cannot see this term")
  )
), sep = "")

post <- which(point == "posterior" & keep)
flatten <- function(lst, mm) {
  # WHY: standalone generated quantities match parameter columns by name and position, in
  # Stan's CSV order. constrain_variables() returns a 3-D array (v5's z_house) as a flat
  # vector in that order, without dims, so the names come from the model's own CSV columns
  # (its one-draw methods fit), not from R's shape. A parameter of size 0 (fixed taus, or
  # E = 1) has no columns.
  pars <- Filter(\(v) length(lst[[v]]) > 0, mm$pars)
  names_csv <- variables(mm$fit$draws(variables = pars))
  x <- unlist(lapply(pars, \(v) as.vector(lst[[v]])))
  stopifnot(length(x) == length(names_csv))
  setNames(x, names_csv)
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
# flatten() orders each variable's values as the CSV does: the old point read back from it
# must match the CSV draw it came from (the round trip moves the 6-figure values by < 1e-5)
flat_err <- max(abs(flatten(maps[[1]]$po, old)[colnames(X)] - X[post[1], ]))
stopifnot("flatten() does not reproduce the CSV draw" = flat_err < 1e-4)
gq_old <- gq_at(old, lapply(maps, \(r) flatten(r$po, old)))
gq_new <- gq_at(new, lapply(maps, \(r) flatten(r$pn, new)))
common <- intersect(colnames(gq_old), colnames(gq_new))
stopifnot(length(common) > 0, any(startsWith(common, "pi_smooth[")), any(startsWith(common, "y_rep[")))
gq_rel <- abs(gq_new[, common] - gq_old[, common]) / pmax(1, abs(gq_old[, common]))
cat(sprintf(
  "generated quantities at %d posterior points: %d values in %d shared columns (old %d, new %d), %d differ, max relative difference %.2e\n",
  length(post), length(gq_rel), length(common), ncol(gq_old), ncol(gq_new), sum(gq_new[, common] != gq_old[, common]), max(gq_rel)
))
gq_pass <- max(gq_rel) < 1e-9

pass <- diff(range(eq$const)) < tol && all(controls[!silent] > tol) && gq_pass &&
  max(eq$path_err) < 1e-8 && max(eq$factor_err) < 1e-10 && max(eq$null_err) < 1e-10
cat(sprintf(
  "EQUIV %s: constant range %.2e over all %d points (tolerance %.1e); max path error %.2e; generated quantities max relative difference %.2e\n",
  if (pass) "PASS" else "FAIL", diff(range(eq$const)), nrow(eq), tol, max(eq$path_err), max(gq_rel)
))
if (!pass) quit(status = 1)
