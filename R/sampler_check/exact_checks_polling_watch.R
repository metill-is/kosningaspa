# Exact checks of Stan/polling_watch_v5_epoch.stan that need no fit: identities of its log
# density, evaluated with cmdstanr's model methods at random parameter points.
#
#   A. The walk's step covariance is C Sigma C over all P parties (C = I - 11'/P, Sigma =
#      diag(sigma) Omega diag(sigma)), whichever party is the reference: L_walk L_walk' is
#      its block over the other parties, and the reference's step (minus their sum) completes
#      it. The (P - 1)-dimensional form of the old models, Sigma's block mapped the same way,
#      gives the reference a variance of about (P - 1) sigma^2 instead.
#   B. Reference invariance: with the parties permuted (twice, each time moving the reference
#      party), the log density at the permuted point is the same. The permuted point carries
#      the same path beta, the same Omega, sigma, bias and shocks in the new party order; its
#      walk coordinates come from inverting the anchored walk (as in
#      equivalence_polling_watch.R). Compared: log_prob(jacobian = FALSE) minus
#      sum_{k >= 2} (P - k) log L_Omega[k, k], the part of lkj_corr_cholesky's density on
#      L_Omega that depends on the order (the Jacobian from Omega to its Cholesky factor);
#      everything else is invariant, the walk's coordinates included, since its density and
#      implicit Jacobians depend only on C Sigma C and on the set of parties absent from each
#      result. Two mutants must fail: the walk's covariance taken as Sigma's block (the old
#      models' form) and the polls' parties sliced by position instead of gathered by identity.
#   C. Entry flatness: before a newcomer's entry no poll speaks to its bias, and the drift
#      shared by the parties already polled is flat in the data too; a house's steps before
#      its first epoch are flat as well. So moving a shock along such a direction changes the
#      log density only through the shock's own std_normal term. The old model (old-rev) must
#      fail where it lets that drift reach the newcomer or the house, and the new model must
#      move with a shock that the data do see (positive controls).
#   D. Reductions: the new model's log density equals the old one's with both taus fixed at
#      0 (v4, the implementation check) and on the window from the 2021 result, where every
#      party and house is polled in epoch 1, so nothing restarts; on the window from 2016
#      with the taus sampled it differs.
#
# Usage, from the project root (reads data/ through read_polling_watch_data(), or saved
# Stan data):
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/exact_checks_polling_watch.R \
#     [--stan=Stan/polling_watch_v5_epoch.stan] [--old-rev=fdf7590] [--n=6] \
#     [--data-json=<2016 window>] [--data-json-2021=<2021 window>] [--out=<csv>]
# Prints EXACT PASS or EXACT FAIL; exits with status 1 on FAIL.
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
stan_new <- arg("stan", "Stan/polling_watch_v5_epoch.stan")
old_rev <- arg("old-rev", "fdf7590")
n_pts <- as.integer(arg("n", "6"))
out_csv <- arg("out")
work <- tempfile("exact_checks_")
dir.create(work)

# Data: the production window (from 2016) and the window from the 2021 result
build_data <- function(start) {
  box::use(R / data[read_polling_watch_data], R / stan_data[prepare_polling_watch_data, prepare_polling_watch_epochs])
  prepare_polling_watch_epochs(prepare_polling_watch_data(read_polling_watch_data(start = start)))$stan_data
}
read_data <- function(json, start) {
  if (is.null(json)) build_data(start) else jsonlite::read_json(json, simplifyVector = TRUE)
}
d16 <- read_data(arg("data-json"), as.Date("2016-01-01"))
d21 <- read_data(arg("data-json-2021"), as.Date("2021-09-25"))
d16_tau0 <- modifyList(d16, list(estimate_tau = 0L, tau_mu_fixed = 0, tau_house_fixed = 0))
write_data <- function(d, name) {
  f <- file.path(work, paste0(name, ".json"))
  write_stan_json(d, f)
  f
}

# Parties permuted: column j of the permuted data is party perm[j] of the original
permute_data <- function(d, perm) {
  y <- d$y_n[, perm, drop = FALSE]
  pi <- matrix(0L, nrow(y), ncol(y))
  for (i in seq_len(nrow(y))) {
    nz <- which(y[i, ] != 0)
    pi[i, seq_along(nz)] <- nz
  }
  modifyList(d, list(y_n = y, party_index_n = pi, n_parties_n = as.integer(rowSums(y != 0))))
}
perm_rev <- rev(seq_len(d16$P))
set.seed(20261105)
perm_rnd <- sample(d16$P)

# The model's transformed data, mirrored: anchors, the reference party and the other
# parties' coordinates (oth), each anchor's present coordinates and offsets, the step
# variances, each house's first epoch and each party's entry epoch
model_struct <- function(d) {
  P <- d$P
  P1 <- P - 1
  el <- which(d$house_n == 1)
  anchors <- sort(unique(d$date_n[el]))
  m <- length(anchors)
  reported <- lapply(anchors, \(a) {
    rows <- el[d$date_n[el] == a]
    sort(unique(unlist(lapply(rows, \(n) d$party_index_n[n, seq_len(d$n_parties_n[n])]))))
  })
  ref <- max(Reduce(intersect, reported))
  oth <- setdiff(seq_len(P), ref)
  w_off <- matrix(0, m, P1)
  for (n in el) {
    j <- match(d$date_n[n], anchors)
    cols <- setdiff(d$party_index_n[n, seq_len(d$n_parties_n[n])], ref)
    r <- log((d$y_n[n, cols] + 0.5) / (d$y_n[n, ref] + 0.5))
    w_off[j, match(cols, oth)] <- r - sum(r) / (length(r) + 1)
  }
  v <- c(0, d$time_diff)
  zcol <- integer(d$D)
  zcol[setdiff(seq_len(d$D), anchors)] <- seq_len(d$D - m)
  poll <- d$house_n > 1
  entry <- vapply(seq_len(P), \(p) {
    r <- poll & d$y_n[, p] > 0
    if (any(r)) min(d$epoch_n[r]) else d$E + 1
  }, numeric(1))
  house_first <- vapply(seq_len(d$H), \(h) if (h > 1) min(d$epoch_n[d$house_n == h]) else NA_real_, numeric(1))
  house_last <- vapply(seq_len(d$H), \(h) if (h > 1) max(d$epoch_n[d$house_n == h]) else NA_real_, numeric(1))
  list(
    P = P, P1 = P1, D = d$D, E = d$E, H = d$H, anchors = anchors, m = m, ref = ref, oth = oth,
    present = lapply(reported, \(r) which(oth %in% r)), w_off = w_off, v = v, cv = cumsum(v),
    zcol = zcol, entry = entry, house_first = house_first, house_last = house_last
  )
}

# The anchored walk's coordinates (anchor_w, z_walk) that build the path b = beta[, oth]'
# (P - 1 x D) with the step factor L (L_walk): the inverse of the model's transformed
# parameters, as in equivalence_polling_watch.R
invert_walk <- function(b, L, s) {
  Sig <- L %*% t(L)
  anchor_w <- matrix(0, s$m, s$P1)
  for (j in s$m:1) {
    a <- s$anchors[j]
    pr <- s$present[[j]]
    ab <- setdiff(seq_len(s$P1), pr)
    if (length(ab)) {
      if (j < s$m) {
        nx <- s$anchors[j + 1]
        L_aa <- t(chol(Sig[ab, ab, drop = FALSE]))
        anchor_w[j, ab] <- solve(L_aa, (b[ab, a] - b[ab, nx]) / sqrt(s$cv[nx] - s$cv[a]))
      } else {
        anchor_w[j, ab] <- b[ab, a]
      }
      anchor_w[j, pr] <- b[pr, a] + sum(b[ab, a]) / (length(pr) + 1) - s$w_off[j, pr]
    } else {
      anchor_w[j, ] <- b[, a] - s$w_off[j, ]
    }
  }
  z_walk <- matrix(NA_real_, s$P1, s$D - s$m)
  for (j in seq_len(s$m - 1)) {
    a <- s$anchors[j]
    nx <- s$anchors[j + 1]
    for (t in seq_len(max(0, nx - a - 1)) + a) {
      R <- s$cv[nx] - s$cv[t - 1]
      sc <- sqrt(s$v[t] * (s$cv[nx] - s$cv[t]) / R)
      z_walk[, s$zcol[t]] <- solve(L, (b[, t] - b[, t - 1] - (s$v[t] / R) * (b[, nx] - b[, t - 1])) / sc)
    }
  }
  for (t in rev(seq_len(s$anchors[1] - 1))) {
    z_walk[, s$zcol[t]] <- solve(L, (b[, t] - b[, t + 1]) / sqrt(s$v[t + 1]))
  }
  for (t in seq_len(s$D - s$anchors[s$m]) + s$anchors[s$m]) {
    z_walk[, s$zcol[t]] <- solve(L, (b[, t] - b[, t - 1]) / sqrt(s$v[t]))
  }
  stopifnot(!anyNA(z_walk))
  list(anchor_w = anchor_w, z_walk = z_walk)
}

# Models: the new file, the old one from git and two mutants of the new one
src_new <- readLines(stan_new)
src_old <- system2("git", c("show", paste0(old_rev, ":Stan/polling_watch_v5_epoch.stan")), stdout = TRUE)
mutate_src <- function(src, from, to) {
  hit <- which(trimws(src) == from)
  stopifnot("mutant anchor not found exactly once" = length(hit) == 1)
  src[hit] <- sub(from, to, src[hit], fixed = TRUE)
  src
}
src_mut_walk <- mutate_src(
  src_new, "L_walk = cholesky_decompose(tcrossprod(CM[oth, ]));",
  "L_walk = cholesky_decompose(tcrossprod(M[oth, ]));"
)
src_mut_slice <- mutate_src(
  src_new, "array[k] int cols = party_index_n[n, 1:k];",
  "array[k] int cols = linspaced_int_array(k, 1, k);"
)
compile_methods <- function(src, name) {
  # WHY: cmdstanr attaches model methods only when it compiles; a private copy in a temp
  # directory leaves the binaries beside the .stan files (and any fit using them) alone
  f <- file.path(work, paste0(name, ".stan"))
  writeLines(src, f)
  cmdstan_model(f, compile_model_methods = TRUE, force_recompile = TRUE, quiet = TRUE)
}
mods <- list(
  new = compile_methods(src_new, "v5_new"), old = compile_methods(src_old, "v5_old"),
  mut_walk = compile_methods(src_mut_walk, "v5_mut_walk"), mut_slice = compile_methods(src_mut_slice, "v5_mut_slice")
)
methods_fit <- function(mod, data_file) {
  fit <- mod$sample(
    data = data_file, chains = 1, iter_warmup = 0, iter_sampling = 1, fixed_param = TRUE,
    init = 0, seed = 1, output_dir = work, refresh = 0, show_messages = FALSE
  )
  fit$init_model_methods()
  fit
}
data_files <- list(
  d16 = write_data(d16, "d16"), rev = write_data(permute_data(d16, perm_rev), "rev"),
  rnd = write_data(permute_data(d16, perm_rnd), "rnd"), d21 = write_data(d21, "d21"),
  tau0 = write_data(d16_tau0, "tau0")
)
fits <- list()
for (mn in names(mods)) {
  for (dn in c("d16", "rev", "rnd")) fits[[paste(mn, dn)]] <- methods_fit(mods[[mn]], data_files[[dn]])
}
for (mn in c("new", "old")) {
  for (dn in c("d21", "tau0")) fits[[paste(mn, dn)]] <- methods_fit(mods[[mn]], data_files[[dn]])
}

# Random points: a realistic base (elections at their results, sigma 0.01 per sqrt(day),
# sigma_gamma 0.1, tau_mu 0.1, tau_house 0.05) plus N(0, 0.3^2) on every unconstrained
# coordinate. Points come from constrain_variables(), so every constraint (the zero sums,
# the unit rows of L_Omega) holds to rounding, and no model method throws.
# WHY: constrain_variables() returns 3-D arrays (z_house, gamma) as flat vectors in Stan's
# column-major order, the order array() fills, so they are reshaped from the data's sizes
reshape3 <- function(x, fit) {
  # WHY: [[ ]], since $ matches partially and x$gamma would return gamma_free
  d <- jsonlite::read_json(fit$data_file(), simplifyVector = TRUE)
  zh <- x[["z_house"]]
  g <- x[["gamma"]]
  if (length(zh) && is.null(dim(zh))) x[["z_house"]] <- zh <- array(zh, c(d$E - 1, d$H - 1, d$P))
  if (length(g) && is.null(dim(g))) x[["gamma"]] <- g <- array(g, c(d$E, d$H, d$P))
  # the reshape is right only if each vector over parties sums to 0 and house 1's gamma is 0
  stopifnot(
    !length(zh) || max(abs(apply(zh, c(1, 2), sum))) < 1e-10,
    !length(g) || (max(abs(apply(g, c(1, 2), sum))) < 1e-10 && all(g[, 1, ] == 0))
  )
  x
}
pars_of <- function(fit, u) reshape3(fit$constrain_variables(u, transformed_parameters = FALSE, generated_quantities = FALSE), fit)
tp_of <- function(fit, u) reshape3(fit$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE), fit)
random_points <- function(fit, n, seed) {
  p0 <- pars_of(fit, fit$unconstrain_draws(draws = fit$draws(), format = "draws_matrix")[1, ])
  p0$log_sigma[] <- log(0.01)
  p0$sigma_gamma <- 0.1
  p0$sigma_phi <- 0.5
  if (length(p0$tau_mu_est)) p0$tau_mu_est[] <- 0.1
  if (length(p0$tau_house_est)) p0$tau_house_est[] <- 0.05
  u0 <- fit$unconstrain_variables(p0)
  set.seed(seed)
  lapply(seq_len(n), \(i) u0 + rnorm(length(u0), 0, 0.3))
}
lp <- function(fit, p, jacobian = FALSE) fit$log_prob(fit$unconstrain_variables(p), jacobian = jacobian)
lkj_order <- function(L) {
  K <- nrow(L)
  sum((K - 2:K) * log(diag(L)[2:K]))
}
results <- list()
record <- function(check, model, case, value, tol, expect) {
  pass <- if (expect == "zero") abs(value) <= tol else abs(value) > tol
  results[[length(results) + 1]] <<- data.frame(check = check, model = model, case = case, value = value, tol = tol, expect = expect, pass = pass)
}
s16 <- model_struct(d16)
pts16 <- random_points(fits[["new d16"]], n_pts, 1)
cat(sprintf(
  "Window from 2016: P=%d, H=%d, E=%d, D=%d, %d anchors; reference party %d; entry epochs %s; house first epochs %s\n",
  s16$P, s16$H, s16$E, s16$D, s16$m, s16$ref, paste(s16$entry, collapse = " "), paste(s16$house_first[-1], collapse = " ")
))

# A. The walk's step covariance
for (i in seq_along(pts16)) {
  cp <- tp_of(fits[["new d16"]], pts16[[i]])
  P <- s16$P
  S <- diag(cp$sigma) %*% tcrossprod(cp$L_Omega) %*% diag(cp$sigma)
  C <- diag(P) - 1 / P
  CSC <- C %*% S %*% C
  Tm <- matrix(0, P, P - 1)
  Tm[s16$oth, ] <- diag(P - 1)
  Tm[s16$ref, ] <- -1
  full <- Tm %*% tcrossprod(cp$L_walk) %*% t(Tm)
  record("A walk covariance = C Sigma C", "new", sprintf("point %d", i), max(abs(full - CSC)) / max(abs(CSC)), 1e-12, "zero")
  # any reference: C Sigma C's block over the other parties, completed by minus their sum
  err_any <- max(vapply(seq_len(P), \(r) {
    o <- setdiff(seq_len(P), r)
    Tr <- matrix(0, P, P - 1)
    Tr[o, ] <- diag(P - 1)
    Tr[r, ] <- -1
    max(abs(Tr %*% CSC[o, o] %*% t(Tr) - CSC))
  }, numeric(1)))
  record("A any reference completes C Sigma C", "algebra", sprintf("point %d", i), err_any / max(abs(CSC)), 1e-12, "zero")
  old_form <- Tm %*% S[s16$oth, s16$oth] %*% t(Tm)
  if (i == 1) {
    cat(sprintf(
      "A: reference variance per day, C Sigma C %.3g vs the (P-1)-dimensional form %.3g; mean other-party variance %.3g\n",
      CSC[s16$ref, s16$ref], old_form[s16$ref, s16$ref], mean(diag(CSC)[s16$oth])
    ))
  }
}

# B. Reference invariance under two permutations
permute_point <- function(pA, cA, perm, fitB, sB) {
  Om <- tcrossprod(pA$L_Omega)
  pB <- pA
  pB$log_sigma <- pA$log_sigma[perm]
  pB$L_Omega <- t(chol(Om[perm, perm]))
  pB$mu_gamma0 <- pA$mu_gamma0[perm]
  pB$gamma_free <- pA$gamma_free[, perm, drop = FALSE]
  pB$z_mu <- pA$z_mu[, perm, drop = FALSE]
  pB$z_house <- pA$z_house[, , perm, drop = FALSE]
  probe <- pB
  probe$anchor_w[] <- 0
  probe$z_walk[] <- 0
  L <- tp_of(fitB, fitB$unconstrain_variables(probe))$L_walk
  w <- invert_walk(t(cA$beta[, perm][, sB$oth, drop = FALSE]), L, sB)
  pB$anchor_w <- w$anchor_w
  pB$z_walk <- w$z_walk
  pB
}
for (pn in c("rev", "rnd")) {
  perm <- if (pn == "rev") perm_rev else perm_rnd
  sB <- model_struct(permute_data(d16, perm))
  cat(sprintf("B: permutation %s = (%s); reference party %d becomes %d\n", pn, paste(perm, collapse = " "), s16$ref, perm[sB$ref]))
  stopifnot(perm[sB$ref] != s16$ref)
  for (mn in c("new", "old", "mut_walk", "mut_slice")) {
    fA <- fits[[paste(mn, "d16")]]
    fB <- fits[[paste(mn, pn)]]
    for (i in seq_along(pts16)) {
      pA <- pars_of(fA, pts16[[i]])
      cA <- tp_of(fA, pts16[[i]])
      pB <- permute_point(pA, cA, perm, fB, sB)
      cB <- tp_of(fB, fB$unconstrain_variables(pB))
      lpA <- lp(fA, pA)
      lpB <- lp(fB, pB)
      tol <- 1e-11 * max(1, abs(lpA))
      path_err <- max(abs(cB$beta - cA$beta[, perm]))
      gamma_err <- max(abs(cB[["gamma"]] - cA[["gamma"]][, , perm, drop = FALSE]))
      expect <- if (mn %in% c("new", "old")) "zero" else "nonzero"
      record(paste0("B permutation (", pn, ")"), mn, sprintf("point %d", i), (lpA - lkj_order(pA$L_Omega)) - (lpB - lkj_order(pB$L_Omega)), tol, expect)
      record(paste0("B path and bias carried (", pn, ")"), mn, sprintf("point %d", i), max(path_err, gamma_err), 1e-9, "zero")
      if (mn == "new") {
        record(paste0("B control: no LKJ order term (", pn, ")"), mn, sprintf("point %d", i), lpA - lpB, tol, "nonzero")
      }
    }
  }
}

# C. Entry flatness: shocks along directions the data cannot see before an entry
shared_dir <- function(B, P) {
  u <- ifelse(seq_len(P) %in% B, 1, -length(B) / (P - length(B)))
  u / sqrt(sum(u^2))
}
stz_dir <- function(P, seed) {
  set.seed(seed)
  u <- rnorm(P)
  u <- u - mean(u)
  u / sqrt(sum(u^2))
}
specs <- list()
for (p in which(s16$entry > 1 & s16$entry <= s16$E)) {
  e_in <- s16$entry[p]
  B <- which(s16$entry < e_in)
  u <- shared_dir(B, s16$P)
  specs[[length(specs) + 1]] <- list(
    name = sprintf("industry drift shared before party %d enters (epoch %d)", p, e_in),
    par = "z_mu", rows = seq_len(e_in - 1), k = NA, u = u, expect_old = "nonzero"
  )
  # WHY: in the old model the drift reaches the likelihood only through a house's polls at
  # or after the entry epoch; a house that stopped polling before it is flat in both models
  for (h in which(s16$house_first < e_in)) {
    specs[[length(specs) + 1]] <- list(
      name = sprintf("house %d drift shared before party %d enters", h, p),
      par = "z_house", rows = s16$house_first[h]:(e_in - 1), k = h - 1, u = u,
      expect_old = if (s16$house_last[h] >= e_in) "nonzero" else "zero"
    )
  }
}
for (h in which(s16$house_first > 1)) {
  specs[[length(specs) + 1]] <- list(
    name = sprintf("house %d steps before its first epoch (%d)", h, s16$house_first[h]),
    par = "z_house", rows = seq_len(s16$house_first[h] - 1), k = h - 1, u = stz_dir(s16$P, h), expect_old = "nonzero"
  )
}
positive <- list(
  list(name = "positive control: industry step into the last epoch", par = "z_mu", rows = s16$E - 1, k = NA, u = stz_dir(s16$P, 99)),
  list(
    name = "positive control: a late house's step after its first epoch", par = "z_house",
    rows = max(s16$house_first, na.rm = TRUE), k = which.max(s16$house_first) - 1, u = stz_dir(s16$P, 98)
  )
)
shift <- function(p, sp, c) {
  dz <- 0
  for (r in sp$rows) {
    if (sp$par == "z_mu") {
      z <- p$z_mu[r, ]
      p$z_mu[r, ] <- z + c * sp$u
      dz <- dz - 0.5 * (sum(p$z_mu[r, ]^2) - sum(z^2))
    } else {
      z <- p$z_house[r, sp$k, ]
      p$z_house[r, sp$k, ] <- z + c * sp$u
      dz <- dz - 0.5 * (sum(p$z_house[r, sp$k, ]^2) - sum(z^2))
    }
  }
  list(p = p, dprior = dz)
}
for (mn in c("new", "old")) {
  f <- fits[[paste(mn, "d16")]]
  for (i in seq_along(pts16)) {
    p <- pars_of(f, pts16[[i]])
    lp0 <- lp(f, p)
    tol <- 1e-11 * max(1, abs(lp0))
    for (sp in c(specs, positive)) {
      sh <- shift(p, sp, 0.8)
      expect <- if (startsWith(sp$name, "positive")) "nonzero" else if (mn == "new") "zero" else sp$expect_old
      record(paste("C", sp$name), mn, sprintf("point %d", i), (lp(f, sh$p) - lp0) - sh$dprior, tol, expect)
    }
  }
}

# D. Reductions to the old model
for (case in c("tau0", "d21", "d16")) {
  fn <- fits[[paste("new", case)]]
  fo <- fits[[paste("old", case)]]
  pts <- if (case == "d16") pts16 else random_points(fn, n_pts, 2)
  for (i in seq_along(pts)) {
    p <- pars_of(fn, pts[[i]])
    a <- lp(fn, p, jacobian = TRUE)
    b <- lp(fo, p, jacobian = TRUE)
    label <- c(tau0 = "D same as old with both taus fixed at 0", d21 = "D same as old on the window from 2021", d16 = "D differs from old on the window from 2016")[[case]]
    record(label, "new vs old", sprintf("point %d", i), a - b, 1e-11 * max(1, abs(a)), if (case == "d16") "nonzero" else "zero")
  }
}
s21 <- model_struct(d21)
cat(sprintf(
  "Window from 2021: E=%d; entry epochs %s; house first epochs %s\n",
  s21$E, paste(s21$entry, collapse = " "), paste(s21$house_first[-1], collapse = " ")
))
stopifnot(all(s21$entry == 1), all(s21$house_first[-1] == 1))

res <- do.call(rbind, results)
summ <- do.call(rbind, lapply(split(res, interaction(res$check, res$model, drop = TRUE, lex.order = TRUE)), \(x) {
  data.frame(
    check = x$check[1], model = x$model[1], expect = x$expect[1], n = nrow(x),
    max_abs = signif(max(abs(x$value)), 3), min_abs = signif(min(abs(x$value)), 3), pass = all(x$pass)
  )
}))
rownames(summ) <- NULL
summ <- summ[order(summ$check, summ$model), ]
print(summ, right = FALSE, row.names = FALSE)
if (!is.null(out_csv)) write.csv(res, out_csv, row.names = FALSE)
pass <- all(res$pass)
cat(sprintf("EXACT %s: %d of %d evaluations as expected\n", if (pass) "PASS" else "FAIL", sum(res$pass), nrow(res)))
if (!pass) quit(status = 1)
