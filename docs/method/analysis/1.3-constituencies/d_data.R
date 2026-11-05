# Chapter 1.3: the kjördæmi data the production fit reads, from the Stan data the fit
# script builds (fit_headless.R --data-only builds it and quits; no model is fitted):
# each kjördæmi observation's date, house, size and lists, the weights that close the
# K-th effect, the prediction weights and sizes, and the data-only information proxy
# info_delta with the partial-centring weight it gives at a few values of sigma_delta.
# Reads the live, git-ignored data/. Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.3-constituencies/d_data.R
options(width = 160)
box::use(
  jsonlite[read_json],
  R / data[read_polling_data, read_constituency_data]
)

out <- tempfile("kd_data_")
status <- system2("Rscript", c("R/sampler_check/fit_headless.R", paste0("--out=", out), "--data-only"),
                  stdout = FALSE)
stopifnot(status == 0)
sd <- read_json(file.path(out, "stan_data.json"), simplifyVector = TRUE)

pd <- read_polling_data()
parties <- levels(pd$flokkur)
houses <- levels(pd$fyrirtaeki)
# The fit script's kjördæmi levels: as_factor() on the filtered rows, in order of first
# appearance in the CSV.
cd <- read_constituency_data() |>
  dplyr::filter(
    date >= as.Date("2021-09-25"), date != as.Date("2024-11-15"),
    (fyrirtaeki != "Gallup") | (date <= as.Date("2024-11-10"))
  ) |>
  tidyr::drop_na()
kd <- levels(forcats::as_factor(cd$kjordaemi))
stopifnot(length(parties) == sd$P, length(kd) == sd$K, parties[1] == "Annað")

W <- sd$constituency_weights
obs_dates <- sort(unique(cd$date))
stopifnot(nrow(W) == sd$N_obs_k, length(obs_dates) == sd$N_obs_k)
cat("K =", sd$K, " N_k =", sd$N_k, " N_obs_k =", sd$N_obs_k, " P =", sd$P, "\n")
cat("kjördæmi levels (constituency_k indexes them; the K-th is closed by the weights):\n  ",
    paste(seq_along(kd), kd, sep = " ", collapse = " | "), "\n\n")

y_k <- sd$y_k
cat("Kjördæmi observations (obs_k), with the national row of the same house and date, if any:\n")
for (o in seq_len(sd$N_obs_k)) {
  rows <- which(sd$obs_k == o)
  h <- houses[unique(sd$house_k[rows])]
  n_tot <- sum(y_k[rows, ])
  missing <- sapply(rows, function(r) paste(parties[y_k[r, ] == 0], collapse = ","))
  nat <- pd[pd$date == obs_dates[o] & pd$fyrirtaeki == h, ]
  cat(sprintf("  %d  %s  %-8s rows %d  n %7d  per kjördæmi: %s\n", o, format(obs_dates[o]), h,
              length(rows), n_tot, paste(rowSums(y_k[rows, ]), collapse = " ")))
  hit <- missing != ""
  cat(sprintf("       lists with 0 in a row: %s\n",
              if (!any(hit)) "none" else paste(kd[sd$constituency_k[rows]][hit], missing[hit], sep = ": ", collapse = "; ")))
  cat(sprintf("       national row, same house and date: %s\n",
              if (nrow(nat)) sprintf("n %d", round(sum(nat$n))) else "none"))
}

all_obs <- read_constituency_data() |> dplyr::distinct(date, fyrirtaeki) |> dplyr::arrange(date)
kept <- paste(all_obs$date, all_obs$fyrirtaeki) %in% paste(cd$date, cd$fyrirtaeki)
cat("\nEvery observation in data/constituency_data.csv; the fit keeps those dated from 2021-09-25,\n",
    "except 2024-11-15, and no Gallup row after 2024-11-10:\n")
print(data.frame(date = format(all_obs$date), house = all_obs$fyrirtaeki, kept = kept), row.names = FALSE)

cat("\nconstituency_weights (each observation's share of its n by kjördæmi):\n")
Wd <- as.data.frame(round(W, 4)); names(Wd) <- kd; rownames(Wd) <- format(obs_dates)
print(Wd)
cat("\nconstituency_weights_pred (row 1, the", format(obs_dates[1]), "observation):",
    paste(sprintf("%.4f", sd$constituency_weights_pred), collapse = " "), "\n")
cat("n_pred_k:", paste(kd, sd$n_pred_k, sep = " ", collapse = " | "), " total", sum(sd$n_pred_k), "\n")
pw <- sd$n_pred_k / sum(sd$n_pred_k)
cat("n_pred_k shares:", paste(sprintf("%.4f", pw), collapse = " "), "\n\n")

cat("The K-th effect is -sum_k (w_k / w_K) delta_k; the ratios w_k / w_K per observation and for the prediction:\n")
R <- W[, 1:(sd$K - 1)] / W[, sd$K]
cat("range of w_k / w_K per observation, to 2 dp from full precision:\n")
print(data.frame(obs = format(obs_dates), min = round(apply(R, 1, min), 2), max = round(apply(R, 1, max), 2)), row.names = FALSE)
Rd <- as.data.frame(round(rbind(R, pred = sd$constituency_weights_pred[1:(sd$K - 1)] / sd$constituency_weights_pred[sd$K]), 3))
names(Rd) <- kd[1:(sd$K - 1)]; rownames(Rd) <- c(format(obs_dates), "pred")
print(Rd)

# info_delta as Stan's transformed data builds it
info <- matrix(0, sd$P - 1, sd$K - 1, dimnames = list(parties[-1], kd[-sd$K]))
for (n in seq_len(sd$N_k)) {
  ns <- sum(y_k[n, ])
  for (j in seq_len(sd$n_parties_k[n])) {
    c <- sd$party_index_k[n, j]
    if (c > 1) {
      s <- y_k[n, c] / ns
      i <- 0.5 * ns * s * (1 - s)
      if (sd$constituency_k[n] < sd$K) {
        info[c - 1, sd$constituency_k[n]] <- info[c - 1, sd$constituency_k[n]] + i
      } else {
        info[c - 1, ] <- info[c - 1, ] + (W[sd$obs_k[n], 1:(sd$K - 1)] / W[sd$obs_k[n], sd$K])^2 * i
      }
    }
  }
}
cat("\ninfo_delta (data-only information proxy per party and free kjördæmi):\n")
print(round(info))
cat("cells with info_delta = 0:", sum(info == 0), "of", length(info), "\n")
lam <- function(s) s^2 * info / (1 + s^2 * info)
for (s in c(0.05, 0.1, 0.2, 0.4)) {
  l <- lam(s)
  cat(sprintf("sigma_delta = %.2f: lambda_delta min %.3f  median %.3f  max %.3f\n", s, min(l), median(l), max(l)))
}

info1 <- info * 0
for (n in which(sd$obs_k == 1)) {
  ns <- sum(y_k[n, ])
  for (j in seq_len(sd$n_parties_k[n])) {
    c <- sd$party_index_k[n, j]
    if (c > 1) {
      s <- y_k[n, c] / ns
      i <- 0.5 * ns * s * (1 - s)
      if (sd$constituency_k[n] < sd$K) info1[c - 1, sd$constituency_k[n]] <- info1[c - 1, sd$constituency_k[n]] + i
      else info1[c - 1, ] <- info1[c - 1, ] + (W[1, 1:(sd$K - 1)] / W[1, sd$K])^2 * i
    }
  }
}
cat(sprintf("\nshare of info_delta from the 2021 result's rows: min %.3f  median %.3f  max %.3f\n",
            min(info1 / info), median(info1 / info), max(info1 / info)))

# The 2021 result: national clr against the weighted mean of the kjördæmi clrs. The model's
# kjördæmi rows put beta + delta_k at clr(kjördæmi k) and, with sum_k w_k delta_k = 0, put
# beta at sum_k w_k clr(kjördæmi k); the national row puts beta at clr(national).
clr <- function(v) log(v) - mean(log(v))
nat_row <- which(sd$house_n == 1 & sd$date_n == sd$date_k[which(sd$obs_k == 1)[1]])
stopifnot(length(nat_row) == 1)
v_nat <- sd$y_n[nat_row, ]
rows1 <- which(sd$obs_k == 1)
clr_k <- t(sapply(rows1, function(r) clr(y_k[r, ])))
wmean <- colSums(W[1, sd$constituency_k[rows1]] * clr_k)
gap <- wmean - clr(v_nat)
sh_nat <- v_nat / sum(v_nat)
sh_w <- exp(wmean) / sum(exp(wmean))
cat("\n2021: weighted mean of the kjördæmi clrs minus the national clr, and the shares each implies (pp):\n")
print(data.frame(list = parties, clr_gap = round(gap, 4), national = round(100 * sh_nat, 2),
                 from_kjordaemi = round(100 * sh_w, 2), diff_pp = round(100 * (sh_w - sh_nat), 2)), row.names = FALSE)

# The 2021 result's own kjördæmi deviations on the log-ratio scale, defined so that they
# satisfy the 2021 weights' constraint: dhat_k = clr(kjördæmi k) - sum_j w_j clr(kjördæmi j).
dhat <- sweep(clr_k, 2, wmean)
dimnames(dhat) <- list(kd[sd$constituency_k[rows1]], parties)
dhat <- dhat[kd, ]
cat("\n2021 deviations dhat (clr scale; columns sum to 0 with the 2021 weights, rows sum to 0):\n")
print(round(t(dhat), 3))
cat("Figure 2's cells: dhat rounded once, from full precision, to 2 dp:\n")
print(round(t(dhat), 2))
cat("sd of dhat across the six kjördæmi, by list:\n")
print(round(apply(dhat, 2, sd), 3))
w1 <- W[1, ]
cat("check: max |sum_k w_k dhat_k| =", signif(max(abs(colSums(w1 * dhat))), 3), "\n")

# What each observation's weights make of the closed (K-th) effect, holding the free effects at dhat
cat("\nSuður's effect implied by each observation's weights, with the five free effects held at dhat:\n")
imp <- t(sapply(seq_len(sd$N_obs_k), function(o) {
  -colSums((W[o, 1:(sd$K - 1)] / W[o, sd$K]) * dhat[1:(sd$K - 1), , drop = FALSE])
}))
dimnames(imp) <- list(format(obs_dates), parties)
print(round(rbind(imp, dhat_2021 = dhat[sd$K, ]), 3))
cat("largest |implied - dhat| per observation:", paste(sprintf("%.3f", apply(abs(sweep(imp, 2, dhat[sd$K, ])), 1, max)), collapse = " "), "\n")

# The closed effect's implied prior: with the free effects independent N(0, sigma), the
# K-th is normal with sd sigma * sqrt(sum_k (w_k / w_K)^2).
mult <- sqrt(rowSums(R^2))
mult_pred <- sqrt(sum((sd$constituency_weights_pred[1:(sd$K - 1)] / sd$constituency_weights_pred[sd$K])^2))
cat("\nSuður's implied prior sd, as a multiple of sigma_delta, per observation:",
    paste(format(obs_dates), sprintf("%.2f", mult), collapse = " | "), "| prediction", sprintf("%.2f", mult_pred), "\n")
cat("largest weight on one free kjördæmi from a Suður row, (w_k / w_K)^2:", sprintf("%.2f", max(R^2)), "\n")

# The forecast's two readings of one beta: the national draw's shares softmax(beta) and
# the n_pred_k-weighted aggregate of the kjördæmi draws' shares softmax(beta + delta_k),
# with delta at the 2021 deviations dhat, for beta at the national clr and at the
# weighted mean of the kjördæmi clrs.
smx <- function(e) exp(e) / sum(exp(e))
agg <- function(beta, w) colSums(w * t(sapply(seq_len(sd$K), function(k) smx(beta + dhat[k, ]))))
for (b in list(list("the 2021 national clr", clr(v_nat)), list("the weighted mean of the kjördæmi clrs", wmean))) {
  for (wt in list(list("n_pred_k shares", pw), list("2021 weights", as.numeric(w1)))) {
    d <- 100 * (agg(b[[2]], wt[[2]]) - smx(b[[2]]))
    cat(sprintf("beta at %s, aggregated with %s: kjördæmi minus national (pp): %s\n", b[[1]], wt[[1]],
                paste(parties, sprintf("%+.2f", d), collapse = ", ")))
  }
}
