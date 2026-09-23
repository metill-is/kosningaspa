# Covariance and precision of the polling-watch random-walk innovations, on the
# IDENTIFIED scales. polling_watch_v4 centres each innovation (innov -= mean(innov)),
# so the likelihood only sees Sz = diag(sigma) Omega diag(sigma) through C Sz C,
# C = I - 11'/P; the saved Omega (make_correlation_plots.R) is the pre-centring,
# partly prior-driven object. Here we plot, per 30 days:
#   * clr scale  : Cov(d beta) = C Sz C           (beta = clr(pi), zero-sum log-ratios)
#   * share scale: Cov(d pi)   = J Sz J',  J = diag(pi) - pi pi'  (delta method at the
#                  latest date; J 1 = 0 absorbs the centring), in percentage points.
# Both have rank P - 1 (null direction 1), so "precision" is the Moore-Penrose
# pseudo-inverse, i.e. the precision within the zero-sum subspace (basis-free).
# All matrices are computed per draw and then averaged; a cell label is bold when
# its 90% posterior interval excludes zero.
#
# Run from the repo root:  Rscript R/make_rw_innovation_cov_plots.R

library(tidyverse)
library(metill)
library(patchwork)
library(here)
library(arrow)
library(cmdstanr)
library(posterior)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill(type = "blog"))

box::use(
  R / party_utils[party_tibble]
)

days_per_step <- 30 # report innovation (co)variance per 30 days

# --- latest fit directory (today, else most recent with a saved fit) ---
fit_dir <- here("data", as.character(Sys.Date()))
if (!file.exists(here(fit_dir, "polling_watch_fit.rds"))) {
  available <- list.dirs(here("data"), recursive = FALSE, full.names = FALSE)
  available <- sort(available[str_detect(available, "^\\d{4}-\\d{2}-\\d{2}$")], decreasing = TRUE)
  fit_dir <- here("data", available[map_lgl(available, \(d) file.exists(here("data", d, "polling_watch_fit.rds")))][1])
}
cat("Reading", fit_dir, "\n")

fit <- readRDS(here(fit_dir, "polling_watch_fit.rds"))
P <- fit$metadata()$stan_variable_sizes$sigma
D <- fit$metadata()$stan_variable_sizes$pi_smooth[1]

# --- party index -> name. Omega draws are written column-major (Omega[1,1], Omega[2,1],
# ...), so the first P rows of a draw give party_names[1:P]; verified below against the
# labelled pi draws, so a wrong mapping fails loudly instead of mislabelling the plot.
om1 <- read_parquet(here(fit_dir, "polling_watch_omega.parquet")) |> filter(.draw == 1)
party_names <- om1$flokkur_i[1:P]

dr <- fit$draws(c("sigma", "Omega", sprintf("pi_smooth[%d,%d]", D, 1:P)), format = "draws_matrix")
pi_last <- dr[, sprintf("pi_smooth[%d,%d]", D, 1:P), drop = FALSE]

pi_lab <- read_parquet(here(fit_dir, "polling_watch_draws.parquet")) |>
  filter(.draw == 1, dags == max(dags))
stopifnot(
  "party index mapping disagrees with the labelled pi draws" =
    isTRUE(all.equal(unname(as.numeric(pi_last[1, ])), pi_lab$value[match(party_names, pi_lab$flokkur)], tolerance = 1e-6))
)
latest_date <- max(read_parquet(here(fit_dir, "polling_watch_draws.parquet"), col_select = "dags")$dags)

codes <- party_tibble() |>
  select(flokkur, bokstafur) |>
  deframe()
labs <- ifelse(is.na(codes[party_names]), party_names, codes[party_names]) |> unname()

# --- per-draw identified matrices ---
Cm <- diag(P) - matrix(1 / P, P, P)
pinv_rank <- function(S, rank) {
  e <- eigen((S + t(S)) / 2, symmetric = TRUE)
  V <- e$vectors[, seq_len(rank), drop = FALSE]
  list(pinv = V %*% diag(1 / e$values[seq_len(rank)]) %*% t(V), gap = e$values[rank] / abs(e$values[rank + 1]))
}
partial <- function(Pm) {
  d <- sqrt(diag(Pm))
  out <- -Pm / (d %o% d)
  diag(out) <- 1
  out
}

S_draws <- nrow(dr)
arr <- function() array(NA_real_, c(S_draws, P, P))
cov_clr <- arr()
cor_clr <- arr()
prec_clr <- arr()
pcor_clr <- arr()
cov_sh <- arr()
cor_sh <- arr()
prec_sh <- arr()
pcor_sh <- arr()
gaps <- matrix(NA_real_, S_draws, 2)

for (s in seq_len(S_draws)) {
  sig <- as.numeric(dr[s, sprintf("sigma[%d]", 1:P)])
  Om <- matrix(as.numeric(dr[s, sprintf("Omega[%d,%d]", rep(1:P, P), rep(1:P, each = P))]), P, P)
  Sz <- diag(sig) %*% Om %*% diag(sig) * days_per_step

  Sc <- Cm %*% Sz %*% Cm # clr, log-ratio units
  p <- as.numeric(pi_last[s, ])
  J <- diag(p) - p %o% p
  Ss <- J %*% Sz %*% t(J) * 1e4 # shares, pp^2

  pc <- pinv_rank(Sc, P - 1)
  ps <- pinv_rank(Ss, P - 1)
  gaps[s, ] <- c(pc$gap, ps$gap)

  cov_clr[s, , ] <- Sc
  cor_clr[s, , ] <- cov2cor(Sc)
  prec_clr[s, , ] <- pc$pinv
  pcor_clr[s, , ] <- partial(pc$pinv)
  cov_sh[s, , ] <- Ss
  cor_sh[s, , ] <- cov2cor(Ss)
  prec_sh[s, , ] <- ps$pinv
  pcor_sh[s, , ] <- partial(ps$pinv)
}
cat(sprintf(
  "Null-space separation (smallest kept / dropped eigenvalue), min over draws: clr %.1e, share %.1e\n",
  min(gaps[, 1]), min(gaps[, 2])
))
stopifnot("rank P-1 pseudo-inverse is ill-separated" = min(gaps) > 1e6)

summarise_arr <- function(A) {
  list(
    mean = apply(A, c(2, 3), mean),
    lo = apply(A, c(2, 3), quantile, 0.05),
    hi = apply(A, c(2, 3), quantile, 0.95)
  )
}

# --- plotting ---
heat <- function(A, title, subtitle, fmt, diag_mode = c("value", "sd", "blank"), limits = NULL,
                 sd_of = function(v) sqrt(v)) {
  diag_mode <- match.arg(diag_mode)
  sm <- summarise_arr(A)
  d <- expand_grid(i = 1:P, j = 1:P) |>
    mutate(
      v = sm$mean[cbind(i, j)],
      sig = sm$lo[cbind(i, j)] > 0 | sm$hi[cbind(i, j)] < 0,
      on_diag = i == j,
      x = i, y = P - j + 1
    )
  offd <- d |> filter(!on_diag)
  lim <- limits %||% c(-1, 1) * max(abs(offd$v))
  d <- d |>
    mutate(
      fill = if_else(on_diag, NA_real_, v),
      label = case_when(
        on_diag & diag_mode == "blank" ~ "",
        on_diag & diag_mode == "sd" ~ sprintf(fmt, sd_of(pmax(v, 0))),
        TRUE ~ sprintf(fmt, v)
      ),
      label = str_replace(label, "\\.", ",")
    )
  ggplot(d, aes(x, y)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = 0.4) +
    geom_text(
      aes(label = label, fontface = if_else(sig & !on_diag, "bold", "plain"), colour = on_diag | !sig),
      size = 2.9
    ) +
    scale_colour_manual(values = c(`FALSE` = "grey5", `TRUE` = "grey45"), guide = "none") +
    scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
      limits = lim, oob = scales::squish, na.value = "grey90"
    ) +
    scale_x_continuous(breaks = 1:P, labels = labs, expand = c(0, 0), position = "top") +
    scale_y_continuous(breaks = 1:P, labels = rev(labs), expand = c(0, 0)) +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = NULL) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 13),
      plot.subtitle = element_text(size = 9.5),
      legend.key.height = unit(1.2, "cm")
    )
}

per <- glue::glue("per {days_per_step} days")
share_at <- glue::glue("at the latest date ({format(latest_date, '%d. %B %Y')})")
caption <- str_c(
  "polling_watch_v4, ", basename(fit_dir), " fit, ", S_draws, " draws. Posterior means of per-draw matrices; ",
  "bold = 90% interval excludes 0; grey diagonal."
)

cov_fig <- (
  heat(cov_clr * 1e3, "Covariance, log-ratio (clr) scale", glue::glue("C·Sz·C × 10³ {per}; diagonal = SD × 100 (≈ % relative change)"), "%.2f", "sd",
    sd_of = function(v) 100 * sqrt(v / 1e3)
  ) +
    heat(cor_clr, "Correlation, log-ratio (clr) scale", "cov2cor(C·Sz·C). Independence (Ω = I) baseline ≈ −1/(P−1) = −0.11, not 0", "%.2f", "blank", limits = c(-1, 1))
) / (
  heat(cov_sh, "Covariance, share scale (pp²)", glue::glue("J·Sz·J' {per}, {share_at}; diagonal = SD in pp"), "%.2f", "sd") +
    heat(cor_sh, "Correlation, share scale", "cov2cor(J·Sz·J')", "%.2f", "blank", limits = c(-1, 1))
) +
  plot_annotation(
    title = "Random-walk innovations: covariance on the identified scales",
    subtitle = "Blue = move together, red = move in opposite directions. Both matrices have rank P − 1 (shares and log-ratios sum to a constant).",
    caption = caption
  )

prec_fig <- (
  heat(prec_clr / 1e3, "Precision, log-ratio (clr) scale", glue::glue("pinv(C·Sz·C) / 10³ {per}"), "%.2f", "value") +
    heat(pcor_clr, "Partial correlation, log-ratio (clr) scale", "−Θ[i,j] / sqrt(Θ[i,i] Θ[j,j]),  Θ = pinv(C·Sz·C) = the log-ratio partial correlation for any reference outside the pair.\nIndependence (Ω = I) baseline ≈ +1/(P−1) = +0.11, not 0: read cells against +0.11", "%.2f", "blank", limits = c(-1, 1))
) / (
  heat(prec_sh, "Precision, share scale (1/pp²): closure-dominated", glue::glue("pinv(J·Sz·J') {per}, {share_at}. Rows sum to 0, so Annað's near-zero\nshare variance (SD ≈ 0.03 pp) forces a huge diagonal that every other entry must offset"), "%.1f", "value") +
    heat(pcor_sh, "Partial correlation, share scale: closure-dominated", "Given the other shares, a pair's sum is almost pinned (they sum to 1 and Annað barely\nmoves), so every pair ≈ −0.9. Read co-movement from the clr row above", "%.2f", "blank", limits = c(-1, 1))
) +
  plot_annotation(
    title = "Random-walk innovations: precision (conditional dependence) on the identified scales",
    subtitle = "Moore–Penrose pseudo-inverse = precision within the zero-sum subspace. Off-diagonal partial correlation: co-movement of two parties given all the others.",
    caption = caption
  )

ggsave(here("Figures", "rw_innovation_covariance.png"), cov_fig, width = 15, height = 13.5, dpi = 144, bg = "white", device = ragg::agg_png)
ggsave(here("Figures", "rw_innovation_precision.png"), prec_fig, width = 15, height = 13.5, dpi = 144, bg = "white", device = ragg::agg_png)
cat("Wrote Figures/rw_innovation_covariance.png and Figures/rw_innovation_precision.png\n")
