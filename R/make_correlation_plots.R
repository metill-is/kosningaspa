# Latent random-walk correlation + precision (partial-correlation) plot for the
# polling-watch model. Reads the party-space innovation correlation Omega saved by
# fit_polling_watch.R (data/<date>/polling_watch_omega.parquet), computes posterior-mean
# marginal correlations and the precision-based partial (conditional) correlations,
# hierarchically clusters parties by co-movement, and renders a clustered heatmap
# (marginal | partial) with a dendrogram. See Stan/polling_watch_variants.md.
#
# Run from the repo root:  Rscript R/make_correlation_plots.R

library(tidyverse)
library(metill)
library(patchwork)
library(ggdendro)
library(here)
library(arrow)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill(type = "blog"))

box::use(
  R / party_utils[party_tibble]
)

# --- locate the latest polling_watch_omega.parquet (today, else most recent) ---
omega_path <- here("data", as.character(Sys.Date()), "polling_watch_omega.parquet")
if (!file.exists(omega_path)) {
  available <- list.dirs(here("data"), recursive = FALSE, full.names = FALSE)
  available <- sort(available[str_detect(available, "^\\d{4}-\\d{2}-\\d{2}$")], decreasing = TRUE)
  for (d in available) {
    p <- here("data", d, "polling_watch_omega.parquet")
    if (file.exists(p)) {
      omega_path <- p
      break
    }
  }
}
stopifnot("no polling_watch_omega.parquet found — run fit_polling_watch.R first" = file.exists(omega_path))
cat("Reading", omega_path, "\n")

om <- read_parquet(omega_path)

# party full name -> letter code (bokstafur); fall back to the name if unmapped
codes <- party_tibble() |>
  select(flokkur, bokstafur) |>
  deframe()
to_code <- function(x) ifelse(is.na(codes[x]), x, codes[x])

parties <- union(om$flokkur_i, om$flokkur_j)
P <- length(parties)
idx <- setNames(seq_along(parties), parties)
om <- om |> mutate(i = idx[flokkur_i], j = idx[flokkur_j])

# Posterior-mean marginal correlation + posterior-mean partial (conditional) correlation.
# Partial correlations are computed per draw from the precision matrix (the inverse of the
# correlation) and then averaged — the statistically correct order (inversion is non-linear).
Rsum <- matrix(0, P, P)
Psum <- matrix(0, P, P)
ok <- 0L
for (dr in split(om, om$.draw)) {
  R <- matrix(0, P, P)
  R[cbind(dr$i, dr$j)] <- dr$value
  R <- (R + t(R)) / 2
  diag(R) <- 1
  Theta <- tryCatch(solve(R), error = function(e) NULL)
  if (is.null(Theta)) next
  d <- sqrt(diag(Theta))
  Pc <- -Theta / (d %o% d)
  diag(Pc) <- 1
  Rsum <- Rsum + R
  Psum <- Psum + Pc
  ok <- ok + 1L
}
Rbar <- Rsum / ok
Pbar <- Psum / ok
labs <- to_code(parties)
dimnames(Rbar) <- dimnames(Pbar) <- list(labs, labs)

# --- cluster parties by co-movement (distance = 1 - marginal correlation) ---
hc <- hclust(as.dist(1 - Rbar), method = "ward.D2")
ordp <- labs[hc$order]
K <- 4
cl <- cutree(hc, k = K)[hc$order]
runs <- rle(cl)
ends <- cumsum(runs$lengths)
starts <- ends - runs$lengths + 1
rects <- tibble(s = starts, e = ends) |>
  mutate(
    xmin = s - 0.5, xmax = e + 0.5,
    ymin = P - e + 1 - 0.5, ymax = P - s + 1 + 0.5
  )

to_long <- function(M, what) {
  as.data.frame(M) |>
    rownames_to_column("a") |>
    pivot_longer(-a, names_to = "b", values_to = "v") |>
    mutate(x = match(a, ordp), y = P - match(b, ordp) + 1, what = what)
}
dat <- bind_rows(
  to_long(Rbar, "Marginal correlation"),
  to_long(Pbar, "Partial (conditional) correlation")
) |>
  mutate(what = factor(what, c("Marginal correlation", "Partial (conditional) correlation")))

heat <- ggplot(dat, aes(x, y, fill = v)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", v)), size = 2.9) +
  geom_rect(
    data = rects, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA, colour = "grey20", linewidth = 0.8
  ) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-1, 1)) +
  scale_x_continuous(breaks = 1:P, labels = ordp, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:P, labels = rev(ordp), expand = c(0, 0)) +
  facet_wrap(~what) +
  coord_equal() +
  labs(
    title = "Latent random-walk co-movement of party support",
    subtitle = "Reference-invariant party-space Omega. Blue = move together, red = trade voters.",
    x = NULL, y = NULL, fill = "corr"
  ) +
  theme(panel.grid = element_blank())

dd <- dendro_data(as.dendrogram(hc))
dendro <- ggplot(segment(dd)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5) +
  scale_x_continuous(breaks = seq_along(ordp), labels = ordp, expand = c(0, 0.5)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())

fig <- dendro / heat + plot_layout(heights = c(1, 4))

out <- here("Figures", "latent_rw_correlation_clustered.png")
ggsave(out, fig, width = 11.5, height = 7, dpi = 144, bg = "white", device = ragg::agg_png)
cat("Wrote", out, "(n_draws =", ok, ")\n")
