# Evaluate the epoch-bias experiments written by R/fit_polling_watch_epoch.R.
#
#   1. Diagnostics of every fit under results/epoch_bias/ (the DIAG lines).
#   2. Leave-future-out backtests: tags "<model>_bt<year>" predict the <year>
#      election from pi_smooth on the last poll date before it. Scored per party
#      (error in pp, 90% interval coverage, CRPS) and summed.
#   3. Full-window fits ("<model>_full"): industry bias per epoch (v5) against
#      the constant bias (v4), the shock SDs, and the current smoothed support.
#
# Run from the project root: Rscript R/epoch_bias_evaluate.R [tag_prefix]
# Writes results/epoch_bias/eval_*.csv and Figures/epoch_bias_*.png.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160)

suppressMessages({
  library(tidyverse)
  library(here)
  library(arrow)
  library(posterior)
  library(metill)
})
theme_set(theme_metill())

box::use(R / data[read_polling_data])

res_dir <- here("results", "epoch_bias")
prefix <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(prefix)) prefix <- ""

short <- c(
  "Sjálfstæðisflokkurinn" = "D", "Framsóknarflokkurinn" = "B",
  "Samfylkingin" = "S", "Vinstri Græn" = "V", "Píratar" = "P",
  "Viðreisn" = "C", "Flokkur Fólksins" = "F", "Miðflokkurinn" = "M",
  "Sósíalistaflokkurinn" = "J", "Annað" = "Other"
)

tags <- list.dirs(res_dir, full.names = FALSE, recursive = FALSE) |>
  keep(\(t) file.exists(file.path(res_dir, t, "diag.txt"))) |>
  keep(\(t) startsWith(t, prefix))

# ---- 1. diagnostics -------------------------------------------------------------
diag <- map(tags, \(t) {
  line <- readLines(file.path(res_dir, t, "diag.txt"))
  kv <- str_match_all(line, "(\\w+)=(\\S+)")[[1]]
  as_tibble(setNames(as.list(kv[, 3]), kv[, 2]))
}) |>
  list_rbind() |>
  type_convert(col_types = cols())
cat("\n== Diagnostics ==\n")
print(diag, n = Inf, width = Inf)
write_csv(diag, here(res_dir, "eval_diagnostics.csv"))

# ---- 2. backtests ---------------------------------------------------------------
pre <- suppressMessages(read_polling_data()) |>
  mutate(across(c(fyrirtaeki, flokkur), as.character))
post <- read_csv(here("data", "post_election_polls.csv"), show_col_types = FALSE)
results <- bind_rows(pre, post) |>
  filter(fyrirtaeki == "Kosning") |>
  mutate(year = as.integer(format(date, "%Y"))) |>
  select(year, flokkur, n)

crps_draws <- function(x, y) {
  # CRPS = E|X - y| - E|X - X'| / 2, the second term via sorted draws.
  x <- sort(x)
  m <- length(x)
  mean(abs(x - y)) - sum((2 * seq_len(m) - m - 1) * x) / m^2
}

bt_tags <- tags[str_detect(tags, "_bt\\d{4}$")]
backtest <- map(bt_tags, \(t) {
  year <- as.integer(str_extract(t, "\\d{4}$"))
  draws <- read_parquet(file.path(res_dir, t, "pi_last.parquet"))
  truth <- results |>
    filter(year == !!year, flokkur %in% unique(draws$flokkur)) |>
    mutate(actual = n / sum(n)) |>
    select(flokkur, actual)
  draws |>
    inner_join(truth, by = "flokkur") |>
    summarise(
      mean = mean(value),
      q05 = quantile(value, 0.05),
      q95 = quantile(value, 0.95),
      crps = crps_draws(value, first(actual)),
      actual = first(actual),
      .by = flokkur
    ) |>
    mutate(
      tag = t,
      model = sub("_bt\\d{4}$", "", t),
      year = year,
      err_pp = 100 * (mean - actual),
      covered = actual >= q05 & actual <= q95,
      width_pp = 100 * (q95 - q05)
    )
}) |>
  list_rbind()

if (nrow(backtest)) {
  write_csv(backtest, here(res_dir, "eval_backtest_parties.csv"))
  bt_summary <- backtest |>
    summarise(
      mae_pp = mean(abs(err_pp)),
      rmse_pp = sqrt(mean(err_pp^2)),
      crps_pp = 100 * sum(crps),
      coverage90 = mean(covered),
      mean_width_pp = mean(width_pp),
      err_B = err_pp[flokkur == "Framsóknarflokkurinn"],
      err_M = if (any(flokkur == "Miðflokkurinn")) err_pp[flokkur == "Miðflokkurinn"] else NA_real_,
      .by = c(year, model)
    ) |>
    arrange(year, model)
  cat("\n== Backtests: predicting each election from the polls before it ==\n")
  print(bt_summary, n = Inf)
  write_csv(bt_summary, here(res_dir, "eval_backtest_summary.csv"))

  model_names <- c(
    v4 = "Constant bias, 2016+ (v4)", v5 = "Epoch random walk, 2016+ (v5)",
    v4w1 = "v4 from previous election", v4w2 = "v4 from election before that"
  )
  pbt <- backtest |>
    mutate(
      party = factor(short[flokkur], levels = short),
      model = factor(coalesce(model_names[model], model), levels = unique(c(model_names, model)))
    ) |>
    ggplot(aes(err_pp, party, colour = model)) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    geom_point(position = position_dodge(width = 0.7), size = 2) +
    facet_wrap(~year, nrow = 1) +
    labs(
      x = "Estimate minus result (percentage points)", y = NULL, colour = NULL,
      title = "Backtests: support on the eve of each election, estimated from the polls before it"
    ) +
    theme(legend.position = "top")
  ggsave(here("Figures", "epoch_bias_backtests.png"), pbt, width = 12, height = 5.5, dpi = 144)
  cat("\nWrote Figures/epoch_bias_backtests.png\n")

  cat("\n== Backtest errors per party (pp, posterior mean minus result) ==\n")
  backtest |>
    mutate(flokkur = short[flokkur]) |>
    select(year, model, flokkur, err_pp) |>
    mutate(err_pp = round(err_pp, 2)) |>
    pivot_wider(names_from = flokkur, values_from = err_pp) |>
    arrange(year, model) |>
    print(n = Inf, width = Inf)
}

# ---- 3. full-window fits -------------------------------------------------------------
# The quantity reported per epoch is the bias of the pollsters ACTIVE in that
# epoch, averaged over them: mean_h gamma[e, h, p]. Not mu_gamma alone: with
# only two houses polling after 2024, how a shift shared by every pollster is
# split between the industry shock and the house shocks is set by the prior, so
# mu_gamma[e] can understate it. The house average is what the polls actually
# carried. It is centred over the parties polled in that epoch (only contrasts
# among them are identified); the M - B contrast needs no centring.
read_fit_meta <- function(t) readRDS(file.path(res_dir, t, "meta.rds"))
model_label <- function(t) sub("_(full|bt\\d{4}).*$", "", t)

active_bias <- function(t, fit = readRDS(file.path(res_dir, t, "fit.rds"))) {
  meta <- read_fit_meta(t)
  sd <- meta$stan_data
  g <- fit$draws("gamma", format = "draws_matrix")
  ix <- str_match(colnames(g), "^gamma\\[(\\d+),(\\d+)(?:,(\\d+))?\\]")
  v5 <- !all(is.na(ix[, 4]))
  e_col <- if (v5) as.integer(ix[, 2]) else NA_integer_
  h_col <- as.integer(if (v5) ix[, 3] else ix[, 2])
  p_col <- as.integer(if (v5) ix[, 4] else ix[, 3])
  pm <- match("Miðflokkurinn", meta$party_names)
  pb <- match("Framsóknarflokkurinn", meta$party_names)
  map(seq_len(sd$E), \(e) {
    rows <- which(sd$epoch_n == e & sd$house_n > 1)
    houses <- sort(unique(sd$house_n[rows]))
    present <- setdiff(sort(unique(as.vector(sd$party_index_n[rows, ]))), 0)
    x <- sapply(seq_len(sd$P), \(q) {
      cols <- which((!v5 | e_col == e) & h_col %in% houses & p_col == q)
      rowMeans(g[, cols, drop = FALSE])
    })
    xc <- x - rowMeans(x[, present, drop = FALSE])
    mb <- if (all(c(pm, pb) %in% present)) x[, pm] - x[, pb] else NULL
    list(
      parties = tibble(
        tag = t, model = model_label(t), epoch = e, p = seq_len(sd$P),
        flokkur = meta$party_names, present = seq_len(sd$P) %in% present,
        mean = colMeans(xc), q5 = apply(xc, 2, quantile, 0.05),
        q95 = apply(xc, 2, quantile, 0.95),
        houses = paste(meta$house_names[houses], collapse = ", ")
      ),
      mb = if (is.null(mb)) NULL else tibble(
        tag = t, model = model_label(t), epoch = e, mean = mean(mb),
        q5 = quantile(mb, 0.05), q95 = quantile(mb, 0.95), p_neg = mean(mb < 0)
      )
    )
  }) |>
    (\(l) list(
      parties = map(l, "parties") |> list_rbind() |> left_join(meta$epoch_table, by = "epoch"),
      mb = map(l, "mb") |> compact() |> list_rbind() |> left_join(meta$epoch_table, by = "epoch")
    ))()
}

full_tags <- tags[str_detect(tags, "_full") & !str_starts(tags, "sim")]
if (length(full_tags)) {
  ab <- map(full_tags, active_bias)
  eb <- map(ab, "parties") |> list_rbind()
  mb <- map(ab, "mb") |> list_rbind()
  write_csv(eb, here(res_dir, "eval_epoch_bias.csv"))
  write_csv(mb, here(res_dir, "eval_contrast_MB.csv"))

  cat("\n== Bias of the active pollsters by epoch (centred logit; negative = under-polled) ==\n")
  eb |>
    filter(present) |>
    mutate(flokkur = short[flokkur], value = sprintf("%.2f", mean)) |>
    select(tag, epoch, first_poll, flokkur, value) |>
    pivot_wider(names_from = flokkur, values_from = value) |>
    arrange(tag, epoch) |>
    print(n = Inf, width = Inf)

  cat("\n== Contrast bias(M) - bias(B), active pollsters (negative = M under-polled relative to B) ==\n")
  mb |>
    mutate(across(c(mean, q5, q95), \(x) round(x, 3))) |>
    select(tag, epoch, first_poll, last_poll, mean, q5, q95, p_neg) |>
    print(n = Inf)

  cat("\n== Shock SDs and cross-house spread ==\n")
  map(full_tags, \(t) {
    read_csv(file.path(res_dir, t, "summary.csv"), show_col_types = FALSE) |>
      filter(variable %in% c("tau_mu", "tau_house", "sigma_gamma")) |>
      mutate(tag = t)
  }) |>
    list_rbind() |>
    select(tag, variable, mean, median, q5, q95, rhat, ess_bulk) |>
    print(n = Inf)

  cat("\n== Smoothed support on the last date (pp) ==\n")
  now <- map(full_tags, \(t) {
    read_parquet(file.path(res_dir, t, "pi_last.parquet")) |>
      summarise(
        mean = 100 * mean(value), q05 = 100 * quantile(value, 0.05),
        q95 = 100 * quantile(value, 0.95), .by = c(date, flokkur)
      ) |>
      mutate(tag = t)
  }) |>
    list_rbind()
  write_csv(now, here(res_dir, "eval_current_support.csv"))
  now |>
    mutate(flokkur = short[flokkur], value = sprintf("%.1f [%.1f, %.1f]", mean, q05, q95)) |>
    select(tag, date, flokkur, value) |>
    pivot_wider(names_from = tag, values_from = value) |>
    arrange(match(flokkur, short)) |>
    print(n = Inf, width = Inf)

  plot_tags <- intersect(c("v4_full", "v5_full"), full_tags)
  if (length(plot_tags)) {
    p <- eb |>
      filter(tag %in% plot_tags, present) |>
      mutate(
        party = short[flokkur],
        model = if_else(model == "v4", "Constant bias (v4)", "Epoch random walk (v5)"),
        mid = first_poll + (last_poll - first_poll) / 2
      ) |>
      ggplot(aes(mid, mean, ymin = q5, ymax = q95, colour = model)) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      geom_pointrange(position = position_dodge(width = 150), size = 0.25) +
      facet_wrap(~party, ncol = 5) +
      labs(
        x = NULL, y = "Bias of active pollsters (logit, 90% interval)", colour = NULL,
        title = "Polling bias per election epoch, 2016-2026",
        subtitle = "Averaged over the pollsters active in each epoch; negative = party under-polled relative to election results"
      ) +
      theme(legend.position = "top")
    ggsave(here("Figures", "epoch_bias_industry.png"), p, width = 12, height = 6, dpi = 144)
    cat("\nWrote Figures/epoch_bias_industry.png\n")
  }
}

# ---- 4. simulation recovery -------------------------------------------------------------
sim_tags <- tags[str_detect(tags, "^sim\\d+_v[45]_full$")]
if (length(sim_tags)) {
  cat("\n== Simulation recovery (truth = known epoch-varying bias) ==\n")
  rec <- map(sim_tags, \(t) {
    truth <- readRDS(here(res_dir, paste0(str_extract(t, "^sim\\d+"), "_data"), "truth.rds"))
    sd <- read_fit_meta(t)$stan_data
    # true bias of the active pollsters, centred over present parties, per epoch
    true_ab <- map(seq_len(sd$E), \(e) {
      rows <- which(sd$epoch_n == e & sd$house_n > 1)
      houses <- sort(unique(sd$house_n[rows]))
      present <- setdiff(sort(unique(as.vector(sd$party_index_n[rows, ]))), 0)
      x <- truth$mu[e, ] + apply(truth$dev[e, houses - 1, , drop = FALSE], 3, mean)
      tibble(epoch = e, p = seq_len(sd$P), true = x - mean(x[present]))
    }) |>
      list_rbind()
    eb <- active_bias(t)$parties |>
      filter(present) |>
      left_join(true_ab, by = c("epoch", "p"))
    pl <- read_parquet(file.path(res_dir, t, "pi_last.parquet")) |>
      summarise(est = mean(value), q05 = quantile(value, 0.05), q95 = quantile(value, 0.95), .by = flokkur) |>
      mutate(true = truth$pi_last[match(flokkur, truth$party_names)])
    tau <- read_csv(file.path(res_dir, t, "summary.csv"), show_col_types = FALSE) |>
      filter(variable %in% c("tau_mu", "tau_house"))
    tibble(
      tag = t,
      bias_rmse = sqrt(mean((eb$mean - eb$true)^2)),
      bias_rmse_last_epoch = with(filter(eb, epoch == max(epoch)), sqrt(mean((mean - true)^2))),
      bias_cov90 = mean(eb$true >= eb$q5 & eb$true <= eb$q95),
      pi_last_mae_pp = 100 * mean(abs(pl$est - pl$true)),
      pi_last_cov90 = mean(pl$true >= pl$q05 & pl$true <= pl$q95),
      tau_mu = if (nrow(tau)) sprintf("%.3f [%.3f, %.3f] (true %.2f)", tau$mean[1], tau$q5[1], tau$q95[1], truth$tau_mu) else NA,
      tau_house = if (nrow(tau)) sprintf("%.3f [%.3f, %.3f] (true %.2f)", tau$mean[2], tau$q5[2], tau$q95[2], truth$tau_house) else NA
    )
  }) |>
    list_rbind()
  print(rec, width = Inf)
  write_csv(rec, here(res_dir, "eval_simulation.csv"))
}
