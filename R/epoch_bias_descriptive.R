# Model-free check of pollster bias per election cycle.
#
# For each anchoring election, compare the polls published in the final weeks
# before it (and, separately, the first weeks after it) with the official
# result. Errors are on the model's scale: log(poll share) - log(result share),
# centred across the parties both report (a centred log-ratio difference), which
# is what a zero-sum logit house effect gamma measures. Also reported in pp.
#
# Question it answers: is the industry bias for each party stable from one
# election to the next (constant gamma is fine), or does it drift (motivates an
# epoch-varying gamma)? Of particular interest: Framsóknarflokkurinn (B) and
# Miðflokkurinn (M).
#
# Run from the project root: Rscript R/epoch_bias_descriptive.R

Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressMessages({
  library(tidyverse)
  library(here)
  library(clock)
  library(metill)
})
theme_set(theme_metill())

box::use(R / data[read_polling_data])

out_dir <- here("results", "epoch_bias")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pre <- suppressMessages(read_polling_data()) |>
  select(date, fyrirtaeki, flokkur, n) |>
  mutate(fyrirtaeki = as.character(fyrirtaeki), flokkur = as.character(flokkur))
post <- read_csv(here("data", "post_election_polls.csv"), show_col_types = FALSE) |>
  select(date, fyrirtaeki, flokkur, n)
polls <- bind_rows(pre, post) |>
  filter(n > 0) |>
  mutate(p = n / sum(n), .by = c(date, fyrirtaeki))

results <- polls |>
  filter(fyrirtaeki == "Kosning") |>
  select(election = date, flokkur, result = p)
elections <- sort(unique(results$election))

# Poll-vs-result errors for polls in a window relative to one election.
window_errors <- function(e, from_days, to_days, label) {
  res <- filter(results, election == e)
  polls |>
    filter(
      fyrirtaeki != "Kosning",
      date >= e + from_days,
      date <= e + to_days
    ) |>
    inner_join(res, by = "flokkur") |>
    mutate(
      log_err = log(p) - log(result),
      # centre over the parties this poll reports (clr difference)
      clr_err = log_err - mean(log_err),
      pp_err = 100 * (p - result),
      .by = c(date, fyrirtaeki)
    ) |>
    mutate(election = e, window = label)
}

errs <- map(elections, \(e) bind_rows(
  window_errors(e, -21, -1, "pre (21d)"),
  window_errors(e, 1, 90, "post (90d)")
)) |>
  list_rbind()

# Per house x election x window: average error over the polls in the window.
house_err <- errs |>
  summarise(
    n_polls = n_distinct(date),
    clr_err = mean(clr_err),
    pp_err = mean(pp_err),
    .by = c(election, window, fyrirtaeki, flokkur)
  )

# Industry: average over houses (each house weighted equally, as mu_gamma does).
industry_err <- house_err |>
  summarise(
    n_houses = n(),
    clr_err = mean(clr_err),
    pp_err = mean(pp_err),
    .by = c(election, window, flokkur)
  )

write_csv(house_err, here(out_dir, "descriptive_house_errors.csv"))
write_csv(industry_err, here(out_dir, "descriptive_industry_errors.csv"))

short <- c(
  "Sjálfstæðisflokkurinn" = "D", "Framsóknarflokkurinn" = "B",
  "Samfylkingin" = "S", "Vinstri Græn" = "V", "Píratar" = "P",
  "Viðreisn" = "C", "Flokkur Fólksins" = "F", "Miðflokkurinn" = "M",
  "Sósíalistaflokkurinn" = "J", "Annað" = "Other"
)

tab <- function(d, col) {
  d |>
    mutate(
      flokkur = short[flokkur],
      election = format(election, "%Y"),
      value = round({{ col }}, 2)
    ) |>
    select(window, flokkur, election, value) |>
    pivot_wider(names_from = election, values_from = value) |>
    arrange(window, match(flokkur, short))
}

cat("\n== Industry error, centred log-ratio (poll vs result) ==\n")
cat("   negative = party under-polled; comparable to the model's mu_gamma\n")
print(tab(industry_err, clr_err), n = Inf)

cat("\n== Industry error, percentage points ==\n")
print(tab(industry_err, pp_err), n = Inf)

cat("\n== B and M by house (pre-election window, clr) ==\n")
house_err |>
  filter(window == "pre (21d)", flokkur %in% c("Framsóknarflokkurinn", "Miðflokkurinn")) |>
  mutate(flokkur = short[flokkur], election = format(election, "%Y"), clr_err = round(clr_err, 2)) |>
  select(fyrirtaeki, flokkur, election, clr_err, n_polls) |>
  arrange(flokkur, fyrirtaeki, election) |>
  print(n = Inf)

# Election-to-election persistence of the industry error: correlation, over
# parties, of the pre-election clr error at consecutive elections, and the SD of
# the change (a model-free guess at the between-election shock scale).
pre_ind <- industry_err |>
  filter(window == "pre (21d)") |>
  select(election, flokkur, clr_err)
pairs <- tibble(e1 = head(elections, -1), e2 = tail(elections, -1)) |>
  mutate(stats = map2(e1, e2, \(a, b) {
    d <- inner_join(
      filter(pre_ind, election == a),
      filter(pre_ind, election == b),
      by = "flokkur", suffix = c("_1", "_2")
    )
    tibble(
      n_parties = nrow(d),
      cor = cor(d$clr_err_1, d$clr_err_2),
      sd_change = sd(d$clr_err_2 - d$clr_err_1),
      sd_level = sd(c(d$clr_err_1, d$clr_err_2))
    )
  })) |>
  unnest(stats)
cat("\n== Persistence of pre-election industry error between consecutive elections ==\n")
print(pairs)
write_csv(pairs, here(out_dir, "descriptive_persistence.csv"))

p <- industry_err |>
  filter(flokkur %in% c("Framsóknarflokkurinn", "Miðflokkurinn")) |>
  mutate(party = short[flokkur], window = fct_rev(window)) |>
  ggplot(aes(election, clr_err, colour = party, linetype = window)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL, y = "Poll minus result (centred log-ratio)", colour = NULL, linetype = NULL,
    title = "Model-free industry polling error for B and M at each election",
    subtitle = "Average over pollsters; 'pre' = final 21 days before the election, 'post' = first 90 days after"
  )
ggsave(here("Figures", "epoch_bias_descriptive_BM.png"), p, width = 9, height = 5, dpi = 144)
cat("Wrote Figures/epoch_bias_descriptive_BM.png\n")
