theme_set(theme_metill())

pred_outcome <- y_rep_draws |> 
filter(dags == max(dags)) |> 
summarise(
  median = median(value),
  .by = flokkur
)

flokkar <- c(
  "Samfylkingin",
  "Sjálfstæðisflokkurinn",
  "Viðreisn",
  "Flokkur Fólksins",
  "Miðflokkurinn",
  "Framsóknarflokkurinn",
  "Píratar",
  "Sósíalistaflokkurinn",
  "Vinstri Græn"
)

p <- polling_data |>
mutate(
  p = n / sum(n),
  .by = c(date, fyrirtaeki)
) |>
filter(
  flokkur %in% flokkar
) |>
select(-n) |>
mutate(
  flokkur = as.character(flokkur)
) |> 
arrange(flokkur) |> 
mutate(
  which_election = cumsum(fyrirtaeki == "Kosning"),
  which_election = which_election - (fyrirtaeki == "Kosning"),
  .by = flokkur
) |>
bind_rows(
  pred_outcome |>
    filter(
    flokkur %in% flokkar
  ) |> 
    rename(p = median) |> 
    mutate(
      date = clock::date_build(2024, 11, 30),
      which_election = 3,
      fyrirtaeki = "Kosning"
    )
) |> 
mutate(
  election_date = date[fyrirtaeki == "Kosning"],
  p_true = p[fyrirtaeki == "Kosning"],
  .by = c(which_election, flokkur)
) |> 
mutate(
  time_to_election = election_date - date,
  error = p - p_true,
  which_election = c(2016, 2017, 2021, 2024)[which_election + 1]
) |>
filter(
  time_to_election <= 50,
  fyrirtaeki != "Kosning"
) |>
  filter(p > 0, p_true > 0) |> 
ggplot(aes(-time_to_election, p)) +
geom_point() +
geom_smooth() +
geom_hline(
  aes(yintercept = p_true),
  lty = 2
) +
scale_x_continuous(
  labels = function(x) number(abs(x)),
  guide = ggh4x::guide_axis_truncated()
) +
scale_y_continuous(
  breaks = breaks_width(0.05),
  labels = label_percent(accuracy = 1, suffix = "%-stig"),
  guide = ggh4x::guide_axis_truncated()
) +
facet_grid(
  cols = vars(which_election),
  rows = vars(flokkur),
  scales = "free_y"
) +
  labs(
    x = "Tími að kosningum",
    y = "Fylgi (niðurstöður)",
    title = "Samanburður á könnunum og niðurstöðum kosninga"
  )

p  

ggsave(
    here("Figures", "compare_error.png"),
    p,
    width = 8,
    height = 1.2 * 8,
    scale = 1.5
  )
