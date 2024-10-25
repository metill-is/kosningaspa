library(tidyverse)


d <- tribble(
  ~date, ~flokkur, ~p,
  "2021-09-25", "Samfylkingin", 0.099,
  "2021-09-25", "Sjálfstæðisflokkurinn", 0.244,
  "2021-09-25", "Miðflokkurinn", 0.054,
  "2021-09-25", "Framsóknarflokkurinn", 0.173,
  "2021-09-25", "Vinstri Græn", 0.126,
  "2021-09-25", "Flokkur Fólksins", 0.088,
  "2021-09-25", "Viðreisn", 0.083,
  "2021-09-25", "Píratar", 0.086,
  "2021-09-25", "Sósíalistaflokkurinn", 0.041,
  "2017-10-28", "Samfylkingin", 0.1205,
  "2017-10-28", "Sjálfstæðisflokkurinn", 0.2525,
  "2017-10-28", "Miðflokkurinn", 0.1087,
  "2017-10-28", "Framsóknarflokkurinn", 0.1071,
  "2017-10-28", "Vinstri Græn", 0.1689,
  "2017-10-28", "Flokkur Fólksins", 0.0688,
  "2017-10-28", "Viðreisn", 0.0669,
  "2017-10-28", "Píratar", 0.092,
  "2017-10-28", "Sósíalistaflokkurinn", 0
)

out <- d |>
  bind_rows(
    tribble(
      ~date, ~flokkur, ~p,
      "2021-09-25", "Annað", 1 - sum(d$p[d$date == "2021-09-25"]),
      "2017-10-28", "Annað", 1 - sum(d$p[d$date == "2017-10-28"])
    )
  ) |>
  mutate(
    n = p * c(199730, 196259),
    fyrirtaeki = "Kosning"
  ) |>
  select(-p)

write_csv(out, here("data", "election_data.csv"))
