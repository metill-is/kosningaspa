library(tidyverse)
library(hagstofa)
library(metill)
library(janitor)
library(clock)
library(slider)
library(here)
library(eurostat)
library(zoo)

countries <- c(
  "Iceland"
)
icelandic <- c(
  "Iceland" = "Ísland"
)


country_table <- tribble(
  ~location, ~geo, ~land, ~color,
  "ISL", "Iceland", "Ísland", "#003f4a",
)

box::use(
  R / prepare_data[read_fundamentals_data]
)

fundamentals_data <- read_fundamentals_data()

vnv <- hg_data(
  "https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/1_vnv/VIS01002.px"
) |>
  collect() |>
  clean_names() |>
  rename(
    value = 4
  ) |>
  separate(manudur, into = c("ar", "man"), sep = "M", convert = T) |>
  mutate(
    dags = date_build(ar, man),
    grunnur = parse_number(grunnur)
  )

econ_data <- vnv |>
  mutate(
    value = value / value[dags == max(dags)],
    .by = c(grunnur, visitala)
  ) |>
  summarise(
    value = mean(value, na.rm = T),
    .by = c(dags, visitala)
  ) |>
  mutate(
    breyting = c(0, diff(log(value))),
    ar_breyting = slide_dbl(breyting, sum, .before = 12, .complete = T),
    .by = visitala
  ) |>
  mutate_at(
    vars(breyting, ar_breyting),
    ~ exp(.) - 1
  ) |>
  filter(
    # grunnur == 1939,
    visitala == "Vísitala neysluverðs"
  ) |>
  drop_na() |>
  select(dags, vnv = ar_breyting) |>
  inner_join(
    fundamentals_data |>
      distinct(date) |>
      mutate(
        date_prior = floor_date(date, "year") - months(6)
      ),
    by = join_by(dags == date_prior)
  ) |>
  select(-dags)







gdp <- hg_data(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/thjodhagsreikningar/landsframl/1_landsframleidsla/THJ01000.px"
) |>
  collect()

gdp <- gdp |>
  janitor::clean_names() |>
  filter(
    skipting == "Verg landsframleiðsla á mann, magnbreyting frá fyrra ári"
  ) |>
  mutate(
    ar = parse_number(ar)
  ) |>
  select(-skipting) |>
  rename(
    growth = 2
  ) |>
  drop_na() |>
  crossing(
    month = 1:12
  ) |>
  mutate(
    dags = date_build(ar, month)
  ) |>
  mutate(
    growth = if_else(month == 1, growth, NA_real_),
    growth = zoo::na.approx(growth, na.rm = FALSE)
  ) |>
  fill(growth, .direction = "down") |>
  inner_join(
    fundamentals_data |>
      distinct(date) |>
      mutate(
        date_prior = floor_date(date, "year") - months(6)
      ),
    by = join_by(dags == date_prior)
  ) |>
  select(-dags, -ar, -month)

econ_data <- econ_data |>
  inner_join(
    gdp,
    by = join_by(date == date)
  )


write_csv(econ_data, here("data", "economy_data.csv"))
