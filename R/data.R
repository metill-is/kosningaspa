#' @export
update_fundamentals_data <- function() {
  box::use(
    googlesheets4[read_sheet, gs4_auth],
    dplyr[filter, mutate, group_by, ungroup, arrange, rename],
    tidyr[pivot_longer, pivot_wider],
    readr[write_csv],
    here[here],
    janitor[clean_names],
    lubridate[ymd],
    clock[date_build]
  )
  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  fundamentals <- read_sheet(
    Sys.getenv("FUNDAMENTALS_SHEET_URL"),
    sheet = "data"
  ) |>
    rename(
      flokkur = party
    ) |>
    mutate(
      date = as.Date(date)
    )

  fundamentals |>
    write_csv(here("data", "fundamentals_data.csv"))
}

#' @export
read_fundamentals_data <- function() {
  box::use(
    readr[read_csv],
    here[here],
    forcats[fct_relevel, as_factor],
    dplyr[mutate, arrange, if_else]
  )
  read_csv(here("data", "fundamentals_data.csv")) |>
    mutate(
      flokkur = if_else(flokkur == "Framsókn", "Framsóknarflokkurinn", flokkur),
      flokkur = fct_relevel(
        as_factor(flokkur),
        "Annað",
        "Sjálfstæðisflokkurinn",
        "Framsóknarflokkurinn",
        "Samfylkingin",
        "Vinstri Græn",
        "Píratar",
        "Viðreisn",
        "Flokkur Fólksins",
        "Miðflokkurinn",
        "Sósíalistaflokkurinn"
      )
    ) |>
    arrange(flokkur, date)
}

#' @export
get_sheet_data <- function(sheet) {
  box::use(
    googlesheets4[read_sheet, gs4_auth],
    janitor[clean_names],
    dplyr[mutate, select, coalesce]
  )
  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  read_sheet(
    Sys.getenv("POLLING_SHEET_URL"),
    sheet = sheet
  ) |>
    janitor::clean_names() |>
    mutate(
      dagur = coalesce(dagur, 15),
      date = clock::date_build(ar, manudur, dagur),
      hlutfall = hlutfall / sum(hlutfall, na.rm = TRUE),
      hlutfall = if_else(
        flokkur == "Annað",
        1 - sum(hlutfall[!flokkur %in% c("Annað", "Lýðræðisflokkurinn")]),
        hlutfall
      ),
      n = hlutfall * fjoldi_alls,
      .by = c(ar, manudur, dagur, fyrirtaeki)
    ) |>
    select(
      date,
      fyrirtaeki,
      flokkur,
      n
    )
}

#' @export
get_felo_data <- function() {
  box::use(
    googlesheets4[read_sheet, gs4_auth],
    janitor[clean_names],
    dplyr[mutate, select, coalesce, left_join, join_by, summarise],
    tidyr[drop_na, pivot_longer],
    lubridate[dmy]
  )

  box::use(
    R / party_utils[party_tibble]
  )

  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  d <- read_sheet(Sys.getenv("FELO_SHEET_URL"))

  d |>
    mutate(
      fyrstidagur = dmy(fyrstidagur),
      sidastidagur = dmy(sidastidagur),
      date = fyrstidagur + (sidastidagur - fyrstidagur) / 2
    ) |>
    select(
      date, fyrirtaeki, D:Other
    ) |>
    pivot_longer(
      c(-date, -fyrirtaeki),
      names_to = "bokstafur",
      values_to = "n",
      values_transform = list(
        "n" = as.numeric
      )
    ) |>
    left_join(
      party_tibble(),
      by = join_by(bokstafur)
    ) |>
    mutate(
      flokkur = coalesce(flokkur, "Annað"),
      litur = coalesce(litur, "grey50")
    ) |>
    summarise(
      n = sum(n, na.rm = TRUE),
      .by = c(date, fyrirtaeki, flokkur)
    ) |>
    drop_na()
}
#' @export
get_election_data <- function() {
  box::use(
    dplyr[tribble, bind_rows, mutate, select],
    clock[date_build]
  )
  d <- tribble(
    ~date, ~flokkur, ~p,
    # 2021
    date_build(2021, 09, 25), "Samfylkingin", 0.099,
    date_build(2021, 09, 25), "Sjálfstæðisflokkurinn", 0.244,
    date_build(2021, 09, 25), "Miðflokkurinn", 0.054,
    date_build(2021, 09, 25), "Framsóknarflokkurinn", 0.173,
    date_build(2021, 09, 25), "Vinstri Græn", 0.126,
    date_build(2021, 09, 25), "Flokkur Fólksins", 0.088,
    date_build(2021, 09, 25), "Viðreisn", 0.083,
    date_build(2021, 09, 25), "Píratar", 0.086,
    date_build(2021, 09, 25), "Sósíalistaflokkurinn", 0.041,
    # 2017
    date_build(2017, 10, 28), "Samfylkingin", 0.1205,
    date_build(2017, 10, 28), "Sjálfstæðisflokkurinn", 0.2525,
    date_build(2017, 10, 28), "Miðflokkurinn", 0.1087,
    date_build(2017, 10, 28), "Framsóknarflokkurinn", 0.1071,
    date_build(2017, 10, 28), "Vinstri Græn", 0.1689,
    date_build(2017, 10, 28), "Flokkur Fólksins", 0.0688,
    date_build(2017, 10, 28), "Viðreisn", 0.0669,
    date_build(2017, 10, 28), "Píratar", 0.092,
    date_build(2017, 10, 28), "Sósíalistaflokkurinn", 0,
    # 2016
    date_build(2016, 10, 29), "Samfylkingin", 0.057,
    date_build(2016, 10, 29), "Sjálfstæðisflokkurinn", 0.29,
    date_build(2016, 10, 29), "Miðflokkurinn", 0,
    date_build(2016, 10, 29), "Framsóknarflokkurinn", 0.115,
    date_build(2016, 10, 29), "Vinstri Græn", 0.159,
    date_build(2016, 10, 29), "Flokkur Fólksins", 0.0354,
    date_build(2016, 10, 29), "Viðreisn", 0.105,
    date_build(2016, 10, 29), "Píratar", 0.145,
    date_build(2016, 10, 29), "Sósíalistaflokkurinn", 0
  )

  out <- d |>
    bind_rows(
      tribble(
        ~date, ~flokkur, ~p,
        date_build(2021, 09, 25), "Annað", 1 - sum(d$p[d$date == date_build(2021, 09, 25)]),
        date_build(2017, 10, 28), "Annað", 1 - sum(d$p[d$date == date_build(2017, 10, 28)]),
        date_build(2016, 10, 29), "Annað", 1 - sum(d$p[d$date == date_build(2016, 10, 29)])
      )
    ) |>
    mutate(
      n = p * c(199730, 196259, 189648),
      fyrirtaeki = "Kosning"
    ) |>
    select(-p)
}

#' @export
combine_datasets <- function() {
  box::use(
    here[here],
    readr[read_csv],
    dplyr[
      bind_rows,
      mutate,
      filter,
      arrange,
      if_else
    ],
    forcats[fct_relevel, as_factor],
    clock[date_build]
  )

  box::use(
    R / party_utils[party_tibble]
  )

  gallup_data <- get_sheet_data("gallup")
  maskina_data <- get_sheet_data("maskina")
  prosent_data <- get_sheet_data("prosent")
  felagsvisindastofnun <- get_sheet_data("felagsvisindastofnun")
  election_data <- get_election_data()
  felo_data <- get_felo_data()


  # combine data
  data <- bind_rows(
    maskina_data,
    prosent_data,
    gallup_data,
    felagsvisindastofnun,
    election_data
  ) |>
    filter(
      (date > date_build(2021, 9, 25)) | (fyrirtaeki == "Kosning")
    ) |>
    bind_rows(
      felo_data
    ) |>
    mutate(
      flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur),
      fyrirtaeki = fct_relevel(
        as_factor(fyrirtaeki),
        "Kosning",
        "Félagsvísindastofnun"
      )
    ) |>
    arrange(date, fyrirtaeki, flokkur)

  data
}

#' @export
update_polling_data <- function(data) {
  box::use(
    readr[write_csv],
    here[here]
  )
  data <- combine_datasets()
  write_csv(data, here("data", "polling_data.csv"))
}

#' @export
read_polling_data <- function() {
  box::use(
    readr[read_csv],
    here[here],
    dplyr[mutate, arrange, rename],
    forcats[fct_relevel, as_factor],
  )
  read_csv(here("data", "polling_data.csv")) |>
    mutate(
      fyrirtaeki = fct_relevel(
        as_factor(fyrirtaeki),
        "Kosning",
        "Félagsvísindastofnun"
      ),
      flokkur = fct_relevel(
        as_factor(flokkur),
        "Annað",
        "Sjálfstæðisflokkurinn",
        "Samfylkingin",
        "Framsóknarflokkurinn",
        "Vinstri Græn",
        "Píratar",
        "Viðreisn",
        "Flokkur Fólksins",
        "Miðflokkurinn",
        "Sósíalistaflokkurinn"
      )
    ) |>
    arrange(date, fyrirtaeki, flokkur)
}

#' @export
get_electorate_byage <- function() {
  box::use(
    dplyr[
      filter,
      mutate,
      rename,
      count,
      case_when,
      arrange
    ],
    janitor[clean_names, remove_constant],
    hagstofa[hg_data, collect],
    stringr[str_replace, str_to_title]
  )

  url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/kosningar/althingi/althkjosendur/KOS02101a.px"

  d <- hg_data(url) |>
    collect() |>
    clean_names() |>
    rename(n = 6) |>
    filter(
      eining == "Fjöldi",
      kjordaemi != "Allt landið",
      aldur != "Alls",
      kyn == "Alls",
      ar == "2021"
    ) |>
    remove_constant() |>
    arrange(kjordaemi) |>
    filter(
      aldur %in% c(
        "18-29",
        "30-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75 - 79",
        "80+"
      )
    ) |>
    mutate(
      aldur = case_when(
        aldur == "18-29" ~ aldur,
        aldur == "30-39" ~ aldur,
        aldur == "40-44" ~ "40-49",
        aldur == "45-49" ~ "40-49",
        aldur == "50-54" ~ "50-59",
        aldur == "55-59" ~ "50-59",
        TRUE ~ "60+"
      ),
      kjordaemi = str_replace(kjordaemi, "kjördæmi", "") |>
        str_replace("Reykjavíkur", "Reykjavík") |>
        str_to_title()
    ) |>
    count(aldur, kjordaemi, name = "n_kjosendur", wt = n)
}

#' @export
update_constituency_data <- function() {
  box::use(
    googlesheets4[read_sheet, gs4_auth],
    janitor[clean_names],
    dplyr[mutate, select, coalesce],
    clock[date_build],
    here[here],
    tidyr[pivot_longer]
  )
  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  d <- read_sheet(
    Sys.getenv("POLLING_SHEET_URL"),
    sheet = "kjordaemi"
  )
  d |>
    pivot_longer(-c(1:5), names_to = "kjordaemi", values_to = "n") |>
    janitor::clean_names() |>
    mutate(
      date = date_build(ar, manudur, dagur)
    ) |>
    select(date, fyrirtaeki, flokkur, kjordaemi, n) |>
    mutate(
      fyrirtaeki = fct_relevel(
        as_factor(fyrirtaeki),
        "Kosning",
        "Félagsvísindastofnun"
      ),
      flokkur = fct_relevel(
        as_factor(flokkur),
        "Annað",
        "Sjálfstæðisflokkurinn",
        "Framsóknarflokkurinn",
        "Samfylkingin",
        "Vinstri Græn",
        "Píratar",
        "Viðreisn",
        "Flokkur Fólksins",
        "Miðflokkurinn",
        "Sósíalistaflokkurinn"
      )
    ) |>
    write_csv(here("data", "constituency_data.csv"))
}

#' @export
read_constituency_data <- function() {
  box::use(
    readr[read_csv],
    here[here]
  )
  read_csv(here("data", "constituency_data.csv"))
}
