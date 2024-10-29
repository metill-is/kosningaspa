#' @export
update_fundamentals_data <- function() {
  box::use(
    googlesheets4[read_sheet, gs4_auth],
    dplyr[filter, mutate, group_by, ungroup, arrange],
    tidyr[pivot_longer, pivot_wider],
    readr[write_csv],
    here[here],
    janitor[clean_names]
  )
  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  fundamentals <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1MHpyMXJxnxhYSMf1nj73yKNKJ1BLJ4qaL1WMmf804Iw/edit?gid=1816405109#gid=1816405109",
    sheet = "kosningar"
  ) |>
    # Convert to proportions
    pivot_longer(cols = -Flokkur, names_to = "ar", values_to = "atkvaedahlutfall") |>
    clean_names() |>
    mutate(
      atkvaedahlutfall = atkvaedahlutfall / sum(atkvaedahlutfall),
      .by = ar
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
    dplyr[mutate, arrange]
  )
  read_csv(here("data", "fundamentals_data.csv")) |>
    mutate(
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
    arrange(flokkur, ar)
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
    "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
    sheet = sheet
  ) |>
    janitor::clean_names() |>
    mutate(
      dagur = coalesce(dagur, 15),
      date = clock::date_build(ar, manudur, dagur),
      n = hlutfall * fjoldi_alls
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
  url <- "https://docs.google.com/spreadsheets/d/1US1qVhLE8I6496dXvoGUPp8QnRvC52FCc2jTyBkRNZs/edit?gid=621143960#gid=621143960"
  gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  d <- read_sheet(url)

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
      (date > max(felo_data$date)) | (fyrirtaeki == "Kosning")
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
prepare_stan_data <- function(polling_data, fundamentals_data) {
  box::use(
    dplyr[
      distinct,
      select,
      mutate,
      pull,
      arrange,
      filter,
      lag,
      summarise,
      rename
    ],
    tidyr[pivot_wider, drop_na],
    clock[date_build],
    forcats[fct_relevel, as_factor]
  )

  #### Polling data ####
  D <- length(unique(polling_data$date))
  P <- length(unique(polling_data$flokkur))
  H <- length(unique(polling_data$fyrirtaeki))
  N <- polling_data |>
    distinct(fyrirtaeki, date) |>
    nrow()


  y <- polling_data |>
    select(date, fyrirtaeki, flokkur, n) |>
    mutate(
      n = as.integer(n)
    ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    select(-date, -fyrirtaeki) |>
    as.matrix()

  house <- polling_data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      house = as.numeric(factor(fyrirtaeki))
    ) |>
    pull(house)

  date <- polling_data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      date = as.numeric(factor(date))
    ) |>
    pull(date)

  time_diff <- polling_data |>
    distinct(date) |>
    arrange(date) |>
    mutate(
      time_diff = c(NA, diff(date))
    ) |>
    drop_na() |>
    pull(time_diff) |>
    as.numeric()

  max_date <- max(polling_data$date)
  election_date <- clock::date_build(2024, 11, 30)
  pred_y_time_diff <- as.numeric(election_date - max_date)
  stjornarslit <- polling_data |>
    distinct(date) |>
    mutate(
      stjornarslit = 1 * (date >= clock::date_build(2024, 10, 14))
    ) |>
    pull(stjornarslit)

  n_election <- polling_data |>
    filter(
      fyrirtaeki == "Kosning",
      date == date_build(2021, 09, 25)
    ) |>
    pull(n) |>
    sum()

  n_parties <- polling_data |>
    summarise(
      n_parties = sum(n != 0),
      .by = c(date, fyrirtaeki)
    ) |>
    arrange(date) |>
    pull(n_parties)

  #### Fundamentals data ####
  X <- fundamentals_data |>
    rename(p = atkvaedahlutfall) |>
    arrange(ar) |>
    filter(p > 0) |>
    mutate(
      logit_p = log(p) - log(1 - p)
    ) |>
    mutate(
      logit_p = logit_p - mean(logit_p),
      .by = ar
    ) |>
    filter(flokkur != "Annað") |>
    select(-p) |>
    pivot_wider(
      names_from = ar,
      values_from = logit_p,
      values_fill = 0
    ) |>
    arrange(flokkur) |>
    select(-flokkur)

  n_parties_fundamentals <- fundamentals_data |>
    rename(p = atkvaedahlutfall) |>
    filter(flokkur != "Annað") |>
    arrange(ar, flokkur) |>
    mutate(
      in_election = 1 * (p > 0)
    ) |>
    mutate(
      in_last_election = lag(in_election, default = 0),
      .by = flokkur
    ) |>
    summarise(
      n_parties = sum(in_last_election),
      .by = ar
    ) |>
    pull(n_parties)

  stan_data <- list(
    # Polling Data
    D = D,
    P = P,
    H = H,
    N = N,
    y = y,
    house = house,
    date = date,
    time_diff = time_diff,
    pred_y_time_diff = pred_y_time_diff,
    stjornarslit = stjornarslit,
    n_pred = as.integer(n_election),
    n_parties = n_parties,
    # Fundamentals Data
    D_f = ncol(X),
    P_f = nrow(X),
    logit_votes = X,
    n_parties_f = n_parties_fundamentals
  )

  stan_data
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
