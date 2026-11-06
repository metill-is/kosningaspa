library(tidyverse)
library(rvest)
library(here)
library(clock)

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  R / party_utils[party_tibble]
)

#### Utility: map party abbreviations to full names ####

party_code_to_name <- function() {
  party_tibble() |>
    select(flokkur, bokstafur)
}

#### Utility: party name stems for regex matching in Icelandic text ####

party_stems <- c(
  "S" = "Samfylking",
  "D" = "Sjálfstæðisflokk",
  "B" = "Framsókn",
  "C" = "Viðreisn",
  "M" = "Miðflokk",
  "F" = "Flokkur\\s+[Ff]ólksins|Flokks?\\s+[Ff]ólksins",
  "P" = "Pírat",
  "V" = "Vinstri",
  "J" = "Sósíalistaflokk"
)

#### Hardcoded post-election polls ####
# Gallup data from ruv.is (Þjóðarpúls): exact percentages
# Maskína data: exact 1-dp values (PDF / maskina.is dashboard / Vísir)
# 2024 election result from althingi.is / electionguide.org

get_hardcoded_polls <- function() {
  # Wide format: each row is one poll with party abbreviation columns (%)
  # Date is floor(midpoint) of the collection period; four Maskína rows with an
  # unpublished field period carry estimated dates (flagged in data_sources.md)
  polls_wide <- tribble(
    ~date,         ~fyrirtaeki, ~n_total,  ~S,    ~D,    ~B,    ~C,    ~M,    ~F,    ~P,    ~V,    ~J,
    # 2024 election result (Nov 30, 2024) — official results
    "2024-11-30",  "Kosning",   212470,    20.75, 19.36, 7.80,  15.82, 12.10, 13.78, 3.00,  2.30,  4.00,
    # Gallup polls (ruv.is Þjóðarpúls) — exact percentages
    "2024-12-24",  "Gallup",    3460,      21.4,  20.1,  6.3,   13.8,  12.4,  13.1,  3.1,   2.1,   6.0,
    "2025-01-17",  "Gallup",    10908,     21.7,  20.5,  6.7,   16.2,  12.7,  10.6,  3.5,   2.2,   5.2,
    "2025-02-16",  "Gallup",    9652,      26.0,  21.5,  6.3,   14.1,  10.1,  8.3,   3.6,   3.1,   6.2,
    "2025-03-17",  "Gallup",    10324,     27.0,  22.4,  5.7,   14.6,  9.3,   7.7,   4.0,   3.3,   5.4,
    "2025-04-15",  "Gallup",    10005,     29.4,  22.3,  6.1,   13.9,  8.9,   7.4,   3.2,   3.3,   4.7,
    "2025-05-16",  "Gallup",    11521,     30.7,  21.7,  5.5,   14.4,  9.1,   7.5,   3.3,   3.6,   3.5,
    "2025-06-16",  "Gallup",    10216,     31.8,  20.6,  5.6,   13.7,  10.7,  6.5,   4.1,   3.2,   3.3,
    "2025-07-16",  "Gallup",    11541,     34.7,  18.7,  4.9,   14.6,  10.5,  6.7,   3.5,   3.4,   2.4,
    "2025-08-16",  "Gallup",    10055,     34.6,  19.7,  4.5,   12.9,  10.7,  7.4,   3.5,   3.7,   1.9,
    "2025-09-15",  "Gallup",    10887,     34.0,  19.5,  5.8,   12.6,  11.8,  6.9,   2.9,   3.6,   2.1,
    "2025-10-17",  "Gallup",    11225,     31.9,  17.6,  5.5,   13.5,  16.3,  5.9,   3.9,   2.6,   2.3,
    "2025-11-16",  "Gallup",    10332,     31.1,  16.5,  5.6,   12.8,  19.5,  5.2,   3.3,   3.2,   2.3,
    "2025-12-15",  "Gallup",    9091,      30.9,  16.9,  5.2,   10.9,  21.7,  5.5,   3.4,   3.6,   1.6,
    "2026-01-20",  "Gallup",    9713,      31.2,  17.1,  5.4,   11.3,  20.8,  5.3,   3.4,   3.1,   1.9,
    "2026-02-15",  "Gallup",    9958,      28.5,  18.1,  6.6,   10.9,  20.3,  5.8,   2.7,   4.2,   2.5,
    "2026-03-16",  "Gallup",    10746,     30.0,  19.5,  4.7,   10.0,  20.6,  5.8,   2.8,   4.3,   1.9,
    "2026-04-15",  "Gallup",    10484,     28.5,  21.4,  5.4,   10.6,  18.9,  5.2,   2.1,   5.1,   2.3,
    "2026-05-15",  "Gallup",    12979,     28.4,  23.5,  6.7,   10.6,  17.6,  4.0,   1.9,   4.3,   2.6,
    "2026-06-15",  "Gallup",    12102,     26.2,  24.9,  5.3,   11.4,  15.1,  4.6,   2.7,   5.0,   4.3,
    "2026-07-17",  "Gallup",    13167,     27.9,  25.4,  5.8,   11.4,  14.6,  5.2,   2.2,   3.9,   3.2,
    "2026-08-17",  "Gallup",    13836,     29.6,  26.8,  5.5,   9.7,   15.6,  4.8,   1.9,   3.6,   2.4,
    "2026-09-14",  "Gallup",    10485,     30.8,  24.7,  5.9,   10.6,  15.0,  4.0,   2.7,   3.7,   2.4,
    "2026-10-15",  "Gallup",    12906,     29.2,  24.9,  6.1,   10.0,  17.2,  3.9,   2.2,   3.5,   2.8,
    # Maskína polls — reconciled against Maskínuskýrsla PDFs and the maskina.is dashboard; provenance in data_sources.md
    "2024-12-12",  "Maskína",   2803,      23.1,  16.3,  8.4,   16.5,  9.0,   10.6,  5.2,   3.8,   6.0,
    "2025-01-11",  "Maskína",   966,       22.2,  19.3,  7.2,   14.0,  11.6,  12.9,  3.6,   3.1,   4.1,
    "2025-02-19",  "Maskína",   2899,      21.9,  21.4,  7.3,   14.9,  11.5,  9.1,   3.2,   2.8,   5.5,
    "2025-03-12",  "Maskína",   1899,      23.3,  24.3,  6.8,   14.8,  10.9,  8.5,   3.1,   3.3,   4.9,
    "2025-04-15",  "Maskína",   1453,      26.2,  20.9,  7.2,   15.8,  10.3,  7.9,   3.9,   2.9,   4.9,
    "2025-05-15",  "Maskína",   1962,      27.4,  18.9,  6.8,   16.8,  9.7,   7.2,   4.6,   3.6,   5.0,
    "2025-06-22",  "Maskína",   876,       28.1,  17.3,  7.0,   15.3,  13.0,  6.6,   4.6,   3.7,   4.4,
    "2025-07-13",  "Maskína",   1855,      31.2,  18.0,  6.8,   16.2,  9.9,   6.6,   5.0,   3.4,   2.9,
    "2025-08-14",  "Maskína",   2228,      31.6,  18.6,  6.5,   16.1,  9.6,   6.3,   4.5,   4.2,   2.6,
    "2025-09-11",  "Maskína",   1713,      31.9,  18.6,  6.3,   14.3,  9.1,   6.3,   5.8,   4.1,   3.5,
    "2025-10-09",  "Maskína",   1765,      29.4,  15.9,  6.5,   16.1,  13.8,  6.3,   4.9,   4.0,   3.0,
    "2025-11-10",  "Maskína",   1741,      29.1,  15.3,  6.6,   13.5,  17.3,  5.6,   4.9,   4.8,   2.8,
    "2025-12-08",  "Maskína",   1893,      28.9,  15.1,  6.6,   13.3,  19.2,  4.8,   4.7,   4.3,   3.1,
    "2026-01-11",  "Maskína",   886,       27.0,  13.5,  7.1,   14.1,  22.2,  4.3,   4.1,   3.7,   4.1,
    "2026-02-10",  "Maskína",   1993,      27.2,  16.2,  7.0,   13.4,  19.0,  4.8,   5.2,   4.1,   3.1,
    "2026-03-08",  "Maskína",   2617,      25.5,  16.1,  7.1,   14.0,  18.4,  5.8,   5.0,   4.4,   3.5,
    "2026-04-05",  "Maskína",   1786,      27.7,  18.1,  7.2,   12.8,  16.4,  5.8,   4.7,   4.3,   3.0,
    "2026-04-29",  "Maskína",   2810,      25.3,  19.8,  7.8,   14.4,  15.0,  4.7,   4.1,   5.3,   3.7,
    "2026-06-06",  "Maskína",   1705,      25.2,  22.7,  8.5,   12.4,  14.2,  4.1,   3.5,   5.4,   3.9,
    "2026-07-02",  "Maskína",   963,       24.4,  25.0,  6.6,   12.7,  13.4,  5.9,   3.1,   4.9,   4.0,
    "2026-08-01",  "Maskína",   3172,      26.2,  25.1,  7.8,   11.5,  12.7,  5.2,   3.5,   4.2,   3.9,
    "2026-09-02",  "Maskína",   4544,      27.5,  25.0,  7.5,   11.9,  13.0,  4.3,   3.3,   4.5,   3.0,
    "2026-09-27",  "Maskína",   1791,      28.6,  21.9,  7.9,   11.4,  13.4,  4.6,   3.3,   4.8,   4.1
  )

  parties <- party_code_to_name()

  polls_wide |>
    mutate(
      date = as.Date(date),
      Other = pmax(0, 100 - S - D - B - C - M - F - P - V - J)
    ) |>
    pivot_longer(
      cols = c(S, D, B, C, M, F, P, V, J, Other),
      names_to = "bokstafur",
      values_to = "pct"
    ) |>
    left_join(parties, by = "bokstafur") |>
    mutate(
      n = pct / 100 * n_total,
      lokadagur = NA_character_,
      p = pct / 100
    ) |>
    select(date, fyrirtaeki, flokkur, n, lokadagur, p) |>
    arrange(date, fyrirtaeki, flokkur)
}

#### Hardcoded post-election kjördæmi polls ####
# The 2024 kjördæmi result and Gallup's monthly Þjóðarpúls kjördæmi shares as RÚV and mbl.is
# print them (mostly Suður and Norðaustur); provenance in data_sources.md. Nothing reads the
# CSV this writes yet.

get_hardcoded_kjordaemi_polls <- function() {
  # One row per poll (or the result) and kjördæmi (%), each poll dated as the national Gallup
  # poll above. n_kd is the result's valid votes or the poll's printed kjördæmi n; where none
  # is printed ("estimated") it is the national poll's n x the kjördæmi's share of the 2024
  # valid votes x 0.334, the median printed/estimated ratio of Gallup's post-2024 kjördæmi
  # polls. A party the outlet does not print is 0, which the model builders read as
  # unreported.
  kd_wide <- tribble(
    ~date,         ~fyrirtaeki, ~kjordaemi,          ~n_kd,   ~n_basis,     ~S,    ~D,    ~B,    ~C,    ~M,    ~F,    ~P,    ~V,    ~J,    ~Other,
    # 2024 election result by kjördæmi (Nov 30, 2024): official valid votes, shares to 0.1 pp
    "2024-11-30",  "Kosning", "Reykjavík Norður",  37020,   "official",   26.1,  17.4,  4.0,   16.3,  8.9,   11.9,  5.4,   2.9,   5.9,   1.2,
    "2024-11-30",  "Kosning", "Reykjavík Suður",   38062,   "official",   22.9,  17.6,  4.4,   17.7,  10.5,  13.5,  3.9,   2.9,   5.6,   1.0,
    "2024-11-30",  "Kosning", "Suðvestur",         62453,   "official",   19.3,  23.4,  5.9,   20.1,  12.0,  11.0,  2.8,   1.5,   2.8,   1.2,
    "2024-11-30",  "Kosning", "Norðvestur",        18141,   "official",   15.9,  18.0,  13.3,  12.6,  14.8,  16.7,  1.8,   2.7,   3.4,   0.8,
    "2024-11-30",  "Kosning", "Norðaustur",        24202,   "official",   21.3,  15.0,  14.2,  9.4,   15.7,  14.3,  1.8,   3.8,   3.8,   0.7,
    "2024-11-30",  "Kosning", "Suður",             31981,   "official",   17.3,  19.6,  12.0,  11.2,  13.6,  20.0,  1.3,   1.3,   2.4,   1.3,
    # Gallup monthly Þjóðarpúls kjördæmi breakdowns
    "2025-01-17",  "Gallup",  "Suður",             681,     "printed",    18.6,  21.1,  12.0,  12.0,  13.7,  15.2,  2.6,   1.8,   2.0,   0.8,
    "2025-02-16",  "Gallup",  "Suður",             482.8,   "estimated",  18.0,  28.9,  11.0,  9.6,   10.9,  13.7,  3.1,   1.4,   2.2,   1.2,
    "2025-03-17",  "Gallup",  "Norðaustur",        394.9,   "estimated",  26.7,  16.6,  10.5,  8.3,   13.7,  9.5,   2.4,   5.5,   5.6,   1.1,
    "2025-03-17",  "Gallup",  "Suður",             631,     "printed",    25.7,  23.4,  11.3,  9.3,   11.0,  13.4,  1.9,   1.5,   2.1,   0.5,
    "2025-05-16",  "Gallup",  "Suður",             569,     "printed",    24.1,  25.3,  10.4,  9.3,   12.5,  12.9,  1.3,   1.6,   2.0,   0.7,
    "2025-06-16",  "Gallup",  "Reykjavík Norður",  594.4,   "estimated",  35.0,  17.5,  2.9,   12.8,  8.0,   6.4,   5.8,   5.4,   0.0,   0.0,
    "2025-06-16",  "Gallup",  "Reykjavík Suður",   598.2,   "estimated",  32.9,  20.1,  3.4,   14.4,  7.9,   4.5,   6.6,   4.3,   0.0,   0.0,
    "2025-06-16",  "Gallup",  "Suðvestur",         1027.0,  "estimated",  30.6,  21.9,  2.4,   18.8,  11.2,  4.8,   0.0,   0.0,   0.0,   0.0,
    "2025-06-16",  "Gallup",  "Norðvestur",        290.2,   "estimated",  26.9,  23.9,  13.8,  10.8,  10.6,  8.4,   0.0,   0.0,   0.0,   0.0,
    "2025-06-16",  "Gallup",  "Norðaustur",        390.7,   "estimated",  36.3,  17.2,  7.4,   9.0,   15.4,  7.4,   0.0,   0.0,   0.0,   0.0,
    "2025-06-16",  "Gallup",  "Suður",             511.0,   "estimated",  28.5,  22.9,  11.6,  8.9,   13.1,  11.1,  0.0,   0.0,   0.0,   0.0,
    "2025-07-16",  "Gallup",  "Norðaustur",        440,     "printed",    37.5,  14.9,  7.5,   8.1,   13.8,  9.6,   0.9,   3.9,   3.3,   0.0,
    "2025-07-16",  "Gallup",  "Suður",             606,     "printed",    25.7,  24.8,  8.2,   8.9,   11.5,  14.7,  0.0,   0.0,   0.0,   0.0,
    "2025-09-15",  "Gallup",  "Suður",             583,     "printed",    25.6,  24.7,  9.4,   8.2,   11.8,  13.6,  0.0,   0.0,   0.0,   0.0,
    "2025-10-17",  "Gallup",  "Suður",             652,     "printed",    24.8,  23.5,  5.8,   8.8,   21.9,  9.9,   0.0,   0.0,   0.0,   0.0,
    "2025-11-16",  "Gallup",  "Suður",             533,     "printed",    25.5,  20.6,  6.6,   6.2,   30.8,  5.9,   0.0,   0.0,   0.0,   0.0,
    "2025-12-15",  "Gallup",  "Norðaustur",        347.7,   "estimated",  29.0,  12.0,  12.5,  3.8,   25.1,  9.6,   1.4,   5.9,   0.3,   0.5,
    "2025-12-15",  "Gallup",  "Suður",             476,     "printed",    26.9,  22.1,  11.3,  5.3,   24.4,  6.2,   0.0,   0.0,   0.0,   0.0,
    "2026-01-20",  "Gallup",  "Norðaustur",        371.5,   "estimated",  23.4,  13.6,  14.4,  3.7,   27.7,  8.1,   1.5,   5.2,   2.2,   0.2,
    "2026-01-20",  "Gallup",  "Suður",             515,     "printed",    26.4,  24.1,  7.5,   7.7,   22.6,  6.7,   0.0,   0.0,   0.0,   0.0,
    "2026-02-15",  "Gallup",  "Suður",             536,     "printed",    20.7,  24.0,  9.4,   6.0,   27.0,  7.7,   1.2,   2.5,   0.0,   0.0,
    "2026-03-16",  "Gallup",  "Norðaustur",        411,     "printed",    32.4,  17.0,  7.0,   6.7,   21.3,  7.5,   1.1,   5.7,   0.4,   0.9,
    "2026-03-16",  "Gallup",  "Suður",             546,     "printed",    22.3,  24.2,  6.9,   5.3,   25.4,  7.9,   2.0,   4.6,   0.7,   0.7,
    "2026-04-15",  "Gallup",  "Norðaustur",        360,     "printed",    33.7,  15.7,  0.0,   3.7,   15.9,  10.0,  0.2,   8.2,   1.7,   0.0,
    "2026-04-15",  "Gallup",  "Suður",             524.5,   "estimated",  17.3,  29.3,  7.8,   7.8,   23.8,  8.2,   1.7,   3.2,   0.3,   0.6,
    "2026-05-15",  "Gallup",  "Norðaustur",        471,     "printed",    28.6,  15.1,  12.5,  8.2,   21.4,  3.1,   1.7,   6.9,   2.3,   0.2,
    "2026-05-15",  "Gallup",  "Suður",             685,     "printed",    21.1,  30.2,  9.4,   5.7,   23.7,  4.5,   0.5,   2.9,   1.9,   0.1,
    "2026-06-15",  "Gallup",  "Norðaustur",        432,     "printed",    26.0,  14.7,  12.2,  4.8,   20.8,  6.6,   2.4,   9.4,   3.0,   0.1,
    "2026-06-15",  "Gallup",  "Suður",             562,     "printed",    18.4,  30.5,  7.7,   6.9,   21.6,  7.2,   2.1,   3.3,   2.1,   0.2,
    "2026-07-17",  "Gallup",  "Suður",             623,     "printed",    22.8,  30.3,  8.4,   6.8,   19.5,  5.2,   1.3,   2.9,   2.5,   0.2,
    "2026-08-17",  "Gallup",  "Norðaustur",        491,     "printed",    26.1,  19.3,  10.8,  4.1,   22.2,  6.7,   1.7,   5.9,   3.2,   0.0,
    "2026-08-17",  "Gallup",  "Suður",             692,     "printed",    25.5,  33.1,  8.0,   4.8,   17.8,  5.4,   0.0,   2.9,   0.0,   0.0,
    "2026-09-14",  "Gallup",  "Norðaustur",        361,     "printed",    34.6,  15.8,  11.2,  4.4,   18.8,  5.5,   2.9,   5.9,   0.8,   0.1,
    "2026-09-14",  "Gallup",  "Suður",             505,     "printed",    17.9,  31.1,  8.2,   5.5,   24.7,  7.0,   1.0,   2.8,   1.2,   0.6
  )

  parties <- party_code_to_name()

  kd_wide |>
    mutate(date = as.Date(date)) |>
    pivot_longer(
      cols = c(S, D, B, C, M, F, P, V, J, Other),
      names_to = "bokstafur",
      values_to = "pct"
    ) |>
    left_join(parties, by = "bokstafur") |>
    mutate(
      n = pct / 100 * n_kd,
      lokadagur = NA_character_,
      p = pct / 100
    ) |>
    select(date, fyrirtaeki, flokkur, kjordaemi, n, lokadagur, p, n_basis) |>
    arrange(date, fyrirtaeki, kjordaemi, flokkur)
}


#### Scraping: Gallup from ruv.is ####

get_gallup_article_urls <- function() {
  page <- read_html("https://www.ruv.is/frettir/tag/thjodarpuls-gallup")

  links <- page |>
    html_elements("a[href*='/frettir/innlent/']") |>
    html_attr("href") |>
    unique()

  # Convert relative URLs to absolute
  links <- ifelse(
    str_starts(links, "http"),
    links,
    paste0("https://www.ruv.is", links)
  )

  tibble(url = links) |>
    mutate(
      date_str = str_extract(url, "\\d{4}-\\d{2}-\\d{2}"),
      date = as.Date(date_str)
    ) |>
    filter(!is.na(date)) |>
    arrange(desc(date))
}

parse_gallup_article <- function(url) {
  tryCatch(
    {
      page <- read_html(url)

      text <- page |>
        html_elements("article, .article-body, .news-single__body, main") |>
        html_text2() |>
        paste(collapse = " ")

      if (nchar(text) < 200) {
        text <- page |> html_text2()
      }

      results <- tibble(bokstafur = character(), pct = numeric())

      for (code in names(party_stems)) {
        stem <- party_stems[code]
        # Party name followed by percentage within ~150 chars
        pattern <- paste0("(?i)(", stem, ").{0,150}?(\\d+[,.]\\d+)\\s*%")
        m <- str_match(text, pattern)

        if (!is.na(m[1, 3])) {
          pct <- str_replace(m[1, 3], ",", ".") |> as.numeric()
          if (pct > 0 & pct < 60) {
            results <- bind_rows(results, tibble(bokstafur = code, pct = pct))
          }
        }
      }

      # Extract sample size
      n_match <- str_match(
        text,
        "(?i)(?:fjöldi\\s+svarenda|svarendur|úrtak)[^\\d]{0,50}?(\\d[\\d\\.]*\\d)"
      )
      n_total <- if (!is.na(n_match[1, 2])) {
        str_remove_all(n_match[1, 2], "\\.") |> as.numeric()
      } else {
        NA_real_
      }

      # Extract collection dates
      date_match <- str_match(
        text,
        "(?i)(\\d+)\\.\\s*(\\w+)\\s*(\\d{4})\\s*(?:til|-)\\s*(\\d+)\\.\\s*(\\w+)\\s*(\\d{4})"
      )

      if (nrow(results) >= 7) {
        list(
          results = results,
          n_total = n_total,
          url = url
        )
      } else {
        NULL
      }
    },
    error = function(e) {
      message("Error fetching ", url, ": ", e$message)
      NULL
    }
  )
}

scrape_gallup <- function(existing_dates = NULL) {
  message("Fetching Gallup article list from ruv.is...")
  articles <- get_gallup_article_urls()

  if (!is.null(existing_dates)) {
    articles <- articles |>
      filter(!date %in% existing_dates)
  }

  if (nrow(articles) == 0) {
    message("No new Gallup articles found.")
    return(tibble())
  }

  message("Found ", nrow(articles), " articles to process.")

  parties <- party_code_to_name()
  all_polls <- tibble()

  for (i in seq_len(nrow(articles))) {
    message("  Parsing: ", articles$url[i])
    parsed <- parse_gallup_article(articles$url[i])

    if (!is.null(parsed)) {
      poll <- parsed$results |>
        left_join(parties, by = "bokstafur") |>
        mutate(
          date = articles$date[i],
          fyrirtaeki = "Gallup",
          n_total = if (!is.na(parsed$n_total)) parsed$n_total else 10000L,
          n = pct / 100 * n_total
        )

      # Add Annað for remainder
      remainder <- max(0, 100 - sum(poll$pct))
      if (remainder > 0) {
        poll <- bind_rows(poll, tibble(
          bokstafur = "Other", flokkur = "Annað",
          date = articles$date[i], fyrirtaeki = "Gallup",
          pct = remainder,
          n_total = poll$n_total[1],
          n = remainder / 100 * poll$n_total[1]
        ))
      }

      all_polls <- bind_rows(all_polls, poll)
      message("    -> Found ", nrow(parsed$results), " parties, n=", parsed$n_total)
    } else {
      message("    -> Could not parse article")
    }

    Sys.sleep(1) # Be polite
  }

  if (nrow(all_polls) > 0) {
    all_polls |>
      mutate(lokadagur = NA_character_, p = pct / 100) |>
      select(date, fyrirtaeki, flokkur, n, lokadagur, p)
  } else {
    tibble()
  }
}


#### Scraping: Maskína from visir.is ####

get_maskina_article_urls <- function() {
  page <- read_html("https://www.visir.is/t/2296")

  links <- page |>
    html_elements("a[href*='/g/']") |>
    html_attr("href") |>
    unique()

  links <- ifelse(
    str_starts(links, "http"),
    links,
    paste0("https://www.visir.is", links)
  )

  # Filter for articles mentioning Maskína in slug
  mask_links <- links[str_detect(links, "(?i)mask|konnun")]

  tibble(url = mask_links) |>
    mutate(
      # Extract year from URL pattern /g/YYYYNNNNNNN/
      year_prefix = str_extract(url, "/g/(\\d{4})", group = 1)
    ) |>
    filter(!is.na(year_prefix))
}

parse_maskina_article <- function(url) {
  tryCatch(
    {
      page <- read_html(url)

      text <- page |>
        html_elements("article, .article-body, main, .article__body") |>
        html_text2() |>
        paste(collapse = " ")

      if (nchar(text) < 200) {
        text <- page |> html_text2()
      }

      # Check this is actually a Maskína national poll (not Reykjavik municipal)
      if (str_detect(text, "(?i)borgarstj|reykjav.k.rborg|sveitarstj")) {
        return(NULL)
      }

      results <- tibble(bokstafur = character(), pct = numeric())

      for (code in names(party_stems)) {
        stem <- party_stems[code]
        pattern <- paste0("(?i)(", stem, ").{0,150}?(\\d+[,.]\\d+)\\s*%")
        m <- str_match(text, pattern)

        if (!is.na(m[1, 3])) {
          pct <- str_replace(m[1, 3], ",", ".") |> as.numeric()
          if (pct > 0 & pct < 60) {
            results <- bind_rows(results, tibble(bokstafur = code, pct = pct))
          }
        }
      }

      # Extract sample size
      n_match <- str_match(
        text,
        "(?i)(?:fjöldi\\s+svarenda|svarendur|svöruðu|þátttakend)[^\\d]{0,50}?(\\d[\\d\\.]*\\d)"
      )
      n_total <- if (!is.na(n_match[1, 2])) {
        str_remove_all(n_match[1, 2], "\\.") |> as.numeric()
      } else {
        NA_real_
      }

      if (nrow(results) >= 5) {
        list(
          results = results,
          n_total = n_total,
          url = url
        )
      } else {
        NULL
      }
    },
    error = function(e) {
      message("Error fetching ", url, ": ", e$message)
      NULL
    }
  )
}

scrape_maskina <- function(existing_dates = NULL) {
  message("Fetching Maskína article list from visir.is...")
  articles <- get_maskina_article_urls()

  if (nrow(articles) == 0) {
    message("No Maskína articles found.")
    return(tibble())
  }

  message("Found ", nrow(articles), " candidate articles to process.")

  parties <- party_code_to_name()
  all_polls <- tibble()

  for (i in seq_len(nrow(articles))) {
    message("  Parsing: ", articles$url[i])
    parsed <- parse_maskina_article(articles$url[i])

    if (!is.null(parsed)) {
      n_est <- if (!is.na(parsed$n_total)) parsed$n_total else 1500L

      poll <- parsed$results |>
        left_join(parties, by = "bokstafur") |>
        mutate(
          date = Sys.Date(), # Placeholder — user should verify
          fyrirtaeki = "Maskína",
          n_total = n_est,
          n = pct / 100 * n_total
        )

      remainder <- max(0, 100 - sum(poll$pct))
      if (remainder > 0) {
        poll <- bind_rows(poll, tibble(
          bokstafur = "Other", flokkur = "Annað",
          date = Sys.Date(), fyrirtaeki = "Maskína",
          pct = remainder, n_total = n_est,
          n = remainder / 100 * n_est
        ))
      }

      all_polls <- bind_rows(all_polls, poll)
      message("    -> Found ", nrow(parsed$results), " parties, n=", parsed$n_total)
      message(
        "    -> Percentages: ",
        paste(parsed$results$bokstafur, "=", parsed$results$pct, collapse = ", ")
      )
    } else {
      message("    -> Could not parse / not a national poll")
    }

    Sys.sleep(1)
  }

  if (nrow(all_polls) > 0) {
    all_polls |>
      mutate(lokadagur = NA_character_, p = pct / 100) |>
      select(date, fyrirtaeki, flokkur, n, lokadagur, p)
  } else {
    tibble()
  }
}


#### Main update function ####

update_post_election_polls <- function(scrape = FALSE) {
  # Start with hardcoded data
  polls <- get_hardcoded_polls()
  message("Loaded ", length(unique(paste(polls$date, polls$fyrirtaeki))), " hardcoded polls.")

  if (scrape) {
    existing_dates <- unique(polls$date)

    gallup_new <- scrape_gallup(existing_dates)
    if (nrow(gallup_new) > 0) {
      message("Scraped ", length(unique(gallup_new$date)), " new Gallup polls.")
      polls <- bind_rows(polls, gallup_new)
    }

    maskina_new <- scrape_maskina(existing_dates)
    if (nrow(maskina_new) > 0) {
      message("Scraped Maskína data (review dates manually).")
      polls <- bind_rows(polls, maskina_new)
    }
  }

  # Save
  output_path <- here("data", "post_election_polls.csv")
  write_csv(polls, output_path)
  message("Saved ", nrow(polls), " rows to ", output_path)

  kd <- get_hardcoded_kjordaemi_polls()
  kd_path <- here("data", "post_election_polls_kjordaemi.csv")
  write_csv(kd, kd_path)
  message("Saved ", nrow(kd), " rows to ", kd_path)

  invisible(polls)
}


#### Generate CSV when run as script ####

update_post_election_polls(scrape = FALSE)
