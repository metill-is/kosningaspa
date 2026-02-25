library(tidyverse)
library(rvest)
library(here)
library(clock)

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
# Maskína data from visir.is: approximate where noted
# 2024 election result from althingi.is / electionguide.org

get_hardcoded_polls <- function() {
  # Wide format: each row is one poll with party abbreviation columns (%)
  # Date is midpoint of collection period
  polls_wide <- tribble(
    ~date,         ~fyrirtaeki, ~n_total,  ~S,    ~D,    ~B,    ~C,    ~M,    ~F,    ~P,    ~V,    ~J,
    # 2024 election result (Nov 30, 2024) — official results
    "2024-11-30",  "Kosning",   212470,    20.75, 19.36, 7.80,  15.82, 12.10, 13.78, 3.00,  2.30,  4.00,
    # Gallup polls (ruv.is Þjóðarpúls) — exact percentages
    "2024-12-24",  "Gallup",    3460,      21.4,  20.1,  6.3,   13.8,  12.4,  13.1,  3.1,   2.1,   6.0,
    "2025-02-16",  "Gallup",    9652,      26.0,  21.5,  6.3,   14.1,  10.1,  8.3,   3.6,   3.1,   6.2,
    "2025-03-17",  "Gallup",    10324,     27.0,  22.4,  5.7,   14.6,  9.3,   7.7,   4.0,   3.3,   5.4,
    "2025-04-15",  "Gallup",    10005,     29.4,  22.3,  6.1,   13.9,  8.9,   7.4,   3.2,   3.3,   4.7,
    "2025-05-16",  "Gallup",    11521,     30.7,  21.7,  5.5,   14.4,  9.1,   7.5,   3.3,   3.6,   3.5,
    "2025-06-16",  "Gallup",    10500,     31.8,  20.6,  5.6,   13.7,  10.7,  6.5,   4.1,   3.2,   3.3,
    "2025-07-16",  "Gallup",    11541,     34.7,  18.7,  4.9,   14.6,  10.5,  6.7,   3.5,   3.4,   2.4,
    "2025-08-16",  "Gallup",    10055,     34.6,  19.7,  4.5,   12.9,  10.7,  7.4,   3.5,   3.7,   1.9,
    "2025-09-15",  "Gallup",    10887,     34.0,  19.5,  5.8,   12.6,  11.8,  6.9,   2.9,   3.6,   2.1,
    "2025-11-16",  "Gallup",    10332,     31.1,  16.5,  5.6,   12.8,  19.5,  5.2,   3.3,   3.2,   2.3,
    "2025-12-15",  "Gallup",    10000,     30.9,  16.8,  5.2,   10.9,  21.7,  5.5,   3.4,   3.6,   1.6,
    "2026-01-15",  "Gallup",    10000,     31.2,  17.1,  5.4,   11.3,  20.8,  5.3,   3.4,   3.1,   1.9,
    # Maskína polls (visir.is) — approximate values noted in comments
    "2024-12-12",  "Maskína",   2803,      23.0,  16.0,  8.0,   16.0,  9.0,   11.0,  5.0,   4.0,   6.0,
    "2025-10-09",  "Maskína",   1765,      29.0,  16.0,  6.0,   16.0,  14.0,  6.0,   5.0,   4.0,   3.0,
    "2025-11-10",  "Maskína",   1500,      29.0,  15.0,  7.0,   13.0,  17.0,  5.6,   5.0,   5.0,   3.0,
    "2026-01-11",  "Maskína",   886,       27.0,  13.5,  7.1,   14.1,  22.2,  4.3,   4.1,   3.7,   4.1,
    "2026-02-24",  "Maskína",   1993,      27.2,  16.2,  7.0,   13.4,  19.0,  4.8,   5.2,   4.1,   3.1
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
  tryCatch({
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
  }, error = function(e) {
    message("Error fetching ", url, ": ", e$message)
    NULL
  })
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
  tryCatch({
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
  }, error = function(e) {
    message("Error fetching ", url, ": ", e$message)
    NULL
  })
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
      message("    -> Percentages: ",
              paste(parsed$results$bokstafur, "=", parsed$results$pct, collapse = ", "))
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

  invisible(polls)
}


#### Generate CSV when run as script ####

update_post_election_polls(scrape = FALSE)
