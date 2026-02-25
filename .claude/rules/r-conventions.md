---
paths:
  - "R/**/*.R"
---

# R Code Conventions

- Use `box::use()` for imports in modules, not `source()` or `library()`. Exception: interactive fitting scripts can use `library()` for heavy deps.
- Export functions with `#' @export` roxygen tags.
- Follow tidyverse style: snake_case, pipe operator `|>` (base pipe preferred over `%>%`).
- Use `here::here()` for file paths — never hardcode absolute paths.
- Icelandic variable names are standard: `flokkur` (party), `kjordaemi` (constituency), `fyrirtaeki` (firm), `bokstafur` (party letter), `saeti` (seats), `fylgi` (support).
- Use `arrow::read_parquet()` / `arrow::write_parquet()` for model output data, CSV for raw input data.
- Visualization scripts use `metill::theme_metill()` as the base ggplot theme.
- When adding a new poll, add it to `get_hardcoded_polls()` in `R/scrape_polls.R`.
