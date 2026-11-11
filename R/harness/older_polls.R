# The older polls table, harness/older_polls.csv: national polls of July 2012 to December
# 2015 that the snapshot lacks, built by harness/sources/older_polls/build.py. Rows with a
# `replaces` key are replacement polls, a group of their own.

#' The houses the older polls table may hold, labelled as in the data.
#' @export
OLDER_POLL_HOUSES <- c("Gallup", "MMR", "Fr\u00e9ttabla\u00f0i\u00f0, St\u00f6\u00f0 2 og V\u00edsir", "F\u00e9lagsv\u00edsindastofnun")

#' The columns of the older polls table, in file order.
#' @export
OLDER_POLL_COLUMNS <- c(
  "date", "fyrirtaeki", "flokkur", "n", "share", "n_total", "n_source", "fieldwork_start",
  "fieldwork_end", "published", "poll_id", "pins", "flags", "replaces"
)

#' Read the older polls table strictly.
#' @param path the CSV
#' @return a tibble with `OLDER_POLL_COLUMNS`: dates as Date (`fieldwork_start`,
#'   `fieldwork_end`, `published` and `replaces` may be NA), `n`, `share` and `n_total` as
#'   double (`share` NA for a party the poll did not report), the rest character (`flags` NA
#'   when empty). An error on other columns, a value that does not parse, a missing value
#'   where one is required, a house outside `OLDER_POLL_HOUSES`, a party outside
#'   `POLL_PARTIES`, an `n_source` other than published, assumed or snapshot, a snapshot size
#'   on a poll with no `replaces` key, a negative count, a share outside 0 to 100, a count
#'   above 0 with no share, or a poll that does not have one row for each poll party and one
#'   date, house, size and `replaces` key
#' @export
read_older_polls <- function(path = harness_path("older_polls.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path, POLL_PARTIES])
  what <- basename(path)
  header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
  if (!identical(header, OLDER_POLL_COLUMNS)) {
    stop(what, ": columns differ from OLDER_POLL_COLUMNS: ", paste(header, collapse = ", "))
  }
  date <- readr::col_date("%Y-%m-%d")
  x <- readr::read_csv(path,
    na = "", col_types = readr::cols(
      date = date, fyrirtaeki = "c", flokkur = "c", n = "d", share = "d", n_total = "d",
      n_source = "c", fieldwork_start = date, fieldwork_end = date, published = date,
      poll_id = "c", pins = "c", flags = "c", replaces = date
    )
  )
  bad <- readr::problems(x)
  if (nrow(bad)) {
    stop(what, ": values that do not parse, first at row ", bad$row[1], " column ", bad$col[1])
  }
  required <- c("date", "fyrirtaeki", "flokkur", "n", "n_total", "n_source", "poll_id", "pins")
  gaps <- required[vapply(required, function(col) anyNA(x[[col]]), logical(1))]
  if (length(gaps)) stop(what, ": missing values in ", paste(gaps, collapse = ", "))
  checks <- c(
    "a house outside OLDER_POLL_HOUSES" = !all(x$fyrirtaeki %in% OLDER_POLL_HOUSES),
    "a party outside POLL_PARTIES" = !all(x$flokkur %in% POLL_PARTIES),
    "an n_source other than published, assumed or snapshot" = !all(x$n_source %in% c("published", "assumed", "snapshot")),
    "a snapshot size on a poll with no replaces key" = any(x$n_source == "snapshot" & is.na(x$replaces)),
    "a negative count" = any(x$n < 0),
    "a size that is not positive" = any(x$n_total <= 0),
    "a share outside 0 to 100" = any(x$share < 0 | x$share > 100, na.rm = TRUE),
    "a count above 0 with no share" = any(is.na(x$share) & x$n > 0)
  )
  if (any(checks)) stop(what, ": ", paste(names(checks)[checks], collapse = "; "))
  per_poll <- x |>
    dplyr::group_by(poll_id) |>
    dplyr::summarise(
      parties = identical(sort(flokkur), sort(POLL_PARTIES)),
      one = dplyr::n_distinct(date) == 1 && dplyr::n_distinct(fyrirtaeki) == 1 &&
        dplyr::n_distinct(n_total) == 1 && dplyr::n_distinct(replaces) == 1,
      .groups = "drop"
    )
  odd <- per_poll$poll_id[!per_poll$parties | !per_poll$one]
  if (length(odd)) {
    stop(what, ": polls without one row per poll party and one date, house, size and key: ", paste(odd, collapse = ", "))
  }
  x
}
