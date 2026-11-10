# Seat draws from the forecast's constituency vote draws.
# Script use, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/predict_seats.R data/<date>
# reads <dir>/constituency_predictions.parquet and writes <dir>/seats_draws.parquet.
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("predict_seats: needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  if (!file.exists("R/election_utils.R")) stop("predict_seats: run from the project root", call. = FALSE)
  options(box.path = normalizePath("."))
}
box::use(R / election_utils[calculate_seats, seats_tibble])

#' Allocate the 63 seats in every draw of a constituency vote forecast
#'
#' Calls `calculate_seats()` once per draw. The residual category wins no seat, but its
#' votes count in the 5% threshold and in the shares that place the adjustment seats.
#'
#' @param draws A tibble with columns `.draw`, `kjordaemi`, `flokkur` and `value` (the
#'   draw's votes), as the fit script writes to `constituency_predictions.parquet`; other
#'   columns are ignored.
#' @param layout The seat layout (`seats_tibble()`, the layout in force since 2024).
#' @param ineligible Lists that can win no seat; the residual category by default.
#' @return A tibble with one row per draw, list and kjordaemi: `.draw`, `flokkur`,
#'   `kjordaemi`, `seats` (all seats) and `kjordaemi_seats` (seats before adjustment).
#' @export
predict_seats <- function(draws, layout = seats_tibble(), ineligible = "Anna\u00f0") {
  per_draw <- split(draws[c("flokkur", "kjordaemi", "value")], draws$.draw)
  out <- lapply(names(per_draw), \(d) {
    draw <- per_draw[[d]]
    names(draw)[names(draw) == "value"] <- "votes"
    s <- calculate_seats(draw, layout, ineligible)
    tibble::tibble(.draw = as.integer(d), s[c("flokkur", "kjordaemi", "seats", "kjordaemi_seats")])
  })
  dplyr::bind_rows(out)
}

main <- function(args) {
  if (length(args) != 1 || !dir.exists(args[1])) {
    message("usage: Rscript R/predict_seats.R <data dir holding constituency_predictions.parquet>")
    return(2L)
  }
  draws <- arrow::read_parquet(file.path(args[1], "constituency_predictions.parquet"))
  arrow::write_parquet(predict_seats(draws), file.path(args[1], "seats_draws.parquet"))
  0L
}

if (is.null(box::name())) quit(save = "no", status = main(commandArgs(trailingOnly = TRUE)))
