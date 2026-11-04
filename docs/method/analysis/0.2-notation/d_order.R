# Chapter 0.2: the party and house order each model indexes by. The forecast model's
# house_n and its party columns follow the factor levels read_polling_data() sets; the
# Fylgisvakt smoother's follow read_polling_watch_data() and then fct_drop() in
# prepare_polling_watch_data(). Reads the live, git-ignored data/ CSVs; fits nothing.
# Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/0.2-notation/d_order.R
options(width = 120)
box::use(
  R / data[read_polling_data, read_polling_watch_data],
  R / party_utils[party_tibble]
)

letters <- party_tibble()
letter_of <- function(x) letters$bokstafur[match(x, letters$flokkur)]

show <- function(f, rows) {
  lv <- levels(droplevels(f[rows]))
  data.frame(index = seq_along(lv), level = lv, rows = as.vector(table(droplevels(f[rows]))[lv]))
}

pd <- read_polling_data()
w <- pd$date >= as.Date("2016-01-01")
cat("Forecast model, read_polling_data(), rows dated from 1 Jan 2016\n")
cat("  parties (column order of y_n; party 1 is the reference):\n")
p <- show(pd$flokkur, w)
p$letter <- letter_of(p$level)
print(p, row.names = FALSE)
cat("  houses (house_n = as.numeric(fyrirtaeki)):\n")
print(show(pd$fyrirtaeki, w), row.names = FALSE)
cat("  all factor levels, window or not:", paste(levels(pd$fyrirtaeki), collapse = " | "), "\n")
pos <- w & pd$n > 0
fc <- tapply(as.numeric(pd$date[pos]), droplevels(pd$flokkur[pos]), min)
cat("  first date with a count above 0:\n")
print(data.frame(flokkur = names(fc), first = as.Date(fc, origin = "1970-01-01")), row.names = FALSE)
cat("\n")

pw <- read_polling_watch_data(start = as.Date("2016-01-01"))
a <- rep(TRUE, nrow(pw))
cat("Fylgisvakt, read_polling_watch_data(start = 2016-01-01), as R/fit_polling_watch.R calls it\n")
cat("  parties:\n")
q <- show(pw$flokkur, a)
q$letter <- letter_of(q$level)
print(q, row.names = FALSE)
cat("  houses:\n")
print(show(pw$fyrirtaeki, a), row.names = FALSE)
