# One-off correction of the 30 Kosning rows of a data/polling_data.csv written before
# get_election_data() matched each election's valid votes by date. Until then the rows
# took the totals 199730, 196259 and 189648 in turn, so a row's share is its n over the
# total it was given, and its n should be that share times its own election's total,
# which must be get_election_data()'s n for the row to within 1e-6. Rewrites only those
# rows' n, as get_election_data()'s n in write_csv()'s number format, and leaves every
# other byte alone; a row that already holds the right n is left as it is, so a second run
# changes nothing. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/correct_election_anchors.R [path]
stopifnot("run under LC_ALL=en_US.UTF-8" = isTRUE(l10n_info()[["UTF-8"]]))
options(box.path = normalizePath("."))
box::use(R / data[get_election_data])

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[1] else here::here("data", "polling_data.csv")

anchors <- get_election_data()
anchors$given <- rep_len(c(199730, 196259, 189648), nrow(anchors))
anchors$total <- c("2021-09-25" = 199730, "2017-10-28" = 196259, "2016-10-29" = 189648)[as.character(anchors$date)]
key <- paste(anchors$date, anchors$flokkur)

lines <- readLines(path, encoding = "UTF-8")
pattern <- "^([0-9]{4}-[0-9]{2}-[0-9]{2}),Kosning,([^,]*),([^,]*),(.*)$"
rows <- grep(pattern, lines)
stopifnot("expected 30 Kosning rows" = length(rows) == nrow(anchors))

fields <- regmatches(lines[rows], regexec(pattern, lines[rows]))
row_key <- vapply(fields, \(f) paste(f[2], f[3]), "")
n_old <- as.numeric(vapply(fields, \(f) f[4], ""))
i <- match(row_key, key)
stopifnot("a Kosning row get_election_data() does not hold" = !anyNA(i), !anyDuplicated(i))

done <- abs(n_old - anchors$n[i]) < 1e-6
stopifnot(
  "a Kosning row matches neither its old nor its corrected n" =
    done | abs(n_old / anchors$given[i] * anchors$total[i] - anchors$n[i]) < 1e-6
)

change <- !done
n_text <- sub("\n$", "", readr::format_csv(data.frame(n = anchors$n[i]), col_names = FALSE, eol = "\n"))
n_text <- strsplit(n_text, "\n", fixed = TRUE)[[1]]
lines[rows[change]] <- vapply(which(change), \(k) {
  f <- fields[[k]]
  paste(f[2], "Kosning", f[3], n_text[k], f[5], sep = ",")
}, "")

tmp <- tempfile(tmpdir = dirname(path), fileext = ".csv")
writeLines(lines, tmp, useBytes = TRUE)
stopifnot(file.rename(tmp, path))
cat(sprintf("%s: corrected %d of %d Kosning rows\n", path, sum(change), length(rows)))
