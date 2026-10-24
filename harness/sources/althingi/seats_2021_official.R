# Builds harness/sources/seats_2021_official.csv from the Alþingi results page text
# (althingiskosningar_2021.txt, committed unchanged beside this script): seats and
# adjustment seats per list × kjördæmi, counted from the 63 members the page lists. It
# writes nothing unless the counts reproduce the page's own national totals, its
# adjustment-seat summary and its per-kjördæmi totals.
# Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript harness/sources/althingi/seats_2021_official.R [<output CSV>]
# The output defaults to harness/sources/seats_2021_official.csv; the staleness test
# (tests/harness/test-generated.R) writes to a temp file instead.
stopifnot("run under LC_ALL=en_US.UTF-8" = isTRUE(l10n_info()[["UTF-8"]]))
options(box.path = normalizePath("."))
box::use(R / harness / constants[KJORDAEMI, harness_path])

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else harness_path("sources", "seats_2021_official.csv")

page <- harness_path("sources", "althingi", "althingiskosningar_2021.txt")
url <- "https://www.althingi.is/thingmenn/althingiskosningar/kosningar-og-kosningaurslit/kosningar-fra-1919/althingiskosningar-2021/"
party_letter <- c(
  "Sjálfstæðisflokkur" = "D", "Framsóknarflokkur" = "B", "Vinstrihreyfingin – grænt framboð" = "V",
  "Samfylkingin" = "S", "Flokkur fólksins" = "F", "Píratar" = "P", "Viðreisn" = "C",
  "Miðflokkurinn" = "M"
)

lines <- trimws(readLines(page, encoding = "UTF-8"))
stopifnot(grepl(url, lines[1], fixed = TRUE))
lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
section <- function(from, to) {
  i <- match(from, lines)
  j <- match(to, lines[-seq_len(i)]) + i
  stopifnot(!is.na(i), !is.na(j))
  paste(lines[(i + 1):(j - 1)], collapse = "\n")
}
pick <- function(text, keys, suffix) {
  pattern <- paste0("(", paste(keys, collapse = "|"), ")\\s+", suffix)
  m <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  stats::setNames(as.integer(sub(paste0("^.*?\\s+", suffix), "\\1", m, perl = TRUE)), sub(pattern, "\\1", m, perl = TRUE))
}

# The page's own summaries
national <- pick(section("Kosningaúrslit", "Jöfnunarþingmenn"), names(party_letter), "[0-9,]+%\\s+([0-9]+) þingmenn")
adjustment <- pick(section("Jöfnunarþingmenn", "Kjördæmi og þingmenn 2021"), names(party_letter), "([0-9]+)")
kjordaemi_total <- pick(
  section("Kjördæmi og þingmenn 2021", "Þingmenn að loknum alþingiskosningum 25. september 2021"),
  KJORDAEMI$hagstofa, "([0-9]+) þingmenn"
)

# The members: a kjördæmi header, then "Kjördæmakjörnir þingmenn" and "Jöfnunarþingm…"
# blocks of "n. Name", with the party after a tab or on the next line
members <- lines[(match("Þingmenn að loknum alþingiskosningum 25. september 2021", lines) + 1):length(lines)]
rows <- list()
kd <- NA_character_
type <- NA_character_
i <- 1
while (i <= length(members)) {
  x <- members[i]
  if (x %in% KJORDAEMI$hagstofa) {
    kd <- x
    type <- NA_character_
  } else if (x == "Kjördæmakjörnir þingmenn") {
    type <- "kjordaemi"
  } else if (startsWith(x, "Jöfnunarþingm")) {
    type <- "jofnun"
  } else if (grepl("^[0-9]+\\. ", x)) {
    parts <- strsplit(x, "\t", fixed = TRUE)[[1]]
    if (length(parts) == 1) {
      i <- i + 1
      parts <- c(parts, members[i])
    }
    party <- trimws(parts[2])
    if (!party %in% names(party_letter)) stop("unknown party: ", party)
    rows[[length(rows) + 1]] <- data.frame(
      hagstofa = kd, number = as.integer(sub("\\..*$", "", parts[1])), type = type,
      list_letter = unname(party_letter[party])
    )
  } else {
    stop("unexpected line: ", x)
  }
  i <- i + 1
}
m <- do.call(rbind, rows)
stopifnot(!anyNA(m))

# Members are numbered 1..n in each kjördæmi; the counts reproduce all three summaries
for (k in unique(m$hagstofa)) stopifnot(identical(m$number[m$hagstofa == k], seq_len(sum(m$hagstofa == k))))
stopifnot(
  nrow(m) == 63,
  setequal(names(kjordaemi_total), KJORDAEMI$hagstofa),
  all(as.vector(table(m$hagstofa)[names(kjordaemi_total)]) == kjordaemi_total),
  length(national) == length(party_letter),
  all(as.vector(table(m$list_letter)[party_letter[names(national)]]) == national),
  sum(national) == 63,
  all(tapply(m$type == "jofnun", m$list_letter, sum)[party_letter[names(adjustment)]] == adjustment),
  sum(m$type == "jofnun") == sum(adjustment)
)

seats <- m |>
  dplyr::summarise(seats = dplyr::n(), adjustment_seats = sum(type == "jofnun"), .by = c(hagstofa, list_letter)) |>
  dplyr::mutate(
    election = as.Date("2021-09-25"),
    kjordaemi = KJORDAEMI$kjordaemi[match(hagstofa, KJORDAEMI$hagstofa)],
    source = paste0(url, " (retrieved 2026-10-23)")
  ) |>
  dplyr::arrange(match(kjordaemi, KJORDAEMI$kjordaemi), list_letter) |>
  dplyr::select(election, kjordaemi, list_letter, seats, adjustment_seats, source)
readr::write_csv(seats, out)
