# Builds harness/sources/wikipedia_{2017,2021,2024}_poll_fieldwork.csv from pinned revisions of
# the English Wikipedia poll tables. The MediaWiki API responses are fetched with the curl
# commands in harness/sources/README.md into one directory and are not committed.
# The 2021 table began as a copy of an earlier fetch of the same page (38 rows, 2026-10-09). The
# script keeps those rows unchanged, checks each against the pinned revision (house, dates and
# shares to find it; fieldwork text, n and note must then agree) and appends any row of the
# revision in their range that they lack.
# It writes nothing unless every check passes: the revision ids, the tables' party columns, the
# width of every row, every fieldwork range parsed with start <= end, every share a number or
# blank, and the coverage windows.
# Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript harness/sources/wikipedia/poll_fieldwork.R <dir of API responses> [<sources dir>]
# The sources dir, where the 2021 table is read and all three are written, defaults to
# harness/sources.
stopifnot("run under LC_ALL=en_US.UTF-8" = isTRUE(l10n_info()[["UTF-8"]]))
options(box.path = normalizePath("."), width = 120)
box::use(R / harness / constants[harness_path])

args <- commandArgs(trailingOnly = TRUE)
api_dir <- args[1]
sources_dir <- if (length(args) > 1) args[2] else harness_path("sources")
stopifnot(
  "pass the directory of API responses" = !is.na(api_dir), dir.exists(api_dir),
  dir.exists(sources_dir)
)

PARTIES <- c("D", "S", "B", "V", "P", "C", "F", "M", "J")
COLUMNS <- c("fieldwork", "house", "start", "end", "note", "n", PARTIES)
PAGES <- list(
  list(
    year = 2017, oldid = 1380684596, section = "Opinion polls",
    election = as.Date("2017-10-28"), cover_from = as.Date("2017-08-15"),
    columns = c("house", "fieldwork", "n", "D", "V", "P", "B", "C", "A", "S", "F", "M", "others", "lead")
  ),
  list(
    year = 2021, oldid = 1324451223, section = "Polls",
    election = as.Date("2021-09-25"), cover_from = as.Date("2021-01-28"),
    columns = c("house", "fieldwork", "n", "resp", "D", "V", "S", "M", "B", "P", "F", "C", "J", "others", "lead")
  ),
  list(
    year = 2024, oldid = 1377749798, section = "Opinion polls",
    election = as.Date("2024-11-30"), cover_from = as.Date("2024-04-04"),
    columns = c(
      "house", "fieldwork", "n", "resp", "D", "B", "V", "S", "F", "P", "C", "M", "J", "L", "Y",
      "others", "lead"
    )
  )
)
DASHES <- c("", "-", "\u2013", "\u2014")
NESTED <- "\\{\\{[^{}]*\\}\\}|\\[\\[[^\\[\\]]*\\]\\]|\\[https?://[^\\[\\]]*\\]"

# WHY: a cell's attribute separator "|" also appears inside templates and links, so those
# are masked before a row is split into cells
mask <- function(x) {
  store <- character()
  repeat {
    m <- regexpr(NESTED, x, perl = TRUE)
    if (m < 0) break
    store <- c(store, regmatches(x, m))
    regmatches(x, m) <- sprintf("\x01%d\x02", length(store))
  }
  list(x = x, store = store)
}
unmask <- function(x, store) {
  repeat {
    m <- regexpr("\x01[0-9]+\x02", x)
    if (m < 0) break
    regmatches(x, m) <- store[as.integer(gsub("[^0-9]", "", regmatches(x, m)))]
  }
  x
}

split_cells <- function(line) {
  m <- mask(sub("^[|!]", "", line))
  # WHY: the sentinel keeps empty cells, which strsplit() would drop
  parts <- utils::head(strsplit(paste0(m$x, "||x"), "||", fixed = TRUE)[[1]], -1)
  lapply(parts, function(p) {
    bar <- regexpr("|", p, fixed = TRUE)
    if (bar < 0) {
      return(list(attr = "", value = unmask(p, m$store)))
    }
    list(attr = unmask(substr(p, 1, bar - 1), m$store), value = unmask(substr(p, bar + 1, nchar(p)), m$store))
  })
}

template_text <- function(t) {
  parts <- strsplit(substr(t, 3, nchar(t) - 2), "|", fixed = TRUE)[[1]]
  name <- tolower(trimws(parts[1]))
  if (name %in% c("n/a", "tba", "efn", "refn", "cn", "citation needed", "dead link", "webarchive")) {
    return("")
  }
  if (name %in% c("usurped", "nowrap", "small")) {
    return(sub("^1=", "", paste(parts[-1], collapse = "|")))
  }
  stop("unknown template in a table cell: ", t)
}

clean <- function(x) {
  x <- gsub("<ref[^>]*/>", "", x, perl = TRUE)
  x <- gsub("(?s)<ref[^>]*>.*?</ref>", "", x, perl = TRUE)
  repeat {
    m <- regexpr("\\{\\{[^{}]*\\}\\}", x, perl = TRUE)
    if (m < 0) break
    regmatches(x, m) <- template_text(regmatches(x, m))
  }
  x <- gsub("\\[\\[(?:[^|\\]]*\\|)?([^\\]]*)\\]\\]", "\\1", x, perl = TRUE)
  x <- gsub("\\[https?://[^ \\]]+(?: ([^\\]]*))?\\]", "\\1", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("'{2,3}", "", x, perl = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

span <- function(attr, what) {
  m <- regmatches(attr, regexec(paste0(what, "\\s*=\\s*\"?([0-9]+)"), attr))[[1]]
  if (length(m)) as.integer(m[2]) else 1L
}

# The section's one table: the header text and its data rows as lists of cells
section_table <- function(wikitext, section) {
  lines <- strsplit(wikitext, "\n", fixed = TRUE)[[1]]
  h <- grep(sprintf("^==\\s*%s\\s*==\\s*$", section), lines)
  stopifnot(length(h) == 1)
  end <- grep("^==[^=]", lines)
  lines <- lines[(h + 1):(min(end[end > h]) - 1)]
  open <- grep("^\\{\\|", lines)
  close <- grep("^\\|\\}", lines)
  stopifnot("one table per section" = length(open) == 1 && length(close) == 1)
  header <- character()
  rows <- list()
  cells <- NULL
  for (x in lines[(open + 1):(close - 1)]) {
    if (startsWith(x, "|-")) {
      if (length(cells)) rows[[length(rows) + 1]] <- cells
      cells <- list()
    } else if (startsWith(x, "!")) {
      stopifnot("a header cell after the data rows" = !length(rows))
      header <- c(header, x)
    } else if (startsWith(x, "|")) {
      cells <- c(cells, split_cells(x))
    } else if (nzchar(trimws(x))) {
      stopifnot("a continuation line before any cell" = length(cells) > 0)
      cells[[length(cells)]]$value <- paste(cells[[length(cells)]]$value, x, sep = "\n")
    }
  }
  if (length(cells)) rows[[length(rows) + 1]] <- cells
  list(header = paste(header, collapse = "\n"), rows = rows)
}

row_error <- function(r, cells, what) {
  stop("data row ", r, " (", paste(vapply(cells[1:2], function(x) clean(x$value), ""), collapse = ", "), ") ", what)
}

# One character vector per row, with colspan and rowspan cells expanded
expand_rows <- function(rows, width) {
  carry <- character(width)
  left <- integer(width)
  lapply(seq_along(rows), function(r) {
    cells <- rows[[r]]
    out <- character(width)
    j <- 1
    k <- 1
    while (j <= width) {
      if (left[j] > 0) {
        out[j] <- carry[j]
        left[j] <<- left[j] - 1L
        j <- j + 1
        next
      }
      if (k > length(cells)) row_error(r, cells, "has too few cells")
      across <- span(cells[[k]]$attr, "colspan")
      down <- span(cells[[k]]$attr, "rowspan")
      idx <- j:(j + across - 1)
      if (max(idx) > width) row_error(r, cells, "is wider than the header")
      out[idx] <- clean(cells[[k]]$value)
      if (down > 1) {
        carry[idx] <<- out[idx]
        left[idx] <<- down - 1L
      }
      j <- j + across
      k <- k + 1
    }
    if (k <= length(cells)) row_error(r, cells, "has more cells than the header")
    out
  })
}

month_number <- function(m) {
  i <- match(tolower(m), c(tolower(month.name), tolower(month.abb), "sept"))
  if (anyNA(i)) stop("unknown month: ", paste(m[is.na(i)], collapse = ", "))
  c(1:12, 1:12, 9)[i]
}
ymd <- function(y, m, d) {
  x <- as.Date(sprintf("%s-%02d-%02d", y, m, as.integer(d)), optional = TRUE)
  stopifnot("an invalid calendar date" = !anyNA(x))
  x
}
month_end <- function(y, m) seq(ymd(y, m, 1), by = "month", length.out = 2)[2] - 1

# Fieldwork text to start, end and note: "month-only" when the page gives only a month, "two
# periods" when it gives two ranges (start of the first, end of the second)
parse_fieldwork <- function(x) {
  s <- gsub("\\s*[-\u2013\u2014]\\s*", "-", x, perl = TRUE)
  d <- "([0-9]{1,2})"
  m <- "([A-Za-z]+)\\.?"
  y <- "([0-9]{4})"
  try_pattern <- function(p) regmatches(s, regexec(paste0("^", p, "$"), s))[[1]][-1]
  if (length(g <- try_pattern(paste0(d, "-", d, " ", m, " ", y)))) {
    mm <- month_number(g[3])
    return(list(start = ymd(g[4], mm, g[1]), end = ymd(g[4], mm, g[2]), note = ""))
  }
  if (length(g <- try_pattern(paste0(d, " ", m, "-", d, " ", m, " ", y)))) {
    m1 <- month_number(g[2])
    m2 <- month_number(g[4])
    y2 <- as.integer(g[5])
    return(list(start = ymd(y2 - (m1 > m2), m1, g[1]), end = ymd(y2, m2, g[3]), note = ""))
  }
  if (length(g <- try_pattern(paste0(d, " ", m, " ", y, "-", d, " ", m, " ", y)))) {
    return(list(start = ymd(g[3], month_number(g[2]), g[1]), end = ymd(g[6], month_number(g[5]), g[4]), note = ""))
  }
  if (length(g <- try_pattern(paste0(d, "-", d, " ", m, " & ", d, "-", d, " ", m, " ", y)))) {
    return(list(
      start = ymd(g[7], month_number(g[3]), g[1]), end = ymd(g[7], month_number(g[6]), g[5]),
      note = "two periods"
    ))
  }
  if (length(g <- try_pattern(paste0(d, " ", m, " ", y)))) {
    day <- ymd(g[3], month_number(g[2]), g[1])
    return(list(start = day, end = day, note = ""))
  }
  if (length(g <- try_pattern(paste0(m, " ", y)))) {
    mm <- month_number(g[1])
    return(list(start = ymd(g[2], mm, 1), end = month_end(g[2], mm), note = "month-only"))
  }
  stop("unparsed fieldwork date: ", x)
}

share_text <- function(x, what) {
  x[x %in% DASHES] <- ""
  bad <- nzchar(x) & !grepl("^[0-9]+(\\.[0-9]+)?$", x)
  if (any(bad)) stop("not a share in ", what, ": ", paste(unique(x[bad]), collapse = ", "))
  x
}
sample_text <- function(x) {
  x <- gsub("[, ]", "", x)
  x[x %in% DASHES] <- "NA"
  bad <- x != "NA" & !grepl("^[0-9]+$", x)
  if (any(bad)) stop("not a sample size: ", paste(unique(x[bad]), collapse = ", "))
  x
}

read_page <- function(page) {
  response <- jsonlite::fromJSON(file.path(api_dir, paste0(page$oldid, ".json")))$parse
  stopifnot("the response is not the pinned revision" = identical(as.numeric(response$revid), page$oldid))
  tab <- section_table(response$wikitext, page$section)
  letters_in_header <- regmatches(tab$header, gregexpr("\\[\\[[^]|]*\\|([A-Z])\\]\\]", tab$header))[[1]]
  letters_in_header <- sub("^.*\\|([A-Z])\\]\\]$", "\\1", letters_in_header)
  stopifnot(
    "the header's party columns differ from the expected layout" =
      identical(letters_in_header, grep("^[A-Z]$", page$columns, value = TRUE)),
    "the response-rate column differs from the expected layout" =
      grepl("Resp.", tab$header, fixed = TRUE) == ("resp" %in% page$columns)
  )
  grid <- do.call(rbind, expand_rows(tab$rows, length(page$columns)))
  colnames(grid) <- page$columns
  election_row <- grepl("election", grid[, "house"], ignore.case = TRUE)
  message(page$year, ": dropped result rows: ", paste(grid[election_row, "house"], collapse = "; "))
  grid <- grid[!election_row, , drop = FALSE]
  stopifnot("a row without a house" = all(nzchar(grid[, "house"])))
  fw <- lapply(grid[, "fieldwork"], parse_fieldwork)
  out <- tibble::tibble(
    fieldwork = grid[, "fieldwork"], house = grid[, "house"],
    start = format(do.call(c, lapply(fw, `[[`, "start"))),
    end = format(do.call(c, lapply(fw, `[[`, "end"))),
    note = vapply(fw, `[[`, "", "note"),
    n = sample_text(grid[, "n"])
  )
  for (p in PARTIES) {
    out[[p]] <- if (p %in% page$columns) share_text(grid[, p], paste(page$year, p)) else ""
  }
  stopifnot(
    "a fieldwork start after its end" = all(out$start <= out$end),
    "a poll ending after the election" = all(as.Date(out$end) <= page$election),
    "the table does not reach back to the coverage start" = min(as.Date(out$end)) <= page$cover_from
  )
  out
}

row_key <- function(t) {
  shares <- vapply(PARTIES, function(p) {
    ifelse(nzchar(t[[p]]), sprintf("%.2f", as.numeric(t[[p]])), "")
  }, character(nrow(t)))
  paste(t$house, t$start, t$end, apply(matrix(shares, nrow = nrow(t)), 1, paste, collapse = "/"), sep = "|")
}

# 2021: the committed rows stay as they are; rows of the pinned revision in their range that
# they lack are appended
extend_2021 <- function(page_rows, page) {
  path <- file.path(sources_dir, "wikipedia_2021_poll_fieldwork.csv")
  held <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = character())
  stopifnot(identical(names(held), COLUMNS))
  in_range <- as.Date(page_rows$end) >= min(as.Date(held$end)) & as.Date(page_rows$end) <= page$election
  at <- match(row_key(held), row_key(page_rows))
  if (anyNA(at)) {
    print(as.data.frame(held[is.na(at), ]))
    stop("held 2021 rows that the pinned revision does not list")
  }
  verbatim <- c("fieldwork", "n", "note")
  differs <- Reduce(`|`, lapply(verbatim, function(col) held[[col]] != page_rows[[col]][at]))
  if (any(differs)) {
    print(as.data.frame(held[differs, c("house", verbatim)]))
    print(as.data.frame(page_rows[at[differs], c("house", verbatim)]))
    stop("held 2021 rows whose fieldwork, n or note differ from the pinned revision")
  }
  missing <- page_rows[in_range & !row_key(page_rows) %in% row_key(held), ]
  message("2021: ", nrow(held), " held rows match the pinned revision; appending ", nrow(missing))
  dplyr::bind_rows(held, missing)
}

tables <- lapply(PAGES, function(page) {
  rows <- read_page(page)
  if (page$year == 2021) rows <- extend_2021(rows, page)
  stopifnot(identical(names(rows), COLUMNS), min(as.Date(rows$end)) <= page$cover_from)
  message(
    page$year, ": ", nrow(rows), " rows, ends ", min(rows$end), " to ", max(rows$end),
    "; houses: ", paste(sort(unique(rows$house)), collapse = ", ")
  )
  rows
})
for (i in seq_along(PAGES)) {
  path <- file.path(sources_dir, sprintf("wikipedia_%d_poll_fieldwork.csv", PAGES[[i]]$year))
  readr::write_csv(tables[[i]], path, na = "NA")
}
