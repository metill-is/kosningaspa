# Official kjördæmi results 2016–2024: votes and seats per list, the harness's scoring
# truth and its past-election data. Votes come from Hagstofa Íslands (the 2016–2021
# sheets in the snapshot, the complete 2016 KOS02118b table committed under
# harness/sources/ because the snapshot's 2016 sheet lacks S, T and V, and the KOS02018
# table for 2024, committed there too). Seats come from the Hagstofa sheets for 2016 and
# 2017, KOS02018 for 2024, and Alþingi's 2021 results page for 2021: Hagstofa's 2021
# seat tables contradict D'Hondt on their own votes (harness/truth/README.md).

# Committed source files, relative to harness/.
KOS02118B_2016 <- c("sources", "hagstofa", "KOS02118b_2016.json")
KOS02018_2024 <- c("sources", "hagstofa", "KOS02018_2024.json")
SEATS_2021 <- c("sources", "seats_2021_official.csv")

# Letterless list labels that Hagstofa uses for an aggregate of small lists; the label
# itself stands in for the letter.
AGGREGATE_LABELS <- "A\u00f0rir"

kjordaemi_order <- function(kjordaemi) {
  box::use(R / harness / constants[KJORDAEMI])
  match(kjordaemi, KJORDAEMI$kjordaemi)
}

# Split "Name  (L)" into name and letter; an AGGREGATE_LABELS label is its own letter.
split_list_label <- function(label, file) {
  m <- regmatches(label, regexec("^(.*\\S)\\s*\\(([^()\\s]+)\\)\\s*$", label, perl = TRUE))
  bad <- lengths(m) == 0 & !label %in% AGGREGATE_LABELS
  if (any(bad)) stop(file, ": list label(s) without a (letter): ", paste(label[bad], collapse = ", "))
  aggregate <- lengths(m) == 0
  name <- ifelse(aggregate, label, vapply(m, `[`, "", 2))
  letter <- ifelse(aggregate, label, vapply(m, `[`, "", 3))
  if (anyDuplicated(letter)) stop(file, ": repeated list letter(s): ", paste(unique(letter[duplicated(letter)]), collapse = ", "))
  tibble::tibble(list_letter = letter, list_name = gsub("\\s+", " ", name))
}

# One Hagstofa kjördæmi sheet in long form: row 3 holds the kjördæmi headers in columns
# 3–8, party rows run from row 4 to the first NA in column 2, and "." is a list that did
# not run there.
read_hagstofa_sheet <- function(path, election, measure) {
  box::use(R / harness / constants[KJORDAEMI])
  file <- basename(path)
  election <- as.Date(election)
  x <- readxl::read_excel(path, col_names = FALSE, col_types = "text", .name_repair = "minimal")

  title <- x[[1]][1]
  if (is.na(title) || !endsWith(title, format(election, "%Y"))) {
    stop(file, ": title is not for ", format(election, "%Y"), ": ", title)
  }
  if (!identical(x[[1]][4], measure)) {
    stop(file, ": expected '", measure, "' in A4, found '", x[[1]][4], "'")
  }

  headers <- unlist(x[3, 3:8], use.names = FALSE)
  k <- match(headers, KJORDAEMI$hagstofa)
  if (anyNA(k) || anyDuplicated(k)) {
    stop(file, ": kj\u00f6rd\u00e6mi headers do not match KJORDAEMI$hagstofa: ", paste(headers, collapse = ", "))
  }

  labels <- x[[2]][-(1:3)]
  n_lists <- match(TRUE, is.na(labels)) - 1
  if (is.na(n_lists)) n_lists <- length(labels)
  if (n_lists < 1) stop(file, ": no list rows from row 4")
  rows <- 3 + seq_len(n_lists)

  cells <- as.matrix(x[rows, 3:8])
  if (!all(grepl("^([0-9]+|\\.)$", cells))) {
    stop(file, ": cells that are neither a count nor '.': ", paste(unique(cells[!grepl("^([0-9]+|\\.)$", cells)]), collapse = ", "))
  }

  lists <- split_list_label(labels[seq_len(n_lists)], file)
  tibble::tibble(
    election = election,
    kjordaemi = rep(KJORDAEMI$kjordaemi[k], each = n_lists),
    list_letter = rep(lists$list_letter, times = 6),
    list_name = rep(lists$list_name, times = 6),
    value = as.integer(ifelse(cells == ".", "0", cells)),
    ran = as.vector(cells != ".")
  )
}

#' Valid votes per list and kjördæmi from a Hagstofa "gild atkvæði" sheet (2016–2021).
#' @param path the xlsx file
#' @param election the election date (Date or ISO string); the sheet title must end in
#'   its year
#' @return a tibble (election, kjordaemi, list_letter, list_name, votes, ran): one row per
#'   list and kjördæmi, with 0 votes and `ran = FALSE` where the list did not run (".")
#' @export
parse_gild_atkvaedi <- function(path, election) {
  box::use(dplyr[rename])
  read_hagstofa_sheet(path, election, "Gild atkv\u00e6\u00f0i") |> rename(votes = value)
}

#' Seats per list and kjördæmi from a Hagstofa "þingsæti" sheet (2016–2021).
#' @inheritParams parse_gild_atkvaedi
#' @return a tibble (election, kjordaemi, list_letter, seats), with 0 seats where the list
#'   did not run (".")
#' @export
parse_thingsaeti <- function(path, election) {
  box::use(dplyr[transmute])
  read_hagstofa_sheet(path, election, "\u00deings\u00e6ti") |>
    transmute(election, kjordaemi, list_letter, seats = value)
}

#' Votes and seats per list and kjördæmi from a Hagstofa JSON-stat results table
#' (Atriði × Flokkur × Kjördæmi, row-major), such as KOS02018 or KOS02118b. Votes and
#' seats are the Atriði categories labelled "Gild atkvæði" and "Þingsæti", whatever their
#' codes. The totals (Flokkur and Kjördæmi "Alls") are checked against the cells and
#' dropped. Letters come from the Flokkur labels, never the codes; kjördæmi match by label.
#' @param path the JSON file
#' @param election the election date (Date or ISO string); the table label must name its
#'   year
#' @return a tibble (election, kjordaemi, list_letter, list_name, votes, ran, seats), with
#'   `ran = votes > 0`
#' @export
parse_jsonstat_results <- function(path, election) {
  box::use(R / harness / constants[KJORDAEMI])
  file <- basename(path)
  election <- as.Date(election)
  j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (!grepl(format(election, "%Y"), j$label, fixed = TRUE)) {
    stop(file, ": table label is not for ", format(election, "%Y"), ": ", j$label)
  }
  dims <- c("Atri\u00f0i", "Flokkur", "Kj\u00f6rd\u00e6mi")
  if (!identical(j$id, dims)) stop(file, ": dimensions are not ", paste(dims, collapse = " \u00d7 "))
  if (length(j$value) != prod(j$size)) stop(file, ": ", length(j$value), " values for size ", paste(j$size, collapse = " \u00d7 "))

  category <- function(d) {
    index <- unlist(j$dimension[[d]]$category$index)
    codes <- names(index)[order(index)]
    stopifnot(identical(sort(unname(index)), seq_along(index) - 1L))
    tibble::tibble(code = codes, label = unname(unlist(j$dimension[[d]]$category$label)[codes]))
  }
  atr <- category("Atri\u00f0i")
  flo <- category("Flokkur")
  kjo <- category("Kj\u00f6rd\u00e6mi")
  stopifnot(identical(j$size, c(nrow(atr), nrow(flo), nrow(kjo))))
  a_votes <- match("Gild atkv\u00e6\u00f0i", atr$label)
  a_seats <- match("\u00deings\u00e6ti", atr$label)
  if (anyNA(c(a_votes, a_seats))) stop(file, ": Atri\u00f0i has no 'Gild atkv\u00e6\u00f0i' or no '\u00deings\u00e6ti'")

  missing <- which(is.na(j$value)) - 1L
  if (!all(as.character(missing) %in% names(j$status))) stop(file, ": null value(s) without a '.' status")
  value <- ifelse(is.na(j$value), 0L, j$value)
  # WHY: row-major values vary Kjördæmi fastest, so reversing the size gives [kjördæmi, flokkur, atriði]
  cube <- array(value, dim = rev(j$size))

  flo_total <- flo$label == "Alls"
  kjo_total <- kjo$label == "Alls"
  stopifnot(sum(flo_total) == 1, sum(kjo_total) == 1)
  for (a in c(a_votes, a_seats)) {
    m <- cube[, , a]
    if (!all(colSums(m[!kjo_total, , drop = FALSE]) == m[kjo_total, ]) ||
      !all(rowSums(m[, !flo_total, drop = FALSE]) == m[, flo_total])) {
      stop(file, ": cells do not add up to the totals for Atri\u00f0i ", atr$label[a])
    }
  }

  k <- match(kjo$label[!kjo_total], KJORDAEMI$hagstofa)
  if (anyNA(k) || anyDuplicated(k) || length(k) != nrow(KJORDAEMI)) {
    stop(file, ": kj\u00f6rd\u00e6mi labels do not match KJORDAEMI$hagstofa: ", paste(kjo$label[!kjo_total], collapse = ", "))
  }
  lists <- split_list_label(flo$label[!flo_total], file)
  votes <- as.integer(cube[!kjo_total, !flo_total, a_votes])
  tibble::tibble(
    election = election,
    kjordaemi = rep(KJORDAEMI$kjordaemi[k], times = nrow(lists)),
    list_letter = rep(lists$list_letter, each = length(k)),
    list_name = rep(lists$list_name, each = length(k)),
    votes = votes,
    ran = votes > 0,
    seats = as.integer(cube[!kjo_total, !flo_total, a_seats])
  )
}

#' Votes and seats per list and kjördæmi for 2024-11-30 from Hagstofa's KOS02018 table.
#' @param path the JSON file
#' @return as `parse_jsonstat_results()`
#' @export
parse_kos02018 <- function(path) {
  parse_jsonstat_results(path, "2024-11-30")
}

#' The 2021 seats from Alþingi's results page: one row per list × kjördæmi that won seats.
#' @param path the CSV, built by `harness/sources/althingi/seats_2021_official.R`
#' @return a tibble (election, kjordaemi, list_letter, seats, adjustment_seats, source)
#' @export
read_seats_2021 <- function(path = do.call(harness_path, as.list(SEATS_2021))) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path, KJORDAEMI])
  s <- readr::read_csv(
    path,
    col_types = readr::cols(
      election = "D", kjordaemi = "c", list_letter = "c", seats = "i", adjustment_seats = "i", source = "c"
    ),
    na = character()
  )
  stopifnot(
    all(s$election == as.Date("2021-09-25")),
    all(s$kjordaemi %in% KJORDAEMI$kjordaemi),
    all(s$seats > 0), all(s$adjustment_seats >= 0 & s$adjustment_seats <= s$seats),
    !anyDuplicated(s[c("kjordaemi", "list_letter")])
  )
  s
}

# Stop unless every cell of `held` exists in `full` with equal `cols`.
assert_cells_agree <- function(held, full, cols, what) {
  key <- c("election", "kjordaemi", "list_letter")
  j <- dplyr::left_join(held[c(key, cols)], full[c(key, cols)], by = key, suffix = c("", ".full"))
  differ <- Reduce(`|`, lapply(cols, \(x) is.na(j[[paste0(x, ".full")]]) | j[[x]] != j[[paste0(x, ".full")]]))
  if (any(differ)) {
    bad <- j[differ, ]
    stop(what, ": ", nrow(bad), " cell(s) differ: ", paste(bad$kjordaemi, bad$list_letter, collapse = "; "))
  }
  invisible(TRUE)
}

# One election's votes and seats joined by list and kjördæmi; both must hold the same
# lists, and a list that did not run cannot hold a seat.
join_votes_seats <- function(votes, seats, source) {
  box::use(dplyr[anti_join, inner_join, mutate])
  key <- c("election", "kjordaemi", "list_letter")
  no_seats <- anti_join(votes, seats, by = key)
  no_votes <- anti_join(seats, votes, by = key)
  if (nrow(no_seats) || nrow(no_votes)) {
    only <- function(d) if (nrow(d)) paste(unique(d$list_letter), collapse = ", ") else "none"
    stop(
      format(votes$election[1]), ": votes and seats hold different lists; only with votes: ",
      only(no_seats), "; only with seats: ", only(no_votes)
    )
  }
  out <- inner_join(votes, seats, by = key)
  if (any(out$seats[!out$ran] > 0)) stop(format(votes$election[1]), ": seats for a list that did not run")
  mutate(out, source = source)
}

#' The official results table: one row per election × kjördæmi × list for 2016, 2017,
#' 2021 and 2024, from the snapshot and the committed sources under `harness/sources/`.
#' @return a tibble (election, kjordaemi, list_letter, list_name, flokkur, votes, seats,
#'   ran, source); `flokkur` is the poll party of the list letter and `source` the
#'   file(s) the row's votes and seats come from
#' @export
build_truth <- function() {
  box::use(
    R / harness / constants[harness_path, party_of_letter],
    R / harness / inputs[snapshot_file],
    dplyr[arrange, bind_rows, left_join, mutate, select]
  )
  sources <- function(...) paste(basename(c(...)), collapse = ";")

  # 2016: votes from KOS02118b, which must equal every cell the truncated xlsx holds;
  # seats from the þingsæti sheet, which must equal KOS02118b's
  e <- "2016-10-29"
  api_path <- do.call(harness_path, as.list(KOS02118B_2016))
  api <- parse_jsonstat_results(api_path, e)
  gild <- snapshot_file("gild_2016")
  assert_cells_agree(parse_gild_atkvaedi(gild, e), api, c("votes", "ran"), paste(basename(gild), "against KOS02118b"))
  thing <- snapshot_file("thingsaeti_2016")
  s16 <- parse_thingsaeti(thing, e)
  assert_cells_agree(s16, api, "seats", paste(basename(thing), "against KOS02118b"))
  t16 <- join_votes_seats(select(api, -seats), s16, sources(api_path, thing))

  e <- "2017-10-28"
  gild <- snapshot_file("gild_2017")
  thing <- snapshot_file("thingsaeti_2017")
  t17 <- join_votes_seats(parse_gild_atkvaedi(gild, e), parse_thingsaeti(thing, e), sources(gild, thing))

  # 2021: seats from Alþingi's results page, 0 for every list × kjördæmi it does not list
  e <- "2021-09-25"
  gild <- snapshot_file("gild_2021")
  v21 <- parse_gild_atkvaedi(gild, e)
  seats_path <- do.call(harness_path, as.list(SEATS_2021))
  official <- read_seats_2021(seats_path)
  unknown <- dplyr::anti_join(official, v21, by = c("election", "kjordaemi", "list_letter"))
  if (nrow(unknown)) stop(basename(seats_path), ": seats for lists with no votes: ", paste(unique(unknown$list_letter), collapse = ", "))
  s21 <- v21[c("election", "kjordaemi", "list_letter")] |>
    left_join(official[c("election", "kjordaemi", "list_letter", "seats")], by = c("election", "kjordaemi", "list_letter")) |>
    mutate(seats = dplyr::coalesce(seats, 0L))
  t21 <- join_votes_seats(v21, s21, sources(gild, seats_path))

  kos <- do.call(harness_path, as.list(KOS02018_2024))
  t24 <- mutate(parse_kos02018(kos), source = sources(kos))

  bind_rows(t16, t17, t21, t24) |>
    mutate(flokkur = party_of_letter(list_letter)) |>
    select(election, kjordaemi, list_letter, list_name, flokkur, votes, seats, ran, source) |>
    arrange(election, kjordaemi_order(kjordaemi), list_letter)
}

#' Write the official results table as CSV.
#' @param path destination CSV
#' @return the table, invisibly
#' @export
write_truth <- function(path = harness_path("truth", "elections_official.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  truth <- build_truth()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(truth, path)
  invisible(truth)
}

# The columns of the official results table and their readr types.
TRUTH_COLUMNS <- c(
  election = "D", kjordaemi = "c", list_letter = "c", list_name = "c", flokkur = "c",
  votes = "i", seats = "i", ran = "l", source = "c"
)

#' The committed official results table.
#' @param path the CSV that `write_truth()` writes
#' @return a tibble as `build_truth()` returns it: election (Date), kjordaemi,
#'   list_letter, list_name, flokkur (character), votes, seats (integer), ran (logical),
#'   source (character); an error if the columns differ
#' @export
read_truth <- function(path = harness_path("truth", "elections_official.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  header <- names(readr::read_csv(path, n_max = 0, col_types = readr::cols(.default = "c")))
  if (!identical(header, names(TRUTH_COLUMNS))) {
    stop(basename(path), ": columns are not ", paste(names(TRUTH_COLUMNS), collapse = ", "))
  }
  t <- readr::read_csv(
    path,
    col_types = do.call(readr::cols, as.list(TRUTH_COLUMNS)), na = character(), progress = FALSE
  )
  if (nrow(readr::problems(t))) stop(basename(path), ": cells that do not parse as their column type")
  tibble::as_tibble(t)
}

#' The result a forecast cell is scored against.
#' @param E the election (Date or ISO string)
#' @param parties the poll parties the run forecasts: `POLL_PARTIES` labels, no repeats,
#'   in the run's order
#' @param truth an official results table (`read_truth()`)
#' @return `list(nat, kd, seats)`, all from `fold_truth(truth)` at E: `nat`, each party's
#'   share of the national valid vote, a numeric vector named by `parties`; `kd`, a tibble
#'   (kjordaemi, flokkur, votes, share) with one row per kjördæmi × party of `parties`,
#'   `share` being the party's share of its kjördæmi's valid vote; `seats`, the official
#'   seats, an integer vector named by `parties`. A party of `parties` with no list at E
#'   has 0 votes, shares and seats. An error if a poll party outside `parties` has votes
#'   at E, since a scorer would otherwise drop them silently.
#' @export
cell_truth <- function(E, parties, truth = read_truth()) {
  box::use(R / harness / constants[POLL_PARTIES], dplyr[filter, mutate, select, summarise])
  E <- as.Date(E)
  stopifnot(length(E) == 1, !is.na(E), is.character(parties), length(parties) > 0, !anyDuplicated(parties))
  unknown <- setdiff(parties, POLL_PARTIES)
  if (length(unknown)) stop("not poll parties: ", paste(unknown, collapse = ", "))
  te <- truth[which(truth$election == E), ]
  if (!nrow(te)) stop("no results for ", format(E))

  folded <- fold_truth(te)
  nat <- summarise(folded, votes = sum(votes), seats = sum(seats), .by = flokkur)
  extra <- nat$flokkur[nat$votes > 0 & !nat$flokkur %in% parties]
  if (length(extra)) {
    stop("truth has votes for parties the run does not forecast: ", paste(extra, collapse = ", "))
  }
  i <- match(parties, nat$flokkur)
  list(
    nat = stats::setNames(nat$votes[i] / sum(nat$votes), parties),
    kd = folded |>
      mutate(share = votes / sum(votes), .by = kjordaemi) |>
      filter(flokkur %in% parties) |>
      select(kjordaemi, flokkur, votes, share),
    seats = stats::setNames(as.integer(nat$seats[i]), parties)
  )
}

#' Fold lists into the poll parties.
#' @param truth a `build_truth()` table
#' @return a tibble (election, kjordaemi, flokkur, votes, seats) with every `POLL_PARTIES`
#'   party for every election × kjördæmi, and 0 votes and seats where no list ran
#' @export
fold_truth <- function(truth) {
  box::use(
    R / harness / constants[POLL_PARTIES],
    dplyr[arrange, summarise],
    tidyr[complete, nesting]
  )
  stopifnot(all(truth$flokkur %in% POLL_PARTIES))
  truth |>
    summarise(votes = sum(votes), seats = sum(seats), .by = c(election, kjordaemi, flokkur)) |>
    complete(nesting(election, kjordaemi), flokkur = POLL_PARTIES, fill = list(votes = 0L, seats = 0L)) |>
    arrange(election, kjordaemi_order(kjordaemi), match(flokkur, POLL_PARTIES))
}

#' National vote shares of the poll parties at one election.
#' @param truth a `build_truth()` table
#' @param election an election in `truth` (Date or ISO string)
#' @return a numeric vector named by `POLL_PARTIES`: each party's share of the national
#'   valid vote, NA for a party with no list that ran at that election
#' @export
national_shares <- function(truth, election) {
  box::use(R / harness / constants[POLL_PARTIES])
  election <- as.Date(election)
  stopifnot(length(election) == 1)
  te <- truth[truth$election == election, ]
  if (!nrow(te)) stop("no results for ", format(election))
  votes <- tapply(te$votes, te$flokkur, sum)
  shares <- stats::setNames(rep(NA_real_, length(POLL_PARTIES)), POLL_PARTIES)
  ran <- intersect(POLL_PARTIES, te$flokkur[te$ran])
  shares[ran] <- votes[ran] / sum(te$votes)
  shares
}

#' National vote shares at the election before E (π̄ for the shock scaling, §6.6).
#' @param E a window election (Date or ISO string)
#' @param truth a `build_truth()` table
#' @param fundamentals the fundamentals table (`fundamentals_data.csv`); used for
#'   E = 2016-10-29, whose previous election (2013) is outside `truth`
#' @return a numeric vector named by `POLL_PARTIES`; for 2016-10-29 the 2013 shares over
#'   their sum, labels renamed by `FUNDAMENTALS_RENAMED` (Framsókn as
#'   Framsóknarflokkurinn) and every other label outside `POLL_PARTIES` (Björt framtíð)
#'   in Annað, otherwise `national_shares(truth, previous_election(E))`; NA for a party
#'   with no list
#' @export
prev_national_shares <- function(E, truth, fundamentals) {
  box::use(R / harness / constants[ELECTIONS, FUNDAMENTALS_RENAMED, POLL_PARTIES, previous_election])
  E <- as.Date(E)
  stopifnot(length(E) == 1)
  prev <- previous_election(E)
  if (E != ELECTIONS[1]) {
    return(national_shares(truth, prev))
  }
  f <- fundamentals[fundamentals$date == prev, ]
  if (!nrow(f)) stop("no fundamentals rows for ", format(prev))
  party <- f$flokkur
  renamed <- party %in% names(FUNDAMENTALS_RENAMED)
  party[renamed] <- unname(FUNDAMENTALS_RENAMED[party[renamed]])
  party[!party %in% POLL_PARTIES] <- "Anna\u00f0"
  share <- tapply(f$voteshare, party, sum)
  out <- stats::setNames(rep(NA_real_, length(POLL_PARTIES)), POLL_PARTIES)
  out[names(share)] <- share / sum(share)
  out
}
