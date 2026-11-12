# The 2013 Alþingi election, the previous election of a 2016 cell, as an anchor input:
# votes and seats per list and kjördæmi from Hagstofa's table KOS02118c, committed under
# harness/sources/hagstofa/. Its list labels carry no letters, and in 2013 the letters J
# and M belonged to other parties than today, while Píratar ran under another letter, so
# each list takes its poll party by name from harness/sources/lists_2013.csv. The table
# stands apart from build_truth() and elections_official.csv (harness/truth/README.md).

# Committed source files, relative to harness/.
KOS02118C_2013 <- c("sources", "hagstofa", "KOS02118c_2013.json")
LISTS_2013 <- c("sources", "lists_2013.csv")

ELECTION_2013 <- as.Date("2013-04-27")
# WHY: Hagstofa's count, 28 above the count announced in April 2013: it adds the ballots
# that Alþingi's credentials committee ruled valid on 6 June 2013.
VALID_VOTES_2013 <- 189023L
SEATS_2013 <- 63L

DIMS <- c("Atri\u00f0i", "Flokkur", "Kj\u00f6rd\u00e6mi")
VOTES_LABEL <- "Gild atkv\u00e6\u00f0i"
SEATS_LABEL <- "\u00deings\u00e6ti"
TOTAL_LABEL <- "Alls"

listed <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"

#' The 2013 lists and the poll party each belongs to.
#' @param path the CSV (list_name, flokkur): each list's name exactly as Hagstofa labels
#'   it in KOS02118c, and its `POLL_PARTIES` label
#' @return a tibble (list_name, flokkur) in the file's order; an error if the columns
#'   differ, a list repeats or a party is not a `POLL_PARTIES` label
#' @export
read_lists_2013 <- function(path = do.call(harness_path, as.list(LISTS_2013))) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path, POLL_PARTIES])
  file <- basename(path)
  l <- readr::read_csv(
    path,
    col_types = readr::cols(.default = "c"), na = character(), trim_ws = FALSE, progress = FALSE
  )
  if (!identical(names(l), c("list_name", "flokkur"))) stop(file, ": columns are not list_name, flokkur")
  if (anyDuplicated(l$list_name)) {
    stop(file, ": repeated list(s): ", listed(unique(l$list_name[duplicated(l$list_name)])))
  }
  unknown <- setdiff(l$flokkur, POLL_PARTIES)
  if (length(unknown)) stop(file, ": not poll parties: ", listed(unknown))
  l
}

#' Votes and seats per list and kjördæmi at the 2013 election, from Hagstofa's KOS02118c
#' table (JSON-stat, Atriði × Flokkur × Kjördæmi, row-major, as the PX API returns it),
#' each list given its poll party by name.
#'
#' Stops, naming the problem, when the file and the list table name different lists, a
#' kjördæmi label is not in `KJORDAEMI$hagstofa`, the cells do not add up to the file's
#' "Alls" totals, the valid votes are not Hagstofa's 189 023, the seats do not sum to 63,
#' or a list marked as not running in a kjördæmi has votes or seats there.
#' @param source the JSON file
#' @param lists the list table CSV, read by `read_lists_2013()`
#' @return a tibble (election, kjordaemi, list_name, flokkur, votes, seats, ran, source):
#'   one row per list and kjördæmi, in `KJORDAEMI` order and then the list table's order;
#'   `kjordaemi` is the poll label, joined by name; `ran` is FALSE, with 0 votes and
#'   seats, where the list's vote cell is "." (did not run); `source` is the JSON file's
#'   name
#' @export
build_anchor_2013 <- function(source = do.call(harness_path, as.list(KOS02118C_2013)),
                              lists = do.call(harness_path, as.list(LISTS_2013))) {
  # WHY: the default arguments are promises, forced after this import.
  box::use(R / harness / constants[harness_path, KJORDAEMI])
  file <- basename(source)
  list_table <- read_lists_2013(lists)
  j <- jsonlite::fromJSON(source, simplifyVector = TRUE)
  if (!grepl(format(ELECTION_2013, "%Y"), j$label, fixed = TRUE)) stop(file, ": table label is not for 2013: ", j$label)
  if (!identical(j$id, DIMS)) stop(file, ": dimensions are not ", paste(DIMS, collapse = " x "))
  n <- prod(j$size)
  if (length(j$value) != n) stop(file, ": ", length(j$value), " values for size ", paste(j$size, collapse = " x "))

  category <- function(d) {
    index <- unlist(j$dimension[[d]]$category$index)
    codes <- names(index)[order(index)]
    if (!identical(sort(unname(index)), seq_along(index) - 1L)) stop(file, ": the ", d, " index is not 0, 1, 2, ...")
    unname(unlist(j$dimension[[d]]$category$label)[codes])
  }
  atr <- category(DIMS[1])
  flo <- category(DIMS[2])
  kjo <- category(DIMS[3])
  if (!identical(as.integer(j$size), c(length(atr), length(flo), length(kjo)))) {
    stop(file, ": size does not match the categories")
  }
  a_votes <- match(VOTES_LABEL, atr)
  a_seats <- match(SEATS_LABEL, atr)
  if (anyNA(c(a_votes, a_seats))) stop(file, ": no '", VOTES_LABEL, "' or no '", SEATS_LABEL, "' category")

  status <- rep(NA_character_, n)
  if (length(j$status)) {
    st <- unlist(j$status)
    status[as.integer(names(st)) + 1L] <- unname(st)
  }
  if (!all(status %in% c(NA, ".", "-"))) stop(file, ": statuses other than '.' and '-': ", listed(setdiff(status, c(NA, ".", "-"))))
  if (any(is.na(j$value) & is.na(status))) stop(file, ": null value(s) without a status")
  value <- ifelse(is.na(j$value), 0L, j$value)
  # WHY: row-major values vary Kjördæmi fastest, so reversing the size gives [kjördæmi, flokkur, atriði]
  cube <- array(value, dim = rev(j$size))
  marks <- array(status, dim = rev(j$size))

  flo_total <- flo == TOTAL_LABEL
  kjo_total <- kjo == TOTAL_LABEL
  if (sum(flo_total) != 1 || sum(kjo_total) != 1) stop(file, ": not one 'Alls' category in Flokkur and one in Kjordaemi")
  for (a in c(a_votes, a_seats)) {
    m <- cube[, , a]
    if (!all(colSums(m[!kjo_total, , drop = FALSE]) == m[kjo_total, ]) ||
      !all(rowSums(m[, !flo_total, drop = FALSE]) == m[, flo_total])) {
      stop(file, ": cells do not add up to the totals for ", atr[a])
    }
  }

  k_labels <- kjo[!kjo_total]
  k <- match(k_labels, KJORDAEMI$hagstofa)
  if (anyNA(k)) stop(file, ": unknown kjordaemi label(s): ", listed(k_labels[is.na(k)]))
  if (anyDuplicated(k) || length(k) != nrow(KJORDAEMI)) stop(file, ": the kjordaemi are not the six of KJORDAEMI once each")

  names_file <- flo[!flo_total]
  if (anyDuplicated(names_file)) stop(file, ": repeated list(s): ", listed(unique(names_file[duplicated(names_file)])))
  only_file <- setdiff(names_file, list_table$list_name)
  only_table <- setdiff(list_table$list_name, names_file)
  if (length(only_file) || length(only_table)) {
    stop(
      file, " and ", basename(lists), " name different lists; only in ", file, ": ", listed(only_file),
      "; only in ", basename(lists), ": ", listed(only_table)
    )
  }

  # WHY: whether a list ran comes from its vote cell: the seat cells also mark
  # Landsbyggðarflokkurinn "." (did not run) in Norðvestur, where it has 326 votes.
  out <- tibble::tibble(
    election = ELECTION_2013,
    kjordaemi = rep(KJORDAEMI$kjordaemi[k], times = length(names_file)),
    list_name = rep(names_file, each = length(k)),
    votes = as.integer(cube[!kjo_total, !flo_total, a_votes]),
    seats = as.integer(cube[!kjo_total, !flo_total, a_seats]),
    ran = !marks[!kjo_total, !flo_total, a_votes] %in% "."
  )
  where <- function(d) paste(d$list_name, "in", d$kjordaemi, collapse = "; ")
  bad <- out[!out$ran & out$votes > 0, ]
  if (nrow(bad)) stop(file, ": votes for a list that did not run: ", where(bad))
  bad <- out[!out$ran & out$seats > 0, ]
  if (nrow(bad)) stop(file, ": seats for a list that did not run: ", where(bad))
  if (sum(out$votes) != VALID_VOTES_2013) stop(file, ": ", sum(out$votes), " valid votes, not ", VALID_VOTES_2013)
  if (sum(out$seats) != SEATS_2013) stop(file, ": ", sum(out$seats), " seats, not ", SEATS_2013)

  out |>
    dplyr::mutate(flokkur = list_table$flokkur[match(list_name, list_table$list_name)], source = file) |>
    dplyr::arrange(match(kjordaemi, KJORDAEMI$kjordaemi), match(list_name, list_table$list_name)) |>
    dplyr::select(election, kjordaemi, list_name, flokkur, votes, seats, ran, source)
}

#' Write the 2013 anchor table as CSV.
#' @param path destination CSV
#' @return the table, invisibly
#' @export
write_anchor_2013 <- function(path = harness_path("truth", "anchor_2013.csv")) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[harness_path])
  anchor <- build_anchor_2013()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(anchor, path)
  invisible(anchor)
}

#' The 2013 result folded into the poll parties per kjördæmi, in the shape of
#' `fold_truth()`'s rows for the window elections.
#' @param anchor a `build_anchor_2013()` table
#' @return a tibble (election, kjordaemi, flokkur, votes, seats) with every `POLL_PARTIES`
#'   party in every kjördæmi, and 0 votes and seats where no list of it ran
#' @export
fold_anchor <- function(anchor = build_anchor_2013()) {
  box::use(R / harness / truth[fold_truth])
  fold_truth(anchor)
}

#' National votes per poll party at the 2013 election.
#' @inheritParams fold_anchor
#' @return a tibble (election, flokkur, votes, share), one row per `POLL_PARTIES` party in
#'   that order; `share` is the party's share of the national valid vote, 0 for a party
#'   with no 2013 list
#' @export
anchor_national <- function(anchor = build_anchor_2013()) {
  fold_anchor(anchor) |>
    dplyr::summarise(votes = sum(votes), .by = c(election, flokkur)) |>
    dplyr::mutate(share = votes / sum(votes))
}
