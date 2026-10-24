# The guarded scorer (spec §5.6, §5.7): the only code that puts an arm's draws next to a
# result. It reads the run's meta.json, passes the freeze guard, and only then reads the
# result and, after it, the draws.

#' The national scores that carry a batch-means standard error (`se_` prefix).
#' @export
SE_SCORES <- c("crps10", "is90", "dss10", "mdss", "d2", "mae8", "cov50", "cov90")

META_FIELDS <- c("arm", "E", "F", "party_order", "n_pred", "n_pred_k", "coalition_event")

#' The fields of a run's `meta.json` that `score_cell()` reads.
#' @param run_dir the run directory
#' @return the parsed `meta.json` (`jsonlite::read_json(simplifyVector = TRUE)`, every
#'   other field kept as parsed) with `arm` a character(1), `E` and `F` Dates,
#'   `party_order` a character vector, `n_pred` a numeric(1), `n_pred_k` a numeric vector
#'   named by kjördæmi and `coalition_event$parties` a character vector of `party_order`
#'   members; an error naming the fields it lacks or the first that does not parse
#' @export
read_run_meta <- function(run_dir) {
  meta <- jsonlite::read_json(file.path(run_dir, "meta.json"), simplifyVector = TRUE)
  lacks <- setdiff(META_FIELDS, names(meta))
  ce <- meta$coalition_event
  if (!is.null(ce) && (!is.list(ce) || is.null(ce$parties))) lacks <- c(lacks, "coalition_event$parties")
  if (length(lacks)) stop("meta.json lacks: ", paste(lacks, collapse = ", "))
  bad <- function(field) stop("meta.json: ", field, " does not parse")
  if (!is.character(meta$arm) || length(meta$arm) != 1) bad("arm")
  for (field in c("E", "F")) {
    meta[[field]] <- tryCatch(as.Date(meta[[field]]), error = function(e) as.Date(NA))
    if (length(meta[[field]]) != 1 || is.na(meta[[field]])) bad(field)
  }
  if (!is.character(meta$party_order) || !length(meta$party_order) || anyDuplicated(meta$party_order)) {
    bad("party_order")
  }
  if (!is.numeric(meta$n_pred) || length(meta$n_pred) != 1 || !(meta$n_pred > 0)) bad("n_pred")
  k <- meta$n_pred_k
  if (!is.list(k) || !length(k) || is.null(names(k)) || any(!nzchar(names(k))) || anyDuplicated(names(k)) ||
    !all(vapply(k, \(x) is.numeric(x) && length(x) == 1 && isTRUE(x > 0), logical(1)))) {
    bad("n_pred_k")
  }
  meta$n_pred_k <- vapply(k, as.numeric, numeric(1))
  meta$n_pred <- as.numeric(meta$n_pred)
  parties <- unlist(ce$parties)
  if (!is.character(parties) || !length(parties) || anyDuplicated(parties) || !all(parties %in% meta$party_order)) {
    bad("coalition_event$parties")
  }
  meta$coalition_event$parties <- parties
  meta
}

#' One reading's national scores, with batch-means standard errors.
#' @param reading the reading's name
#' @param X a draw × party share matrix (`wide_draws()`)
#' @param y the national result, named by the parties of `X`
#' @param zero_share passed to `national_scores()`
#' @return a one-row tibble: `reading`, `zero_share`, the `national_scores()` columns and
#'   `se_<score>` for each of `SE_SCORES`, from `batch_se()` over the 16 blocks of
#'   `batch_of()`
national_row <- function(reading, X, y, zero_share) {
  box::use(R / harness / scores[national_scores, batch_of, batch_se])
  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  se <- batch_se(\(x) unlist(national_scores(x, y, zero_share)[SE_SCORES]), X, block)
  out <- national_scores(X, y, zero_share)
  out[paste0("se_", SE_SCORES)] <- as.list(unname(se[SE_SCORES]))
  tibble::add_column(out, reading = reading, zero_share = zero_share, .before = 1)
}

#' Batch-means standard errors of scores of whole draws of a long draws table.
#' @param long a data frame with `.chain`, `.iteration` and `.draw`, `.draw` numbered across
#'   chains, and a row per draw and cell
#' @param score a function from the rows of some of the draws of `long` to a named numeric
#'   vector
#' @return a named numeric vector: `batch_se()` of `score` over the 16 blocks of `batch_of()`
#'   of the draws' `.chain` and `.iteration`, the blocks of `national_row()`
draws_se <- function(long, score) {
  box::use(R / harness / scores[batch_of, batch_se])
  ids <- unique(long[c(".chain", ".iteration", ".draw")])
  ids <- ids[order(ids$.chain, ids$.iteration), ]
  batch_se(\(x) score(long[long$.draw %in% x[, 1], ]), matrix(ids$.draw), batch_of(ids$.chain, ids$.iteration))
}

#' Score one forecast cell against its election result: the only function that may.
#' @param run_dir a run directory with `meta.json` (`read_run_meta()`),
#'   `eday_kjordaemi.parquet` (`.chain`, `.iteration`, `.draw`, `kjordaemi`, `flokkur`,
#'   `votes`; `.draw` numbered across chains) and `eday_national.parquet` (`.chain`,
#'   `.iteration`, `.draw`, `flokkur`, `share`)
#' @param truth the result reader, called once as `truth(E, party_order)` and returning a
#'   list as `cell_truth()` does
#' @param root the root whose `harness/` holds the freeze, decision and confirm files; the
#'   coalitions, seat layouts and every other source are the project's
#' @return a list:
#'   - `cell`: `arm`, `E`, `F` and the cell's `tag`;
#'   - `national`: two rows. `reading = "kd_aggregate"`, the scored one, is
#'   `kd_to_nat(eday_kjordaemi)` with `zero_share = 0.5 / sum(n_pred_k)`;
#'   `reading = "eday_national"`, reported beside it, has `zero_share = 0.5 / n_pred`.
#'   Each row has the `national_scores()` columns and the batch SEs `se_<score>` of
#'   `SE_SCORES`, so coverage comes with its Monte Carlo error;
#'   - `gap_pp`: 100 × the largest absolute difference between the two readings'
#'   posterior means;
#'   - `per_party`: `party_scores()` of the scored reading, with `pit_se` from `batch_se()`;
#'   - `kjordaemi`: `kd_scores()`, with the batch SEs `se_kd_crps` and `se_kd_cov90`;
#'   - `seats` and `seats_per_party`: `seat_scores()` of
#'   `seat_draws(eday_kjordaemi, layout_for(E), ineligible = "Annað")`, for the coalition
#'   `coalition_event$parties`, with `formed = formed_coalition(E, read_coalitions())`;
#'   `seats` adds the batch SEs `se_seat_crps`, `se_seat_cov90` and `se_brier`.
#'
#'   Every batch SE is `batch_se()` over the same 16 blocks of `batch_of()` of the draws'
#'   chain and iteration, the score recomputed on each block's draws.
#'
#'   With the default `truth`, `cell_truth`, it stops unless `root` is `project_root()`:
#'   that reader reads this project's results, whatever `root` is. Then, in order, it reads
#'   `meta.json`, calls `freeze_guard(E, F, root)`, refuses a confirmatory cell ("2017
#'   cells are scored by the Tier C runbook, which writes harness/CONFIRM.md"), stops with
#'   "coalition event changed since the fit: refit <run>" unless `coalition_event(F,
#'   read_coalitions())$parties` and meta.json's are the same set (the sha-skip does not
#'   cover the event), calls `truth()`, stops with "no list at E: <party>" for a party of
#'   `party_order` whose national result is 0, and only then reads the draws.
#' @export
score_cell <- function(run_dir, truth = cell_truth, root = project_root()) {
  # WHY: the default arguments are promises, forced after this import.
  box::use(
    R / harness / constants[project_root],
    R / harness / truth[cell_truth],
    R / harness / freeze[freeze_guard, target_cell],
    R / harness / tables[coalition_event, formed_coalition, read_coalitions],
    R / harness / scores[wide_draws, kd_to_nat, party_scores, kd_scores, batch_of, batch_se],
    R / harness / scores[seat_draws, seat_scores],
    R / election_utils[layout_for]
  )
  if (identical(truth, cell_truth) &&
    !identical(normalizePath(root, mustWork = FALSE), normalizePath(project_root(), mustWork = FALSE))) {
    stop("the default result reader reads this project's results, so root must be project_root(), not ", root)
  }
  meta <- read_run_meta(run_dir)
  E <- meta$E
  freeze_guard(E, meta$F, root)
  cell <- target_cell(E, meta$F)
  if (cell$confirmatory) stop("2017 cells are scored by the Tier C runbook, which writes harness/CONFIRM.md")
  coalitions <- read_coalitions()
  if (!setequal(coalition_event(meta$F, coalitions)$parties, meta$coalition_event$parties)) {
    stop("coalition event changed since the fit: refit ", basename(run_dir))
  }

  result <- truth(E, meta$party_order)
  y <- result$nat
  no_list <- meta$party_order[which(y[meta$party_order] == 0)]
  if (length(no_list)) stop("no list at E: ", paste(no_list, collapse = ", "))

  eday_kd <- arrow::read_parquet(file.path(run_dir, "eday_kjordaemi.parquet"))
  eday_nat <- arrow::read_parquet(file.path(run_dir, "eday_national.parquet"))
  # WHY: kd_scores() refuses a repeated (.draw, kjordaemi, flokkur) before kd_to_nat() pools by `.draw`.
  kjordaemi <- kd_scores(eday_kd, result$kd)
  kd_se <- draws_se(eday_kd, \(d) unlist(kd_scores(d, result$kd)[c("kd_crps", "kd_cov90")]))
  kjordaemi[c("se_kd_crps", "se_kd_cov90")] <- as.list(unname(kd_se[c("kd_crps", "kd_cov90")]))
  X <- wide_draws(kd_to_nat(eday_kd), "share")
  X_nat <- wide_draws(eday_nat, "share")
  national <- rbind(
    national_row("kd_aggregate", X, y, 0.5 / sum(meta$n_pred_k)),
    national_row("eday_national", X_nat, y, 0.5 / meta$n_pred)
  )

  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  per_party <- party_scores(X, y)
  pit_se <- batch_se(\(x) stats::setNames(party_scores(x, y)$pit, colnames(x)), X, block)
  per_party$pit_se <- unname(pit_se[per_party$flokkur])

  seats <- seat_draws(eday_kd, layout_for(E), ineligible = "Anna\u00f0")
  coalition <- meta$coalition_event$parties
  scored_seats <- seat_scores(seats, result$seats, coalition = coalition, formed = formed_coalition(E, coalitions))
  seat_se <- draws_se(seats, \(d) {
    unlist(seat_scores(d, result$seats, coalition = coalition)$summary[c("seat_crps", "seat_cov90", "brier")])
  })
  scored_seats$summary[c("se_seat_crps", "se_seat_cov90", "se_brier")] <-
    as.list(unname(seat_se[c("seat_crps", "seat_cov90", "brier")]))
  list(
    cell = tibble::tibble(arm = meta$arm, E = E, F = meta$F, tag = cell$tag),
    national = national,
    gap_pp = 100 * max(abs(colMeans(X) - colMeans(X_nat)[colnames(X)])),
    per_party = per_party,
    kjordaemi = kjordaemi,
    seats = scored_seats$summary,
    seats_per_party = scored_seats$per_party
  )
}
