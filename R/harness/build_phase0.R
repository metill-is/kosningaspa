# Phase 0 rebuild of the v2 harness. A plain script, not a box module.
# Run from the project root: LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R
#
# It verifies the snapshot, then rewrites every generated file committed under harness/
# that builds offline, through its own writer and each after the files it reads:
#   harness/sources/seats_2021_official.csv   harness/sources/althingi/seats_2021_official.R
#   harness/truth/elections_official.csv      write_truth()
#   harness/availability.csv and availability_report.md   write_availability()
#   harness/entry_table.csv                   write_entry_table()
# It then prints the polls per availability rule and each forecast cell's admitted polls.
# It does not rebuild the Wikipedia fieldwork tables (their parser needs page revisions
# fetched from the web; harness/sources/README.md) or snapshot_manifest.csv (written only
# by a deliberate snapshot_inputs()). The output is deterministic, so a run on a clean
# checkout leaves `git status --porcelain harness/` empty.
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("build_phase0.R needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
# WHY: the box path below is the working directory, so check it is the project root first.
stopifnot("run from the project root (no R/harness/constants.R here)" = file.exists("R/harness/constants.R"))
options(box.path = normalizePath("."), width = 120)
box::use(
  R / harness / availability[availability_counts, read_constituency, read_polling, write_availability],
  R / harness / constants[ELECTION_HOUSE, TARGETS, WINDOW_START],
  R / harness / info_set[info_set, read_inputs, validate_inputs],
  R / harness / inputs[verify_snapshot],
  R / harness / tables[write_entry_table],
  R / harness / truth[write_truth]
)
show <- function(x) print(as.data.frame(x), row.names = FALSE)

verify_snapshot()
cat("Snapshot verified against harness/snapshot_manifest.csv\n")

seats_script <- file.path("harness", "sources", "althingi", "seats_2021_official.R")
status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(seats_script))
if (!identical(as.integer(status), 0L)) stop(seats_script, " exited with status ", status)
write_truth()
av <- write_availability()
write_entry_table()
cat(
  "Rewrote harness/sources/seats_2021_official.csv, harness/truth/elections_official.csv,",
  "harness/availability.csv, harness/availability_report.md, harness/entry_table.csv\n"
)

polls <- dplyr::bind_rows(
  national = dplyr::distinct(dplyr::filter(read_polling(), date >= WINDOW_START), date, fyrirtaeki),
  kjordaemi = dplyr::distinct(
    dplyr::filter(read_constituency(), date >= WINDOW_START, fyrirtaeki != ELECTION_HOUSE), date, fyrirtaeki
  ),
  .id = "level"
)
counts <- availability_counts(av)
cat("\nPolls per availability rule\n")
show(counts)
per_level <- c(national = sum(polls$level == "national"), kjordaemi = sum(polls$level == "kjordaemi"))
counted <- as.vector(tapply(counts$n, counts$level, sum)[names(per_level)])
cat(sprintf(
  "Counts sum to %d; the snapshot holds %d polls from %s (national %d, kjordaemi %d)\n",
  sum(counts$n), nrow(polls), format(WINDOW_START), per_level[["national"]], per_level[["kjordaemi"]]
))
stopifnot("the availability counts do not sum to the polls" = identical(counted, unname(per_level)))
non_election <- av[av$rule != "election", ]
stopifnot("a non-election poll is available on or before its midpoint" = all(non_election$available_from > non_election$date))
cat(sprintf("available_from > date for all %d non-election rows\n", nrow(non_election)))

inp <- read_inputs()
cells <- dplyr::bind_rows(lapply(seq_len(nrow(TARGETS)), \(i) {
  admitted <- validate_inputs(info_set(TARGETS$E[i], TARGETS$F[i], inp))$admitted
  tibble::tibble(
    E = TARGETS$E[i], F = TARGETS$F[i], tag = TARGETS$tag[i],
    national = sum(admitted$level == "national"), kjordaemi = sum(admitted$level == "kjordaemi")
  )
}))
cat("\nAdmitted polls per forecast cell (every cell passes validate_inputs())\n")
show(cells)
