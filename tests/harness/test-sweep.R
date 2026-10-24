# Every forecast day of the three target cycles through info_set() and validate_inputs(),
# so a leak or a broken availability rule shows on any day, not only on the ten TARGETS
# cells. Slow: runs only with HARNESS_SLOW=1.
box::use(
  R / harness / constants[previous_election, ELECTION_HOUSE, WINDOW_START],
  R / harness / info_set[info_set, read_inputs, validate_inputs]
)

# WHY: pinned from a sweep on 2026-10-24, so a changed availability rule or economy release shows.
SWEEP <- tibble::tibble(
  E = c("2017-10-28", "2021-09-25", "2024-11-30"),
  first_ok = c("2017-03-09", "2021-02-26", "2024-02-29"),
  n_ok = c(233L, 211L, 275L)
)

av <- readr::read_csv(
  root("harness", "availability.csv"),
  col_types = readr::cols(date = "D", fieldwork_end = "D", available_from = "D", .default = "c"),
  progress = FALSE
)

keys <- function(rows) sort(unique(paste(rows$date, rows$fyrirtaeki)), method = "radix")

oracle_nat <- function(F) {
  keys(av[av$level == "national" & av$fyrirtaeki != ELECTION_HOUSE & av$date >= WINDOW_START & av$available_from <= F, ])
}

subset_of <- function(a, b) {
  all(a$nat %in% b$nat) && all(a$kd %in% b$kd) && all(a$parties %in% b$parties)
}

kosning_fixed <- function(day, first) {
  k <- day$kosning
  old <- k$flokkur %in% first$parties
  identical(k[old, ], first$kosning) &&
    all(k$flokkur[!old] %in% setdiff(day$parties, first$parties)) && all(k$n[!old] == 0)
}

test_that("every day of the 2017, 2021 and 2024 cycles validates from the economy release on", {
  skip_without_snapshot()
  skip_unless_slow()
  inp <- read_inputs()
  for (i in seq_len(nrow(SWEEP))) {
    E <- as.Date(SWEEP$E[i])
    days <- seq(previous_election(E) + 1, E - 1, by = "day")
    res <- lapply(days, \(F) tryCatch(
      {
        info <- info_set(E, F, inp)
        validate_inputs(info)
        list(
          nat = keys(info$polls_nat), kd = keys(info$polls_kd), parties = info$parties,
          fixed = list(info$results, info$prev_shares), kosning = info$kosning_nat,
          late = max(info$polls_nat$date) >= F || max(c(info$polls_nat$available_from, info$polls_kd$available_from)) > F
        )
      },
      error = conditionMessage
    ))
    ok <- vapply(res, is.list, logical(1))
    expect_equal(sum(ok), SWEEP$n_ok[i])
    expect_equal(min(days[ok]), as.Date(SWEEP$first_ok[i]))
    expect_true(all(ok[days >= as.Date(SWEEP$first_ok[i])]))
    expect_true(all(grepl("before the first release of economy covariate", unlist(res[!ok]), fixed = TRUE)))
    good <- res[ok]
    expect_false(any(vapply(good, `[[`, logical(1), "late")))
    expect_true(all(vapply(seq_along(good), \(j) identical(good[[j]]$nat, oracle_nat(days[ok][j])), logical(1))))
    expect_true(all(vapply(seq_along(good)[-1], \(j) subset_of(good[[j - 1]], good[[j]]), logical(1))))
    expect_true(all(vapply(good[-1], \(g) identical(g$fixed, good[[1]]$fixed), logical(1))))
    # WHY: a newcomer's structural-zero election rows join as it enters, so only existing rows must stay fixed.
    expect_true(all(vapply(good[-1], \(g) kosning_fixed(g, good[[1]]), logical(1))))
  }
})
