box::use(
  R / harness / anchors[anchor_national],
  R / harness / availability[with_older_polls],
  R / harness / constants[FUNDAMENTALS_RENAMED],
  R / harness / fundamentals[fundamentals_party],
  R / harness / info_set[blank_party_polls, info_set, validate_inputs],
  R / harness / r0[fundamentals_rows, r0_stan_data, read_r0_config],
  R / harness / runs[run_id],
  R / harness / tables[grid_dates],
  R / harness / truth[prev_national_shares],
  R / harness / v2[v2_stan_data],
  R / harness / v2_reference[v2_mutants]
)
E16 <- as.Date("2016-10-29")
ANCHOR16 <- as.Date("2013-04-27")
DAYS16 <- as.Date(c("2016-05-02", "2016-09-12", "2016-10-15", "2016-10-28"))
# The date of the window's first poll, Gallup's of July 2012 in the older polls table.
FIRST16 <- as.Date("2012-07-12")
NEWCOMERS16 <- POLL_PARTIES[6:7]
PIRATAR16 <- POLL_PARTIES[5]
OLDER16 <- POLL_PARTIES[c(1:5, 10)]

# The four 2016 cells and both arms' builds of them, built on first call and kept for the run;
# V2 takes stage1_stub()'s mu_f, so no stage-1 fit runs.
cells16 <- function() {
  skip_without_snapshot()
  cached("arms16_cells", lapply(DAYS16, \(F) info_set(E16, F, v2_inputs())))
}
v2_16 <- function() cached("arms16_v2", lapply(cells16(), \(x) v2_stan_data(x, cfg_v2(), stage1 = stage1_stub)))
r0_16 <- function() cached("arms16_r0", lapply(cells16(), \(x) r0_stan_data(x, read_r0_config())))

# The grid and the epochs the two builders use for one cell, as text lines, and their sha256.
arms_text <- function(info, v2, r0) {
  line <- function(name, x) paste0(name, ": ", paste(if (inherits(x, "Date")) format(x) else as.character(x), collapse = " "))
  c(
    line("grid", grid_dates(info, v2$data$campaign_days)),
    line("v2 G", v2$data$G),
    line("v2 delta_days", sprintf("%.17g", c(v2$data$delta_days))),
    line("v2 n_epoch", v2$data$n_epoch),
    line("v2 closed", v2$meta$closed_elections),
    line("v2 grid_res", c(v2$data$grid_res)),
    line("v2 grid_epoch", c(v2$data$grid_epoch)),
    line("v2 campaign", c(v2$data$campaign)),
    line("v2 entry_epoch", paste(v2$meta$party_order, c(v2$data$entry_epoch))),
    line("r0 n_e", paste(names(r0$meta$n_e), r0$meta$n_e)),
    line("r0 mu_gamma_scale", sprintf("%.17g", r0$data$mu_gamma_scale))
  )
}
arms_digest <- function(info, v2, r0) {
  digest::digest(paste(arms_text(info, v2, r0), collapse = "\n"), algo = "sha256", serialize = FALSE)
}
# WHY: recorded on 2026-11-11 with arms_digest() from the tree at 23a0b77, before the builders
# read the cell's own elections, so a change that reaches an existing cell's grid or epochs shows here.
ARMS_DIGESTS <- tibble::tribble(
  ~E, ~F, ~G, ~n_epoch, ~digest,
  "2017-10-28", "2017-10-14", 71L, 2L, "380a13e65de60b4b668f9ba3bf43be7d32c6ea8d4369fe1eaa01865ceac7bab7",
  "2017-10-28", "2017-10-27", 77L, 2L, "e56e4d2466225afbf8717fe5095d6eeb311386d522ad7baff73fa5635ff15fb1",
  "2021-09-25", "2021-03-29", 162L, 3L, "a4357160be4795cc43d03661e8128a2e169d88d246dfa9fd0b9187fbe7e5f25f",
  "2021-09-25", "2021-08-09", 174L, 3L, "c9142a135dfb6a9a0fea1698b1aa23ebb69a132a56e2d294e7c81e04dddf9867",
  "2021-09-25", "2021-09-11", 182L, 3L, "5bc54e9af27cfd9ccdc54047b0ec8c86e991cb91e95fb70b6383d9ee197a702c",
  "2021-09-25", "2021-09-24", 190L, 3L, "2720e030bb208e5c9f276d1b238b7bc1a5800aa3a57d9c8877288ab9ee3d051b",
  "2024-11-30", "2024-06-03", 251L, 4L, "c1dd90a8133a497303f81f41a5aaa6a3011420cd1db043cedaa7d5c6ac902c01",
  "2024-11-30", "2024-10-14", 258L, 4L, "c4207fd99cb1d94e49e9310e90d3b127e9d0f6f5af8a31c7302aae513aa02658",
  "2024-11-30", "2024-11-16", 273L, 4L, "b363e0d8c9e36ede7852b37ba47e190633dc09d2d4bcb12fad8eb2404afe18e9",
  "2024-11-30", "2024-11-29", 278L, 4L, "cb5a3753d97c353948fdc3d1c2267cddd9e121e9cc1057ef81ee73b1371fbd23"
)

test_that("V2 builds each 2016 day with 2013 as its one closed election after the window's first 40 polls, no kjordaemi poll, the cell's parties, each party existing from its entry date, Piratar entering in epoch 1 and the newcomers in epoch 2 (a grid that still starts at the election, or a builder that counts epochs from the global list)", {
  cells <- cells16()
  for (i in seq_along(cells)) {
    x <- cells[[i]]
    b <- v2_16()[[i]]
    d <- b$data
    lab <- format(x$F)
    g <- grid_dates(x, d$campaign_days)
    res <- c(d$grid_res)
    expect_identical(d$n_closed, 1L, label = lab)
    expect_identical(d$n_epoch, 2L, label = lab)
    expect_identical(b$meta$closed_elections, format(ANCHOR16), label = lab)
    expect_identical(g[1], FIRST16, label = lab)
    expect_identical(g[res], ANCHOR16, label = lab)
    expect_identical(c(d$grid_epoch), rep(1:2, c(res, d$G - res)), label = lab)
    expect_identical(sum(c(d$grid_nat) < res), 40L, label = lab)
    expect_equal(sum(d$y_res), 189023, label = lab)
    expect_identical(c(d$N_kd, d$J), c(0L, 0L), label = lab)
    expect_identical(dim(d$y_kd), c(0L, d$P), label = lab)
    expect_identical(b$meta$party_order, intersect(POLL_PARTIES, x$parties), label = lab)
    expect_true(all(OLDER16 %in% x$parties), label = lab)
    expect_identical(d$P, length(x$parties), label = lab)
    e <- x$entry[match(b$meta$party_order, x$entry$flokkur), ]
    expect_identical(e$entry_epoch, ifelse(e$flokkur %in% NEWCOMERS16, 2L, 1L), label = lab)
    expect_identical(c(d$entry_epoch), ifelse(e$flokkur %in% NEWCOMERS16, 2L, ifelse(e$flokkur == PIRATAR16, 1L, 0L)), label = lab)
    expect_identical(b$meta$newcomers, intersect(c(PIRATAR16, NEWCOMERS16), x$parties), label = lab)
    first <- apply(d$exists, 2, \(col) which(col == 1L)[1])
    expect_identical(g[first], e$first_nonzero, label = lab)
    for (p in seq_len(d$P)) expect_identical(as.integer(d$exists[, p]), as.integer(seq_len(d$G) >= first[p]), label = paste(lab, e$flokkur[p]))
  }
  expect_identical(lengths(lapply(cells, `[[`, "parties")), c(7L, 8L, 8L, 8L))
})

test_that("V2 marks as campaign every step of each 2016 grid that lies inside the 47 days before the 2013 election or before E, and no other: 18 steps covering 47 days before 2013 (a builder that takes the elections from the global list, which lacks 2013, leaving the 2013 campaign unmarked)", {
  cells <- cells16()
  for (i in seq_along(cells)) {
    x <- cells[[i]]
    d <- v2_16()[[i]]$data
    lab <- format(x$F)
    expect_equal(d$campaign_days, 47, label = lab)
    g <- grid_dates(x, d$campaign_days)
    start <- g[-length(g)]
    end <- g[-1]
    inside <- function(election) start >= election - 47 & end <= election
    want <- as.integer(inside(ANCHOR16) | inside(E16))
    marked <- as.integer(c(d$campaign))
    expect_identical(marked, want, label = lab)
    in2013 <- marked == 1L & end <= ANCHOR16
    expect_identical(sum(in2013), 18L, label = lab)
    expect_equal(sum(as.numeric(end - start)[in2013]), 47, label = lab)
  }
})

test_that("R0 builds each 2016 day with the 2013 national result as one election row of 189 023 votes after the window's first 40 polls, and one closed election for the older parties (a 2016 cell fitted with no anchor at all, or with no poll before it)", {
  cells <- cells16()
  nat <- anchor_national(v2_inputs()$anchor_2013)
  for (i in seq_along(cells)) {
    x <- cells[[i]]
    b <- r0_16()[[i]]
    d <- b$data
    po <- b$meta$party_order
    lab <- format(x$F)
    k <- x$kosning_nat
    expect_identical(unique(k$date), ANCHOR16, label = lab)
    expect_identical(unique(k$fyrirtaeki), "Kosning", label = lab)
    expect_identical(k$flokkur, intersect(POLL_PARTIES, x$parties), label = lab)
    expect_equal(k$n, nat$votes[match(k$flokkur, nat$flokkur)], label = lab)
    expect_no_error(validate_inputs(x))
    row <- which(d$house_n == 1)
    expect_length(row, 1)
    expect_identical(as.integer(d$date_n[row]), match(ANCHOR16, sort(unique(c(k$date, x$polls_nat$date)))), label = lab)
    expect_identical(sum(d$date_n < d$date_n[row]), 40L, label = lab)
    expect_equal(sum(d$y_n[row, ]), 189023, label = lab)
    expect_equal(unname(d$y_n[row, ]), nat$votes[match(po, nat$flokkur)], label = lab)
    expect_equal(sum(d$y_k[d$house_k == 1, ]), 189023, label = lab)
    expect_equal(c(d$N_k, d$N_obs_k), c(6, 1), label = lab)
    expect_equal(unname(b$meta$n_e), as.numeric(!po %in% NEWCOMERS16), label = lab)
    expect_equal(unname(b$meta$n_e), vapply(po, \(p) sum(k$flokkur == p & k$n > 0), numeric(1), USE.NAMES = FALSE), label = lab)
    expect_equal(d$mu_gamma_scale, as.numeric(!po[-1] %in% NEWCOMERS16), label = lab)
    expect_identical(b$meta$newcomers, intersect(NEWCOMERS16, x$parties), label = lab)
  }
})

test_that("each arm builds each 2016 day, R0's Annad target row carrying Annad's share of the official 2013 result and R0's newcomer check reading the cell's prev_shares (an Annad row built from the fundamentals table's rounded shares, or a newcomer check that bypasses the cell's prev_shares)", {
  cells <- cells16()
  nat <- anchor_national(v2_inputs()$anchor_2013)
  annad <- POLL_PARTIES[10]
  for (i in seq_along(cells)) {
    x <- cells[[i]]
    lab <- format(x$F)
    b <- r0_16()[[i]]
    fund <- fundamentals_rows(x, b$meta$party_order)
    expect_equal(fund$voteshare_prior[fund$flokkur == annad & fund$date == E16], 100 * nat$share[nat$flokkur == annad], label = lab)
    expect_setequal(v2_16()[[i]]$meta$party_order, b$meta$party_order)
  }
  x <- cells[[4]]
  x$prev_shares$share[x$prev_shares$flokkur == NEWCOMERS16[1]] <- 0.01
  expect_error(r0_stan_data(x, read_r0_config()), paste("a newcomer had votes at 2013-04-27:", NEWCOMERS16[1]), fixed = TRUE)
})

# WHY: as tests/harness/test-r0-inputs.R does on the 2024 eve, a label with no rename folds into
# Annad, as Framsokn would without its FUNDAMENTALS_RENAMED entry; here the label is changed in the
# table the cell is built from, so the cell's prev_shares are whatever info_set() takes for 2013.
# Every row of the party is relabelled: with the 2013 row alone, fundamentals_inputs() refuses the
# cell, the party's 2016 prior having no 2013 votes beside it.
test_that("R0 refuses the 2016 eve cell built from a fundamentals table whose Framsoknarflokkurinn rows, 2013's among them, carry a label outside FUNDAMENTALS_RENAMED, because the cell's previous shares are the official 2013 result, while prev_national_shares() of the same table gives that party no previous share, so a check reading it could not see the fold (R0's newcomer check comparing the fundamentals table with itself)", {
  inp <- v2_inputs()
  party <- POLL_PARTIES[2]
  label <- "Framsokn gamli"
  expect_false(label %in% c(names(FUNDAMENTALS_RENAMED), POLL_PARTIES))
  fund <- inp$fundamentals
  hit <- fundamentals_party(fund$flokkur) == party
  expect_true(any(hit & fund$date == ANCHOR16))
  fund$flokkur[hit] <- label
  expect_true(is.na(prev_national_shares(E16, inp$truth, fund)[[party]]))
  expect_gt(prev_national_shares(E16, inp$truth, inp$fundamentals)[[party]], 0)
  mislabelled <- inp
  mislabelled$fundamentals <- fund
  x <- info_set(E16, DAYS16[4], mislabelled)
  expect_true(party %in% x$parties)
  expect_false(party %in% x$fundamentals$S_f)
  expect_gt(x$prev_shares$share[x$prev_shares$election == E16 & x$prev_shares$flokkur == party], 0)
  expect_error(r0_stan_data(x, read_r0_config()), paste("R0 input: a newcomer had votes at 2013-04-27:", party), fixed = TRUE)
})

# The rows of each arm's national poll data, as "date house" keys: V2's in date and house order,
# R0's from date_n and house_n, the 2013 Kosning row included.
v2_keys <- function(x) {
  k <- dplyr::distinct(x$polls_nat, date, fyrirtaeki)
  k <- k[order(k$date, k$fyrirtaeki, method = "radix"), ]
  paste(format(k$date), k$fyrirtaeki)
}
r0_keys <- function(x, b) {
  dates <- sort(unique(c(x$kosning_nat$date, x$polls_nat$date)))
  paste(format(dates[b$data$date_n]), b$meta$house_levels[b$data$house_n])
}

test_that("on each 2016 day both arms read Annad as not reported in each poll the rule names, a count of 0 outside the poll's reported parties, whose count is one lower than the parties the raw row reports, and as reported in every other poll whose Annad is positive (a builder that reads a 2016 cell's 0 for Annad as an observed count, or drops such a poll)", {
  cells <- cells16()
  inp <- v2_inputs()
  raw <- with_older_polls(inp$polling, inp$older_polls)
  annad <- POLL_PARTIES[10]
  for (i in seq_along(cells)) {
    x <- cells[[i]]
    lab <- format(x$F)
    adm <- dplyr::semi_join(raw, dplyr::distinct(x$polls_nat, date, fyrirtaeki), by = c("date", "fyrirtaeki"))
    blank <- blank_party_polls(adm)
    rule <- paste(format(blank$date), blank$fyrirtaeki)
    expect_length(rule, c(2L, 5L, 10L, 13L)[i])
    keys <- v2_keys(x)
    v2 <- v2_16()[[i]]
    r0 <- r0_16()[[i]]
    arms <- list(
      v2 = list(po = v2$meta$party_order, y = v2$data$y_nat, n_rep = v2$data$n_rep_nat, idx = v2$data$idx_nat, keys = keys),
      r0 = list(po = r0$meta$party_order, y = r0$data$y_n, n_rep = r0$data$n_parties_n, idx = r0$data$party_index_n, keys = r0_keys(x, r0))
    )
    expect_identical(nrow(arms$v2$y), length(keys), label = lab)
    expect_identical(nrow(arms$r0$y), length(keys) + 1L, label = lab)
    for (key in keys) {
      rows <- adm[paste(format(adm$date), adm$fyrirtaeki) == key, ]
      reports <- intersect(x$parties, rows$flokkur[rows$n > 0])
      if (!annad %in% reports) next
      named <- key %in% rule
      for (a in names(arms)) {
        m <- arms[[a]]
        j <- match(key, m$keys)
        at <- paste(lab, a, key)
        expect_false(is.na(j), label = at)
        col <- match(annad, m$po)
        idx <- as.integer(m$idx[j, ])
        if (named) {
          expect_identical(as.integer(m$y[j, col]), 0L, label = at)
          expect_false(col %in% idx, label = at)
          expect_identical(as.integer(m$n_rep[j]), length(m$po) - length(setdiff(m$po, reports)) - 1L, label = at)
          expect_identical(sort(idx[idx > 0]), sort(match(setdiff(reports, annad), m$po)), label = at)
        } else {
          expect_true(col %in% idx, label = at)
        }
      }
    }
  }
})

test_that("for every TARGETS cell outside 2016 the grid and the epochs both builders use are as recorded before the cell's own elections were read (an existing cell's walk or hedge moved)", {
  skip_without_snapshot()
  earlier <- TARGETS[TARGETS$E != E16, ]
  expect_identical(paste(ARMS_DIGESTS$E, ARMS_DIGESTS$F), paste(earlier$E, earlier$F))
  for (i in seq_len(nrow(ARMS_DIGESTS))) {
    r <- ARMS_DIGESTS[i, ]
    x <- info_set(r$E, r$F, v2_inputs())
    expect_identical(x$elections, ELECTIONS)
    v2 <- v2_stan_data(x, cfg_v2(), stage1 = stage1_stub)
    r0 <- r0_stan_data(x, read_r0_config())
    lab <- paste(r$E, r$F)
    expect_identical(c(v2$data$G, v2$data$n_epoch), c(r$G, r$n_epoch), label = lab)
    expect_identical(arms_digest(x, v2, r0), r$digest, label = lab)
  }
})

test_that("an information set without its elections is refused by grid_dates() and both builders (a builder that falls back to the global list)", {
  x <- cells16()[[4]]
  x$elections <- NULL
  expect_error(grid_dates(x), "grid_dates: info has no elections", fixed = TRUE)
  expect_error(v2_stan_data(x, cfg_v2(), stage1 = stage1_stub), "v2 input: info$elections does not hold E", fixed = TRUE)
  expect_error(r0_stan_data(x, read_r0_config()), "R0 input: info$elections holds E", fixed = TRUE)
  x$elections <- ELECTIONS
  expect_error(v2_stan_data(x, cfg_v2(), stage1 = stage1_stub), "v2 input: E is not a window election after the first", fixed = TRUE)
})

test_that("a 2013 Kosning row in the polling data is refused rather than doubled, and a 2013 list given to a party outside the cell is refused rather than dropped (two election rows on one date; the letter rule's J and M lists)", {
  inp <- v2_inputs()
  planted <- inp
  k16 <- inp$polling[inp$polling$fyrirtaeki == "Kosning" & inp$polling$date == E16, ]
  k16$date <- ANCHOR16
  planted$polling <- rbind(inp$polling, k16)
  av <- inp$availability[inp$availability$fyrirtaeki == "Kosning" & inp$availability$date == E16, ]
  av$date <- ANCHOR16
  av$available_from <- ANCHOR16 + 1
  planted$availability <- rbind(inp$availability, av)
  expect_error(
    info_set(E16, DAYS16[4], planted),
    "the polling data hold a Kosning row for 2013-04-27, which the 2013 anchor's row would duplicate",
    fixed = TRUE
  )
  moved <- inp
  i <- which(moved$anchor_2013$flokkur == POLL_PARTIES[10] & moved$anchor_2013$votes > 0)[1]
  moved$anchor_2013$flokkur[moved$anchor_2013$list_name == moved$anchor_2013$list_name[i]] <- POLL_PARTIES[8]
  expect_error(
    info_set(E16, DAYS16[4], moved),
    paste0("would drop Kosning row(s) with n > 0: Kosning 2013-04-27 ", POLL_PARTIES[8]),
    fixed = TRUE
  )
})

test_that("V2's Stan log density on the 2016 eve cell, which has no kjordaemi poll, matches the reference at posterior, off-posterior and prior points, and each mutant it exercises is rejected (a guard that drops or double-counts a term when N_kd is 0)", {
  skip_unless_slow()
  d <- v2_16()[[4]]$data
  expect_identical(c(d$N_kd, d$J), c(0L, 0L))
  m <- v2_methods("Stan/forecast_v2.stan", d)
  pts <- lp_points(m, d, n_post = 20, n_off = 10)
  ok <- expect_lp_matches(m, d, pts)
  ratios <- c()
  for (mu in v2_mutants(d)) if (mutant_exercised(d, pts, mu)) ratios[mu] <- expect_lp_rejects(m, d, pts, mu)
  expect_true(all(c("no_leaving_step", "fund_uncentred") %in% names(ratios)))
  message(sprintf(
    "lp 2016 eve (N_kd 0, J 0): %d points, lp constant range %.3g (tol %.3g); exercised mutants, range / (100 tol): %s",
    length(pts), ok$range, ok$tol, paste(sprintf("%s %.3g", names(ratios), ratios), collapse = ", ")
  ))
})

test_that("a short fit of each arm on the 2016 eve cell runs to DONE through fit_cell.R (data that build but that CmdStan cannot read or sample, such as V2's zero-row kjordaemi matrices)", {
  skip_unless_slow()
  skip_without_snapshot()
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  for (a in c("r0", "v2")) {
    out <- withr::with_dir(root(), suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("R/harness/fit_cell.R", a, format(E16), format(DAYS16[4]), "1", "20", "10"),
      stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
    )))
    expect_null(attr(out, "status"), info = paste(utils::tail(out, 30), collapse = "\n"))
    dir <- file.path(results, run_id(a, E16, DAYS16[4], 1))
    expect_true(paste("DONE", dir) %in% out, label = a)
    expect_true(file.exists(file.path(dir, "meta.json")), label = a)
    meta <- jsonlite::read_json(file.path(dir, "meta.json"))
    expect_identical(c(meta$E, meta$F), c(format(E16), format(DAYS16[4])))
    expect_identical(meta$n_polls_kd, 0L, label = a)
    if (a == "v2") {
      expect_identical(c(meta$n_closed, meta$n_kd_polls), c(1L, 0L))
      expect_identical(unlist(meta$closed_elections), format(ANCHOR16))
    }
  }
})
