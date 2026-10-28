box::use(
  R / harness / r0[R0_KJORDAEMI, r0_production_inputs, r0_stan_data, read_r0_config],
  R / harness / info_set[info_set, read_inputs],
  R / harness / constants[previous_election],
  R / harness / fundamentals[fundamentals_party]
)
inp <- if (snapshot_present()) read_inputs()
infos <- if (snapshot_present()) lapply(seq_len(nrow(TARGETS)), \(i) info_set(TARGETS$E[i], TARGETS$F[i], inp))
r0 <- if (snapshot_present()) lapply(infos, r0_stan_data)
at <- function(E) r0[which(TARGETS$E == as.Date(E))]
info_at <- function(E) infos[which(TARGETS$E == as.Date(E))]
cell <- function(E, F) which(TARGETS$E == as.Date(E) & TARGETS$F == as.Date(F))

test_that("the config holds the four keys, and a missing or extra key is an error", {
  expect_identical(
    read_r0_config(),
    list(campaign_days = 47L, desired_weight = 0.22, weight_time = 180L, debut = "median_new_party_before_E")
  )
  cfg <- jsonlite::read_json(root("harness", "configs", "r0.json"))
  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(cfg[-1], path, auto_unbox = TRUE)
  expect_error(read_r0_config(path), "campaign_days")
  jsonlite::write_json(c(cfg, list(extra = 1)), path, auto_unbox = TRUE)
  expect_error(read_r0_config(path), "extra")
  jsonlite::write_json(utils::modifyList(cfg, list(debut = "mean")), path, auto_unbox = TRUE)
  expect_error(read_r0_config(path), "debut")
})

test_that("the hedge scale is 1/sqrt(n_e), with no correction for a party new to the window", {
  skip_without_snapshot()
  for (x in at("2024-11-30")) expect_equal(unname(x$data$mu_gamma_scale), c(rep(1 / sqrt(3), 7), 1 / sqrt(2), 1))
  for (x in at("2021-09-25")) expect_equal(unname(x$data$mu_gamma_scale), c(rep(1 / sqrt(2), 7), 1, 0))
  for (x in at("2017-10-28")) {
    expect_equal(unname(x$data$mu_gamma_scale), c(rep(1, 7), 0))
    expect_equal(x$meta$party_order, c("Annað", setdiff(POLL_PARTIES, c("Annað", "Sósíalistaflokkurinn"))))
    expect_equal(x$data$P, 9)
  }
})

test_that("expected valid votes are the previous election's, by name", {
  skip_without_snapshot()
  nk <- function(E) at(E)[[1]]$meta$n_pred_k
  expect_equal(names(nk("2024-11-30")), R0_KJORDAEMI)
  expect_equal(unname(nk("2024-11-30")), c(35118, 35504, 58608, 17251, 23536, 29713))
  expect_equal(unname(nk("2021-09-25")), c(35878, 35721, 55740, 17252, 23611, 28057))
  expect_equal(unname(nk("2017-10-28")), c(34988, 34880, 53301, 16760, 22703, 27016))
  for (x in r0) expect_identical(x$data$n_pred_k, unname(x$meta$n_pred_k))
})

test_that("n_e is each party's count of window elections, from the entry table and the election rows alike", {
  skip_without_snapshot()
  for (i in seq_along(r0)) {
    k <- infos[[i]]$kosning_nat
    counts <- vapply(r0[[i]]$meta$party_order, \(p) sum(k$flokkur == p & k$n > 0), numeric(1))
    expect_equal(r0[[i]]$meta$n_e, counts)
    expect_equal(names(r0[[i]]$meta$n_e), r0[[i]]$meta$party_order)
  }
})

test_that("the kjördæmi levels are R0_KJORDAEMI and the prediction weights are n_pred_k's shares", {
  skip_without_snapshot()
  expect_identical(R0_KJORDAEMI, c("Reykjavík Norður", "Reykjavík Suður", "Suðvestur", "Norðvestur", "Norðaustur", "Suður"))
  for (x in r0) {
    expect_identical(x$meta$kjordaemi_levels, R0_KJORDAEMI)
    expect_identical(colnames(x$data$constituency_weights), R0_KJORDAEMI)
    expect_equal(x$data$constituency_weights_pred, x$data$n_pred_k / sum(x$data$n_pred_k), tolerance = 1e-12)
    expect_equal(x$data$K, 6)
  }
})

test_that("the campaign is 47 days and month_before_election marks the 47 days before each election", {
  skip_without_snapshot()
  for (i in seq_along(r0)) {
    x <- r0[[i]]
    info <- infos[[i]]
    expect_equal(x$data$days_between_stjornarslit_and_election, 47)
    dates <- sort(unique(c(info$kosning_nat$date, info$polls_nat$date)))
    window <- ELECTIONS[ELECTIONS <= info$E]
    days_to_next <- vapply(dates, \(d) as.numeric(min(window[window >= d]) - d), numeric(1))
    expect_equal(x$data$D, length(dates))
    expect_equal(as.numeric(x$data$month_before_election), as.numeric(days_to_next <= 47))
    expect_equal(x$meta$eday_index, x$data$D + x$data$pred_y_time_diff)
    expect_equal(x$data$pred_y_time_diff, as.numeric(info$E - max(dates)))
  }
})

test_that("the newcomers get the median debut share, centred like the others", {
  skip_without_snapshot()
  newcomers <- list("2017-10-28" = "Miðflokkurinn", "2021-09-25" = "Sósíalistaflokkurinn", "2024-11-30" = character(0))
  debut <- c("2017-10-28" = 0.0715, "2021-09-25" = 0.072)
  for (i in seq_along(r0)) {
    x <- r0[[i]]
    info <- infos[[i]]
    E <- format(info$E)
    d <- x$data
    col <- d$D_f + 1
    po <- x$meta$party_order
    expect_identical(x$meta$newcomers, newcomers[[E]])
    expect_identical(rownames(d$y_f)[d$index_f[seq_len(d$P), col]], po)
    expect_equal(d$n_parties_f[col], d$P)
    expect_equal(sum(d$index_f[, col] > 0), d$P)
    old <- setdiff(po, newcomers[[E]])
    expect_equal(mean(d$x_f[old, col]), 0, tolerance = 1e-12)

    train <- info$fundamentals$train
    prev <- train[train$date == previous_election(info$E), ]
    label <- fundamentals_party(prev$flokkur)
    label[!label %in% po] <- "Annað"
    share <- as.vector(tapply(prev$voteshare, label, sum)[old])
    share <- share / sum(share)
    centre <- mean(qlogis(share))
    expect_equal(unname(d$x_f[old, col]), unname(qlogis(share) - centre), tolerance = 1e-12)
    for (p in newcomers[[E]]) {
      expect_equal(x$meta$debut_share, debut[[E]], tolerance = 1e-12)
      expect_equal(d$x_f[p, col], qlogis(debut[[E]]) - centre, tolerance = 1e-12)
      for (m in c("y_f", "incumbent_f", "incumbent_years", "vnv", "growth")) {
        expect_true(all(d[[m]][p, ] == 0), label = paste(E, p, m))
      }
    }
    expect_equal(d$P_f, nrow(d$y_f))
  }
})

test_that("only polls dated after the previous election enter the kjördæmi rows", {
  skip_without_snapshot()
  for (i in c(cell("2021-09-25", "2021-03-29"), cell("2017-10-28", "2017-10-14"), cell("2017-10-28", "2017-10-27"), cell("2024-11-30", "2024-06-03"))) {
    expect_equal(r0[[i]]$data$N_k, 6, label = format(TARGETS$F[i]))
    expect_equal(r0[[i]]$data$N_obs_k, 1)
    expect_true(all(r0[[i]]$data$house_k == 1))
  }
  eve <- cell("2024-11-30", "2024-11-29")
  kd <- infos[[eve]]$polls_kd
  polls <- nrow(dplyr::distinct(kd[kd$date > as.Date("2021-09-25"), ], date, fyrirtaeki))
  expect_gt(polls, 0)
  expect_equal(r0[[eve]]$data$N_k, 6 * (1 + polls))
  expect_equal(r0[[eve]]$meta$n_polls_kd, polls)
})

test_that("the kjördæmi weights are each date's shares by name, and obs_k picks the row of the date", {
  skip_without_snapshot()
  eve <- cell("2024-11-30", "2024-11-29")
  x <- r0[[eve]]
  kd <- r0_production_inputs(infos[[eve]])$constituency
  dates <- sort(unique(kd$date))
  w <- x$data$constituency_weights
  expect_identical(rownames(w), format(dates))
  for (j in seq_along(dates)) {
    rows <- kd[kd$date == dates[j], ]
    n <- as.vector(tapply(rows$n, as.character(rows$kjordaemi), sum)[R0_KJORDAEMI])
    expect_equal(unname(w[j, ]), unname(n / sum(n)), tolerance = 1e-12)
  }
  date_levels <- sort(unique(c(infos[[eve]]$kosning_nat$date, infos[[eve]]$polls_nat$date)))
  expect_identical(format(date_levels[x$data$date_k]), rownames(w)[x$data$obs_k])
})

test_that("the active parties grow with each party's first national row", {
  skip_without_snapshot()
  for (x in r0) {
    d <- x$data
    expect_equal(d$n_active_d[d$D], d$P)
    expect_equal(d$active_index_d[d$D, ], seq_len(d$P))
    expect_equal(dim(d$active_index_d), c(d$D, d$P))
    for (t in seq_len(d$D)) {
      k <- d$n_active_d[t]
      expect_true(all(d$active_index_d[t, seq_len(k)] > 0) && all(d$active_index_d[t, -seq_len(k)] == 0))
    }
  }
  for (i in which(TARGETS$E == as.Date("2021-09-25"))) {
    d <- r0[[i]]$data
    info <- infos[[i]]
    j <- match("Sósíalistaflokkurinn", r0[[i]]$meta$party_order)
    date_levels <- sort(unique(c(info$kosning_nat$date, info$polls_nat$date)))
    has_j <- apply(d$active_index_d, 1, \(r) j %in% r)
    expect_equal(has_j, date_levels >= as.Date("2018-10-16"))
  }
})

#' Production's `prepare_stan_data()` at `R0_PARENT_COMMIT`, read with `git show`.
parent_prepare <- function() {
  withr::local_envvar(c(GIT_DIR = NA, GIT_WORK_TREE = NA, GIT_INDEX_FILE = NA))
  spec <- paste0(R0_PARENT_COMMIT, ":R/stan_data.R")
  src <- suppressWarnings(system2("git", c("-C", shQuote(root()), "show", shQuote(spec)), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(src, "status"))) stop("git show ", spec, " failed")
  env <- new.env()
  eval(parse(text = src, keep.source = FALSE), envir = env)
  env$prepare_stan_data
}

# WHY: stan_data_sha() hashes the JSON in field order, so a field R0 adds that lands anywhere but here retires every finished run as STALE.
R0_APPENDED <- c(
  "constituency_weights_pred", "constituency_weights", "N_obs_k", "obs_k", "desired_weight", "weight_time",
  "mu_gamma_scale"
)

# WHY: P_f is y_f's row count, so the prediction column's newcomer rows change it too.
R0_OVERRIDDEN <- c(
  "days_between_stjornarslit_and_election", "n_pred_k", "P_f", "y_f", "x_f", "index_f", "n_parties_f",
  "incumbent_f", "incumbent_years", "vnv", "growth"
)

test_that("the data keep the parent's fields, in its order and with its values but for the overrides, then R0's own", {
  skip_without_snapshot()
  parent <- parent_prepare()
  for (i in seq_along(r0)) {
    inputs <- r0_production_inputs(infos[[i]])
    old <- parent(inputs$polling, inputs$fundamentals, inputs$constituency, election_date = infos[[i]]$E)
    old <- lapply(old, \(x) if (is.data.frame(x)) as.matrix(x) else x)
    d <- r0[[i]]$data
    kept <- setdiff(names(old), c("N", "xm", "n_election", "time_diff_f"))
    expect_identical(names(d), c(kept, R0_APPENDED))
    changed <- kept[!vapply(kept, \(f) identical(d[[f]], old[[f]]), logical(1))]
    expect_identical(setdiff(changed, R0_OVERRIDDEN), character(0), info = format(TARGETS$F[i]))
  }
})

test_that("the houses start with Kosning, then follow each house's first admitted poll", {
  skip_without_snapshot()
  for (i in seq_along(r0)) {
    x <- r0[[i]]
    nat <- infos[[i]]$polls_nat
    first <- tapply(nat$date, nat$fyrirtaeki, min)
    houses <- names(first)[order(first, names(first), method = "radix")]
    expect_identical(x$meta$house_levels, c("Kosning", houses))
    expect_equal(x$data$H, length(houses) + 1)
    expect_equal(x$meta$n_polls_nat, nrow(dplyr::distinct(nat, date, fyrirtaeki)))
    expect_equal(x$meta$max_poll_date, max(nat$date))
    expect_identical(x$meta$coalition_event, infos[[i]]$coalition_event)
  }
})

test_that("the data hold production's fields plus the overrides, with no NA and no field outside the data block", {
  skip_without_snapshot()
  for (x in r0) {
    expect_false(any(c("N", "xm", "n_election", "time_diff_f") %in% names(x$data)))
    expect_true(all(c("mu_gamma_scale", "n_active_d", "active_index_d", "constituency_weights", "N_obs_k", "obs_k") %in% names(x$data)))
    expect_false(any(vapply(x$data, anyNA, logical(1))))
    expect_false(any(vapply(x$data, is.data.frame, logical(1))))
    expect_equal(x$data$desired_weight, 0.22)
    expect_equal(x$data$weight_time, 180)
  }
})

test_that("an input the builder cannot map is an error", {
  skip_without_snapshot()
  info <- infos[[cell("2017-10-28", "2017-10-14")]]
  x <- info
  hit <- x$results$election == as.Date("2016-10-29") & x$results$flokkur == "Sósíalistaflokkurinn"
  x$results$votes[which(hit)[1]] <- 10L
  expect_error(r0_stan_data(x), "R0 input: a dropped party has votes")

  eve <- infos[[cell("2024-11-30", "2024-11-29")]]
  x <- eve
  x$polls_kd$date[x$polls_kd$date == max(x$polls_kd$date)] <- as.Date("2024-11-28")
  expect_error(r0_stan_data(x), "R0 input: a kjördæmi date is not a national date")
  x <- eve
  last <- x$polls_kd$date == max(x$polls_kd$date)
  x$polls_kd$fyrirtaeki[which(last)[1]] <- "Kosning2"
  expect_error(r0_stan_data(x), "R0 input: a kjördæmi date has more than one house")
  x <- eve
  x$F <- max(x$polls_nat$date) - 1
  expect_error(r0_stan_data(x), "R0 input: every national and kjördæmi date is on or before F")
  x <- eve
  x$parties <- setdiff(x$parties, "Annað")
  expect_error(r0_stan_data(x), "R0 input")
})

test_that("the guards on expected votes, newcomers and the prediction column's index_f fire", {
  skip_without_snapshot()
  i <- cell("2024-11-30", "2024-11-29")
  eve <- infos[[i]]
  x <- eve
  x$valid_votes_open$votes[1] <- x$valid_votes_open$votes[1] + 0.5
  expect_error(r0_stan_data(x), "R0 input: the expected valid votes of each kjördæmi are positive whole numbers", fixed = TRUE)

  x <- eve
  train <- x$fundamentals$train
  # WHY: a label with no rename folds into Annað, as Framsókn would without its FUNDAMENTALS_RENAMED entry.
  hit <- train$party == "Framsóknarflokkurinn"
  train$party[hit] <- "Framsókn gamli"
  train$flokkur[hit] <- "Framsókn gamli"
  x$fundamentals$train <- train
  x$fundamentals$target <- x$fundamentals$target[x$fundamentals$target$party != "Framsóknarflokkurinn", ]
  x$fundamentals$S_f <- setdiff(x$fundamentals$S_f, "Framsóknarflokkurinn")
  expect_error(r0_stan_data(x), "R0 input: a newcomer had votes at 2021-09-25: Framsóknarflokkurinn", fixed = TRUE)

  # WHY: r0.R builds index_f by name, so no input swaps it; the guard is called on a swapped copy of real data.
  guard <- get("check_prediction_index", envir = environment(r0_stan_data))
  d <- r0[[i]]$data
  po <- r0[[i]]$meta$party_order
  expect_no_error(guard(d, po))
  col <- d$D_f + 1
  sb <- match(c("Samfylkingin", "Framsóknarflokkurinn"), po)
  d$index_f[sb, col] <- d$index_f[rev(sb), col]
  expect_error(guard(d, po), "R0 input: the prediction column's index_f names the party order", fixed = TRUE)
})

test_that("obs_k follows each y_k row's date, not its position", {
  skip_without_snapshot()
  x <- infos[[cell("2024-11-30", "2024-11-29")]]
  kd <- x$polls_kd
  first <- min(kd$date[kd$date > as.Date("2021-09-25")])
  drop <- which(kd$date == first & kd$flokkur == "Annað" & kd$kjordaemi == "Reykjavík Norður")
  expect_length(drop, 1)
  x$polls_kd <- kd[-drop, ]
  d <- r0_stan_data(x)$data
  date_levels <- sort(unique(c(x$kosning_nat$date, x$polls_nat$date)))
  expect_false(identical(d$obs_k, rep(seq_len(d$N_obs_k), each = 6)))
  expect_identical(format(date_levels[d$date_k]), rownames(d$constituency_weights)[d$obs_k])
})

test_that("the fundamentals inputs carry one Annað target row and the party order first", {
  skip_without_snapshot()
  for (i in seq_along(r0)) {
    info <- infos[[i]]
    f <- r0_production_inputs(info)$fundamentals
    po <- r0[[i]]$meta$party_order
    a <- f[f$flokkur == "Annað" & f$date == info$E, ]
    expect_equal(nrow(a), 1)
    share <- info$prev_shares$share[info$prev_shares$election == info$E & info$prev_shares$flokkur == "Annað"]
    expect_equal(a$voteshare_prior, 100 * share)
    expect_true(all(a[c("incumbent", "incumbent_years", "incumbent_prime_minister", "new_party", "party_split")] == 0))
    expect_true(all(is.na(a[c("voteshare", "voteshare_change", "total_valid_votes")])))
    expect_equal(c(a$year, a$vnv, a$growth), c(info$fundamentals$target$year[1], info$fundamentals$target$vnv[1], info$fundamentals$target$growth[1]))
    present <- intersect(po, as.character(f$flokkur))
    expect_identical(levels(f$flokkur)[seq_along(present)], present)
    expect_false(is.unsorted(as.integer(f$flokkur)))
  }
})
