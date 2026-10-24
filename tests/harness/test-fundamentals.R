box::use(R/harness/fundamentals[fundamentals_inputs, fundamentals_party], R/harness/tables[read_coalitions],
         R/harness/truth[build_truth, national_shares])
fi <- function(E, F, parties = POLL_PARTIES) {
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  x <- readr::read_csv(snapshot_file("economy_data"), show_col_types = FALSE)
  fundamentals_inputs(as.Date(E), as.Date(F), f, x, read_coalitions(), parties)
}
test_that("S_f is the parties with a previous result, without Annað", {
  skip_without_snapshot()
  s17 <- c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Samfylkingin", "Vinstri Græn", "Píratar",
           "Viðreisn", "Flokkur Fólksins")
  expect_setequal(fi("2017-10-28", "2017-10-14", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))$S_f, s17)
  expect_setequal(fi("2021-09-25", "2021-09-11")$S_f, c(s17, "Miðflokkurinn"))
  expect_setequal(fi("2024-11-30", "2024-11-16")$S_f, setdiff(POLL_PARTIES, "Annað"))
})
test_that("the target is masked and training stops before E", {
  skip_without_snapshot(); x <- fi("2017-10-28", "2017-10-27", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))
  expect_true(all(is.na(x$target$voteshare)) && all(is.na(x$target$voteshare_change)))
  expect_equal(max(x$train$date), as.Date("2016-10-29"))
  expect_false(anyNA(x$target$vnv)); expect_false(anyNA(x$target$growth))
})
test_that("target priors agree with the official previous result to rounding", {
  skip_without_snapshot(); x <- fi("2021-09-25", "2021-09-24"); t <- build_truth()
  prev <- 100 * national_shares(t, as.Date("2017-10-28"))[x$target$flokkur]
  expect_true(all(abs(x$target$voteshare_prior - prev) <= 0.06))
})
test_that("every forecast cell builds, with train's columns in the target", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) {
    E <- TARGETS$E[i]
    parties <- if (TARGETS$confirmatory[i]) setdiff(POLL_PARTIES, "Sósíalistaflokkurinn") else POLL_PARTIES
    x <- fi(E, TARGETS$F[i], parties)
    label <- paste(E, TARGETS$F[i])
    expect_identical(names(x$target), names(x$train), label = label)
    expect_identical(x$target$party, x$S_f, label = label)
    expect_false(anyNA(x$target[c("voteshare_prior", "incumbent", "incumbent_prime_minister", "incumbent_years")]),
                 label = label)
  }
})
test_that("the target masks E's outcomes at every forecast cell", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) {
    parties <- if (TARGETS$confirmatory[i]) setdiff(POLL_PARTIES, "Sósíalistaflokkurinn") else POLL_PARTIES
    x <- fi(TARGETS$E[i], TARGETS$F[i], parties)
    label <- paste(TARGETS$E[i], TARGETS$F[i])
    expect_true(all(is.na(x$target$total_valid_votes)), label = label)
    expect_true(all(is.na(x$target$voteshare)) && all(is.na(x$target$voteshare_change)), label = label)
  }
})

# Synthetic inputs: elections 2009, 2013 and E = 2016-10-29 (whose previous election is
# 2013-04-27). At E, Alþýðuflokkur and Annað both map to Annað, Viðreisn is new, and the
# coalition at F (D + B, prime minister D) differs from the fundamentals' own coding.
syn_fund <- function() {
  tibble::tribble(
    ~year, ~date, ~flokkur, ~new_party, ~incumbent, ~incumbent_years, ~incumbent_prime_minister,
    ~party_split, ~voteshare, ~voteshare_prior, ~voteshare_change, ~total_valid_votes,
    2009, "2009-04-25", "Annað", 0, 0, 0, 0, 0, 10, NA, NA, 100,
    2009, "2009-04-25", "Framsókn", 0, 1, 2, 1, 0, 40, NA, NA, 100,
    2009, "2009-04-25", "Alþýðuflokkur", 0, 0, 0, 0, 0, 50, NA, NA, 100,
    2013, "2013-04-27", "Annað", 0, 0, 0, 0, 0, 10, 10, 0, 110,
    2013, "2013-04-27", "Framsókn", 0, 1, 6, 1, 0, 40, 40, 0, 110,
    2013, "2013-04-27", "Alþýðuflokkur", 0, 0, 0, 0, 0, 20, 50, -30, 110,
    2013, "2013-04-27", "Sjálfstæðisflokkurinn", 1, 0, 0, 0, 0, 30, 5, 25, 110,
    2016, "2016-10-29", "Annað", 0, 0, 0, 0, 0, 25, 10, 15, 120,
    2016, "2016-10-29", "Alþýðuflokkur", 0, 0, 0, 0, 0, 5, 20, -15, 120,
    2016, "2016-10-29", "Framsókn", 0, 1, 10, 1, 1, 30, 40, -10, 120,
    2016, "2016-10-29", "Sjálfstæðisflokkurinn", 0, 0, 0, 0, 0, 30, 30, 0, 120,
    2016, "2016-10-29", "Viðreisn", 1, 0, 0, 0, 0, 10, 0, 10, 120
  ) |> dplyr::mutate(date = as.Date(date))
}
syn_econ <- function() {
  tibble::tibble(vnv = c(0.1, 0.2, 0.3), date = as.Date(c("2009-04-25", "2013-04-27", "2016-10-29")), growth = c(1, 2, 3))
}
syn_co <- tibble::tibble(
  cabinet = "X", from = as.Date("2013-05-23"), to = as.Date(NA), parties = "D;B", pm = "D",
  caretaker = FALSE, source_url = "u"
)
syn <- function(E = "2016-10-29", F = "2016-10-15", fund = syn_fund(), econ = syn_econ(), parties = POLL_PARTIES) {
  fundamentals_inputs(as.Date(E), as.Date(F), fund, econ, syn_co, parties)
}
at <- function(f, date, flokkur) f$date == as.Date(date) & f$flokkur == flokkur

test_that("fundamentals_party maps Framsókn, keeps poll labels and folds the rest into Annað", {
  expect_identical(
    fundamentals_party(c("Framsókn", "Píratar", "Björt framtíð", "Annað", "Alþýðuflokkur", "Framsóknarflokkurinn")),
    c("Framsóknarflokkurinn", "Píratar", "Annað", "Annað", "Annað", "Framsóknarflokkurinn")
  )
  expect_identical(fundamentals_party(character(0)), character(0))
  expect_error(fundamentals_party(c("Píratar", NA)), "missing")
})
test_that("train keeps every earlier row in its own labels and flags S_f by the training rule", {
  x <- syn()
  expect_identical(x$train$flokkur, syn_fund()$flokkur[1:7])
  expect_identical(x$train$party, c(
    "Annað", "Framsóknarflokkurinn", "Alþýðuflokkur",
    "Annað", "Framsóknarflokkurinn", "Alþýðuflokkur", "Sjálfstæðisflokkurinn"
  ))
  # NA prior (2009), Annað, and a new party with a prior (2013 D) are all out
  expect_identical(x$train$in_sf, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(x$train$growth, rep(c(1, 2), c(3, 4)))
})
test_that("the target has train's columns and types, POLL_PARTIES order and incumbency at F", {
  x <- syn()
  expect_identical(x$S_f, c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn"))
  expect_identical(names(x$target), names(x$train))
  expect_identical(vapply(x$target, typeof, ""), vapply(x$train, typeof, ""))
  expect_identical(x$target$flokkur, x$S_f)
  expect_identical(x$target$party, x$S_f)
  expect_identical(x$target$date, as.Date(c("2016-10-29", "2016-10-29")))
  expect_identical(x$target$incumbent, c(1, 1))
  expect_identical(x$target$incumbent_prime_minister, c(1, 0))
  expect_identical(x$target$year, c(2016, 2016))
  expect_identical(x$target$new_party, c(0, 0))
  expect_identical(x$target$voteshare_prior, c(30, 40))
  expect_identical(x$target$incumbent_years, c(0, 10))
  expect_identical(x$target$party_split, c(0, 1))
  expect_identical(x$target$vnv, c(0.3, 0.3))
  expect_identical(x$target$growth, c(3, 3))
  expect_identical(x$target$voteshare, c(NA_real_, NA_real_))
  expect_identical(x$target$voteshare_change, c(NA_real_, NA_real_))
  expect_identical(x$target$total_valid_votes, c(NA_real_, NA_real_))
  expect_identical(x$target$in_sf, c(TRUE, TRUE))
})
test_that("fundamentals_inputs stops on a missing, incomplete or repeated economy row", {
  e <- syn_econ()
  expect_error(syn(econ = e[-1, ]), "no economy row for training election\\(s\\) 2009-04-25")
  expect_error(syn(econ = e[-3, ]), "economy row for E = 2016-10-29")
  expect_error(syn(econ = dplyr::bind_rows(e, e[2, ])), "more than one row for 2013-04-27")
  e$growth[3] <- NA
  expect_error(syn(econ = e), "economy row for E = 2016-10-29")
})
test_that("S_f must equal the parties with votes at the previous election", {
  f <- syn_fund()
  expect_error(syn(fund = f[!at(f, "2013-04-27", "Sjálfstæðisflokkurinn"), ]),
               "differ.*only by voteshare_prior: Sjálfstæðisflokkurinn")
  f$voteshare_prior[at(f, "2016-10-29", "Sjálfstæðisflokkurinn")] <- 0
  expect_error(syn(fund = f), "differ.*only by votes at 2013-04-27: Sjálfstæðisflokkurinn")
  expect_identical(syn(fund = f, parties = setdiff(POLL_PARTIES, "Sjálfstæðisflokkurinn"))$S_f, "Framsóknarflokkurinn")
})
test_that("fundamentals_inputs stops on a poll party mapped from two rows at E", {
  f <- syn_fund()
  dup <- f[at(f, "2016-10-29", "Framsókn"), ]
  dup$flokkur <- "Framsóknarflokkurinn"
  expect_error(syn(fund = dplyr::bind_rows(f, dup)), "more than one row maps to Framsóknarflokkurinn")
})
test_that("fundamentals_inputs stops on F not before E, not one date each, or a party outside POLL_PARTIES", {
  expect_error(syn(F = "2016-10-29"), "must be before E")
  expect_error(syn(F = c("2016-10-01", "2016-10-02")), "one date each")
  expect_error(syn(F = NA), "one date each")
  expect_error(syn(parties = c(POLL_PARTIES, "Björt framtíð")), "outside POLL_PARTIES: Björt framtíð")
})
test_that("fundamentals_inputs stops on a fund column it neither carries, takes at F nor masks", {
  f <- syn_fund()
  f$turnout <- 80
  expect_error(syn(fund = f), "neither carried, taken at F nor masked: turnout")
})
