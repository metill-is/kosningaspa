box::use(R / harness / availability[parse_lokadagur, match_fieldwork, house_max_fieldwork, poll_availability, excluded_near])

test_that("lokadagur datetimes parse to their calendar date", {
  expect_equal(
    parse_lokadagur(c("2024-11-13T00:00:00Z", "2024-11-13", "NA", NA)),
    as.Date(c("2024-11-13", "2024-11-13", NA, NA))
  )
})

wiki1 <- tibble::tibble(
  house = "Gallup", start = as.Date("2021-09-01"), end = as.Date("2021-09-07"),
  D = 24.0, S = 10.0
)
map1 <- tibble::tibble(wikipedia_house = "Gallup", fyrirtaeki = "Gallup")
poll <- function(date, d, s) {
  tibble::tibble(
    date = as.Date(date), fyrirtaeki = "Gallup",
    shares = list(c(D = d, S = s))
  )
}
test_that("matching needs house, date and shares, and is one-to-one", {
  m <- match_fieldwork(
    dplyr::bind_rows(poll("2021-09-04", 24.1, 10.1), poll("2021-09-06", 24.2, 10.2)),
    wiki1, map1
  )
  expect_equal(nrow(m), 1)
  expect_equal(m$date, as.Date("2021-09-04"))
  expect_equal(nrow(match_fieldwork(poll("2021-09-04", 24.26, 10.26), wiki1, map1)), 0) # 0.26 pp
  expect_equal(nrow(match_fieldwork(poll("2021-09-12", 24.0, 10.0), wiki1, map1)), 0) # 8 days
})

test_that("longest fieldwork per house, and the overall maximum for houses with none", {
  h <- house_max_fieldwork(tibble::tibble(
    fyrirtaeki = c("Gallup", "MMR"),
    start = as.Date(c("2021-08-01", "2021-08-01")), end = as.Date(c("2021-08-31", "2021-08-04"))
  ))
  expect_equal(h$max_days[h$fyrirtaeki == "MMR"], 3)
  expect_equal(attr(h, "overall"), 30)
})

test_that("real snapshot: every poll has one row, none at its midpoint", {
  skip_without_snapshot()
  av <- poll_availability()
  expect_equal(nrow(dplyr::count(av, level, date, fyrirtaeki) |> dplyr::filter(n > 1)), 0)
  nonel <- av[av$rule != "election", ]
  expect_true(all(nonel$available_from > nonel$date))
  g <- av[av$date == as.Date("2024-11-15") & av$fyrirtaeki == "Gallup", ]
  expect_equal(g$available_from, as.Date(c("2024-11-22", "2024-11-22"))) # national and kjördæmi
  p <- av[av$date == as.Date("2024-09-26") & av$fyrirtaeki == "Prósent", ]
  expect_gt(p$available_from, as.Date("2024-10-14"))
})

# Synthetic inputs: Wikipedia rows (2024 table), corrections and publication rows as CSV
# lines, and polls with five parties whose shares are 30, 25, 20, 15 and 10 percent.
wk_header <- "fieldwork,house,start,end,note,n,D,S,B,V,P,C,F,M,J"
wk <- function(house, start, end, shares = "30,25,20,15,10,,,,") {
  sprintf("x,%s,%s,%s,,100,%s", house, start, end, shares)
}
write_sources <- function(wiki = character(), corrections = character(), publication = character(),
                          dir = withr::local_tempdir(.local_envir = parent.frame())) {
  writeLines(wk_header, file.path(dir, "wikipedia_2017_poll_fieldwork.csv"))
  writeLines(wk_header, file.path(dir, "wikipedia_2021_poll_fieldwork.csv"))
  writeLines(c(wk_header, wiki), file.path(dir, "wikipedia_2024_poll_fieldwork.csv"))
  writeLines(c("wikipedia_house,fyrirtaeki", "Gallup,Gallup", "MMR,MMR"), file.path(dir, "wikipedia_house_map.csv"))
  writeLines(c("level,date,fyrirtaeki,end,source,note", corrections), file.path(dir, "fieldwork_corrections.csv"))
  writeLines(c("level,date,fyrirtaeki,published,source,note", publication), file.path(dir, "poll_publication.csv"))
  dir
}
parties5 <- tibble::tibble(
  flokkur = c("Sjálfstæðisflokkurinn", "Samfylkingin", "Framsóknarflokkurinn", "Vinstri Græn", "Píratar"),
  n = c(30, 25, 20, 15, 10)
)
national_polls <- function(date, fyrirtaeki, lokadagur = NA_character_) {
  tidyr::crossing(tibble::tibble(date = as.Date(date), fyrirtaeki, lokadagur), parties5)
}
kjordaemi_polls <- function(date, fyrirtaeki) {
  tidyr::crossing(tibble::tibble(date = as.Date(date), fyrirtaeki, kjordaemi = "Suður"), parties5)
}
no_kjordaemi <- kjordaemi_polls(character(), character())
av_lines <- function(av) paste(av$level, format(av$date), av$rule, format(av$available_from))

test_that("the first rule that applies decides, at both levels", {
  # Each publication row is no later than the date of the rule that outranks it, so the
  # floor does not bind and a swap of two rules changes the result.
  dir <- write_sources(
    wiki = c(wk("MMR", "2024-01-13", "2024-01-17"), wk("Gallup", "2024-02-08", "2024-02-12"), wk("Gallup", "2024-03-07", "2024-03-13")),
    corrections = c("national,2024-01-10,Gallup,2024-01-20,s,", "kjordaemi,2024-01-10,Gallup,2024-01-25,s,"),
    publication = c(
      "national,2024-01-10,Gallup,2024-01-20,s,", "national,2024-01-15,MMR,2024-01-16,s,",
      "national,2024-02-10,Gallup,2024-02-12,s,", "kjordaemi,2024-03-10,Gallup,2024-03-14,s,",
      "national,2024-04-20,MMR,2024-04-22,s,", "kjordaemi,2024-04-25,Gallup,2024-04-28,s,"
    )
  )
  polling <- national_polls(
    c("2024-01-10", "2024-01-15", "2024-02-10", "2024-03-10", "2024-04-10", "2024-04-20", "2024-05-01"),
    c("Gallup", "MMR", "Gallup", "Gallup", "MMR", "MMR", "Kosning"),
    c("2024-01-12T00:00:00Z", "2024-01-16T00:00:00Z", NA, NA, NA, NA, NA)
  )
  constituency <- kjordaemi_polls(c("2024-01-10", "2024-03-10", "2024-04-20", "2024-04-25"), "Gallup")
  expect_equal(av_lines(poll_availability(polling, constituency, dir)), c(
    "national 2024-01-10 correction 2024-01-21", # beats lokadagur (01-13) and published (01-20)
    "national 2024-01-15 lokadagur 2024-01-17", # beats wikipedia (01-18) and published (01-16)
    "national 2024-02-10 wikipedia 2024-02-13", # beats published (02-12)
    "national 2024-03-10 wikipedia 2024-03-14",
    "national 2024-04-10 house_max 2024-04-15", # MMR's longest span, 4 days
    "national 2024-04-20 published 2024-04-22", # beats house_max (04-25)
    "national 2024-05-01 election 2024-05-02",
    "kjordaemi 2024-01-10 correction 2024-01-26", # beats twin (01-21)
    "kjordaemi 2024-03-10 twin 2024-03-14", # beats published on the tie (03-14)
    "kjordaemi 2024-04-20 house_max 2024-04-27", # Gallup's longest national span, 6 days
    "kjordaemi 2024-04-25 published 2024-04-28" # beats house_max (05-02)
  ))
})

test_that("a publication after the rule's date floors available_from", {
  dir <- write_sources(publication = "national,2024-06-05,Gallup,2024-06-12,s,")
  av <- poll_availability(national_polls("2024-06-05", "Gallup", "2024-06-09T00:00:00Z"), no_kjordaemi, dir)
  expect_equal(av_lines(av), "national 2024-06-05 published 2024-06-12") # lokadagur alone: 06-10
  expect_equal(nrow(excluded_near(av, "2024-06-10")), 1)
})

test_that("the publication floor never lowers a date", {
  dir <- write_sources(
    corrections = "national,2024-06-06,MMR,2024-06-12,s,",
    publication = c("national,2024-06-05,Gallup,2024-06-09,s,", "national,2024-06-06,MMR,2024-06-12,s,")
  )
  polling <- national_polls(c("2024-06-05", "2024-06-06"), c("Gallup", "MMR"), c("2024-06-09T00:00:00Z", NA))
  expect_equal(av_lines(poll_availability(polling, no_kjordaemi, dir)), c(
    "national 2024-06-05 lokadagur 2024-06-10", "national 2024-06-06 correction 2024-06-13"
  ))
})

test_that("a Wikipedia end published that same day still gets end + 1", {
  dir <- write_sources(
    wiki = wk("Gallup", "2024-06-08", "2024-06-12"),
    publication = "national,2024-06-10,Gallup,2024-06-12,s,"
  )
  av <- poll_availability(national_polls("2024-06-10", "Gallup"), no_kjordaemi, dir)
  expect_equal(av_lines(av), "national 2024-06-10 wikipedia 2024-06-13")
})

test_that("a publication before the known fieldwork end stops the build", {
  polling <- national_polls("2024-06-05", "Gallup", "2024-06-09T00:00:00Z")
  national <- write_sources(publication = "national,2024-06-05,Gallup,2024-06-08,s,")
  expect_error(poll_availability(polling, no_kjordaemi, national), "publication before fieldwork end")
  kjordaemi <- write_sources(
    corrections = "kjordaemi,2024-06-05,Gallup,2024-06-11,s,",
    publication = "kjordaemi,2024-06-05,Gallup,2024-06-10,s,"
  )
  expect_error(
    poll_availability(polling, kjordaemi_polls("2024-06-05", "Gallup"), kjordaemi),
    "publication before fieldwork end"
  )
})

test_that("a kjördæmi poll is never available before its floored national twin", {
  dir <- write_sources(
    corrections = c(
      "kjordaemi,2024-06-05,Gallup,2024-06-09,s,", "kjordaemi,2024-06-20,Gallup,2024-06-22,s,",
      "kjordaemi,2024-07-05,Gallup,2024-07-07,s,"
    ),
    publication = c(
      "national,2024-06-05,Gallup,2024-06-15,s,", "national,2024-06-20,Gallup,2024-06-30,s,",
      "kjordaemi,2024-06-20,Gallup,2024-06-30,s,", "national,2024-07-05,Gallup,2024-07-10,s,",
      "kjordaemi,2024-07-05,Gallup,2024-07-12,s,"
    )
  )
  dates <- c("2024-06-05", "2024-06-20", "2024-07-05")
  polling <- national_polls(dates, "Gallup", c("2024-06-09T00:00:00Z", "2024-06-22T00:00:00Z", "2024-07-07T00:00:00Z"))
  expect_equal(av_lines(poll_availability(polling, kjordaemi_polls(dates, "Gallup"), dir)), c(
    "national 2024-06-05 published 2024-06-15", "national 2024-06-20 published 2024-06-30",
    "national 2024-07-05 published 2024-07-10",
    "kjordaemi 2024-06-05 twin 2024-06-15", # correction alone: 06-10
    "kjordaemi 2024-06-20 twin 2024-06-30", # its own publication ties the twin
    "kjordaemi 2024-07-05 published 2024-07-12" # later than the twin (07-10)
  ))
})

test_that("a Wikipedia match needs five shared parties", {
  four <- wk("Gallup", "2024-03-07", "2024-03-13", "30,25,20,15,,,,,")
  fw <- readr::read_csv(I(c(wk_header, four)), show_col_types = FALSE)
  polls <- dplyr::distinct(national_polls("2024-03-10", "Gallup"), date, fyrirtaeki) |>
    dplyr::mutate(shares = list(c(D = 30, S = 25, B = 20, V = 15, P = 10)))
  map <- tibble::tibble(wikipedia_house = "Gallup", fyrirtaeki = "Gallup")
  expect_equal(nrow(match_fieldwork(polls, fw, map, min_parties = 4)), 1)
  expect_equal(nrow(match_fieldwork(polls, fw, map, min_parties = 5)), 0)
  av <- poll_availability(national_polls("2024-03-10", "Gallup"), no_kjordaemi, write_sources(wiki = four))
  expect_equal(av$rule, "house_max")
})

test_that("the build stops when a poll or a Wikipedia row has two match candidates", {
  two_rows <- write_sources(wiki = c(wk("Gallup", "2024-03-07", "2024-03-13"), wk("Gallup", "2024-03-08", "2024-03-12")))
  expect_error(poll_availability(national_polls("2024-03-10", "Gallup"), no_kjordaemi, two_rows), "more than one")
  one_row <- write_sources(wiki = wk("Gallup", "2024-03-07", "2024-03-13"))
  two_polls <- national_polls(c("2024-03-09", "2024-03-11"), "Gallup")
  expect_error(poll_availability(two_polls, no_kjordaemi, one_row), "more than one")
})

test_that("corrections and publication rows may not name a Kosning row", {
  polling <- national_polls(c("2024-03-10", "2024-05-01"), c("Gallup", "Kosning"), c("2024-03-12T00:00:00Z", NA))
  corr <- write_sources(corrections = "national,2024-05-01,Kosning,2024-05-01,s,")
  expect_error(poll_availability(polling, no_kjordaemi, corr), "naming no poll")
  pub <- write_sources(publication = "national,2024-05-01,Kosning,2024-05-02,s,")
  expect_error(poll_availability(polling, no_kjordaemi, pub), "naming no poll")
})

test_that("real snapshot: the first publication floors the polls that leaked", {
  skip_without_snapshot()
  av <- poll_availability()
  from <- function(level, date, f) av$available_from[av$level == level & av$date == as.Date(date) & av$fyrirtaeki == f]
  expect_equal(from("national", "2021-09-09", "MMR"), as.Date("2021-09-13"))
  expect_equal(from("national", "2024-10-07", "Gallup"), as.Date("2024-11-01"))
  expect_equal(from("national", "2024-10-10", "Prósent"), as.Date("2024-10-25"))
  expect_equal(from("national", "2024-11-15", "Gallup"), as.Date("2024-11-22"))
  expect_equal(from("kjordaemi", "2024-11-15", "Gallup"), as.Date("2024-11-22"))
  # Published on its last fieldwork day (Wikipedia's 17–18 Oct 2017): still end + 1.
  m <- av[av$level == "national" & av$date == as.Date("2017-10-17") & av$fyrirtaeki == "MMR", ]
  expect_equal(paste(m$rule, format(m$available_from)), "wikipedia 2017-10-19")
})

test_that("real snapshot: every national poll available in the 45 days before a forecast date has a publication row", {
  skip_without_snapshot()
  sources <- root("harness", "sources")
  av <- poll_availability(sources_dir = sources)
  pub <- readr::read_csv(file.path(sources, "poll_publication.csv"), col_types = readr::cols(date = "D", .default = "c"))
  nat <- av[av$level == "national" & av$rule != "election", ]
  near <- dplyr::distinct(dplyr::bind_rows(lapply(seq_len(nrow(TARGETS)), \(i) {
    F <- TARGETS$F[i]
    nat[nat$available_from > F - 45 & nat$available_from <= F, c("date", "fyrirtaeki")]
  })))
  expect_equal(nrow(near), 69)
  undated <- dplyr::anti_join(near, pub[pub$level == "national", ], by = c("date", "fyrirtaeki"))
  expect_identical(nrow(undated), 0L, info = paste("no publication row:", paste(format(undated$date), undated$fyrirtaeki, collapse = "; ")))
})
