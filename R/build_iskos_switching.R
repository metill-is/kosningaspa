# Election-to-election switching tables from the ÍSKOS voter surveys (1983–2021).
#
# Each wave asks the vote just cast (prtvoteYY) and the recalled vote at the previous
# election (prtfvoteYY). Party codes and list letters change between waves (M was
# Landsbyggðarflokkurinn in 2013, J Regnboginn), so parties are matched by the
# Icelandic value-label TEXT, never by code or letter; an unmatched label stops the
# script. Each party also gets a model group: itself if the fundamentals data names it
# for that election, otherwise "Annað".
#
# Non-party answers are classified with the turnout questions (voteYY / fvoteYY):
# "Kaus ekki", "Ekki kosningarétt", "Autt/ógilt", "Vill ekki segja", "Man ekki",
# "Kaus, óþekkt" (voted, party missing), "Óþekkt" (turnout unknown). Files are read
# with user_na = TRUE: 1995/2009/2013/2016/2017 store refusals, blanks and "no vote
# yet" as SPSS user-missing, which read_sav() otherwise turns into NA. A code with no
# value label stops the script unless mapped explicitly (2021 prtfvote17 == 987).
#
# Eligibility at the previous election is re-derived from year of birth: anyone born
# too late to reach the voting age (20 until 1983, 18 from 1987; turnout by birth
# cohort confirms both) becomes "Ekki kosningarétt", whatever they reported — many said
# "Nei, kaus ekki", and a few named a party. 1987 has no fvote83 and folds everyone
# else without a 1983 party into prtfvote83 == 90 "Annað", which becomes "Óflokkað".
#
# Reads data-raw/iskos/voter_survey/ (R/download_iskos.R). Writes, to the gitignored
# data/iskos/ (derived from GAGNÍS-licensed data, so never commit them):
#   switching_long.{parquet,csv}  one row per wave x weight scheme x prev x cur cell
#   validation.csv                survey vs official shares, current and recalled vote
#   label_map.csv                 audit: every (wave, variable, code, label) -> party/status/group
#   diagnostics.csv               per wave: birth-year ineligibility overrides and what they replaced
#
# Run from the repo root:  Rscript R/build_iskos_switching.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(here)
  library(arrow)
})
Sys.setlocale("LC_ALL", "is_IS.UTF-8")
options(width = 150)
box::use(R / data[read_fundamentals_data])

waves <- tribble(
  ~year, ~prev,
  1983, 1979, 1987, 1983, 1991, 1987, 1995, 1991, 1999, 1995, 2003, 1999,
  2007, 2003, 2009, 2007, 2013, 2009, 2016, 2013, 2017, 2016, 2021, 2017
)

# Ordered: first match wins. Patterns are matched case-insensitively on the IS label.
party_patterns <- tribble(
  ~pattern, ~party,
  "^Sjálfstæðisflokk", "Sjálfstæðisflokkurinn",
  "^Framsóknarflokk", "Framsóknarflokkurinn",
  "^Alþýðuflokk", "Alþýðuflokkur",
  "^Alþýðubandalag", "Alþýðubandalag",
  "^Alþýðufylking", "Alþýðufylkingin",
  "^Bandalag jafnaðarmanna", "Bandalag jafnaðarmanna",
  "kvennalista", "Samtök um kvennalista",
  "^Frjálslynda / Borgaraflokk", "Frjálslyndir (1991)",
  "^Borgaraflokk", "Borgaraflokkur",
  "Stefán Valgeirsson|jafnrétti og félagsh", "Samtök um jafnrétti og félagshyggju",
  "^Flokk mannsins", "Flokkur mannsins",
  "^Þjóðarflokk", "Þjóðarflokkurinn",
  "^Þjóðvak", "Þjóðvaki",
  "^Samfylking", "Samfylkingin",
  "^Vinstrihreyfing|^Vinstri græn", "Vinstri Græn",
  "^Frjálslynda lýðræðisflokk", "Frjálslyndi lýðræðisflokkurinn",
  "^Frjálslynda flokk|^Frjálslyndi flokk", "Frjálslyndi flokkurinn",
  "^Borgarahreyfing", "Borgarahreyfingin",
  "^Björt|^Bjarta", "Björt framtíð",
  "^Pírat", "Píratar",
  "^Viðreisn", "Viðreisn",
  "^Miðflokk", "Miðflokkurinn",
  "^Flokk(ur)? fólksins", "Flokkur Fólksins",
  "^Sósíalistaflokk", "Sósíalistaflokkurinn",
  "^Dögun", "Dögun",
  "^Lýðræðisvakt", "Lýðræðisvaktin",
  "^Lýðræðishreyfing", "Lýðræðishreyfingin",
  "^Hægri græn", "Hægri grænir",
  "^Húmanista", "Húmanistaflokkurinn",
  "^Landsbyggðarflokk", "Landsbyggðarflokkurinn",
  "^Regnbog", "Regnboginn",
  "^Flokk(ur)? heimilanna", "Flokkur heimilanna",
  "Sturlu Jónssonar", "Framboð Sturlu Jónssonar",
  "^Íslenska þjóðfylking", "Íslenska þjóðfylkingin",
  "^Ábyrg", "Ábyrg framtíð",
  "^Nýtt afl", "Nýtt afl",
  "^T ?- ?lista Kristjáns", "T-listi Kristjáns Pálssonar",
  "^Íslandshreyfing", "Íslandshreyfingin",
  "^Kristileg", "Kristileg framboð",
  "^Anarkist", "Anarkistar á Íslandi",
  "^Náttúrulagaflokk", "Náttúrulagaflokkurinn",
  "^Vestfjarða", "Vestfjarðalistinn",
  "^Suðurlandslist", "Suðurlandslistinn",
  "^Heimastjórnarsamtök", "Heimastjórnarsamtökin",
  "^Grænt framboð", "Grænt framboð",
  "^Öfgasinnað", "Öfgasinnaðir jafnaðarmenn",
  "^Fylking", "Fylkingin",
  "^S-listi / Júlíus", "S-listi Júlíusar Sólnes",
  "^L-listi / Eggert", "L-listi Eggerts Haukdal",
  "^T-lista$", "T-listi (1983)",
  "^Annan flokk|^Annað$", "Annar flokkur (ótilgreint)"
)

# Non-party answers in the party question; anything else non-party falls back to turnout.
status_patterns <- tribble(
  ~pattern, ~status,
  "kaus ekki", "Kaus ekki",
  "kosningarétt", "Ekki kosningarétt",
  "auðu|ógild", "Autt/ógilt",
  "vill ekki segja", "Vill ekki segja",
  "^Man ", "Man ekki"
)

first_match <- function(label, patterns, value) {
  out <- rep(NA_character_, length(label))
  for (k in seq_len(nrow(patterns))) {
    hit <- is.na(out) & !is.na(label) & str_detect(label, regex(patterns$pattern[k], ignore_case = TRUE))
    out[hit] <- patterns[[value]][k]
  }
  out
}

label_of <- function(x) {
  labs <- attr(x, "labels")
  names(labs)[match(as.numeric(x), as.numeric(labs))]
}

turnout_of <- function(x) {
  lab <- label_of(x)
  case_when(
    str_detect(lab, "^Já, kaus") ~ "voted",
    str_detect(lab, "^Nei, kaus ekki") ~ "nonvoter",
    str_detect(lab, "kosningar[ée]tt") ~ "ineligible",
    .default = "unknown"
  )
}

classify <- function(prt, turnout, code_status = character()) {
  code <- as.numeric(prt)
  lab <- label_of(prt)
  party <- first_match(lab, party_patterns, "party")
  status <- first_match(lab, status_patterns, "status")
  forced <- unname(code_status[as.character(code)])
  unlabelled <- unique(code[!is.na(code) & is.na(lab) & is.na(forced)])
  if (length(unlabelled)) stop("Codes without a value label: ", paste(unlabelled, collapse = ", "))
  unmatched <- unique(lab[is.na(party) & is.na(status) & !is.na(lab) &
    !str_detect(lab, regex("neitar|veit ekki|brottfall|á ekki við|ekki spurt|^NA$", ignore_case = TRUE))])
  if (length(unmatched)) stop("Unmatched labels: ", paste(unmatched, collapse = " | "))
  case_when(
    !is.na(forced) ~ forced,
    !is.na(party) ~ party,
    turnout == "ineligible" | status %in% "Ekki kosningarétt" ~ "Ekki kosningarétt",
    turnout == "nonvoter" | status %in% "Kaus ekki" ~ "Kaus ekki",
    !is.na(status) ~ status,
    turnout == "voted" ~ "Kaus, óþekkt",
    .default = "Óþekkt"
  )
}

yy <- \(y) sprintf("%02d", y %% 100)
non_party <- c(
  "Kaus ekki", "Ekki kosningarétt", "Autt/ógilt", "Vill ekki segja", "Man ekki",
  "Kaus, óþekkt", "Óþekkt", "Óflokkað"
)

fundamentals <- read_fundamentals_data() |> mutate(flokkur = as.character(flokkur))
named <- fundamentals |>
  filter(flokkur != "Annað", voteshare > 0) |>
  distinct(year, flokkur)
group_of <- function(party, election) {
  is_named <- paste(election, party) %in% paste(named$year, named$flokkur)
  if_else(party %in% non_party | is_named, party, "Annað")
}

read_wave <- function(year, prev) {
  f <- list.files(here("data-raw", "iskos", "voter_survey", year), "\\.sav$", full.names = TRUE)
  f <- f[!str_detect(f, regex("english|_en\\.sav", ignore_case = TRUE))]
  stopifnot(length(f) == 1)
  d <- read_sav(f, user_na = TRUE)
  v_cur <- paste0("prtvote", yy(year))
  v_prev <- paste0("prtfvote", yy(prev))
  t_cur <- paste0("vote", yy(year))
  t_prev <- paste0("fvote", yy(prev))
  cur <- classify(d[[v_cur]], turnout_of(d[[t_cur]]))
  prev_turnout <- if (t_prev %in% names(d)) turnout_of(d[[t_prev]]) else rep("unknown", nrow(d))
  # 2021: 987 is unlabelled; 65 of its 72 respondents were born too late to vote in 2017.
  prev_codes <- if (year == 2021) c(`987` = "Ekki kosningarétt") else character()
  prev_party <- classify(d[[v_prev]], prev_turnout, prev_codes)
  if (year == 1987) prev_party[as.numeric(d[[v_prev]]) %in% 90] <- "Óflokkað"
  yob <- as.numeric(d$yob)
  vote_age <- if (prev <= 1983) 20 else 18
  too_young <- !is.na(yob) & yob > 1850 & yob <= year & yob >= prev - vote_age + 1
  diag <- tibble(
    year_prev = prev, year_cur = year, n = nrow(d), vote_age,
    n_too_young = sum(too_young),
    was_ekki_kosningarett = sum(too_young & prev_party == "Ekki kosningarétt"),
    was_kaus_ekki = sum(too_young & prev_party == "Kaus ekki"),
    was_party = sum(too_young & !prev_party %in% non_party),
    was_other = sum(too_young & prev_party %in% setdiff(non_party, c("Kaus ekki", "Ekki kosningarétt")))
  )
  prev_party[too_young] <- "Ekki kosningarétt"
  weights <- list(none = rep(1, nrow(d)))
  for (w in intersect(c("weight", "demweight", "polweight", "polmatweight"), names(d))) {
    weights[[w]] <- as.numeric(d[[w]])
    if (anyNA(weights[[w]])) message(sprintf("%d: %s is NA for %d respondents (dropped from that scheme)", year, w, sum(is.na(weights[[w]]))))
  }
  audit <- bind_rows(
    tibble(side = "current vote", var = v_cur, code = as.numeric(d[[v_cur]]), label = label_of(d[[v_cur]]), assigned = cur),
    tibble(side = "recalled vote", var = v_prev, code = as.numeric(d[[v_prev]]), label = label_of(d[[v_prev]]), assigned = prev_party)
  ) |>
    count(side, var, code, label, assigned, name = "n_resp") |>
    mutate(year_prev = prev, year_cur = year, .before = 1)
  list(
    resp = imap(weights, \(w, scheme) tibble(
      year_prev = prev, year_cur = year, weight_scheme = scheme,
      prev_party, cur_party = cur, w = w
    )) |> list_rbind(),
    audit = audit,
    diag = diag
  )
}

read <- map2(waves$year, waves$prev, read_wave)
resp <- map(read, "resp") |> list_rbind()
diagnostics <- map(read, "diag") |> list_rbind()
label_map <- map(read, "audit") |> list_rbind() |>
  mutate(group = group_of(assigned, if_else(side == "current vote", year_cur, year_prev)))

switching <- resp |>
  filter(!is.na(w)) |>
  summarise(n_resp = n(), n_w = sum(w), .by = c(year_prev, year_cur, weight_scheme, prev_party, cur_party)) |>
  mutate(
    prev_group = group_of(prev_party, year_prev),
    cur_group = group_of(cur_party, year_cur),
    prev_is_party = !prev_party %in% non_party,
    cur_is_party = !cur_party %in% non_party
  ) |>
  relocate(prev_group, .after = prev_party) |>
  relocate(cur_group, .after = cur_party) |>
  arrange(year_cur, weight_scheme, desc(n_w))

# --- validation: party-group shares among party voters vs the official result ---
official <- fundamentals |>
  mutate(flokkur = group_of(flokkur, year)) |>
  summarise(official = sum(voteshare) / 100, .by = c(year, flokkur))
side_shares <- function(side, year_col) {
  switching |>
    filter(.data[[paste0(side, "_is_party")]]) |>
    summarise(n_w = sum(n_w), n_resp = sum(n_resp), .by = c(year_cur, weight_scheme, all_of(year_col), all_of(paste0(side, "_group")))) |>
    mutate(survey = n_w / sum(n_w), .by = c(year_cur, weight_scheme)) |>
    mutate(election = .data[[year_col]]) |>
    rename(flokkur = all_of(paste0(side, "_group"))) |>
    mutate(side = if (side == "cur") "current vote" else "recalled vote")
}
shares <- bind_rows(side_shares("cur", "year_cur"), side_shares("prev", "year_prev"))
validation <- shares |>
  distinct(year_cur, weight_scheme, side, election) |>
  inner_join(official, by = c("election" = "year"), relationship = "many-to-many") |>
  full_join(shares, by = c("year_cur", "weight_scheme", "side", "election", "flokkur")) |>
  mutate(
    survey = coalesce(survey, 0), n_resp = coalesce(n_resp, 0L), n_w = coalesce(n_w, 0),
    diff_pp = 100 * (survey - official)
  ) |>
  arrange(year_cur, side, weight_scheme, desc(official))

out_dir <- here("data", "iskos")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_parquet(switching, file.path(out_dir, "switching_long.parquet"))
write_csv(switching, file.path(out_dir, "switching_long.csv"))
write_csv(validation, file.path(out_dir, "validation.csv"))
write_csv(label_map, file.path(out_dir, "label_map.csv"))
write_csv(diagnostics, file.path(out_dir, "diagnostics.csv"))

# --- summary ---
cat("\nCycle       N   both-party  retention  top switches (share of party->party voters)\n")
switching |>
  filter(weight_scheme == "none") |>
  summarise(
    N = sum(n_resp),
    both = sum(n_resp[prev_is_party & cur_is_party]),
    stay = sum(n_resp[prev_is_party & cur_is_party & prev_group == cur_group & prev_group != "Annað"]),
    top = {
      x <- pick(everything()) |>
        filter(prev_is_party, cur_is_party, prev_group != cur_group) |>
        summarise(n = sum(n_resp), .by = c(prev_group, cur_group)) |>
        slice_max(n, n = 3, with_ties = FALSE)
      paste0(x$prev_group, "→", x$cur_group, " ", round(100 * x$n / sum(n_resp[prev_is_party & cur_is_party]), 1), "%", collapse = "; ")
    },
    .by = c(year_prev, year_cur)
  ) |>
  mutate(line = sprintf("%d→%d %5d  %5.1f%%      %5.1f%%     %s", year_prev, year_cur, N, 100 * both / N, 100 * stay / both, top)) |>
  pull(line) |>
  walk(\(l) cat(l, "\n"))

cat("\nValidation (unweighted), max |survey - official| in pp over party groups:\n")
validation |>
  filter(weight_scheme == "none", !is.na(official)) |>
  summarise(max_abs_pp = max(abs(diff_pp), na.rm = TRUE), worst = flokkur[which.max(abs(diff_pp))], .by = c(year_cur, side)) |>
  pivot_wider(names_from = side, values_from = c(max_abs_pp, worst)) |>
  print(n = Inf)

cat("\nBirth-year ineligibility at the previous election (overrides applied):\n")
print(as.data.frame(diagnostics), row.names = FALSE)
