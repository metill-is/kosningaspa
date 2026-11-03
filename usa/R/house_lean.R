# District lean for the House race layer (usa/us-house-layer-plan.html, step 3): the
# presidential vote summed onto the congressional districts of a map, from MEDSL's precinct
# returns (paths under raw/medsl-precinct/, unzipped into csv=; usa/README.md has where they
# come from).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/house_lean.R csv=<unzipped dir> raw=<raw dir> out=<dir>
#
# MEDSL's president rows carry no district, but its US House rows do, so a precinct's
# presidential vote goes to the district its House rows name. For one map year:
#   1. Each precinct's House votes, by district, give its weights (a precinct split between
#      districts is shared by their House votes).
#   2. Presidential votes are linked to those precincts: by the exact precinct key when the
#      presidential year is the map year, and otherwise by county and a normalised precinct
#      name (upper case, letters and digits only, leading zeros dropped).
#   3. Votes that do not link (county-wide absentee rows, renamed precincts) are shared out
#      within their county by the map year's House votes there, the Democratic presidential
#      vote by the districts' Democratic House votes and the Republican by the Republican, so
#      a county split between a city district and a rural one sends each party's votes
#      where that party's voters are. A county with no House votes is left unallocated.
# The leans built are 2016 on the 2016 and 2018 maps, 2020 on the 2020, 2022 and 2024 maps,
# and 2024 on the 2024 maps. Writes <out>/house_lean.csv (state, district, map_year,
# pres_year, d, r, margin = 100 (D - R) / (D + R)) and <out>/house_lean_states.csv (per
# state: the D + R votes linked by precinct, shared within county, and unallocated, against
# MEDSL's state totals).

library(data.table)

opt <- list(csv = NULL, raw = NULL, out = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}

norm_precinct <- function(x) {
  x <- toupper(gsub("[^A-Za-z0-9]+", " ", x))
  x <- gsub("\\b0+([0-9])", "\\1", x, perl = TRUE)
  gsub(" ", "", x, fixed = TRUE)
}

district_number <- function(x) {
  x <- toupper(trimws(x))
  out <- suppressWarnings(as.integer(x))
  out[x %in% c("AT-LARGE", "AT LARGE")] <- 0L
  out[!is.na(out) & out == 0L] <- 1L
  out
}

# One year's rows, standardised: state, county, juris, precinct, office (PRES or HOUSE),
# district, party (D, R or O), votes. Mode rows are summed later.
read_year <- function(dir, year, offices = c("PRES", "HOUSE")) {
  files <- list.files(file.path(dir, year), pattern = "\\.csv$", full.names = TRUE)
  rbindlist(lapply(files, function(f) {
    if (year == 2016) {
      x <- fread(f, encoding = "Latin-1", select = c(
        "state_postal", "county_fips", "jurisdiction",
        "precinct", "office", "district", "party", "votes"
      ))
      setnames(x, c("state_postal", "jurisdiction"), c("state", "juris"))
      x[, jname := juris]
      x[, office := fifelse(office == "US President", "PRES", fifelse(office == "US House", "HOUSE", NA_character_))]
      # WHY: Minnesota's Democrats are the Democratic-Farmer-Labor party, North Dakota's the Democratic-NPL.
      x[, party := fifelse(grepl("^democrat", tolower(party)), "D", fifelse(tolower(party) == "republican", "R", "O"))]
    } else {
      x <- fread(f, select = c(
        "state_po", "county_fips", "jurisdiction_fips", "jurisdiction_name", "precinct", "office",
        "district", "party_simplified", "party_detailed", "votes", "stage"
      ), colClasses = c(
        county_fips = "character",
        jurisdiction_fips = "character", district = "character", precinct = "character"
      ))
      x <- x[stage %in% c("GEN", "gen", "")]
      setnames(x, c("state_po", "jurisdiction_fips", "jurisdiction_name", "party_simplified"), c("state", "juris", "jname", "party"))
      x[, office := fifelse(office == "US PRESIDENT", "PRES", fifelse(office == "US HOUSE", "HOUSE", NA_character_))]
      # WHY: Nebraska's 2024 file labels the nominees' simplified party NONPARTISAN; the
      # detailed party is right.
      pd <- toupper(x$party_detailed)
      x[, party := fifelse(party == "DEMOCRAT" | grepl("^DEMOCRAT", pd), "D", fifelse(party == "REPUBLICAN" | grepl("^REPUBLICAN", pd), "R", "O"))]
      x[, party_detailed := NULL]
      x[, stage := NULL]
    }
    x <- x[office %in% offices]
    x[, `:=`(
      county = as.character(county_fips), juris = as.character(juris), precinct = as.character(precinct),
      votes = suppressWarnings(as.numeric(votes))
    )]
    x[is.na(votes), votes := 0]
    x[is.na(party), party := "O"]
    # WHY: some files store FIPS codes as numbers, dropping the leading zero (6001 for 06001).
    pad <- function(v) ifelse(grepl("^[0-9]+$", v), ifelse(nchar(v) <= 5, formatC(as.numeric(v), width = 5, flag = "0", format = "d"),
      formatC(as.numeric(v), width = 10, flag = "0", format = "d")), v)
    x[, `:=`(county = pad(county), juris = pad(juris))]
    x[, .(state, county, juris, jname = as.character(jname), precinct, office, district = as.character(district), party, votes)]
  }))
}

# Presidential D and R per precinct, and House weights per precinct and district.
pres_precincts <- function(x) {
  x[office == "PRES" & party %in% c("D", "R"), .(d = sum(votes[party == "D"]), r = sum(votes[party == "R"])),
    by = .(state, county, juris, jname, precinct)
  ]
}
house_weights <- function(x) {
  h <- x[office == "HOUSE", .(v = sum(votes), vd = sum(votes[party == "D"]), vr = sum(votes[party == "R"])),
    by = .(state, county, juris, jname, precinct, district)]
  h[, district := district_number(district)]
  h <- h[!is.na(district) & v > 0]
  h[, w := v / sum(v), by = .(state, county, juris, precinct)]
  h
}

# Name variants tried, in order, when matching a precinct across years: the whole name;
# the part before an underscore (Texas adds a VTD suffix in some years); the
# jurisdiction's name joined to it (Michigan writes "Arbela Township, Precinct 2" one year
# and precinct "2" of jurisdiction Arbela Township another, so the name is tried with and
# without the jurisdiction in front). Looser variants (digits alone, the part after an
# underscore) matched precincts to the wrong districts and are not used. All
# variants share one set of keys within a county, so one year's form can meet another's. Words such as PRECINCT and PCT are dropped first.
precinct_variants <- function(jname, precinct) {
  strip <- function(x) gsub("\\b(PRECINCT|PRECINCTS|PRECT|PREC|PCT|PCTS|VOTING|ELECTION|DISTRICT|DIST|WARD)\\b", " ", toupper(x), perl = TRUE)
  p <- strip(precinct); j <- strip(jname)
  jn <- norm_precinct(j)
  no_j <- mapply(function(a, b) if (nzchar(b)) gsub(b, " ", a, fixed = TRUE) else a, p, j, USE.NAMES = FALSE)
  list(
    norm_precinct(p),
    norm_precinct(sub("_.*", "", p)),
    paste0(jn, "|", norm_precinct(no_j)),
    paste0(jn, norm_precinct(no_j))
  )
}

build_lean <- function(pres, hw, same_year) {
  # Each county's House votes by district on the target map, Democratic and Republican
  # apart, for sharing out the presidential votes that do not link to a precinct.
  cshare <- hw[, .(vd = sum(vd), vr = sum(vr), v = sum(v)), by = .(state, county, district)]
  cshare[, `:=`(
    sd = if (sum(vd) > 0) vd / sum(vd) else v / sum(v),
    sr = if (sum(vr) > 0) vr / sum(vr) else v / sum(v)
  ), by = .(state, county)]
  if (same_year) {
    pres[, key := paste(state, county, juris, precinct)]
    hw[, key := paste(state, county, juris, precinct)]
  } else {
    pv <- precinct_variants(pres$jname, pres$precinct)
    hv <- precinct_variants(hw$jname, hw$precinct)
    hw_all <- rbindlist(lapply(seq_along(hv), function(k) {
      hw[, .(key = paste(state, county, hv[[k]]), src = paste(juris, precinct), district, w)]
    }))
    # WHY: a key that more than one House precinct produces is ambiguous, so it is not used.
    hw_all <- hw_all[, if (uniqueN(src) == 1) .SD, by = key]
    hw_all <- hw_all[, .(w = sum(w)), by = .(key, district)]
    hw_all[, w := w / sum(w), by = key]
    pres[, key := NA_character_]
    for (k in seq_along(pv)) {
      cand <- paste(pres$state, pres$county, pv[[k]])
      uniq <- !(duplicated(cand) | duplicated(cand, fromLast = TRUE))
      hit <- is.na(pres$key) & uniq & cand %in% hw_all$key & !grepl("\\|#?$", pv[[k]]) & nzchar(pv[[k]])
      pres[hit, key := cand[hit]]
    }
    pres[is.na(key), key := paste("unmatched", .I)]
    hw <- hw_all
  }
  linked <- merge(pres, hw[, .(key, district, w)], by = "key", allow.cartesian = TRUE)
  linked <- linked[, .(d = sum(d * w), r = sum(r * w)), by = .(state, county, district)]
  un <- pres[!key %in% hw$key, .(d = sum(d), r = sum(r)), by = .(state, county)]
  spread <- merge(un, cshare[, .(state, county, district, sd, sr)], by = c("state", "county"), allow.cartesian = TRUE)
  spread <- spread[, .(state, county, district, d = d * sd, r = r * sr)]
  shares <- unique(cshare[, .(state, county)])
  lean <- rbind(linked, spread)[, .(d = sum(d), r = sum(r)), by = .(state, district)]
  diag <- merge(
    linked[, .(linked = sum(d + r)), by = state],
    spread[, .(shared = sum(d + r)), by = state],
    by = "state", all = TRUE
  )
  lost <- un[!paste(state, county) %in% paste(shares$state, shares$county), .(unallocated = sum(d + r)), by = state]
  diag <- merge(diag, lost, by = "state", all = TRUE)
  for (j in c("linked", "shared", "unallocated")) set(diag, which(is.na(diag[[j]])), j, 0)
  list(lean = lean, diag = diag)
}

if (!is.null(opt$csv)) {
  options(width = 150)
  dir.create(opt$out, showWarnings = FALSE, recursive = TRUE)
  combos <- data.table(pres_year = c(2016, 2016, 2020, 2020, 2020, 2024), map_year = c(2016, 2018, 2020, 2022, 2024, 2024))
  pres_cache <- list()
  house_cache <- list()
  get_pres <- function(y) {
    if (is.null(pres_cache[[as.character(y)]])) {
      x <- read_year(opt$csv, y, "PRES")
      pres_cache[[as.character(y)]] <<- pres_precincts(x)
    }
    copy(pres_cache[[as.character(y)]])
  }
  get_house <- function(y) {
    if (is.null(house_cache[[as.character(y)]])) {
      house_cache[[as.character(y)]] <<- house_weights(read_year(opt$csv, y, "HOUSE"))
    }
    copy(house_cache[[as.character(y)]])
  }
  medsl <- fread(file.path(opt$raw, "medsl", "president.csv"))
  state_tot <- medsl[party_simplified %in% c("DEMOCRAT", "REPUBLICAN"), .(official = sum(candidatevotes)), by = .(year, state = state_po)]
  leans <- list()
  diags <- list()
  for (i in seq_len(nrow(combos))) {
    py <- combos$pres_year[i]
    my <- combos$map_year[i]
    b <- build_lean(get_pres(py), get_house(my), same_year = py == my)
    leans[[i]] <- b$lean[, `:=`(map_year = my, pres_year = py)]
    d <- merge(b$diag, state_tot[year == py, .(state, official)], by = "state", all.x = TRUE)
    diags[[i]] <- d[, `:=`(map_year = my, pres_year = py)]
    cat(sprintf(
      "pres %d on map %d: %d districts; linked %.1f%%, shared %.1f%%, unallocated %.2f%% of D + R in the file; file / official %.3f\n",
      py, my, nrow(b$lean), 100 * sum(d$linked) / sum(d$linked + d$shared + d$unallocated),
      100 * sum(d$shared) / sum(d$linked + d$shared + d$unallocated),
      100 * sum(d$unallocated) / sum(d$linked + d$shared + d$unallocated),
      sum(d$linked + d$shared + d$unallocated) / sum(d$official, na.rm = TRUE)
    ))
  }
  lean <- rbindlist(leans)
  lean[, margin := 100 * (d - r) / (d + r)]
  # WHY: Maine and Nebraska award electoral votes by district, so their official district
  # results replace the precinct sums on each presidential year's own map (Maine's 2024 file
  # has no House rows for district 1). 2016 has percentages only.
  off <- fread(file.path(opt$raw, "president", "derived", "me_ne_cd_president.csv"))[year %in% c(2016, 2020, 2024)]
  off[, `:=`(od = fifelse(is.na(dem_votes), dem_pct, as.numeric(dem_votes)), or = fifelse(is.na(rep_votes), rep_pct, as.numeric(rep_votes)))]
  off <- off[, .(state = state_po, district, map_year = year, pres_year = year, od, or)]
  lean <- merge(lean, off, by = c("state", "district", "map_year", "pres_year"), all = TRUE)
  lean[!is.na(od), `:=`(d = od, r = or, margin = 100 * (od - or) / (od + or))]
  lean[, c("od", "or") := NULL]
  setcolorder(lean, c("state", "district", "map_year", "pres_year", "d", "r", "margin"))
  fwrite(lean[order(map_year, pres_year, state, district)], file.path(opt$out, "house_lean.csv"))
  fwrite(rbindlist(diags), file.path(opt$out, "house_lean_states.csv"))
}
