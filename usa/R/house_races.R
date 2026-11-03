# House district history for the House race layer (usa/us-house-layer-plan.html, step 2):
# one row per district and general election, 2006-2024, from Cha, Kuriwaki and Snyder,
# Candidates in American General Elections (Harvard Dataverse doi:10.7910/DVN/DGDRDT, CC0).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/house_races.R raw=<raw dir> tables=<tables dir> out=<csv>
#
# Each district is a contest between its leading Democrat (the D slot) and its leading
# Republican (the R slot); everything else is Other. Where a runoff decided the seat
# (Louisiana's December runoffs, Georgia's), the runoff's votes replace the first round's.
# A district with no candidate in one slot is uncontested (no Republican, no Democrat, or a
# top-two race between two candidates of one party): its winner is kept for the seat counts
# but it carries no margin. Columns:
#   year, state, dist, map      the map in force (usa/house_maps.csv from 2016; before it the
#                               2002 and 2012 maps, and the 2006 Texas and Georgia remedial maps)
#   d_votes, r_votes, total     the slots' votes and every candidate's
#   contested                   both slots on the ballot
#   margin                      100 (D - R) / (D + R), contested districts only
#   winner                      D, R or O, from the CKS won flag
#   inc                         +1 for a Democratic incumbent in either slot, -1 Republican, 0 open
#   freshman                    the incumbent first won the seat at the previous general election
#   same_map, prev_margin       the map did not change since the previous cycle, and that
#                               cycle's margin there (NA when it changed or was uncontested)
# Odd-year specials are left out, as for the Senate.

library(haven)

opt <- list(raw = NULL, tables = NULL, out = NULL, maps = "usa/house_maps.csv")
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}

# WHY: a runoff row and its first round share year, state and district; the runoff decides.
final_round <- function(h) {
  h$runoff[is.na(h$runoff)] <- 0
  key <- paste(h$year, h$state, h$dist)
  has_runoff <- key %in% key[h$runoff == 1]
  h[!has_runoff | h$runoff == 1, ]
}

house_history <- function(cks, maps) {
  h <- as.data.frame(cks[cks$office == "H" & cks$type == "G" & cks$year %% 2 == 0, ])
  h <- final_round(h)
  h$inc <- as.integer(!is.na(h$inc) & h$inc > 0)
  # WHY: unopposed Florida candidates are not on the ballot and carry no vote count.
  h$candidatevotes[is.na(h$candidatevotes)] <- 0
  rows <- lapply(split(h, paste(h$year, h$state, h$dist)), function(d) {
    top <- function(p) {
      x <- d[d$party == p, ]
      if (nrow(x) == 0) {
        return(NULL)
      }
      x[which.max(x$candidatevotes), ]
    }
    dd <- top("D")
    rr <- top("R")
    won <- d[d$won == 1, ]
    data.frame(
      year = d$year[1], state = d$state[1], dist = d$dist[1],
      d_votes = if (is.null(dd)) 0 else dd$candidatevotes,
      r_votes = if (is.null(rr)) 0 else rr$candidatevotes,
      total = sum(d$candidatevotes),
      winner = if (nrow(won) != 1) NA else if (won$party == "D") "D" else if (won$party == "R") "R" else "O",
      inc = (if (!is.null(dd) && dd$inc == 1) 1 else 0) - (if (!is.null(rr) && rr$inc == 1) 1 else 0),
      inc_name = if (nrow(won) == 1 && won$inc == 1) won$name_snyder else NA,
      winner_name = if (nrow(won) == 1) won$name_snyder else NA
    )
  })
  out <- do.call(rbind, rows)
  out$contested <- out$d_votes > 0 & out$r_votes > 0
  out$margin <- ifelse(out$contested, 100 * (out$d_votes - out$r_votes) / (out$d_votes + out$r_votes), NA)
  m <- maps[, c("state", "cycle", "map")]
  out <- merge(out, m, by.x = c("state", "year"), by.y = c("state", "cycle"), all.x = TRUE)
  early <- is.na(out$map)
  out$map[early] <- ifelse(
    out$state[early] %in% c("AK", "DE", "MT", "ND", "SD", "VT", "WY"), "at-large",
    paste0(out$state[early], ifelse(out$year[early] < 2012, "-2002", "-2012"))
  )
  # WHY: the 2006 remedial maps in Texas (LULAC v. Perry) and Georgia (2005 legislature).
  out$map[out$year %in% 2006:2010 & out$state %in% c("TX", "GA")] <-
    paste0(out$state[out$year %in% 2006:2010 & out$state %in% c("TX", "GA")], "-2006")
  out <- out[order(out$state, out$dist, out$year), ]
  prev <- out[, c("state", "dist", "year", "map", "margin", "winner_name")]
  prev$year <- prev$year + 2
  names(prev)[4:6] <- c("prev_map", "prev_margin", "prev_winner")
  out <- merge(out, prev, by = c("state", "dist", "year"), all.x = TRUE)
  out$same_map <- !is.na(out$prev_map) & out$prev_map == out$map
  out$prev_margin[!out$same_map] <- NA
  # WHY: CKS marks incumbency, not tenure; a freshman is an incumbent who was not the
  # incumbent two years before but won that election, here matched by name and district.
  out$freshman <- !is.na(out$inc_name) & !is.na(out$prev_winner) & out$inc_name == out$prev_winner &
    !(paste(out$state, out$dist, out$year - 2, out$inc_name) %in% paste(out$state, out$dist, out$year, out$inc_name)[out$inc != 0])
  out[order(out$year, out$state, out$dist), c(
    "year", "state", "dist", "map", "d_votes", "r_votes", "total",
    "contested", "margin", "winner", "inc", "freshman", "same_map", "prev_margin"
  )]
}

if (!is.null(opt$raw)) {
  options(width = 140)
  cks <- read_dta(file.path(opt$raw, "dataverse-DGDRDT", "candidates_2006-2024.dta"))
  maps <- read.csv(opt$maps)
  out <- house_history(cks, maps)
  write.csv(out, opt$out, row.names = FALSE)

  nat <- read.csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"))
  agg <- do.call(rbind, lapply(split(out, out$year), function(d) {
    data.frame(
      year = d$year[1], districts = nrow(d), contested = sum(d$contested),
      d_votes = sum(d$d_votes), r_votes = sum(d$r_votes),
      d_won = sum(d$winner == "D", na.rm = TRUE), r_won = sum(d$winner == "R", na.rm = TRUE),
      no_winner = sum(is.na(d$winner)), same_map = sum(d$same_map), freshmen = sum(d$freshman)
    )
  }))
  agg <- merge(agg, nat[, c("year", "dem_votes", "rep_votes", "dem_seats", "rep_seats")], by = "year")
  agg$d_vote_ratio <- round(agg$d_votes / agg$dem_votes, 4)
  agg$r_vote_ratio <- round(agg$r_votes / agg$rep_votes, 4)
  print(agg[, c(
    "year", "districts", "contested", "d_vote_ratio", "r_vote_ratio", "d_won", "dem_seats",
    "r_won", "rep_seats", "no_winner", "same_map", "freshmen"
  )], row.names = FALSE)
}
