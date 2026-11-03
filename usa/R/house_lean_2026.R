# District lean on the 2026 maps for the House race layer (usa/us-house-layer-plan.html):
# the 2024 presidential margin, D minus R as a share of the two-party vote, on the lines in
# force on 3 November 2026.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/house_lean_2026.R dir=<house dir> raw=<raw dir>
#
# By source, in order of preference:
#   own map       states whose map did not change after 2024: house_lean.R's 2024 lean on
#                 the 2024 map (MEDSL precincts, CC0; Maine and Nebraska official);
#   new-map table Texas, Florida and Tennessee: the 2024 result on the new lines from their
#                 redistricting articles (Wikipedia, CC BY-SA), which print Trump minus Harris;
#   PVI           North Carolina, Ohio, Utah, California, Louisiana and Alabama, which publish
#                 no such table: the district's 2026 Cook PVI as Wikipedia lists it, mapped to
#                 a 2024 margin by a line fitted on the unchanged states, where the PVI and the
#                 own-map lean describe the same lines. On the three states with a new-map
#                 table that line misses by 1.3 points on average (3 November 2026);
#   statewide     at-large seats: the state's 2024 two-party margin (MEDSL, CC0).
# Writes <dir>/house_lean_2026.csv (state, district, margin, source).

library(data.table)

opt <- list(dir = NULL, raw = NULL, maps = "usa/house_maps.csv")
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}

maps <- fread(opt$maps)[cycle == 2026]
wiki <- file.path(opt$raw, "wiki-2026-maps")
pvi <- fread(file.path(wiki, "pvi_2026.csv"))
own <- fread(file.path(opt$dir, "house_lean.csv"))[map_year == 2024 & pres_year == 2024, .(state, district, margin)]
table_new <- fread(file.path(wiki, "pres2024_newmap.csv"))[, .(state, district, margin = pres2024_margin_new)]

unchanged <- maps[changed == 0 & map != "at-large", state]
calib <- merge(pvi[state %in% unchanged], own, by = c("state", "district"))
fit <- lm(margin ~ pvi_2026, calib)
cat(sprintf("PVI line: margin = %.2f + %.3f PVI (n %d, residual SD %.2f)\n", coef(fit)[1], coef(fit)[2], nrow(calib), sigma(fit)))
check <- merge(pvi, table_new, by = c("state", "district"))
cat(sprintf(
  "on the new-map tables: mean absolute miss %.2f, mean miss %+.2f (n %d)\n",
  mean(abs(predict(fit, check) - check$margin)), mean(predict(fit, check) - check$margin), nrow(check)
))

pres <- fread(file.path(opt$raw, "medsl", "president.csv"))[year == 2024 & party_simplified %in% c("DEMOCRAT", "REPUBLICAN")]
statewide <- pres[, .(margin = 100 * (sum(candidatevotes[party_simplified == "DEMOCRAT"]) - sum(candidatevotes[party_simplified == "REPUBLICAN"])) /
  sum(candidatevotes)), by = .(state = state_po)]

at_large <- maps[map == "at-large", state]
by_pvi <- maps[changed == 1 & !state %in% table_new$state, state]
out <- rbind(
  own[state %in% unchanged][, source := "own map"],
  table_new[, source := "new-map table"],
  pvi[state %in% by_pvi][, .(state, district, margin = predict(fit, .SD), source = "PVI")],
  statewide[state %in% at_large][, `:=`(district = 1L, source = "statewide")],
  use.names = TRUE
)
# WHY: a seat with no House race on the 2024 ballot (Oklahoma's 3rd) has no precinct lean.
gap <- pvi[!paste(state, district) %in% paste(out$state, out$district) & !state %in% at_large]
out <- rbind(out, gap[, .(state, district, margin = predict(fit, .SD), source = "PVI")])
setcolorder(out, c("state", "district", "margin", "source"))
fwrite(out[order(state, district)], file.path(opt$dir, "house_lean_2026.csv"))
cat("districts:", nrow(out), "|", paste(out[, .N, by = source][, paste(source, N)], collapse = ", "), "\n")
