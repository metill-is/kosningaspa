# Chapter 1.4: what folding lists into Annað does to seats, on the official results.
# The forecast allocates over poll parties with Annað ineligible; the official count allocates
# over the lists that ran. Reads only committed files. Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.4-seats/d_fold.R
options(width = 120)
box::use(
  R / harness / truth[read_truth],
  R / election_utils[calculate_seats, layout_for]
)
t <- read_truth()
for (e in as.character(sort(unique(t$election)))) {
  te <- t[t$election == as.Date(e) & t$ran, ]
  folded <- aggregate(votes ~ flokkur + kjordaemi, data = te, FUN = sum)
  s <- calculate_seats(folded, layout_for(e), ineligible = "Annað")
  got <- tapply(s$seats, s$flokkur, sum)
  off <- tapply(te$seats, te$flokkur, sum)
  annad_share <- sum(te$votes[te$flokkur == "Annað"]) / sum(te$votes)
  diff <- got[names(off)] - off
  cat(e, " Annað's share", sprintf("%.2f%%", 100 * annad_share),
      " official Annað seats", off[["Annað"]],
      " seats that move:", sum(abs(diff)) / 2, "\n")
  if (any(diff != 0)) {
    print(rbind(official = off, folded = got[names(off)]))
    # Cell by cell, and the split into constituency and adjustment seats. The unfolded run
    # reproduces the official seats, so its split stands for the official one.
    u <- calculate_seats(data.frame(flokkur = te$list_letter, kjordaemi = te$kjordaemi, votes = te$votes), layout_for(e))
    u$flokkur <- te$flokkur[match(paste(u$flokkur, u$kjordaemi), paste(te$list_letter, te$kjordaemi))]
    ua <- aggregate(cbind(seats, kjordaemi_seats) ~ flokkur + kjordaemi, data = u, FUN = sum)
    m <- merge(ua, s, by = c("flokkur", "kjordaemi"), suffixes = c("_official", "_folded"))
    cat("  cells that change (list, kjördæmi: official -> folded):\n")
    ch <- m[m$seats_official != m$seats_folded | m$kjordaemi_seats_official != m$kjordaemi_seats_folded, ]
    print(ch, row.names = FALSE)
    adj_off <- tapply(m$seats_official - m$kjordaemi_seats_official, m$flokkur, sum)
    adj_fold <- tapply(m$seats_folded - m$kjordaemi_seats_folded, m$flokkur, sum)
    cat("  adjustment seats by list:\n"); print(rbind(official = adj_off, folded = adj_fold))
  }
}
