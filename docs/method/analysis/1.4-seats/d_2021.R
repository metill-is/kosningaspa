# Chapter 1.4: the 2021 allocation traced step by step from the official results, and
# checked against calculate_seats() and the official seats. Reads only committed files.
# Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.4-seats/d_2021.R
options(width = 120)
box::use(
  R / harness / truth[read_truth],
  R / election_utils[calculate_seats, layout_for]
)

t <- read_truth()
e <- "2021-09-25"
te <- t[t$election == as.Date(e) & t$ran, ]
lay <- layout_for(e)
draw <- data.frame(flokkur = te$list_letter, kjordaemi = te$kjordaemi, votes = te$votes)

V <- tapply(draw$votes, list(draw$flokkur, draw$kjordaemi), sum)
V[is.na(V)] <- 0
lay <- lay[match(colnames(V), lay$kjordaemi), ]
cat("valid votes:", sum(V), " lists:", nrow(V), "\n")
nat <- rowSums(V); share <- nat / sum(nat)
print(round(sort(100 * share, decreasing = TRUE), 2))

# Constituency seats: D'Hondt in each kjördæmi
S <- V * 0
for (k in seq_len(ncol(V))) {
  q <- V[, k]
  for (s in seq_len(lay$n_seats[k])) {
    w <- which.max(q); S[w, k] <- S[w, k] + 1; q[w] <- V[w, k] / (S[w, k] + 1)
  }
}
cat("\nconstituency seats (54):\n"); print(S[rowSums(S) > 0, ])

kd <- "Norðvestur"
cat("\nD'Hondt in", kd, "(", lay$n_seats[lay$kjordaemi == kd], "seats ): quotients v / d\n")
v <- V[V[, kd] > 0, kd]
qt <- outer(v, 1:4, "/")
print(floor(qt[order(-v), ] + 0.5))  # halves rounded up
cut <- sort(as.vector(qt), decreasing = TRUE)[lay$n_seats[lay$kjordaemi == kd] + 0:1]
cat("last quotient elected:", round(cut[1]), " first not elected:", round(cut[2]), "\n")

# Adjustment seats: national D'Hondt continued over lists with >= 5%, placed by share
avail <- lay$n_jofnun; names(avail) <- lay$kjordaemi
adj <- nat * 0; Sa <- S
cat("\nadjustment seats in order:\n")
for (i in seq_len(sum(lay$n_jofnun))) {
  qn <- nat * (share >= 0.05) / (rowSums(S) + adj + 1)
  w <- which.max(qn)
  adj[w] <- adj[w] + 1
  r <- V[w, ] / (Sa[w, ] + 1) / colSums(V)
  free <- avail
  ord <- order(r, decreasing = TRUE)
  for (p in ord) {
    if (avail[p] > 0) { Sa[w, p] <- Sa[w, p] + 1; avail[p] <- avail[p] - 1; break }
  }
  skipped <- colnames(V)[ord[seq_len(match(p, ord) - 1)]]
  cat(sprintf("%d  %-2s  national quotient %7.0f  (votes %6.0f / %2d)  placed in %-16s share quotient %.4f  skipped (full): %s\n",
              i, names(w), qn[w], nat[w], rowSums(S)[w] + adj[w], colnames(V)[p], r[p],
              if (length(skipped)) paste(skipped, collapse = ", ") else "none"))
  if (i == sum(lay$n_jofnun)) {
    cat("  seat", i, "in full:", names(w), "votes, seats so far, share quotient and free adjustment seats per kjordaemi\n")
    print(data.frame(votes = V[w, ord], seats_before = Sa[w, ord] - (seq_along(ord) == match(p, ord)),
                     valid_votes = colSums(V)[ord], share_quotient = round(r[ord], 4), free = free[ord]))
  }
}
cat("lists under 5%:", paste(names(share)[share < 0.05], collapse = ", "), "\n")
nxt <- nat * (share >= 0.05) / (rowSums(S) + adj + 1)
cat("next national quotients after the 9th seat:\n"); print(round(sort(nxt[nxt > 0], decreasing = TRUE)))
cat("D votes", nat[["D"]], " B votes", nat[["B"]], "\n")

cs <- calculate_seats(draw, layout_for(e))
mine <- data.frame(flokkur = rep(rownames(Sa), ncol(Sa)), kjordaemi = rep(colnames(Sa), each = nrow(Sa)),
                   trace = as.vector(Sa))
chk <- merge(merge(cs, mine), data.frame(flokkur = te$list_letter, kjordaemi = te$kjordaemi, official = te$seats))
stopifnot(all(chk$seats == chk$trace), all(chk$seats == chk$official))
cat("\ntrace == calculate_seats() == official seats in all", nrow(chk), "cells\n")
cat("national seats:\n"); print(tapply(chk$seats, chk$flokkur, sum))
