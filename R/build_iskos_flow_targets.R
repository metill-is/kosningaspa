# Flow-informed prior targets (Omega_0) for the forecast model's RW correlation, one per
# election, from the ÍSKOS switching tables. Method and Stan snippet: R/flow_prior.R.
#
# Each election uses the previous cycle's table (2024 <- 2017->2021), so a backtest does
# not leak; 2028 reuses 2017->2021 until the ÍSKOS 2024 wave is released. Variants:
#   base      gamma = 1 (additive edge shocks), 2% minimum share
#   gamma2    gamma = 2 (multiplicative edge shocks)
#   min3      3% minimum share (robustness of the small-party regulariser)
# (Dropping the reservoir is not a useful variant for Omega_0: in clr coordinates the
# reservoir only adds independent per-party log noise; see R/flow_prior.R.)
#
# Writes the gitignored data/iskos/flow_targets.rds (derived from GAGNÍS-licensed data):
# a list by variant then election, each with pi, W, Sigma_q, R_q, Sigma0, Omega0,
# Omega0_inv, imputed parties and the table used.
#
# Run from the repo root:  Rscript R/build_iskos_flow_targets.R   (needs data/iskos/,
# from R/build_iskos_switching.R)

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})
invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
options(width = 150)
box::use(R / flow_prior[flow_prior_target, read_switching, model_parties])

elections <- c(2016, 2017, 2021, 2024, 2028)
variants <- list(
  base = list(gamma = 1, min_share = 0.02),
  gamma2 = list(gamma = 2, min_share = 0.02),
  min3 = list(gamma = 1, min_share = 0.03)
)
switching <- read_switching()
targets <- map(variants, \(v) {
  map(elections, \(e) flow_prior_target(e, switching, gamma = v$gamma, min_share = v$min_share)) |>
    set_names(elections)
})
write_rds(targets, here("data", "iskos", "flow_targets.rds"))

abbr <- c(
  "Annað" = "Oth", "Sjálfstæðisflokkurinn" = "D", "Framsóknarflokkurinn" = "B", "Samfylkingin" = "S",
  "Vinstri Græn" = "V", "Píratar" = "P", "Viðreisn" = "C", "Flokkur Fólksins" = "F",
  "Miðflokkurinn" = "M", "Sósíalistaflokkurinn" = "J"
)
ut <- \(m, keep) {
  m <- m[keep, keep]
  m[upper.tri(m)]
}

cat("Targets (base):\n")
walk(targets$base, \(t) cat(sprintf(
  "  %d <- ÍSKOS %d->%d  flows used n=%.0f, unknown dropped n=%.0f, imputed: %s, min eigen %.2g\n",
  t$election, t$table[["prev"]], t$table[["cur"]], t$n_used, t$n_dropped,
  if (length(t$imputed)) paste(abbr[t$imputed], collapse = " ") else "-", t$min_eigen
)))

t24 <- targets$base[["2024"]]
cat("\nOmega_0 for the 2024 forecast (base; beta coordinates = clr of parties 2..10):\n")
o <- round(t24$Omega0, 2)
dimnames(o) <- list(abbr[rownames(o)], abbr[colnames(o)])
print(o)

cat("\nStability between consecutive targets (parties observed in both): correlation of off-diagonals\n")
cat("  (share-space R_q is the flow structure itself; Omega_0 adds the clr map, which amplifies small parties)\n")
for (k in 2:4) {
  a <- targets$base[[k - 1]]
  b <- targets$base[[k]]
  keep <- setdiff(model_parties()[-1], c(a$imputed, b$imputed))
  cat(sprintf(
    "  %d vs %d: R_q r = %.2f, Omega_0 r = %.2f over %d parties (%s)\n", a$election, b$election,
    cor(ut(a$R_q, keep), ut(b$R_q, keep)), cor(ut(a$Omega0, keep), ut(b$Omega0, keep)),
    length(keep), paste(abbr[keep], collapse = " ")
  ))
}

cat("\nSensitivity (2024), Omega_0 off-diagonals vs the base target:\n")
for (v in c("gamma2", "min3")) {
  keep <- model_parties()[-1]
  o <- targets[[v]][["2024"]]$Omega0
  cat(sprintf(
    "  %-7s r = %.2f, max |difference| = %.2f\n", v,
    cor(ut(t24$Omega0, keep), ut(o, keep)), max(abs(ut(t24$Omega0, keep) - ut(o, keep)))
  ))
}
