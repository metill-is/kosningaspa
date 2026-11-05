# Chapter 1.2: which year each fundamentals matrix column holds, and which lists each
# training election's likelihood reads. prepare_fundamentals_data() drops the first
# election's column from y_f, incumbent_f, incumbent_years, vnv and growth, but not from
# x_f (the lagged regressor), index_f or n_parties_f. The Stan model reads column d of
# all of them together. Reads the live, git-ignored data/ CSVs; fits nothing.
# Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.2-fundamentals/d_align.R
options(width = 160)
box::use(R / data[read_fundamentals_data], R / stan_data[prepare_fundamentals_data])

econ <- readr::read_csv(here::here("data", "economy_data.csv"), show_col_types = FALSE)
fd <- read_fundamentals_data() |> dplyr::inner_join(econ, by = "date")

report <- function(fd, label) {
  f <- prepare_fundamentals_data(fd)
  cat("==", label, "==\n")
  for (n in c("y_f", "x_f", "incumbent_years", "vnv", "growth", "index_f")) {
    cn <- colnames(f[[n]])
    cat(sprintf("  %-15s %2d cols: %s .. %s\n", n, ncol(f[[n]]), cn[1], cn[length(cn)]))
  }
  cat(sprintf("  D_f = %d; the model declares x_f, incumbent_years, vnv, growth and index_f with D_f + 1 = %d columns\n",
              f$D_f, f$D_f + 1))
  cat(sprintf("  n_parties_f has %d entries\n", length(f$n_parties_f)))
  if (ncol(f$incumbent_years) != f$D_f + 1) {
    cat("  incumbent_years has", ncol(f$incumbent_years), "columns, not D_f + 1: these data do not fit the declared sizes\n\n")
    return(invisible(NULL))
  }
  lists <- rownames(f$y_f)
  tot_lost <- 0
  cat("\n  training column d: the result's year, the x_f year, the index_f year, and the lists the likelihood reads\n")
  for (d in seq_len(f$D_f)) {
    yr <- colnames(f$y_f)[d]
    read <- f$index_f[seq_len(f$n_parties_f[d]), d]
    ran <- which(f$y_f[, d] > 0)
    left_out <- setdiff(ran, read)
    zero_in <- intersect(read, which(f$y_f[, d] == 0))
    v <- f$y_f[, d]
    lost <- sum(v[left_out]) / sum(v)
    tot_lost <- tot_lost + length(left_out)
    cat(sprintf("  d=%2d  y %s  x %s  incumbent %s  index %s | lists read %d, ran %d | left out %4.1f%% of votes: %s | read with 0 votes: %s\n",
                d, yr, colnames(f$x_f)[d], colnames(f$incumbent_years)[d], colnames(f$index_f)[d],
                length(read), length(ran), 100 * lost,
                if (length(left_out)) paste(lists[left_out], collapse = ", ") else "-",
                if (length(zero_in)) paste(lists[zero_in], collapse = ", ") else "-"))
  }
  pc <- f$D_f + 1
  cat(sprintf("\n  prediction column %d: x %s, incumbent %s, index %s; lists: %s\n\n", pc,
              colnames(f$x_f)[pc], colnames(f$incumbent_years)[pc], colnames(f$index_f)[pc],
              paste(lists[f$index_f[seq_len(f$n_parties_f[pc]), pc]], collapse = ", ")))
}

report(fd, "data/fundamentals_data.csv as it stands")
report(dplyr::mutate(fd, voteshare = dplyr::if_else(year == 2024, NA_real_, voteshare)),
       "the same, with the 2024 vote shares blanked (the 2024 forecast's layout: the target election's rows carry voteshare_prior but no result)")
