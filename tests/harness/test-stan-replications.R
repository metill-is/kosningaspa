KJORDAEMI_STAN <- "Stan/polling_and_fundamentals_kjordaemi.stan"

# Production's generated quantities without comments or whitespace, cut at each `for(`, so
# that each piece holds a loop's linear predictor together with the draw that uses it.
gq_loops <- function() {
  x <- paste(readLines(root(KJORDAEMI_STAN), encoding = "UTF-8"), collapse = "\n")
  x <- gsub("(?s)/\\*.*?\\*/", "", x, perl = TRUE)
  x <- gsub("//[^\n]*", "", x)
  x <- gsub("[[:space:]]+", "", x)
  gq <- regmatches(x, regexpr("generatedquantities\\{.*$", x))
  strsplit(gq, "for(", fixed = TRUE)[[1]]
}

loop_drawing <- function(loops, draw) {
  hit <- loops[grepl(draw, loops, fixed = TRUE)]
  expect_length(hit, 1)
  hit
}

test_that("replications use the likelihood's terms, without the forecast's hedge", {
  loops <- gq_loops()
  # WHY: the polls' likelihood has no mu_gamma_pred, and gamma already holds mu_gamma.
  y_rep_k <- loop_drawing(loops, "y_rep_k[n,cols]=")
  expect_match(y_rep_k, "eta_k[2:P]=beta[,date_k[n]]+gamma[,house_k[n]]+delta[,constituency_k[n]];", fixed = TRUE)
  expect_no_match(y_rep_k, "mu_gamma_pred", fixed = TRUE)
  expect_no_match(loop_drawing(loops, "y_rep_national[d,cols]="), "mu_gamma_pred", fixed = TRUE)
})

test_that("the forecasts keep the hedge", {
  loops <- gq_loops()
  expect_match(loop_drawing(loops, "y_rep_national[D+d,]="), "+mu_gamma_pred;", fixed = TRUE)
  expect_match(loop_drawing(loops, "y_pred_constituency[k,]="), "+mu_gamma_pred;", fixed = TRUE)
})
