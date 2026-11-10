FIT_SCRIPTS <- c(
  fit_script = "R/fit_polling_and_fundamentals_kjordaemi_model.R"
)

is_assign <- function(e, targets) {
  is.call(e) && identical(e[[1]], as.name("<-")) && paste(deparse(e[[2]]), collapse = "") %in% targets
}

# The fit script is interactive and needs live data, so run only its box::use() calls and
# the top-level statements from the first assignment to one of `from` through the last to
# one of `to`, in an environment holding `...`.
run_block <- function(path, from, to, ...) {
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("tibble")
  exprs <- as.list(parse(root(path), encoding = "UTF-8", keep.source = FALSE))
  starts <- which(vapply(exprs, is_assign, logical(1), from))
  ends <- which(vapply(exprs, is_assign, logical(1), to))
  uses <- Filter(\(e) is.call(e) && identical(e[[1]], quote(box::use)), exprs)
  env <- list2env(list(...))
  for (e in c(uses, exprs[min(starts):max(ends)])) eval(e, env)
  env
}
