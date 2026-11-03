FIT_SCRIPTS <- c(
  fit_script = "R/fit_polling_and_fundamentals_kjordaemi_model.R",
  fit_headless = "R/sampler_check/fit_headless.R"
)

is_assign <- function(e, targets) {
  is.call(e) && identical(e[[1]], as.name("<-")) && paste(deparse(e[[2]]), collapse = "") %in% targets
}

# The statements that build stan_data: the fit script's top level, build_stan_data()'s body
# in fit_headless.R.
stan_data_statements <- function(path) {
  exprs <- as.list(parse(root(path), encoding = "UTF-8", keep.source = FALSE))
  builder <- Find(\(e) is_assign(e, "build_stan_data"), exprs)
  if (is.null(builder)) exprs else as.list(builder[[3]][[3]])[-1]
}

# Both scripts are interactive or need live data, so run only their box::use() calls and
# the statements from the first assignment to one of `from` through the last to one of `to`,
# in an environment holding `...`.
run_block <- function(path, from, to, ...) {
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("tibble")
  exprs <- stan_data_statements(path)
  starts <- which(vapply(exprs, is_assign, logical(1), from))
  ends <- which(vapply(exprs, is_assign, logical(1), to))
  uses <- Filter(\(e) is.call(e) && identical(e[[1]], quote(box::use)), exprs)
  env <- list2env(list(...))
  for (e in c(uses, exprs[min(starts):max(ends)])) eval(e, env)
  env
}
