# Points CMDSTAN at an empty directory for the caller's frame, after checking that cmdstanr
# then finds no CmdStan: a script run under it stops at its first CmdStan call, before any
# compile or sample. test-v2-equivalence.R and test-fit-cell.R run their refusals under it.
local_no_cmdstan <- function(env = parent.frame()) {
  withr::local_envvar(CMDSTAN = withr::local_tempdir(.local_envir = env), .local_envir = env)
  probe <- withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("-e", shQuote("cmdstanr::cmdstan_version()")),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
  if (is.null(attr(probe, "status"))) stop("cmdstanr finds CmdStan with CMDSTAN set to an empty directory: not running the script")
}
