# WHY: under C, R translates every name tag to a native symbol, escaped or not;
# source() or a bare parse() also reads non-ASCII literals natively (R54).
HARNESS_FILES <- c(Sys.glob(root("R", "harness", "*.R")), root("R", "election_utils.R"))
non_ascii <- function(x) grepl("[^\\x01-\\x7f]", x, perl = TRUE)
tokens <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[pd$terminal, ]
  pd[order(pd$line1, pd$col1), ]
}

test_that("no string literal or symbol in a harness module is non-ASCII", {
  for (f in HARNESS_FILES) {
    tk <- tokens(f)
    bad <- tk[tk$token %in% c("STR_CONST", "SYMBOL", "SYMBOL_SUB", "SYMBOL_FUNCTION_CALL") & non_ascii(tk$text), ]
    expect_equal(nrow(bad), 0, label = paste(basename(f), "lines", paste(bad$line1, collapse = ", ")))
  }
})

test_that("every name tag in a harness module is plain ASCII", {
  for (f in HARNESS_FILES) {
    tk <- tokens(f)
    tag <- which(tk$token == "EQ_SUB") - 1L
    tag <- tag[tk$token[tag] %in% c("STR_CONST", "SYMBOL_SUB")]
    bad <- tk[tag[non_ascii(tk$text[tag]) | grepl("\\", tk$text[tag], fixed = TRUE)], ]
    expect_equal(nrow(bad), 0, label = paste(basename(f), "lines", paste(bad$line1, collapse = ", ")))
  }
})

test_that("a harness loaded under the C locale still reads the snapshot right", {
  skip_without_snapshot()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    'options(box.path = normalizePath("."))',
    "box::use(R/harness/constants[FUNDAMENTALS_RENAMED, KJORDAEMI], R/election_utils[layout_for],",
    "         R/harness/info_set[info_set, read_inputs, validate_inputs])",
    'invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))',
    'info <- info_set("2024-11-30", "2024-11-29", read_inputs())',
    'stopifnot(identical(names(FUNDAMENTALS_RENAMED), "Frams\\u00f3kn"),',
    '          length(info$fundamentals$S_f) == 9, "Frams\\u00f3knarflokkurinn" %in% info$fundamentals$S_f,',
    '          all(layout_for("2024-11-30")$kjordaemi %in% KJORDAEMI$kjordaemi))',
    "validate_inputs(info)",
    'cat("R54 OK\\n")'
  ), script)
  out <- withr::with_dir(root(), system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=C"
  ))
  expect_true("R54 OK" %in% out, info = paste(out, collapse = "\n"))
})
