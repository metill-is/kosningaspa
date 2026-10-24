# The frozen input snapshot: the live data/ and data-raw/ files are copied once into
# harness/snapshot/ (gitignored) and sha256-checked on every read against
# harness/snapshot_manifest.csv (committed). Public results files the harness reads are
# committed under harness/sources/ instead.

#' Snapshot source files, relative to the source root (the main checkout). Each is
#' stored in the snapshot under its basename.
#' @export
SNAPSHOT_FILES <- c(
  polling_data = "data/polling_data.csv",
  constituency_data = "data/constituency_data.csv",
  fundamentals_data = "data/fundamentals_data.csv",
  economy_data = "data/economy_data.csv",
  gild_2016 = "data-raw/hagstofa/althingiskosningar/gild_atkvaedi_2016.xlsx",
  gild_2017 = "data-raw/hagstofa/althingiskosningar/gild_atkvaedi_2017.xlsx",
  gild_2021 = "data-raw/hagstofa/althingiskosningar/gild_atkvaedi_2021.xlsx",
  thingsaeti_2016 = "data-raw/hagstofa/althingiskosningar/thingsaeti_2016.xlsx",
  thingsaeti_2017 = "data-raw/hagstofa/althingiskosningar/thingsaeti_2017.xlsx",
  thingsaeti_2021 = "data-raw/hagstofa/althingiskosningar/thingsaeti_2021.xlsx"
)

sha256_of <- function(path) {
  vapply(path, \(f) digest::digest(file = f, algo = "sha256"), character(1), USE.NAMES = FALSE)
}

read_manifest <- function(manifest) {
  if (!file.exists(manifest)) stop("no snapshot manifest: ", manifest)
  readr::read_csv(
    manifest,
    col_types = readr::cols(.default = "c", bytes = "i"), na = character()
  )
}

check_file <- function(file, sha256, dest) {
  path <- file.path(dest, file)
  if (!file.exists(path)) stop("missing snapshot file: ", file)
  if (sha256_of(path) != sha256) stop("sha256 mismatch: ", file)
  path
}

#' Copy every source in `files` into `dest` and write the manifest.
#' @param src_root the directory the `files` paths are relative to
#' @param dest snapshot directory; it must not exist, so re-snapshotting is a deliberate
#'   manual delete
#' @param manifest path of the manifest CSV to write
#' @param files a named character vector of source paths relative to `src_root`, such as
#'   `SNAPSHOT_FILES`; each is stored in `dest` under its basename, so an error if two
#'   basenames repeat
#' @return the manifest: a tibble (name, file, source, bytes, sha256)
#' @export
snapshot_inputs <- function(src_root, dest = harness_path("snapshot"),
                            manifest = harness_path("snapshot_manifest.csv"),
                            files = SNAPSHOT_FILES) {
  # WHY: the default arguments are promises, forced after this import.
  box::use(R / harness / constants[harness_path])
  stopifnot(
    is.character(files), length(files) > 0, !is.null(names(files)),
    all(nzchar(names(files))), !anyDuplicated(names(files))
  )
  stored <- basename(files)
  if (anyDuplicated(stored)) stop("basenames repeat: ", paste(unique(stored[duplicated(stored)]), collapse = ", "))
  if (file.exists(dest)) {
    stop("snapshot destination exists: ", dest, " (delete it by hand to re-snapshot)")
  }
  src <- file.path(src_root, files)
  absent <- !file.exists(src)
  if (any(absent)) stop("missing source file(s): ", paste(src[absent], collapse = ", "))

  dir.create(dest, recursive = TRUE)
  copied <- file.copy(src, file.path(dest, stored), copy.date = TRUE)
  if (!all(copied)) stop("could not copy: ", paste(src[!copied], collapse = ", "))

  m <- tibble::tibble(
    name = names(files),
    file = stored,
    source = unname(files),
    bytes = as.integer(file.size(file.path(dest, stored))),
    sha256 = sha256_of(file.path(dest, stored))
  )
  readr::write_csv(m, manifest)
  m
}

#' Check every manifest file in `dest` against its sha256.
#' @return `invisible(TRUE)`; errors on a missing file or a mismatched hash
#' @export
verify_snapshot <- function(dest = harness_path("snapshot"),
                            manifest = harness_path("snapshot_manifest.csv")) {
  box::use(R / harness / constants[harness_path])
  m <- read_manifest(manifest)
  for (i in seq_len(nrow(m))) check_file(m$file[i], m$sha256[i], dest)
  invisible(TRUE)
}

#' Absolute path of one snapshot file, after checking its sha256.
#' @param name a manifest name, such as the `SNAPSHOT_FILES` name "polling_data"
#' @export
snapshot_file <- function(name, dest = harness_path("snapshot"),
                          manifest = harness_path("snapshot_manifest.csv")) {
  box::use(R / harness / constants[harness_path])
  m <- read_manifest(manifest)
  i <- match(name, m$name)
  if (length(name) != 1 || is.na(i)) {
    stop("not in the snapshot manifest: ", paste(name, collapse = ", "))
  }
  normalizePath(check_file(m$file[i], m$sha256[i], dest))
}
