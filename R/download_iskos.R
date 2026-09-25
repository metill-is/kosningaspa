# Download the Icelandic National Election Study (ÍSKOS / ICENES) from the GAGNÍS
# Dataverse into data-raw/iskos/ (gitignored). Every voter-survey wave records the
# current vote (prtvoteYY) and the recalled previous-election vote (prtfvoteYY), i.e.
# an election-to-election switching table; the campaign surveys are pre/post panels.
#
# Tabular files are fetched in their ORIGINAL format (SPSS .sav, keeps value labels);
# every file is checked against the archive's MD5. Restricted files are skipped and
# logged, so re-running picks them up once released. Idempotent: files already on
# disk with a matching MD5 are not re-downloaded. Writes data-raw/iskos/manifest.csv.
#
# Licence: the 1983–2017 voter surveys fall under the GAGNÍS user terms (non-commercial
# research/teaching/study; cite the data and GAGNÍS; report publications to GAGNÍS;
# share derived material only with registered users). 2021 and the campaign surveys
# are CC0. Downloading the GAGNÍS-terms files means accepting those terms.
#
# Run from the repo root:  Rscript R/download_iskos.R [voter_survey|campaign_survey ...]
# (optional args restrict the run to those surveys; the manifest keeps other surveys' rows).

library(here)
library(jsonlite)

server <- "https://gagnis.hi.is"
out_root <- here("data-raw", "iskos")
pause_s <- 5
options(timeout = 120)

datasets <- data.frame(
  survey = c(rep("voter_survey", 12), rep("campaign_survey", 3)),
  year = c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2009, 2013, 2016, 2017, 2021, 2016, 2017, 2021),
  doi = c(
    sprintf("doi:10.34881/1.%05d", 1:11), "doi:10.34881/0ERQOZ",
    "doi:10.34881/SZUY8A", "doi:10.34881/PZJCHA", "doi:10.34881/HVPGFX"
  )
)
surveys <- commandArgs(trailingOnly = TRUE)
if (length(surveys)) datasets <- datasets[datasets$survey %in% surveys, ]
stopifnot("no datasets selected" = nrow(datasets) > 0)

md5_of <- function(path) unname(tools::md5sum(path))

manifest <- list()
for (k in seq_len(nrow(datasets))) {
  ds <- datasets[k, ]
  # WHY: pause before EVERY request. A first run of ~115 back-to-back requests got this
  # IP blocked by the hi.is firewall (2026-09-23; still blocked after 40 min, lifted by 09-25).
  Sys.sleep(pause_s)
  meta <- fromJSON(
    sprintf("%s/api/datasets/:persistentId/?persistentId=%s", server, ds$doi),
    simplifyVector = FALSE
  )$data$latestVersion
  licence <- if (!is.null(meta$license$name)) meta$license$name else if (nzchar(meta$termsOfUse %||% "")) "GAGNÍS user terms" else "unspecified"
  dest_dir <- file.path(out_root, ds$survey, ds$year)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  for (f in meta$files) {
    df <- f$dataFile
    tabular <- !is.null(df$originalFileName)
    fname <- if (tabular) df$originalFileName else df$filename
    dest <- file.path(dest_dir, fname)
    status <- "downloaded"
    if (isTRUE(f$restricted)) {
      status <- "restricted_skipped"
    } else if (file.exists(dest) && identical(md5_of(dest), df$md5)) {
      status <- "already_present"
    } else {
      Sys.sleep(pause_s)
      url <- sprintf("%s/api/access/datafile/%s%s", server, df$id, if (tabular) "?format=original" else "")
      status <- tryCatch(
        {
          download.file(url, dest, mode = "wb", quiet = TRUE)
          if (identical(md5_of(dest), df$md5)) "downloaded" else "md5_mismatch"
        },
        error = function(e) "failed"
      )
      if (status != "downloaded" && file.exists(dest)) file.remove(dest)
    }
    message(sprintf("%-15s %d  %-19s %s", ds$survey, ds$year, status, fname))
    on_disk <- status %in% c("downloaded", "already_present")
    manifest[[length(manifest) + 1]] <- data.frame(
      survey = ds$survey, year = ds$year, doi = ds$doi, file_id = df$id,
      file = if (on_disk) file.path(ds$survey, ds$year, fname) else NA_character_,
      bytes = if (on_disk) file.size(dest) else NA_real_,
      md5 = df$md5, status = status, licence = licence,
      checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  }
}

manifest <- do.call(rbind, manifest)
manifest_path <- file.path(out_root, "manifest.csv")
if (file.exists(manifest_path)) {
  old <- read.csv(manifest_path, fileEncoding = "UTF-8")
  old <- old[!paste(old$survey, old$year) %in% paste(datasets$survey, datasets$year), ]
  manifest_all <- rbind(old, manifest)
} else {
  manifest_all <- manifest
}
write.csv(manifest_all, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
ok <- manifest$status %in% c("downloaded", "already_present")
bad <- manifest$status %in% c("failed", "md5_mismatch")
message(sprintf(
  "\n%d files OK (%d downloaded, %d already present), %d restricted skipped, %d FAILED -> %s",
  sum(ok), sum(manifest$status == "downloaded"), sum(manifest$status == "already_present"),
  sum(manifest$status == "restricted_skipped"), sum(bad), out_root
))
if (any(bad)) {
  message("Re-run to retry: ", paste(manifest$file_id[bad], collapse = ", "))
  quit(status = 1)
}
