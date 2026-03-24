#!/usr/bin/env Rscript

# ============================================================
# 04_bundle_results.R
#
# Packages annotation results into ZIP files for BOTH waves.
#
# Produces (per wave):
#   data/qualtrics/<TEAM_SLUG>/<WAVE>/results/
#     bundle_<TEAM_SLUG>_<WAVE>_<timestamp>.zip
# ============================================================

library(stringr)

# ----------------------------
# CONFIG (EDIT THESE)
# ----------------------------
TEAM_SLUG <- "XX"  # <-- your ISO2 country code (e.g. "GB", "US")


# ----------------------------
# Helpers
# ----------------------------
FILES_TO_BUNDLE <- c("annotations_avg.csv", "annotations_app.csv",
                     "sample_avg.csv", "sample_app.csv")

get_script_dir <- function() {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0) return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = TRUE)
}

timestamp_str <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

write_manifest <- function(results_dir, wave, included_files) {
  manifest_path <- file.path(results_dir, "bundle_manifest.txt")
  writeLines(c(
    paste0("TEAM_SLUG: ", TEAM_SLUG),
    paste0("WAVE: ", wave),
    paste0("CREATED_AT: ", as.character(Sys.time())),
    "", "FILES_INCLUDED:", paste0(" - ", included_files)
  ), manifest_path, useBytes = TRUE)
  manifest_path
}

make_zip_in_dir <- function(results_dir, zip_path, files_rel) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(results_dir)
  files_rel <- files_rel[file.exists(files_rel)]
  if (length(files_rel) == 0) stop("No files to zip found in: ", results_dir, call. = FALSE)
  utils::zip(zipfile = zip_path, files = files_rel)
  invisible(zip_path)
}

# ----------------------------
# Bundle both waves
# ----------------------------
if (is.na(TEAM_SLUG) || !nzchar(trimws(TEAM_SLUG))) {
  stop("TEAM_SLUG is blank. Set it to your ISO2 country code (e.g. \"GB\", \"US\") in the CONFIG section.",
       call. = FALSE)
}

BASE_DIR <- get_script_dir()

for (WAVE in c("baseline", "endline")) {
  message(sprintf("\n========== Bundling %s ==========", toupper(WAVE)))

  RESULTS_DIR <- file.path(BASE_DIR, "data", "qualtrics", TEAM_SLUG, WAVE, "results")

  if (!dir.exists(RESULTS_DIR)) {
    message(sprintf("Skipping %s — results directory not found: %s", WAVE, RESULTS_DIR))
    message("(Run 03_run_app.R and complete at least one annotation task first.)")
    next
  }

  existing_rel <- FILES_TO_BUNDLE[file.exists(file.path(RESULTS_DIR, FILES_TO_BUNDLE))]

  if (length(existing_rel) == 0) {
    message(sprintf("Skipping %s — no annotation files found in: %s", WAVE, RESULTS_DIR))
    next
  }

  manifest_path <- write_manifest(RESULTS_DIR, WAVE, existing_rel)
  manifest_rel  <- basename(manifest_path)
  files_rel     <- c(existing_rel, manifest_rel)

  zip_name <- paste0("bundle_", TEAM_SLUG, "_", WAVE, "_", timestamp_str(), ".zip")
  zip_path <- file.path(RESULTS_DIR, zip_name)

  make_zip_in_dir(RESULTS_DIR, zip_path, files_rel)

  cat(sprintf("\u2705 %s bundle created:\n  %s\n", tools::toTitleCase(WAVE), zip_path))
  cat("Includes:\n")
  cat(paste0(" - ", files_rel), sep = "\n")
  cat("\n")
}

cat("Submit your ZIP files at: https://forms.gle/szGhMHymtzTqEEjh8\n")
