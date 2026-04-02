#!/usr/bin/env Rscript

# ============================================================
# 00_check_setup.R
#
# Run this immediately after unzipping your data package.
# Verifies that all required files are present and that
# screenshot paths in the derived CSVs resolve correctly.
#
# If everything passes, run 03_run_app.R next.
# If anything fails, contact Chris (cb5691@nyu.edu).
# ============================================================

# ----------------------------
# Install any missing packages
# ----------------------------
required_packages <- c("readr", "shiny", "dplyr", "stringr", "tidyr", "tibble")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(readr)

# ----------------------------
# CONFIG (EDIT THIS)
# ----------------------------
TEAM_SLUG <- ""   # <-- your ISO2 country code (e.g. "GB", "DK", "NL")


# ----------------------------
# Run
# ----------------------------
get_script_dir <- function() {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0) return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = FALSE)
}
SCRIPT_DIR <- get_script_dir()

if (!nzchar(trimws(TEAM_SLUG))) {
  stop("TEAM_SLUG is blank. Set it to your ISO2 country code at the top of this script.", call. = FALSE)
}

cat(sprintf("\nChecking setup for team: %s\n", TEAM_SLUG))
cat(rep("-", 50), "\n", sep = "")

all_ok <- TRUE
issues <- character(0)

for (wave in c("endline", "baseline")) {
  cat(sprintf("\n[%s]\n", toupper(wave)))

  derived_dir <- file.path(SCRIPT_DIR, "data", "qualtrics", TEAM_SLUG, wave, "derived")
  avg_csv <- file.path(derived_dir, "average_screentime_for_annotation.csv")
  app_csv <- file.path(derived_dir, "app_screentime_for_annotation.csv")

  # Check derived CSVs
  for (f in c(avg_csv, app_csv)) {
    if (file.exists(f)) {
      cat(sprintf("  \u2705 Found: %s\n", f))
    } else {
      cat(sprintf("  \u274c MISSING: %s\n", f))
      issues <- c(issues, sprintf("Missing: %s", f))
      all_ok <- FALSE
    }
  }

  # Check screenshot files referenced in the CSVs
  for (csv_path in c(avg_csv, app_csv)) {
    if (!file.exists(csv_path)) next

    df <- tryCatch(
      read_csv(csv_path, show_col_types = FALSE),
      error = function(e) { cat("  WARNING: could not read", basename(csv_path), "\n"); NULL }
    )
    if (is.null(df)) next

    path_cols <- grep("_path$", names(df), value = TRUE)
    if (!length(path_cols)) next

    paths <- na.omit(unlist(df[path_cols]))
    paths <- paths[nzchar(paths)]

    n_total   <- length(paths)
    n_missing <- sum(!file.exists(paths))

    if (n_total == 0) {
      cat(sprintf("  WARNING: no screenshot paths found in %s\n", basename(csv_path)))
    } else if (n_missing == 0) {
      cat(sprintf("  \u2705 %s: %d screenshots, all present\n", basename(csv_path), n_total))
    } else {
      cat(sprintf("  \u274c %s: %d/%d screenshots MISSING\n", basename(csv_path), n_missing, n_total))
      issues <- c(issues, sprintf("%d screenshots missing in %s/%s", n_missing, wave, basename(csv_path)))
      all_ok <- FALSE
    }
  }
}

cat(rep("-", 50), "\n", sep = "")

if (all_ok) {
  cat("\n\u2705 All checks passed. You are ready to annotate.\n")
  cat("   Next step: Rscript 03_run_app.R\n\n")
} else {
  cat("\n\u274c Some checks failed:\n")
  for (issue in issues) cat("   -", issue, "\n")
  cat("\nPlease contact Chris (cb5691@nyu.edu) and share this output.\n\n")
}
