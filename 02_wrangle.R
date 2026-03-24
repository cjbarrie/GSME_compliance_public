#!/usr/bin/env Rscript

# ============================================================
# 02_wrangle.R
#
# Processes responses into annotation-ready CSVs for BOTH waves.
# Runs endline first, then baseline.
#
# Reads (per wave):
#   data/qualtrics/<TEAM_SLUG>/<WAVE>/responses.csv
#   data/qualtrics/<TEAM_SLUG>/<WAVE>/uploaded_files_manifest.csv
#
# Writes (per wave):
#   data/qualtrics/<TEAM_SLUG>/<WAVE>/derived/average_screentime_for_annotation.csv
#   data/qualtrics/<TEAM_SLUG>/<WAVE>/derived/app_screentime_for_annotation.csv
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)


# ----------------------------
# CONFIG (EDIT THESE)
# ----------------------------
TEAM_SLUG <- "XX"  # <-- your ISO2 country code (e.g. "GB", "US")

# Participant ID column — stable ID that matches across waves, from your panel provider
# (e.g. "ID", "PanelID", "ExternalReference"). Set to NA if you don't have one.
PARTICIPANT_ID_COL <- "ID"

# For test data only: generate dummy IDs if the column is all NA. Set FALSE for real data.
GENERATE_DUMMY_IDS <- FALSE


# ----------------------------
# Wave-specific column mappings
# ----------------------------
WAVE_CONFIG <- list(
  baseline = list(
    device_col = "iPhoneorAndroid2",
    iphone = list(
      total     = c("IPhoneReportTotal2_1",    "IPhoneReportTotal2_2"),
      instagram = c("IPhoneInsta2_1",           "IPhoneInsta2_2"),
      facebook  = c("IPhoneFacebook2_1",        "IPhoneFacebook2_2"),
      tiktok    = c("IPhoneTikTok2_1",          "IPhoneTikTok2_2"),
      twitter   = c("IPhoneTwitter2_1",         "IPhoneTwitter2_2"),
      ss        = c("IPhoneSS12_Id", "iPhoneSS22_Id", "iPhoneSS32_Id", "iPhoneSS42_Id")
    ),
    android = list(
      total     = c("_AndroidReportTotal2_1",   "_AndroidReportTotal2_2"),
      instagram = c("_AndroidInsta2_1",         "_AndroidInsta2_2"),
      facebook  = c("_AndroidFacebook2_1",      "_AndroidFacebook2_2"),
      tiktok    = c("_AndroidTikTok2_1",        "_AndroidTikTok2_2"),
      twitter   = c("_AndroidTwitter2_1",       "_AndroidTwitter2_2"),
      ss        = c("_AndroidSS12_Id", "_AndroidSS22_Id", "_AndroidSS32_Id", "_AndroidSS42_Id"),
      prefix_regex = "^%d_Android(SS[1-4]2_Id|ReportTotal2_[12]|Insta2_[12]|Facebook2_[12]|TikTok2_[12]|Twitter2_[12])$"
    )
  ),
  endline = list(
    device_col = "iPhoneorAndroid3",
    iphone = list(
      total     = c("IPhoneLastWeekTotal3_1",   "IPhoneLastWeekTotal3_2"),
      instagram = c("IPhoneLastWeekInsta3_1",   "IPhoneLastWeekInsta3_2"),
      facebook  = c("IPhoneLastWeekFB3_1",      "IPhoneLastWeekFB3_2"),
      tiktok    = c("IPhoneLastWeekTT3_1",      "IPhoneLastWeekTT3_2"),
      twitter   = c("IPhoneLastWeekTwit3_1",    "IPhoneLastWeekTwit3_2"),
      ss        = c("IPhoneLastWeekSS13_Id", "IPhoneLastWeekSS23_Id",
                    "IPhoneLastWeekSS33_Id", "IPhoneLastWeekSS43_Id")
    ),
    android = list(
      total     = c("_AndroidLastWeekTot3_1",   "_AndroidLastWeekTot3_2"),
      instagram = c("_AndroidLastWeekInst3_1",  "_AndroidLastWeekInst3_2"),
      facebook  = c("_AndroidLastWeekFB3_1",    "_AndroidLastWeekFB3_2"),
      tiktok    = c("_AndroidLastWeekTT3_1",    "_AndroidLastWeekTT3_2"),
      twitter   = c("_AndroidLaskWeekTw3_1",    "_AndroidLaskWeekTw3_2"),  # typo preserved
      ss        = c("_AndroidLastWeekSS13_Id", "_AndroidLastWeekSS23_Id",
                    "_AndroidLastWeekSS33_Id", "_AndroidLastWeekSS43_Id"),
      prefix_regex  = "^%d_AndroidLastWeek(SS[1-4]3_Id|Tot3_[12]|Inst3_[12]|FB3_[12]|TT3_[12])$",
      twitter_regex = "^%d_AndroidLaskWeekTw3_[12]$"
    )
  )
)

PREFIX_TO_DAY <- c("1"="Monday","2"="Tuesday","3"="Wednesday","4"="Thursday",
                   "5"="Friday","6"="Saturday","7"="Sunday")
prefix_to_day <- function(prefix_int) unname(PREFIX_TO_DAY[as.character(prefix_int)])

RESP_ID_COL <- "ResponseId"

# ----------------------------
# Helper functions
# ----------------------------
normalize_device <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  out <- ifelse(str_detect(x_chr, "android"), "Android",
                ifelse(str_detect(x_chr, "iphone|ios"), "iOS", NA_character_))
  is_na <- is.na(out) | !nzchar(out)
  suppressWarnings({ x_num <- as.numeric(x_chr) })
  out[is_na & !is.na(x_num) & x_num == 1] <- "iOS"
  out[is_na & !is.na(x_num) & x_num == 2] <- "Android"
  out
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

calculate_android_target_date <- function(completion_timestamp, target_day_name) {
  if (is.na(completion_timestamp) || is.na(target_day_name)) return(NA_character_)
  completion_date <- tryCatch(as.Date(completion_timestamp), error = function(e) NA)
  if (is.na(completion_date)) return(NA_character_)
  day_to_num <- c(Monday=1,Tuesday=2,Wednesday=3,Thursday=4,Friday=5,Saturday=6,Sunday=7)
  target_day_num <- day_to_num[target_day_name]
  if (is.na(target_day_num)) return(NA_character_)
  completion_day_num <- as.integer(format(completion_date, "%u"))
  days_back <- (completion_day_num - target_day_num + 7) %% 7
  if (days_back == 0) days_back <- 7
  format(completion_date - days_back, "%Y-%m-%d")
}

mget_col_by_row <- function(df, col_names_vec) {
  df0 <- as.data.frame(df, stringsAsFactors = FALSE)
  stopifnot(length(col_names_vec) == nrow(df0))
  idx <- match(col_names_vec, names(df0))
  out <- rep(NA_character_, nrow(df0))
  ok  <- !is.na(idx)
  if (any(ok)) {
    rows <- which(ok)
    out[ok] <- as.character(df0[cbind(rows, idx[ok])])
  }
  out
}

pick_android_prefix <- function(df, wave_cfg) {
  prefixes  <- 1:7
  score_mat <- sapply(prefixes, function(p) {
    cols1 <- grep(sprintf(wave_cfg$android$prefix_regex, p), names(df), value = TRUE)
    cols2 <- character(0)
    if (!is.null(wave_cfg$android$twitter_regex))
      cols2 <- grep(sprintf(wave_cfg$android$twitter_regex, p), names(df), value = TRUE)
    cols <- unique(c(cols1, cols2))
    if (length(cols) == 0) return(rep(0L, nrow(df)))
    x <- as.data.frame(lapply(df[, cols, drop = FALSE], as.character), stringsAsFactors = FALSE)
    rowSums(!is.na(x) & nzchar(x))
  })
  if (is.null(dim(score_mat))) score_mat <- matrix(score_mat, nrow = nrow(df), byrow = TRUE)
  best_idx   <- max.col(score_mat, ties.method = "first")
  best_score <- score_mat[cbind(seq_len(nrow(df)), best_idx)]
  out <- prefixes[best_idx]
  out[best_score == 0] <- NA_integer_
  out
}

get_col_or_na  <- function(df, col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
ensure_cols    <- function(df, cols, fill = NA_character_) {
  for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- fill
  df
}

add_screenshot_paths <- function(df, manifest, id_col = "ResponseId") {
  file_id_cols <- grep("^ss[1-4]_file_id$", names(df), value = TRUE)
  if (length(file_id_cols) == 0) return(df)
  ss_long <- df %>%
    select(all_of(id_col), all_of(file_id_cols)) %>%
    pivot_longer(cols = all_of(file_id_cols), names_to = "ss_slot", values_to = "file_id") %>%
    mutate(file_id = as.character(file_id),
           ss_slot = str_replace(ss_slot, "_file_id$", "")) %>%
    filter(!is.na(file_id) & nzchar(file_id))
  if (nrow(ss_long) == 0) return(df)
  by_map  <- c(stats::setNames("response_id", id_col), "file_id" = "file_id")
  ss_long2 <- ss_long %>%
    left_join(manifest %>% select(response_id, file_id, saved_path, ok, http_status), by = by_map)
  ss_wide <- ss_long2 %>%
    select(all_of(id_col), ss_slot, saved_path) %>%
    pivot_wider(names_from = ss_slot, values_from = saved_path, values_fn = dplyr::first)
  names(ss_wide) <- sub("^(ss[1-4])$", "\\1_path", names(ss_wide))
  df %>% left_join(ss_wide, by = id_col)
}

# ----------------------------
# Process both waves
# ----------------------------
for (WAVE in c("endline", "baseline")) {
  message(sprintf("\n========== Wrangling %s ==========", toupper(WAVE)))

  OUT_DIR       <- file.path("data", "qualtrics", TEAM_SLUG, WAVE)
  RESPONSES_CSV <- file.path(OUT_DIR, "responses.csv")
  MANIFEST_CSV  <- file.path(OUT_DIR, "uploaded_files_manifest.csv")
  DERIVED_DIR   <- file.path(OUT_DIR, "derived")
  dir.create(DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)
  AVG_OUT_CSV   <- file.path(DERIVED_DIR, "average_screentime_for_annotation.csv")
  APP_OUT_CSV   <- file.path(DERIVED_DIR, "app_screentime_for_annotation.csv")

  if (!file.exists(RESPONSES_CSV))
    stop("Missing responses CSV: ", RESPONSES_CSV,
         "\nRun 01_download.R first.", call. = FALSE)
  if (!file.exists(MANIFEST_CSV))
    stop("Missing manifest CSV: ", MANIFEST_CSV,
         "\nRun 01_download.R first.", call. = FALSE)

  cfg        <- WAVE_CONFIG[[WAVE]]
  DEVICE_COL <- cfg$device_col

  responses <- read_csv(RESPONSES_CSV, show_col_types = FALSE)
  manifest  <- read_csv(MANIFEST_CSV, show_col_types = FALSE) %>%
    mutate(response_id = as.character(response_id),
           file_id     = as.character(file_id),
           saved_path  = as.character(saved_path))

  stopifnot(RESP_ID_COL %in% names(responses))
  stopifnot(DEVICE_COL  %in% names(responses))

  responses <- responses %>% mutate(device = normalize_device(.data[[DEVICE_COL]]))

  # Participant ID
  if (!is.na(PARTICIPANT_ID_COL) && PARTICIPANT_ID_COL %in% names(responses)) {
    responses$participant_id <- as.character(responses[[PARTICIPANT_ID_COL]])
    if (GENERATE_DUMMY_IDS && all(is.na(responses$participant_id) | responses$participant_id == "")) {
      message("Generating dummy participant IDs...")
      responses$participant_id <- sprintf("P%03d", seq_len(nrow(responses)))
    }
  } else {
    responses$participant_id <- NA_character_
    if (GENERATE_DUMMY_IDS) {
      message("Generating dummy participant IDs...")
      responses$participant_id <- sprintf("P%03d", seq_len(nrow(responses)))
    }
  }

  # iPhone
  iphone_df <- responses %>%
    filter(device == "iOS") %>%
    transmute(
      respondent_id         = as.character(.data[[RESP_ID_COL]]),
      participant_id,
      device,
      end_date              = get_col_or_na(., "EndDate"),
      screenshot_day_prefix = NA_integer_,
      screenshot_day        = NA_character_,
      android_target_date   = NA_character_,
      total_hours           = to_num(get_col_or_na(., cfg$iphone$total[1])),
      total_minutes         = to_num(get_col_or_na(., cfg$iphone$total[2])),
      instagram_hours       = to_num(get_col_or_na(., cfg$iphone$instagram[1])),
      instagram_minutes     = to_num(get_col_or_na(., cfg$iphone$instagram[2])),
      facebook_hours        = to_num(get_col_or_na(., cfg$iphone$facebook[1])),
      facebook_minutes      = to_num(get_col_or_na(., cfg$iphone$facebook[2])),
      tiktok_hours          = to_num(get_col_or_na(., cfg$iphone$tiktok[1])),
      tiktok_minutes        = to_num(get_col_or_na(., cfg$iphone$tiktok[2])),
      twitter_hours         = to_num(get_col_or_na(., cfg$iphone$twitter[1])),
      twitter_minutes       = to_num(get_col_or_na(., cfg$iphone$twitter[2])),
      ss1_file_id           = as.character(get_col_or_na(., cfg$iphone$ss[1])),
      ss2_file_id           = as.character(get_col_or_na(., cfg$iphone$ss[2])),
      ss3_file_id           = as.character(get_col_or_na(., cfg$iphone$ss[3])),
      ss4_file_id           = as.character(get_col_or_na(., cfg$iphone$ss[4]))
    )

  # Android
  android_raw    <- responses %>% filter(device == "Android")
  android_prefix <- pick_android_prefix(android_raw, cfg)

  android_df <- android_raw %>%
    mutate(android_prefix = android_prefix,
           end_date       = get_col_or_na(android_raw, "EndDate"),
           inferred_day   = prefix_to_day(android_prefix)) %>%
    transmute(
      respondent_id         = as.character(.data[[RESP_ID_COL]]),
      participant_id,
      device,
      end_date,
      screenshot_day_prefix = as.integer(android_prefix),
      screenshot_day        = inferred_day,
      android_target_date   = mapply(calculate_android_target_date, end_date, inferred_day,
                                     USE.NAMES = FALSE),
      total_hours           = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$total[1]))),
      total_minutes         = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$total[2]))),
      instagram_hours       = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$instagram[1]))),
      instagram_minutes     = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$instagram[2]))),
      facebook_hours        = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$facebook[1]))),
      facebook_minutes      = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$facebook[2]))),
      tiktok_hours          = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$tiktok[1]))),
      tiktok_minutes        = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$tiktok[2]))),
      twitter_hours         = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$twitter[1]))),
      twitter_minutes       = to_num(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$twitter[2]))),
      ss1_file_id           = as.character(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$ss[1]))),
      ss2_file_id           = as.character(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$ss[2]))),
      ss3_file_id           = as.character(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$ss[3]))),
      ss4_file_id           = as.character(mget_col_by_row(android_raw, paste0(android_prefix, cfg$android$ss[4])))
    )

  canonical <- bind_rows(iphone_df, android_df) %>%
    rename(ResponseId = respondent_id) %>%
    mutate(ResponseId = as.character(ResponseId))

  canonical <- ensure_cols(canonical, c(
    "participant_id","end_date","screenshot_day_prefix","screenshot_day","android_target_date",
    "ss1_file_id","ss2_file_id","ss3_file_id","ss4_file_id"
  ), fill = NA_character_)

  canonical2 <- add_screenshot_paths(canonical, manifest, id_col = "ResponseId")
  canonical2 <- ensure_cols(canonical2, c("ss1_path","ss2_path","ss3_path","ss4_path"),
                             fill = NA_character_)

  avg_out <- canonical2 %>%
    transmute(
      respondent_id            = ResponseId,
      participant_id, device, end_date,
      screenshot_day_prefix, screenshot_day, android_target_date,
      total_hours, total_minutes,
      total_screenshot_file_id = ss1_file_id,
      total_screenshot_path    = ss1_path
    )

  app_out <- canonical2 %>%
    transmute(
      respondent_id = ResponseId,
      participant_id, device, end_date,
      screenshot_day_prefix, screenshot_day, android_target_date,
      instagram_hours, instagram_minutes,
      facebook_hours,  facebook_minutes,
      tiktok_hours,    tiktok_minutes,
      twitter_hours,   twitter_minutes,
      app_screenshot1_file_id = ss2_file_id, app_screenshot1_path = ss2_path,
      app_screenshot2_file_id = ss3_file_id, app_screenshot2_path = ss3_path,
      app_screenshot3_file_id = ss4_file_id, app_screenshot3_path = ss4_path
    )

  write_csv(avg_out, AVG_OUT_CSV)
  write_csv(app_out, APP_OUT_CSV)

  message(sprintf("\u2705 %s wrangle complete", tools::toTitleCase(WAVE)))
  message(" - ", AVG_OUT_CSV)
  message(" - ", APP_OUT_CSV)
}

message("\n\u2705 All wrangling complete. Next: Rscript 03_run_app.R")
