library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)


# ----------------------------
# CONFIG (EDIT THESE)
# ----------------------------
TEAM_SLUG <- ""  # <-- your ISO2 country code (e.g. "GB", "US", "IN", "NL", "DK")

# Device filter — NA for all devices (set to "Android" or "iOS" to restrict to one device type if needed)
FILTER_DEVICE <- NA


# ----------------------------
# Paths (resolved relative to script location, not R working directory)
# ----------------------------
get_script_dir <- function() {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0) return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = FALSE)
}
SCRIPT_DIR <- get_script_dir()

BASE_DIR <- file.path(SCRIPT_DIR, "data", "qualtrics", TEAM_SLUG)

ENDLINE_AVG_IN   <- file.path(BASE_DIR, "endline",  "derived", "average_screentime_for_annotation.csv")
ENDLINE_APP_IN   <- file.path(BASE_DIR, "endline",  "derived", "app_screentime_for_annotation.csv")
BASELINE_AVG_IN  <- file.path(BASE_DIR, "baseline", "derived", "average_screentime_for_annotation.csv")
BASELINE_APP_IN  <- file.path(BASE_DIR, "baseline", "derived", "app_screentime_for_annotation.csv")

ENDLINE_RESULTS  <- file.path(BASE_DIR, "endline",  "results")
BASELINE_RESULTS <- file.path(BASE_DIR, "baseline", "results")

# Phase order: baseline first, then endline (baseline filtered to endline completers)
PHASE_ORDER <- c("baseline_avg", "baseline_app", "endline_avg", "endline_app", "done")

SAMPLE_PATHS <- list(
  endline_avg  = file.path(ENDLINE_RESULTS,  "sample_avg.csv"),
  endline_app  = file.path(ENDLINE_RESULTS,  "sample_app.csv"),
  baseline_avg = file.path(BASELINE_RESULTS, "sample_avg.csv"),
  baseline_app = file.path(BASELINE_RESULTS, "sample_app.csv")
)

ANN_PATHS <- list(
  endline_avg  = file.path(ENDLINE_RESULTS,  "annotations_avg.csv"),
  endline_app  = file.path(ENDLINE_RESULTS,  "annotations_app.csv"),
  baseline_avg = file.path(BASELINE_RESULTS, "annotations_avg.csv"),
  baseline_app = file.path(BASELINE_RESULTS, "annotations_app.csv")
)

AUDIT_PATHS <- list(
  endline_avg  = file.path(ENDLINE_RESULTS,  "audit_log_avg.csv"),
  endline_app  = file.path(ENDLINE_RESULTS,  "audit_log_app.csv"),
  baseline_avg = file.path(BASELINE_RESULTS, "audit_log_avg.csv"),
  baseline_app = file.path(BASELINE_RESULTS, "audit_log_app.csv")
)

phase_label <- function(ph) switch(ph,
  endline_avg  = "Endline \u2014 Average screenshots",
  endline_app  = "Endline \u2014 App-level screenshots",
  baseline_avg = "Baseline \u2014 Average screenshots",
  baseline_app = "Baseline \u2014 App-level screenshots",
  "Done"
)
phase_type <- function(ph) if (endsWith(ph, "avg")) "avg" else "app"

# ----------------------------
# Helpers
# ----------------------------
safe_read <- function(path) readr::read_csv(path, show_col_types = FALSE)

file_ok_vec <- function(p) {
  p1 <- vapply(p, function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- as.character(x)
    if (length(x) == 0) return(NA_character_)
    if (length(x) > 1) x <- x[[1]]
    x
  }, character(1))
  ok <- !is.na(p1) & nzchar(p1)
  ok[ok] <- file.exists(p1[ok])
  ok
}

file_ok1 <- function(p) {
  if (is.null(p)) return(FALSE)
  if (is.list(p)) p <- unlist(p, use.names = FALSE)
  p <- as.character(p)
  if (length(p) == 0) return(FALSE)
  if (length(p) > 1) p <- p[[1]]
  !is.na(p) && nzchar(p) && file.exists(p)
}

guess_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("jpg","jpeg")) return("image/jpeg")
  if (ext == "png")  return("image/png")
  if (ext == "webp") return("image/webp")
  "application/octet-stream"
}

ensure_cols2 <- function(df, cols, fill = NA) {
  for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- fill
  df
}

make_task_id <- function(prefix, respondent_id, i) {
  sprintf("%s_%s_%04d", prefix, as.character(respondent_id), i)
}

standardize_annotations <- function(df) {
  df <- ensure_cols2(df,
    c("task_id","respondent_id","reviewer","screenshot_correct","numbers_match","notes","annotated_at"),
    fill = NA)
  df %>% mutate(across(everything(), as.character))
}

load_annotations <- function(path) {
  if (!file.exists(path)) return(standardize_annotations(tibble()))
  df <- tryCatch(safe_read(path), error = function(e) tibble())
  standardize_annotations(df)
}

save_annotations <- function(path, df) write_csv(standardize_annotations(df), path)

append_audit <- function(path, rec) {
  rec <- standardize_annotations(rec)
  if (file.exists(path)) {
    write_csv(rec, path, append = TRUE, col_names = FALSE)
  } else {
    write_csv(rec, path, append = FALSE, col_names = TRUE)
  }
}

first_unannotated_idx <- function(tasks_df, anns_df) {
  done_ids <- anns_df %>%
    filter(!is.na(task_id) & nzchar(task_id)) %>%
    pull(task_id) %>% unique()
  idx <- which(!tasks_df$task_id %in% done_ids)
  if (length(idx) == 0L) nrow(tasks_df) else idx[[1L]]
}

build_or_load_sample <- function(type = c("avg", "app"), df, sample_path) {
  type <- match.arg(type)
  if (file.exists(sample_path)) return(safe_read(sample_path))

  if (type == "avg") {
    df <- ensure_cols2(df, c("respondent_id","total_screenshot_path","total_hours","total_minutes",
                              "device","screenshot_day"))
    df <- df %>%
      mutate(total_screenshot_path = as.character(total_screenshot_path)) %>%
      filter(file_ok_vec(total_screenshot_path))
    if (nrow(df) == 0) stop("No usable avg screenshots found on disk.", call. = FALSE)
    samp <- df %>%
      mutate(task_id = make_task_id("avg", respondent_id, row_number()))
    write_csv(samp, sample_path)
    return(samp)
  }

  df <- ensure_cols2(df, c(
    "respondent_id","device","screenshot_day",
    "instagram_hours","instagram_minutes","facebook_hours","facebook_minutes",
    "tiktok_hours","tiktok_minutes","twitter_hours","twitter_minutes",
    "app_screenshot1_path","app_screenshot2_path","app_screenshot3_path"
  ))
  df <- df %>%
    mutate(across(starts_with("app_screenshot"), as.character)) %>%
    filter(file_ok_vec(app_screenshot1_path) | file_ok_vec(app_screenshot2_path) |
             file_ok_vec(app_screenshot3_path))
  if (nrow(df) == 0) stop("No usable app screenshots found on disk.", call. = FALSE)
  samp <- df %>%
    mutate(task_id = make_task_id("app", respondent_id, row_number()))
  write_csv(samp, sample_path)
  samp
}

fmt_day  <- function(device, day) {
  if (is.na(device) || !nzchar(device) || !grepl("android", tolower(device))) return(NA_character_)
  if (is.na(day)    || !nzchar(day))    return(NA_character_)
  day
}
fmt_date <- function(device, date) {
  if (is.na(device) || !nzchar(device) || !grepl("android", tolower(device))) return(NA_character_)
  if (is.na(date)   || !nzchar(date))   return(NA_character_)
  date
}
fmt_hm <- function(h, m) {
  paste0(ifelse(is.na(h), "", as.character(h)), "h ",
         ifelse(is.na(m), "", as.character(m)), "m")
}

# ----------------------------
# Check inputs
# ----------------------------
if (is.na(TEAM_SLUG) || !nzchar(trimws(TEAM_SLUG))) {
  stop("TEAM_SLUG is blank. Set it to your ISO2 country code (e.g. \"GB\", \"US\") in the CONFIG section.",
       call. = FALSE)
}

dir.create(ENDLINE_RESULTS,  recursive = TRUE, showWarnings = FALSE)
dir.create(BASELINE_RESULTS, recursive = TRUE, showWarnings = FALSE)

for (p in c(ENDLINE_AVG_IN, ENDLINE_APP_IN, BASELINE_AVG_IN, BASELINE_APP_IN)) {
  if (!file.exists(p))
    stop("Missing input file: ", p,
         "\nThe data package may not have been unzipped correctly.",
         "\nMake sure the data/ folder sits in the same directory as this script.",
         "\nIf the problem persists, contact Chris (cb5691@nyu.edu).",
         call. = FALSE)
}

# ----------------------------
# Load data and build samples
# ----------------------------
endline_avg_raw  <- safe_read(ENDLINE_AVG_IN)
endline_app_raw  <- safe_read(ENDLINE_APP_IN)

# Optional device filter (set FILTER_DEVICE in CONFIG above)
if (!is.na(FILTER_DEVICE)) {
  endline_avg_raw <- endline_avg_raw %>% filter(tolower(device) == tolower(FILTER_DEVICE))
  endline_app_raw <- endline_app_raw %>% filter(tolower(device) == tolower(FILTER_DEVICE))
}

# Filter baseline to endline completers
endline_ids <- endline_avg_raw %>%
  filter(!is.na(participant_id) & nzchar(as.character(participant_id))) %>%
  pull(participant_id) %>% as.character() %>% unique()

baseline_avg_raw <- safe_read(BASELINE_AVG_IN) %>%
  filter(as.character(participant_id) %in% endline_ids)
baseline_app_raw <- safe_read(BASELINE_APP_IN) %>%
  filter(as.character(participant_id) %in% endline_ids)

if (nrow(baseline_avg_raw) == 0)
  stop("No baseline respondents matched any endline participant_id.\n",
       "The data package may be incomplete or mismatched. Contact Chris (cb5691@nyu.edu).", call. = FALSE)

message(sprintf("Endline: %d avg, %d app respondents", nrow(endline_avg_raw), nrow(endline_app_raw)))
message(sprintf("Baseline (filtered to endline completers): %d avg, %d app respondents",
                nrow(baseline_avg_raw), nrow(baseline_app_raw)))

all_tasks <- list(
  endline_avg  = build_or_load_sample("avg", endline_avg_raw,  SAMPLE_PATHS$endline_avg),
  endline_app  = build_or_load_sample("app", endline_app_raw,  SAMPLE_PATHS$endline_app),
  baseline_avg = build_or_load_sample("avg", baseline_avg_raw, SAMPLE_PATHS$baseline_avg),
  baseline_app = build_or_load_sample("app", baseline_app_raw, SAMPLE_PATHS$baseline_app)
)

# Pre-load annotations to compute initial indices (resume from where left off)
init_anns <- lapply(ANN_PATHS, load_annotations)

# Determine initial phase: first phase that still has unannotated tasks
init_phase <- local({
  ph <- "done"
  for (p in PHASE_ORDER[PHASE_ORDER != "done"]) {
    done_ids <- init_anns[[p]] %>%
      filter(!is.na(task_id) & nzchar(task_id)) %>%
      pull(task_id) %>% unique()
    if (any(!all_tasks[[p]]$task_id %in% done_ids)) { ph <- p; break }
  }
  ph
})

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; }
      .muted { color: #666; }
      .panel { padding: 12px; border: 1px solid #ddd; border-radius: 12px; background: #fff; }
      .big { font-size: 18px; }
      .tight-table td, .tight-table th { padding: 6px !important; }
      .phase-pill { display: inline-block; padding: 4px 10px; border-radius: 999px;
                    background: #f2f2f2; font-weight: 600; }
      .screenshot-wrap { max-height: 640px; overflow-y: scroll; border: 1px solid #eee;
                         border-radius: 12px; padding: 8px; background: #fafafa; }
      .screenshot-wrap img { display: block; margin: 0 auto; cursor: zoom-in;
                             height: 600px; width: auto; }
      .screenshot-wrap::-webkit-scrollbar { width: 12px; }
      .screenshot-wrap::-webkit-scrollbar-track { background: #e0e0e0; border-radius: 6px; }
      .screenshot-wrap::-webkit-scrollbar-thumb { background: #999; border-radius: 6px; }
      .screenshot-wrap::-webkit-scrollbar-thumb:hover { background: #666; }
      .progress-bar-wrap { height: 10px; border-radius: 999px; background: #eee; overflow: hidden; }
      .progress-bar-fill { height: 10px; background: #111; width: 0%; }
      .meta-grid { display: grid; grid-template-columns: 110px 1fr; gap: 6px 10px; }
      .meta-k { font-weight: 600; color: #444; }
      .meta-v { color: #111; }
      .big-radio .radio { margin: 6px 0; }
      .big-radio .radio label {
        display: flex; align-items: center;
        padding: 10px 14px; font-size: 17px; font-weight: 600;
        border: 2px solid #ccc; border-radius: 8px; cursor: pointer;
        background: #fafafa; transition: background 0.1s, border-color 0.1s;
      }
      .big-radio .radio label:hover { background: #f0f0f0; border-color: #999; }
      .big-radio input[type='radio'] { width: 18px; height: 18px; margin-right: 10px; cursor: pointer; }
      .nav-btn { font-size: 18px !important; padding: 10px 0 !important;
                 font-weight: 700 !important; width: 100% !important; }
      .prior-ann-badge { background: #e8f5e9; border: 1px solid #a5d6a7; border-radius: 6px;
                         padding: 6px 10px; font-size: 13px; color: #2e7d32; margin-bottom: 8px; }
      .info-icon { cursor: pointer; color: #0055aa; font-size: 15px; margin-left: 6px;
                   font-weight: bold; font-style: normal; text-decoration: none;
                   background: #ddeeff; border: 1px solid #99bbdd; border-radius: 4px;
                   padding: 1px 5px; }
      .kbd-legend { font-size: 11px; color: #888; line-height: 1.8; }
    ")),
    tags$script(HTML("
      // Info icon click
      document.addEventListener('click', function(e) {
        var el = e.target;
        if (el && el.classList && el.classList.contains('info-icon')) {
          Shiny.setInputValue('info_clicked', el.getAttribute('data-info'), {priority:'event'});
        }
      });

      // Fullscreen image click
      document.addEventListener('click', function(e) {
        var img = e.target;
        if (img && img.tagName && img.tagName.toLowerCase() === 'img') {
          var parent = img.closest('.screenshot-wrap');
          if (parent) Shiny.setInputValue('img_src_clicked', img.getAttribute('src'), {priority:'event'});
        }
      });

      // Keyboard shortcuts
      document.addEventListener('keydown', function(e) {
        if (['INPUT','TEXTAREA','SELECT'].includes(e.target.tagName)) return;
        if (e.key === '1') setRadioVal('screenshot_correct', 'Yes');
        if (e.key === '2') setRadioVal('screenshot_correct', 'No');
        if (e.key === '3') setRadioVal('numbers_match', 'Yes');
        if (e.key === '4') setRadioVal('numbers_match', 'No');
        if (e.key === 'ArrowRight' || e.key === 'Enter') {
          var btn = document.getElementById('next_btn');
          if (btn) btn.click();
        }
        if (e.key === 'ArrowLeft') {
          var btn = document.getElementById('prev_btn');
          if (btn) btn.click();
        }
      });
      function setRadioVal(name, val) {
        var radios = document.querySelectorAll('input[name=\"' + name + '\"]');
        radios.forEach(function(r) {
          r.checked = (r.value === val);
          if (r.checked) r.dispatchEvent(new Event('change', {bubbles: true}));
        });
        Shiny.setInputValue(name, val, {priority: 'event'});
      }
    "))
  ),

  titlePanel(paste0("Manual Screenshot Annotation \u2014 ", TEAM_SLUG)),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "panel",
        textInput("reviewer", "Reviewer name", ""),
        hr(),
        uiOutput("phase_label"),
        hr(),
        uiOutput("progress"),
        br(),
        uiOutput("progress_bar"),
        br(),
        actionButton("jump_btn", "Jump to next unannotated \u2192",
                     class = "btn-default", style = "width:100%; font-size:13px;"),
        hr(),
        div(class = "kbd-legend",
          tags$b("Keyboard shortcuts:"), br(),
          "1 / 2 \u2014 Yes / No (screenshot)", br(),
          "3 / 4 \u2014 Yes / No (numbers)", br(),
          "\u2192 or Enter \u2014 Next", br(),
          "\u2190 \u2014 Prev"
        )
      )
    ),

    mainPanel(
      width = 9,
      fluidRow(
        column(7, uiOutput("images_panel")),
        column(5,
          div(class = "panel",
            uiOutput("numbers_panel"),
            hr(),
            uiOutput("prior_ann_notice"),
            div(class = "big-radio",
              radioButtons("screenshot_correct",
                HTML('Correct screenshot? <span class="info-icon" data-info="screenshot_correct">\u24d8</span>'),
                choices = c("Yes", "No"), selected = character(0), inline = FALSE),
              radioButtons("numbers_match",
                HTML('Numbers match? <span class="info-icon" data-info="numbers_match">\u24d8</span>'),
                choices = c("Yes", "No"), selected = character(0), inline = FALSE)
            ),
            textAreaInput("notes", "Notes (optional)", value = "", rows = 2),
            hr(),
            fluidRow(
              column(6, actionButton("prev_btn", "\u2190 Prev", class = "nav-btn btn-default")),
              column(6, actionButton("next_btn", "Next \u2192", class = "nav-btn btn-primary"))
            ),
            div(class = "muted", style = "margin-top:8px;", "Click screenshot to open fullscreen.")
          )
        )
      ),
      br(),
      uiOutput("done_panel")
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {

  state <- reactiveValues(
    phase          = init_phase,
    i_endline_avg  = first_unannotated_idx(all_tasks$endline_avg,  init_anns$endline_avg),
    i_endline_app  = first_unannotated_idx(all_tasks$endline_app,  init_anns$endline_app),
    i_baseline_avg = first_unannotated_idx(all_tasks$baseline_avg, init_anns$baseline_avg),
    i_baseline_app = first_unannotated_idx(all_tasks$baseline_app, init_anns$baseline_app)
  )

  anns <- reactiveValues(
    endline_avg  = init_anns$endline_avg,
    endline_app  = init_anns$endline_app,
    baseline_avg = init_anns$baseline_avg,
    baseline_app = init_anns$baseline_app
  )

  prior_ann_notice <- reactiveVal(NULL)

  get_idx <- function() {
    ph <- state$phase
    if (ph == "done") return(1L)
    state[[paste0("i_", ph)]]
  }

  set_idx <- function(v) {
    ph <- state$phase
    if (ph == "done") return()
    n <- nrow(all_tasks[[ph]])
    state[[paste0("i_", ph)]] <- max(1L, min(as.integer(v), n))
  }

  tasks   <- reactive({ if (state$phase == "done") tibble() else all_tasks[[state$phase]] })
  current <- reactive({
    df <- tasks(); if (nrow(df) == 0) return(NULL)
    i  <- max(1L, min(get_idx(), nrow(df)))
    df[i, , drop = FALSE]
  })

  advance_phase_if_needed <- function() {
    ph <- state$phase
    if (ph == "done") return()
    if (state[[paste0("i_", ph)]] > nrow(all_tasks[[ph]])) {
      state[[paste0("i_", ph)]] <- nrow(all_tasks[[ph]])
      next_ph <- PHASE_ORDER[[which(PHASE_ORDER == ph) + 1L]]
      state$phase <- next_ph
    }
  }

  # Prefill saved annotation when task changes
  observeEvent(
    list(state$phase, state$i_endline_avg, state$i_endline_app,
         state$i_baseline_avg, state$i_baseline_app),
    {
      ph <- state$phase
      if (ph == "done") { prior_ann_notice(NULL); return() }
      r <- current(); if (is.null(r)) return()
      cur <- standardize_annotations(anns[[ph]])
      hit <- cur %>% filter(task_id == as.character(r$task_id[[1]]))
      if (nrow(hit) >= 1) {
        updateRadioButtons(session, "screenshot_correct", selected = hit$screenshot_correct[[1]])
        updateRadioButtons(session, "numbers_match",      selected = hit$numbers_match[[1]])
        updateTextAreaInput(session, "notes", value = ifelse(is.na(hit$notes[[1]]), "", hit$notes[[1]]))
        if (!is.na(hit$reviewer[[1]]) && nzchar(hit$reviewer[[1]]))
          updateTextInput(session, "reviewer", value = hit$reviewer[[1]])
        ts <- tryCatch(
          format(as.POSIXct(hit$annotated_at[[1]]), "%Y-%m-%d %H:%M"),
          error = function(e) as.character(hit$annotated_at[[1]])
        )
        reviewer_txt <- if (!is.na(hit$reviewer[[1]]) && nzchar(hit$reviewer[[1]])) hit$reviewer[[1]] else "unknown"
        prior_ann_notice(paste0("\u2713 Annotated by ", reviewer_txt,
                                if (!is.na(ts) && nzchar(ts)) paste0(" on ", ts) else ""))
      } else {
        updateRadioButtons(session, "screenshot_correct", selected = character(0))
        updateRadioButtons(session, "numbers_match",      selected = character(0))
        updateTextAreaInput(session, "notes", value = "")
        prior_ann_notice(NULL)
      }
    },
    ignoreInit = TRUE
  )

  output$prior_ann_notice <- renderUI({
    msg <- prior_ann_notice()
    if (is.null(msg)) return(NULL)
    div(class = "prior-ann-badge", msg)
  })

  # Info icon modals
  observeEvent(input$info_clicked, {
    ph <- isolate(state$phase)
    is_avg <- phase_type(ph) == "avg"
    if (input$info_clicked == "screenshot_correct") {
      showModal(modalDialog(
        title = "What should the screenshot show?",
        easyClose = TRUE, footer = modalButton("Got it"),
        if (is_avg) tags$div(
          tags$p(tags$b("iOS:")),
          tags$p("Settings \u2192 Screen Time \u2192 See All Activity \u2192 select the", tags$b("Week"), "tab \u2192 swipe right to reach", tags$b("\u201cLast Week\u2019s Average\u201d.")),
          tags$p("The screenshot should show the weekly average bar chart with the S\u2013M\u2013T\u2013W\u2013T\u2013F\u2013S calendar bar at the top."),
          tags$hr(),
          tags$p(tags$b("Android:")),
          tags$p("Settings \u2192 Digital Wellbeing \u2192 tap today\u2019s total \u2192 navigate back to the", tags$b("target day shown above.")),
          tags$p("The screenshot should show the daily Digital Wellbeing screen with the", tags$b("correct date clearly visible"), "(e.g. \u201cTue, Dec. 19\u201d).")
        ) else tags$div(
          tags$p(tags$b("iOS:")),
          tags$p("The same Screen Time weekly view, scrolled down to show all apps used for", tags$b("35 minutes or more last week."), "Apps are in descending order. The respondent may have uploaded", tags$b("multiple screenshots"), "if many apps met this threshold."),
          tags$hr(),
          tags$p(tags$b("Android:")),
          tags$p("The Digital Wellbeing daily view for the target day, scrolled down to show all apps used for", tags$b("5 minutes or more on that day."), "The respondent may have uploaded", tags$b("multiple screenshots"), "if many apps met this threshold.")
        )
      ))
    } else {
      showModal(modalDialog(
        title = "Do the numbers match?",
        easyClose = TRUE, footer = modalButton("Got it"),
        if (is_avg) tags$div(
          tags$p("Check that the total screen time", tags$b("hours and minutes"), "visible in the screenshot match the", tags$b("reported total"), "shown on this screen.")
        ) else tags$div(
          tags$p("Check that the per-app usage times in the screenshot(s) match the values in the", tags$b("reported app screen time table"), "on this screen."),
          tags$p("Check Instagram, Facebook, TikTok, and X", tags$b("individually.")),
          tags$p(tags$em("It is fine if the screenshot contains additional apps not listed in the table.")),
          tags$p(tags$em("It is also fine if Instagram, Facebook, TikTok, or X do not appear in the screenshot, if they were used for less than the specified cutoff time (35 min/week for iOS; 5 min/day for Android)."))
        )
      ))
    }
  }, ignoreInit = TRUE)

  do_save <- function() {
    ph <- state$phase
    if (ph == "done") return()
    r <- current(); if (is.null(r)) return()
    rec <- tibble(
      task_id            = as.character(r$task_id[[1]]),
      respondent_id      = as.character(r$respondent_id[[1]]),
      reviewer           = as.character(input$reviewer),
      screenshot_correct = if (length(input$screenshot_correct) > 0) as.character(input$screenshot_correct) else NA_character_,
      numbers_match      = if (length(input$numbers_match) > 0) as.character(input$numbers_match) else NA_character_,
      notes              = as.character(input$notes),
      annotated_at       = as.character(Sys.time())
    ) %>% standardize_annotations()
    # Update main annotations (latest per task)
    cur  <- standardize_annotations(anns[[ph]])
    cur2 <- cur %>%
      filter(is.na(task_id) | task_id != rec$task_id[[1]]) %>%
      bind_rows(rec) %>% standardize_annotations()
    anns[[ph]] <- cur2
    save_annotations(ANN_PATHS[[ph]], cur2)
    # Append to audit log (never overwrites — full history)
    append_audit(AUDIT_PATHS[[ph]], rec)
  }

  observeEvent(input$next_btn, {
    if (!nzchar(trimws(input$reviewer))) {
      showModal(modalDialog(
        title = NULL, easyClose = TRUE, footer = modalButton("OK"),
        tags$div(
          style = "text-align:center; padding: 10px 0;",
          tags$div(style = "font-size:40px; margin-bottom:12px;", "\u270d\ufe0f"),
          tags$div(style = "font-size:20px; font-weight:700; margin-bottom:8px;",
                   "Reviewer name required"),
          tags$div(style = "font-size:15px; color:#555;",
                   "Please enter your name in the Reviewer name field before continuing.")
        )
      ))
      return()
    }
    if (length(input$screenshot_correct) == 0 || length(input$numbers_match) == 0) {
      showModal(modalDialog(
        title = NULL, easyClose = TRUE, footer = modalButton("OK"),
        tags$div(
          style = "text-align:center; padding: 10px 0;",
          tags$div(style = "font-size:40px; margin-bottom:12px;", "\u26a0\ufe0f"),
          tags$div(style = "font-size:20px; font-weight:700; margin-bottom:8px;",
                   "Both questions required"),
          tags$div(style = "font-size:15px; color:#555;",
                   "Please select Yes or No for both questions before continuing.")
        )
      ))
      return()
    }
    do_save()
    ph <- state$phase; if (ph == "done") return()
    state[[paste0("i_", ph)]] <- state[[paste0("i_", ph)]] + 1L
    advance_phase_if_needed()
  })

  observeEvent(input$prev_btn, {
    ph <- state$phase; if (ph == "done") return()
    do_save()
    key <- paste0("i_", ph)
    state[[key]] <- max(1L, state[[key]] - 1L)
  })

  observeEvent(input$jump_btn, {
    ph <- state$phase; if (ph == "done") return()
    idx <- first_unannotated_idx(all_tasks[[ph]], anns[[ph]])
    state[[paste0("i_", ph)]] <- idx
  })

  # Fullscreen modal
  observeEvent(input$img_src_clicked, {
    src <- as.character(input$img_src_clicked)
    if (is.na(src) || !nzchar(src)) return()
    showModal(modalDialog(
      title = "Fullscreen screenshot", size = "l", easyClose = TRUE,
      footer = tagList(modalButton("Close")),
      sliderInput("modal_zoom", "Zoom", min = 0.5, max = 4, value = 1, step = 0.1),
      uiOutput("modal_image_ui")
    ))
  }, ignoreInit = TRUE)

  output$modal_image_ui <- renderUI({
    req(input$modal_zoom)
    src <- as.character(input$img_src_clicked)
    if (is.na(src) || !nzchar(src)) return(tags$div("No image."))
    tags$div(
      style = "max-height:75vh;overflow:auto;border:1px solid #eee;border-radius:12px;padding:8px;background:#fafafa;",
      tags$img(src = src, style = paste0("transform:scale(", input$modal_zoom,
                                          ");transform-origin:top left;display:block;max-width:none;"))
    )
  })

  # --- UI outputs ---
  output$phase_label <- renderUI({
    tags$div(class = "phase-pill", phase_label(state$phase))
  })

  output$progress <- renderUI({
    ph <- state$phase
    if (ph == "done") {
      return(tags$div(tags$div(class="big", "All tasks complete \u2705"),
                      tags$div(class="muted", "Run 04_bundle_results.R to package your results.")))
    }
    df   <- tasks()
    done <- 0L
    if (nrow(anns[[ph]]) > 0) {
      done <- anns[[ph]] %>%
        filter(!is.na(task_id) & nzchar(task_id)) %>%
        summarise(n = dplyr::n_distinct(task_id)) %>% pull(n)
      done <- ifelse(length(done) == 0 || is.na(done), 0L, as.integer(done))
    }
    tags$div(
      tags$div(class="big", paste0("Task ", get_idx(), " / ", nrow(df))),
      tags$div(class="muted", paste0("Completed: ", done, " / ", nrow(df)))
    )
  })

  output$progress_bar <- renderUI({
    ph <- state$phase
    if (ph == "done") return(NULL)
    df   <- tasks(); if (nrow(df) == 0) return(NULL)
    done <- 0L
    if (nrow(anns[[ph]]) > 0) {
      done <- anns[[ph]] %>%
        filter(!is.na(task_id) & nzchar(task_id)) %>%
        summarise(n = dplyr::n_distinct(task_id)) %>% pull(n)
      done <- ifelse(length(done) == 0 || is.na(done), 0L, as.integer(done))
    }
    pct <- round(100 * done / nrow(df), 1)
    tags$div(class="panel",
      tags$div(class="muted", paste0("Completion: ", pct, "%")),
      tags$div(class="progress-bar-wrap",
               tags$div(class="progress-bar-fill", style=paste0("width:",pct,"%;")))
    )
  })

  output$numbers_panel <- renderUI({
    ph <- state$phase
    if (ph == "done") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    device <- if ("device"              %in% names(r)) as.character(r$device[[1]])              else NA_character_
    day    <- if ("screenshot_day"      %in% names(r)) as.character(r$screenshot_day[[1]])      else NA_character_
    date   <- if ("android_target_date" %in% names(r)) as.character(r$android_target_date[[1]]) else NA_character_
    day2   <- fmt_day(device, day)
    date2  <- fmt_date(device, date)
    meta   <- list(
      tags$div(class="meta-k","Respondent"), tags$div(class="meta-v", as.character(r$respondent_id[[1]])),
      tags$div(class="meta-k","Device"),     tags$div(class="meta-v", ifelse(is.na(device)|!nzchar(device),"NA",device))
    )
    if (phase_type(ph) == "app") {
      if (!is.na(day2)  && nzchar(day2))
        meta <- c(meta, list(tags$div(class="meta-k","Day (Android)"),  tags$div(class="meta-v",day2)))
      if (!is.na(date2) && nzchar(date2))
        meta <- c(meta, list(tags$div(class="meta-k","Date (Android)"), tags$div(class="meta-v",date2)))
    }

    if (phase_type(ph) == "avg") {
      h <- if ("total_hours"   %in% names(r)) r$total_hours[[1]]   else NA
      m <- if ("total_minutes" %in% names(r)) r$total_minutes[[1]] else NA
      is_android <- !is.na(device) && grepl("android", tolower(device))
      android_instruction <- if (is_android && !is.na(date2) && !is.na(day2)) {
        tags$div(
          tags$div(class="muted", style="font-size:12px; margin-bottom:2px;", "Screenshot should be for"),
          tags$div(style="font-size:32px; font-weight:700; line-height:1.1; margin-bottom:10px;",
                   sprintf("%s (%s)", day2, date2))
        )
      }
      tags$div(
        if (is_android) android_instruction,
        tags$div(class="muted", style="font-size:12px; margin-bottom:2px;", "Reported total screen time"),
        tags$div(style="font-size:32px; font-weight:700; line-height:1.1; margin-bottom:10px;", fmt_hm(h,m)),
        tags$div(class="meta-grid", meta),
        if (is_android) tags$div(
          style = "margin-top:10px; font-size:12px; color:#888; border-top:1px solid #eee; padding-top:8px;",
          tags$em("Note: for some Android users the UI will show an overall weekly average. This is permissible even if the specific date is not visible.")
        )
      )
    } else {
      getv <- function(nm) if (nm %in% names(r)) r[[nm]][[1]] else NA
      tags$div(
        tags$div(class="muted", style="font-size:12px; margin-bottom:6px;", "Reported app screen time"),
        tags$table(class="table table-striped tight-table",
          tags$thead(tags$tr(tags$th("App"),tags$th("Hours"),tags$th("Minutes"))),
          tags$tbody(
            tags$tr(tags$td("Instagram"),tags$td(getv("instagram_hours")),tags$td(getv("instagram_minutes"))),
            tags$tr(tags$td("Facebook"), tags$td(getv("facebook_hours")), tags$td(getv("facebook_minutes"))),
            tags$tr(tags$td("TikTok"),   tags$td(getv("tiktok_hours")),   tags$td(getv("tiktok_minutes"))),
            tags$tr(tags$td("X"),        tags$td(getv("twitter_hours")),  tags$td(getv("twitter_minutes")))
          )
        ),
        tags$div(class="meta-grid", meta)
      )
    }
  })

  output$img_avg <- renderImage({
    req(phase_type(state$phase) == "avg")
    r <- current(); req(!is.null(r))
    p <- if ("total_screenshot_path" %in% names(r)) as.character(r$total_screenshot_path[[1]]) else NA_character_
    req(file_ok1(p))
    list(src = p, contentType = guess_content_type(p), alt = "screenshot")
  }, deleteFile = FALSE)

  output$img_app1 <- renderImage({
    req(phase_type(state$phase) == "app")
    r <- current(); req(!is.null(r))
    p <- if ("app_screenshot1_path" %in% names(r)) as.character(r$app_screenshot1_path[[1]]) else NA_character_
    req(file_ok1(p))
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 1")
  }, deleteFile = FALSE)

  output$img_app2 <- renderImage({
    req(phase_type(state$phase) == "app")
    r <- current(); req(!is.null(r))
    p <- if ("app_screenshot2_path" %in% names(r)) as.character(r$app_screenshot2_path[[1]]) else NA_character_
    req(file_ok1(p))
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 2")
  }, deleteFile = FALSE)

  output$img_app3 <- renderImage({
    req(phase_type(state$phase) == "app")
    r <- current(); req(!is.null(r))
    p <- if ("app_screenshot3_path" %in% names(r)) as.character(r$app_screenshot3_path[[1]]) else NA_character_
    req(file_ok1(p))
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 3")
  }, deleteFile = FALSE)

  output$images_panel <- renderUI({
    ph <- state$phase
    if (ph == "done") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    if (phase_type(ph) == "avg") {
      p_avg <- if ("total_screenshot_path" %in% names(r)) as.character(r$total_screenshot_path[[1]]) else NA_character_
      tags$div(class="panel",
               if (file_ok1(p_avg))
                 tags$div(class="screenshot-wrap", imageOutput("img_avg"))
               else
                 tags$p(style="color:#c00; padding:20px;",
                        "Screenshot file not found on disk. Note this in the Notes field, select No for both questions, and continue.")
      )
    } else {
      p1 <- if ("app_screenshot1_path" %in% names(r)) as.character(r$app_screenshot1_path[[1]]) else NA_character_
      p2 <- if ("app_screenshot2_path" %in% names(r)) as.character(r$app_screenshot2_path[[1]]) else NA_character_
      p3 <- if ("app_screenshot3_path" %in% names(r)) as.character(r$app_screenshot3_path[[1]]) else NA_character_
      tabs <- list()
      if (file_ok1(p1)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 1", tags$div(class="screenshot-wrap", imageOutput("img_app1")))
      if (file_ok1(p2)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 2", tags$div(class="screenshot-wrap", imageOutput("img_app2")))
      if (file_ok1(p3)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 3", tags$div(class="screenshot-wrap", imageOutput("img_app3")))
      tags$div(class="panel",
               if (length(tabs) > 0) do.call(tabsetPanel, c(id="app_tabs", tabs))
               else tags$p("No image files found for this task."))
    }
  })

  output$done_panel <- renderUI({
    if (state$phase != "done") return(NULL)
    tags$div(class="panel",
      tags$h3("All annotation tasks complete \u2705"),
      tags$p("Your files have been saved automatically:"),
      tags$ul(lapply(unlist(ANN_PATHS), function(p) tags$li(tags$code(p)))),
      tags$p(class="muted", "Next step: run 04_bundle_results.R and submit the ZIPs.")
    )
  })
}

message("Launching annotation app\u2026")
shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)
