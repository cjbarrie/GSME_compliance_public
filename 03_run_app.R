library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)


# ----------------------------
# CONFIG (EDIT THESE)
# ----------------------------
TEAM_SLUG <- "XX"  # <-- your ISO2 country code (e.g. "GB", "US")


# ----------------------------
# Paths
# ----------------------------
BASE_DIR <- file.path("data", "qualtrics", TEAM_SLUG)

ENDLINE_AVG_IN   <- file.path(BASE_DIR, "endline",  "derived", "average_screentime_for_annotation.csv")
ENDLINE_APP_IN   <- file.path(BASE_DIR, "endline",  "derived", "app_screentime_for_annotation.csv")
BASELINE_AVG_IN  <- file.path(BASE_DIR, "baseline", "derived", "average_screentime_for_annotation.csv")
BASELINE_APP_IN  <- file.path(BASE_DIR, "baseline", "derived", "app_screentime_for_annotation.csv")

ENDLINE_RESULTS  <- file.path(BASE_DIR, "endline",  "results")
BASELINE_RESULTS <- file.path(BASE_DIR, "baseline", "results")
dir.create(ENDLINE_RESULTS,  recursive = TRUE, showWarnings = FALSE)
dir.create(BASELINE_RESULTS, recursive = TRUE, showWarnings = FALSE)

SEED  <- suppressWarnings(as.integer(Sys.getenv("ANNOT_SEED",  "12345")))
N_AVG <- suppressWarnings(as.integer(Sys.getenv("ANNOT_N_AVG", "30")))
N_APP <- suppressWarnings(as.integer(Sys.getenv("ANNOT_N_APP", "30")))

# Phase order: endline first, then baseline (filtered to endline completers)
PHASE_ORDER <- c("endline_avg", "endline_app", "baseline_avg", "baseline_app", "done")

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

build_or_load_sample <- function(type = c("avg", "app"), df, n, seed, sample_path) {
  type <- match.arg(type)
  if (file.exists(sample_path)) return(safe_read(sample_path))
  set.seed(seed)

  if (type == "avg") {
    df <- ensure_cols2(df, c("respondent_id","total_screenshot_path","total_hours","total_minutes",
                              "device","screenshot_day"))
    df <- df %>%
      mutate(total_screenshot_path = as.character(total_screenshot_path)) %>%
      filter(file_ok_vec(total_screenshot_path))
    if (nrow(df) == 0) stop("No usable avg screenshots found on disk.", call. = FALSE)
    if (nrow(df) < n) n <- nrow(df)
    samp <- df %>% slice_sample(n = n) %>%
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
  if (nrow(df) < n) n <- nrow(df)
  samp <- df %>% slice_sample(n = n) %>%
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
for (p in c(ENDLINE_AVG_IN, ENDLINE_APP_IN, BASELINE_AVG_IN, BASELINE_APP_IN)) {
  if (!file.exists(p))
    stop("Missing input file: ", p, "\nRun 02_wrangle.R first.", call. = FALSE)
}

# ----------------------------
# Load data and build samples
# ----------------------------
endline_avg_raw  <- safe_read(ENDLINE_AVG_IN)
endline_app_raw  <- safe_read(ENDLINE_APP_IN)

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
       "Check PARTICIPANT_ID_COL in 02_wrangle.R.", call. = FALSE)

message(sprintf("Endline: %d avg, %d app respondents", nrow(endline_avg_raw), nrow(endline_app_raw)))
message(sprintf("Baseline (filtered to endline completers): %d avg, %d app respondents",
                nrow(baseline_avg_raw), nrow(baseline_app_raw)))

all_tasks <- list(
  endline_avg  = build_or_load_sample("avg", endline_avg_raw,  N_AVG, SEED, SAMPLE_PATHS$endline_avg),
  endline_app  = build_or_load_sample("app", endline_app_raw,  N_APP, SEED, SAMPLE_PATHS$endline_app),
  baseline_avg = build_or_load_sample("avg", baseline_avg_raw, N_AVG, SEED, SAMPLE_PATHS$baseline_avg),
  baseline_app = build_or_load_sample("app", baseline_app_raw, N_APP, SEED, SAMPLE_PATHS$baseline_app)
)

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
      .screenshot-wrap { height: 70vh; overflow: auto; border: 1px solid #eee;
                         border-radius: 12px; padding: 8px; background: #fafafa; }
      .screenshot-wrap img { display: block; margin: 0 auto; cursor: zoom-in;
                             max-width: 100%; height: auto; }
      .progress-bar-wrap { height: 10px; border-radius: 999px; background: #eee; overflow: hidden; }
      .progress-bar-fill { height: 10px; background: #111; width: 0%; }
      .meta-grid { display: grid; grid-template-columns: 110px 1fr; gap: 6px 10px; }
      .meta-k { font-weight: 600; color: #444; }
      .meta-v { color: #111; }
    ")),
    tags$script(HTML("
      document.addEventListener('click', function(e) {
        var img = e.target;
        if (img && img.tagName && img.tagName.toLowerCase() === 'img') {
          var parent = img.closest('.screenshot-wrap');
          if (parent) Shiny.setInputValue('img_src_clicked', img.getAttribute('src'), {priority:'event'});
        }
      });
    "))
  ),

  titlePanel(paste0("Manual Screenshot Annotation — ", TEAM_SLUG)),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "panel",
        div(class = "muted", paste0("Seed: ", SEED, " | avg=", N_AVG, ", app=", N_APP,
                                    " per wave")),
        hr(),
        textInput("reviewer", "Reviewer name", ""),
        hr(),
        uiOutput("phase_label"),
        hr(),
        fluidRow(
          column(6, actionButton("prev_btn", "\u2190 Prev", class = "big")),
          column(6, actionButton("next_btn", "Next \u2192", class = "big"))
        ),
        numericInput("jump", "Jump to index", value = 1, min = 1, step = 1),
        actionButton("go_btn", "Go"),
        hr(),
        uiOutput("progress"),
        hr(),
        radioButtons("screenshot_correct", "Correct screenshot?", choices = c("Yes","No","Unsure")),
        radioButtons("numbers_match",      "Numbers match screenshot?", choices = c("Yes","No","Unsure")),
        textAreaInput("notes", "Notes (optional)", value = "", rows = 4),
        div(class = "muted", "Tip: click the screenshot to open fullscreen.")
      )
    ),

    mainPanel(
      width = 8,
      uiOutput("header"),
      br(),
      uiOutput("progress_bar"),
      br(),
      fluidRow(
        column(4, uiOutput("numbers_panel")),
        column(8, uiOutput("images_panel"))
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
    phase         = "endline_avg",
    i_endline_avg  = 1L,
    i_endline_app  = 1L,
    i_baseline_avg = 1L,
    i_baseline_app = 1L
  )

  anns <- reactiveValues(
    endline_avg  = load_annotations(ANN_PATHS[["endline_avg"]]),
    endline_app  = load_annotations(ANN_PATHS[["endline_app"]]),
    baseline_avg = load_annotations(ANN_PATHS[["baseline_avg"]]),
    baseline_app = load_annotations(ANN_PATHS[["baseline_app"]])
  )

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
      if (ph == "done") return()
      r <- current(); if (is.null(r)) return()
      cur <- standardize_annotations(anns[[ph]])
      hit <- cur %>% filter(task_id == as.character(r$task_id[[1]]))
      if (nrow(hit) >= 1) {
        updateRadioButtons(session, "screenshot_correct", selected = hit$screenshot_correct[[1]])
        updateRadioButtons(session, "numbers_match",      selected = hit$numbers_match[[1]])
        updateTextAreaInput(session, "notes", value = ifelse(is.na(hit$notes[[1]]), "", hit$notes[[1]]))
        if (!is.na(hit$reviewer[[1]]) && nzchar(hit$reviewer[[1]]))
          updateTextInput(session, "reviewer", value = hit$reviewer[[1]])
      } else {
        updateRadioButtons(session, "screenshot_correct", selected = character(0))
        updateRadioButtons(session, "numbers_match",      selected = character(0))
        updateTextAreaInput(session, "notes", value = "")
      }
      updateNumericInput(session, "jump", value = get_idx(), min = 1, max = nrow(tasks()))
    },
    ignoreInit = TRUE
  )

  do_save <- function() {
    ph <- state$phase
    if (ph == "done") return()
    r <- current(); if (is.null(r)) return()
    rec <- tibble(
      task_id            = as.character(r$task_id[[1]]),
      respondent_id      = as.character(r$respondent_id[[1]]),
      reviewer           = as.character(input$reviewer),
      screenshot_correct = as.character(input$screenshot_correct),
      numbers_match      = as.character(input$numbers_match),
      notes              = as.character(input$notes),
      annotated_at       = as.character(Sys.time())
    ) %>% standardize_annotations()
    cur  <- standardize_annotations(anns[[ph]])
    cur2 <- cur %>%
      filter(is.na(task_id) | task_id != rec$task_id[[1]]) %>%
      bind_rows(rec) %>% standardize_annotations()
    anns[[ph]] <- cur2
    save_annotations(ANN_PATHS[[ph]], cur2)
  }

  observeEvent(input$next_btn, {
    do_save()
    ph <- state$phase; if (ph == "done") return()
    state[[paste0("i_", ph)]] <- state[[paste0("i_", ph)]] + 1L
    advance_phase_if_needed()
  })

  observeEvent(input$prev_btn, {
    do_save()
    ph <- state$phase; if (ph == "done") return()
    key <- paste0("i_", ph)
    state[[key]] <- max(1L, state[[key]] - 1L)
  })

  observeEvent(input$go_btn, {
    do_save()
    set_idx(input$jump)
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

  output$header <- renderUI({
    if (state$phase == "done") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    tags$div(tags$h3(paste0("Task: ", r$task_id[[1]])))
  })

  output$numbers_panel <- renderUI({
    ph <- state$phase
    if (ph == "done") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    device <- if ("device"            %in% names(r)) as.character(r$device[[1]])            else NA_character_
    day    <- if ("screenshot_day"    %in% names(r)) as.character(r$screenshot_day[[1]])    else NA_character_
    date   <- if ("android_target_date" %in% names(r)) as.character(r$android_target_date[[1]]) else NA_character_
    day2   <- fmt_day(device, day)
    date2  <- fmt_date(device, date)
    meta   <- list(
      tags$div(class="meta-k","Respondent"), tags$div(class="meta-v", as.character(r$respondent_id[[1]])),
      tags$div(class="meta-k","Device"),     tags$div(class="meta-v", ifelse(is.na(device)|!nzchar(device),"NA",device))
    )
    if (!is.na(day2)  && nzchar(day2))
      meta <- c(meta, list(tags$div(class="meta-k","Day (Android)"),  tags$div(class="meta-v",day2)))
    if (!is.na(date2) && nzchar(date2))
      meta <- c(meta, list(tags$div(class="meta-k","Date (Android)"), tags$div(class="meta-v",date2)))

    if (phase_type(ph) == "avg") {
      h <- if ("total_hours"   %in% names(r)) r$total_hours[[1]]   else NA
      m <- if ("total_minutes" %in% names(r)) r$total_minutes[[1]] else NA
      tags$div(class="panel",
        tags$h4("Check total screen time"),
        tags$div(class="big", tags$strong(fmt_hm(h,m))),
        tags$hr(), tags$div(class="meta-grid", meta), tags$hr(),
        tags$div(class="muted","Click screenshot to open fullscreen.")
      )
    } else {
      getv <- function(nm) if (nm %in% names(r)) r[[nm]][[1]] else NA
      tags$div(class="panel",
        tags$h4("Check app screen time"),
        tags$table(class="table table-striped tight-table",
          tags$thead(tags$tr(tags$th("App"),tags$th("Hours"),tags$th("Minutes"))),
          tags$tbody(
            tags$tr(tags$td("Instagram"),tags$td(getv("instagram_hours")),tags$td(getv("instagram_minutes"))),
            tags$tr(tags$td("Facebook"), tags$td(getv("facebook_hours")), tags$td(getv("facebook_minutes"))),
            tags$tr(tags$td("TikTok"),   tags$td(getv("tiktok_hours")),   tags$td(getv("tiktok_minutes"))),
            tags$tr(tags$td("Twitter"),  tags$td(getv("twitter_hours")),  tags$td(getv("twitter_minutes")))
          )
        ),
        tags$hr(), tags$div(class="meta-grid", meta), tags$hr(),
        tags$div(class="muted","Click screenshot to open fullscreen.")
      )
    }
  })

  output$img_avg <- renderImage({
    if (phase_type(state$phase) != "avg") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    p <- if ("total_screenshot_path" %in% names(r)) as.character(r$total_screenshot_path[[1]]) else NA_character_
    if (!file_ok1(p)) return(NULL)
    list(src = p, contentType = guess_content_type(p), alt = "screenshot")
  }, deleteFile = FALSE)

  output$img_app1 <- renderImage({
    if (phase_type(state$phase) != "app") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    p <- if ("app_screenshot1_path" %in% names(r)) as.character(r$app_screenshot1_path[[1]]) else NA_character_
    if (!file_ok1(p)) return(NULL)
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 1")
  }, deleteFile = FALSE)

  output$img_app2 <- renderImage({
    if (phase_type(state$phase) != "app") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    p <- if ("app_screenshot2_path" %in% names(r)) as.character(r$app_screenshot2_path[[1]]) else NA_character_
    if (!file_ok1(p)) return(NULL)
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 2")
  }, deleteFile = FALSE)

  output$img_app3 <- renderImage({
    if (phase_type(state$phase) != "app") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    p <- if ("app_screenshot3_path" %in% names(r)) as.character(r$app_screenshot3_path[[1]]) else NA_character_
    if (!file_ok1(p)) return(NULL)
    list(src = p, contentType = guess_content_type(p), alt = "screenshot 3")
  }, deleteFile = FALSE)

  output$images_panel <- renderUI({
    ph <- state$phase
    if (ph == "done") return(NULL)
    r <- current(); if (is.null(r)) return(NULL)
    if (phase_type(ph) == "avg") {
      tags$div(class="panel", tags$h4("Screenshot (click to fullscreen)"),
               tags$div(class="screenshot-wrap", imageOutput("img_avg")))
    } else {
      p1 <- if ("app_screenshot1_path" %in% names(r)) as.character(r$app_screenshot1_path[[1]]) else NA_character_
      p2 <- if ("app_screenshot2_path" %in% names(r)) as.character(r$app_screenshot2_path[[1]]) else NA_character_
      p3 <- if ("app_screenshot3_path" %in% names(r)) as.character(r$app_screenshot3_path[[1]]) else NA_character_
      tabs <- list()
      if (file_ok1(p1)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 1", tags$div(class="screenshot-wrap", imageOutput("img_app1")))
      if (file_ok1(p2)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 2", tags$div(class="screenshot-wrap", imageOutput("img_app2")))
      if (file_ok1(p3)) tabs[[length(tabs)+1]] <- tabPanel("Screenshot 3", tags$div(class="screenshot-wrap", imageOutput("img_app3")))
      tags$div(class="panel", tags$h4("Screenshots (click to fullscreen)"),
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
