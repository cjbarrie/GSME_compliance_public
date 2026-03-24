# Screenshot Compliance Check — Country Team Scripts

This repository contains the scripts for country survey teams participating in the Global Social Media Experiment (GSME).

Your job is to:
1. Download your Qualtrics survey data
2. Process it into annotation-ready files
3. Manually review a random sample of screenshots using the Shiny app
4. Bundle your results and send them back

---

## Prerequisites

### Install R packages

Run once in R:

```r
install.packages(c(
  "qualtRics", "httr", "readr", "dplyr", "stringr", "tidyr", "shiny", "tibble"
))
```

### Set your Qualtrics API key

You need to set `QUALTRICS_API_KEY` before running the download script.

**macOS/Linux:**

```bash
export QUALTRICS_API_KEY="YOUR_KEY_HERE"
```

**Windows (PowerShell):**

```powershell
setx QUALTRICS_API_KEY "YOUR_KEY_HERE"
```

Or add it permanently to `~/.Renviron`:

```
QUALTRICS_API_KEY=YOUR_KEY_HERE
```

The easiest way to open `.Renviron` is:

```r
usethis::edit_r_environ()
```

Restart R after editing `.Renviron` for the change to take effect.

### Find your Qualtrics data center

Each script has a line:

```r
BASE_URL <- "ca1.qualtrics.com"
```

Change this if your Qualtrics account uses a different data center. See the [Qualtrics guide](https://www.qualtrics.com/support/integrations/api-integration/finding-qualtrics-ids/#LocatingtheDatacenterID) for how to find yours.

---

## Quick start

You will run this workflow **twice** — once for **baseline** and once for **endline**.

### Step 1 — Edit config at the top of each script

In each of the four scripts, set:

- `TEAM_SLUG` — your ISO 2-letter country code (e.g. `"GB"`, `"US"`, `"BR"`). See [ISO 3166-1 alpha-2 codes](https://www.iso.org/obp/ui/#search).
- `WAVE` — `"baseline"` or `"endline"`
- `SURVEY_ID` (in `01_download.R` only) — found in your Qualtrics survey URL, starts with `SV_`

You can find the Survey ID by opening the survey in Qualtrics and looking at the URL:

![qualtrics1.png](qualtrics1.png)

### Step 2 — Run the scripts in order

```bash
Rscript 01_download.R
Rscript 02_wrangle.R
Rscript 03_run_app.R
Rscript 04_bundle_results.R
```

### Step 3 — Send the ZIP

After `04_bundle_results.R` completes, send the ZIP file it creates to **cb5691@nyu.edu**.

The ZIP will be in:
```
data/qualtrics/<TEAM_SLUG>/<WAVE>/results/bundle_<TEAM_SLUG>_<WAVE>_<timestamp>.zip
```

---

## What each script does

### `01_download.R` — Download survey data

Downloads all Qualtrics responses and uploaded screenshot files for the wave.

**Edit before running:**
- `TEAM_SLUG`
- `WAVE`
- `SURVEY_ID` (your baseline or endline survey ID)
- `BASE_URL` (your Qualtrics data center, if different)

**Outputs:**
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/responses.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/uploaded_files_manifest.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/uploads/<ResponseId>/...` (screenshot files)

---

### `02_wrangle.R` — Prepare for annotation

Processes responses into standardised CSVs ready for the annotation app.

**Edit before running:**
- `TEAM_SLUG` and `WAVE` (must match `01_download.R`)
- `PARTICIPANT_ID_COL` — the Qualtrics column containing your panel provider's stable participant ID (e.g. `"ID"`, `"PanelID"`). This is used to link respondents across baseline and endline. Set to `NA` if you don't have one.
- `GENERATE_DUMMY_IDS` — leave as `FALSE` for real data

**Outputs:**
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/derived/average_screentime_for_annotation.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/derived/app_screentime_for_annotation.csv`

**Notes:**
- Automatically detects iOS vs Android from the device field
- For Android, detects which day-of-week branch each respondent completed (prefixes 1–7 = Monday–Sunday) and calculates the corresponding calendar date

---

### `03_run_app.R` — Manual annotation (Shiny app)

Opens a browser-based app for reviewing screenshots.

**Edit before running:**
- `TEAM_SLUG` and `WAVE`

**Run:**

```bash
Rscript 03_run_app.R
```

A browser window opens automatically.

**How the app works:**

- **No Save button.** Annotations are saved automatically when you click Next, Prev, or Go.
- Tasks run in two phases: first **Average** screenshots, then **App-level** screenshots.
- The app shows the reported screentime value alongside the screenshot so you can check they match.
- For Android respondents, the app shows the expected day of week and calendar date.
- Click any screenshot to open it fullscreen.
- You can close and re-open the app at any time — your progress is preserved.

**For each screenshot, answer two questions:**

1. **Correct screenshot?** — Is this the right type of screenshot (iOS Screen Time weekly summary, or Android Digital Wellbeing daily view)?
2. **Numbers match?** — Do the numbers visible in the screenshot match the values the respondent reported?

Answer Yes, No, or Unsure. Add a note if needed.

**Outputs** (saved automatically):
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/results/sample_avg.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/results/sample_app.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/results/annotations_avg.csv`
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/results/annotations_app.csv`

The sample files are created on first run with a stable random seed — deleting them will regenerate the sample.

---

### `04_bundle_results.R` — Bundle and send

Packages your annotation results into a ZIP file for submission.

**Edit before running:**
- `TEAM_SLUG` and `WAVE`

**Run:**

```bash
Rscript 04_bundle_results.R
```

**Outputs:**
- `data/qualtrics/<TEAM_SLUG>/<WAVE>/results/bundle_<TEAM_SLUG>_<WAVE>_<timestamp>.zip`

**Send this ZIP to cb5691@nyu.edu.**

---

## Folder structure

After running all scripts, your directory will look like:

```
data/
  qualtrics/
    <TEAM_SLUG>/
      baseline/
        responses.csv
        uploaded_files_manifest.csv
        uploads/
          <ResponseId>/
            <screenshot files>
        derived/
          average_screentime_for_annotation.csv
          app_screentime_for_annotation.csv
        results/
          sample_avg.csv
          sample_app.csv
          annotations_avg.csv
          annotations_app.csv
          bundle_<TEAM_SLUG>_baseline_<timestamp>.zip   ← send this
      endline/
        ... same structure ...
```

---

## Troubleshooting

**`SURVEY_ID is blank`** — Edit `01_download.R` and set `SURVEY_ID` to your survey's ID.

**`Missing QUALTRICS_API_KEY`** — Set the environment variable (see Prerequisites above).

**`No usable avg screenshots found on disk`** — The download may have failed or the manifest paths are wrong. Re-run `01_download.R` with `FORCE_UPLOADS <- TRUE`.

**App shows no screenshots** — Make sure `02_wrangle.R` ran successfully and the `derived/` files exist.

**Want to redo the sample** — Delete `results/sample_avg.csv` and/or `results/sample_app.csv`, then re-run `03_run_app.R`.
