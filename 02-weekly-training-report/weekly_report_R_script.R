# Weekly Report

library(readxl)
library(dplyr)
library(lubridate)
library(rmarkdown)

# ==== USER SETTINGS ====
DATAFILE  <- "Polar_Excel.xlsx"
OUTPUT_DIR <- "reports"

# Define the week to report 
report_start <- as.Date("2025-02-03")  # Monday 
report_end   <- report_start + 6       # Sunday

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# ---- 1. Load data ----
# GPS Sheet
gps_data <- read_excel(DATAFILE, sheet = "GPS") |>
  mutate(
    Date = as.Date(Date),          
    Name = as.character(Name)
  ) |>
  mutate(
    across(
      c(
        "Duration",
        "TD",       
        "m_min",      
        "MaxSpeed",
        "Sprints",
        "RPE",
        "sRPE",
        "eTRIMP",
        "HSR",
        "EXP",
        "7day",
        "28day",
        "ACWR",
        "StDev",
        "TD_Monotony"              
      ),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# Wellness sheet
wellness <- read_excel(DATAFILE, sheet = "Wellness") %>%
  mutate(
    Date = as.Date(Date),
    Name = as.character(Name)
  )

# ---- 2. Filter to the selected week ----
week_data <- gps_data %>%
  filter(
    Date >= report_start,
    Date <= report_end,
    !is.na(Name),
    !is.na(Type)
  )

# ---- 3. Render R Markdown to PDF ----
output_file <- paste0(
  "Weekly_Report_",
  format(report_start, "%Y-%m-%d"),
  "_to_",
  format(report_end, "%Y-%m-%d"),
  ".pdf"
)

render(
  input        = "weekly_report.Rmd",
  output_file  = output_file,
  output_dir   = OUTPUT_DIR,
  output_format = "pdf_document",
  params       = list(
    report_start  = report_start,
    report_end    = report_end,
    gps_all       = gps_data,
    week_data     = week_data,
    wellness      = wellness      
  ),
  envir = new.env()
)

# Run in console: source("Weekly_Report_R_Script.R")