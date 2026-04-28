# Daily Report

library(readxl)
library(dplyr)
library(lubridate)
library(rmarkdown)

# ==== USER SETTINGS ====
DATAFILE  <- "Polar_Excel.xlsx"
OUTPUT_DIR <- "reports"

# Define the day to report 
report_date <- as.Date("2025-02-03")  

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

weekly_goal <- 21000

# ---- 1. Load data ----
# GPS Sheet
gps_data_raw <- read_excel(DATAFILE, sheet = "GPS") |>
  mutate(
    Date = as.Date(Date),          
    Name = as.character(Name)
  )

gps_data <- gps_data_raw |>
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

# ---- 2. Filter to the selected day ----
day_gps_data <- gps_data |>
  filter(Date == report_date) |>
  filter(!is.na(Name), !is.na(Type))

day_well_data <- wellness |>
  filter(Date == report_date) |>
  filter(!is.na(Name))

# ---- 3. Create summary tables ----

# Weekly distance vs goal 

report_week_start <- floor_date(report_date, unit = "week", week_start = 1)  

weekly_summary <- gps_data |>
  filter(
    Date >= report_week_start,
    Date <= report_date
  ) |>
  group_by(Date) |>
  summarise(
    daily_avg_distance = mean(TD, na.rm = TRUE),
    daily_avg_hsr = mean(HSR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  summarise(
    'Week Start'           = report_week_start,
    'Avg Distance To Date' = sum(daily_avg_distance, na.rm = TRUE),  
    'Avg HSR To Date'      = sum(daily_avg_hsr, na.rm = TRUE),
    'Week Goal'            = weekly_goal,
    .groups = "drop"
  )

# ---- 4. Render R Markdown to PDF ----
output_file <- paste0(
  "Daily_Report_",
  format(report_date, "%Y-%m-%d"),
  ".pdf")

render(
  input        = "Daily_report.Rmd",
  output_file  = output_file,
  output_dir   = OUTPUT_DIR,
  output_format = "pdf_document",
  params = list(
    report_date  = report_date,
    day_gps_data  = day_gps_data,
    day_well_data = day_well_data,
    weekly_summary = weekly_summary
  ),
  envir = new.env()
)
# Run in console: source("daily_report_R_script.R")