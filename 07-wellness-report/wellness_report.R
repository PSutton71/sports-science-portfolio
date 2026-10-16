#!/usr/bin/env Rscript
# =============================================================================
# Wellness & RPE report builder
# =============================================================================

suppressPackageStartupMessages(library(jsonlite))

# ---- SETTINGS ---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

reports_dir <- "C:/Users/paisutt/OneDrive - Oklahoma A and M System/Sports Science Portfolio/Wellness Report"
output_dir  <- reports_dir   # save reports straight into the Wellness Report folder
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

input_file    <- if (length(args) >= 1) args[1] else file.path(reports_dir, "Athlete Morning Survey.xlsx")
template_file <- if (length(args) >= 2) args[2] else file.path(reports_dir, "wellness_report.html")
output_file   <- if (length(args) >= 3) args[3] else
  file.path(output_dir, paste0("wellness_report_", format(Sys.Date()), ".html"))

session_minutes <- 120    
baseline_days   <- 28     
baseline_min_n  <- 3      
sd_floor        <- 0.5    
z_green         <- -0.5   
z_red           <- -1.0   
pct_green       <- 0.75   
pct_red         <- 0.50   
write_processed_csv <- TRUE   
# -----------------------------------------------------------------------------

items      <- c("fatigue", "muscles", "stress", "mood", "sleep_quality")
item_max   <- 5
max_composite <- item_max * length(items)

# ---- Read the export ----------------------------------------------------------
read_survey <- function(path) {
  if (!file.exists(path)) stop("Can't find the data file: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Reading Excel needs the readxl package: install.packages('readxl')")
    df <- as.data.frame(readxl::read_excel(path), check.names = FALSE)
  } else {
    first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    n_tab   <- lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE)))
    n_comma <- lengths(regmatches(first, gregexpr(",",  first, fixed = TRUE)))
    df <- read.csv(path, sep = if (n_tab > n_comma) "\t" else ",",
                   check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  }
  df
}

find_col <- function(df, pattern, label, required = TRUE) {
  hit <- grep(pattern, names(df), ignore.case = TRUE, perl = TRUE)
  if (!length(hit)) {
    if (required) stop(sprintf("Couldn't find the '%s' column (looked for /%s/).\nColumns found:\n  %s",
                               label, pattern, paste(names(df), collapse = "\n  ")))
    return(rep(NA, nrow(df)))
  }
  df[[hit[1]]]
}

parse_day <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))   
  x <- sub("\\s.*$", "", trimws(as.character(x)))                       
  out <- as.Date(rep(NA_character_, length(x)))
  f4 <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)
  f2 <- grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$",  x)
  fi <- grepl("^\\d{4}-\\d{2}-\\d{2}$",      x)
  out[f4] <- as.Date(x[f4], format = "%m/%d/%Y")
  out[f2] <- as.Date(x[f2], format = "%m/%d/%y")
  out[fi] <- as.Date(x[fi], format = "%Y-%m-%d")
  out
}

parse_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  as.numeric(as.POSIXct(trimws(as.character(x)), format = "%m/%d/%Y %H:%M", tz = "UTC"))
}

# Pull the leading number out of an answer ("4", "4.5", "4 - Good" -> 4 / 4.5 / 4)
to_num <- function(x) {
  x <- trimws(as.character(x))
  ok <- grepl("^[0-9]+(\\.[0-9]+)?", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub("^([0-9]+(\\.[0-9]+)?).*$", "\\1", x[ok]))
  out
}

blank_to_na <- function(x) { x <- trimws(as.character(x)); x[x == ""] <- NA; x }

raw <- read_survey(input_file)

name_ex   <- blank_to_na(find_col(raw, "Name \\(Ex", "Name (Ex: First Last)", required = FALSE))
name_plain <- blank_to_na(find_col(raw, "^\\s*Name\\s*$", "Name", required = FALSE))
athlete   <- ifelse(is.na(name_ex), name_plain, name_ex)
if (all(is.na(athlete))) stop("No athlete names found in either Name column.")

d <- data.frame(
  name          = athlete,
  date          = parse_day(find_col(raw, "Today.s Date", "Today's Date")),
  submitted     = parse_time(find_col(raw, "Completion time", "Completion time", required = FALSE)),
  trained       = grepl("^\\s*y", tolower(as.character(find_col(raw, "practice or have a game", "Did you practice")))),
  reason        = blank_to_na(find_col(raw, "Why did you not", "Why did you not practice", required = FALSE)),
  rpe           = to_num(find_col(raw, "How was your practice", "RPE (How was your practice/game)")),
  fatigue       = to_num(find_col(raw, "fatigued", "Fatigue")),
  muscles       = to_num(find_col(raw, "muscles", "Muscles")),
  stress        = to_num(find_col(raw, "stress", "Stress")),
  mood          = to_num(find_col(raw, "mood", "Mood")),
  sleep_hours   = blank_to_na(find_col(raw, "how long did you sleep", "Sleep hours", required = FALSE)),
  sleep_quality = to_num(find_col(raw, "quality of sleep", "Sleep quality")),
  row           = seq_len(nrow(raw)),
  stringsAsFactors = FALSE
)

d$name <- gsub("\\s+", " ", d$name)
dropped <- sum(is.na(d$name) | is.na(d$date))
d <- d[!is.na(d$name) & !is.na(d$date), ]
if (!nrow(d)) stop("No usable rows (every row is missing a name or Today's Date).")

d <- d[order(d$name, d$date, d$submitted, d$row, na.last = FALSE), ]
dupes <- sum(duplicated(d[c("name", "date")], fromLast = TRUE))
d <- d[!duplicated(d[c("name", "date")], fromLast = TRUE), ]

# ---- Calculations -------------------------------------------------------------
d$composite <- rowSums(d[items])                 
d$rpe[!d$trained] <- NA
d$load      <- ifelse(d$trained & !is.na(d$rpe), d$rpe * session_minutes, NA)
d$load_date <- d$date - 1                        

d <- d[order(d$name, d$date), ]
d$base_mean <- NA_real_; d$base_sd <- NA_real_; d$z <- NA_real_; d$base_n <- 0L
for (i in seq_len(nrow(d))) {
  prior <- d$composite[d$name == d$name[i] & d$date < d$date[i] &
                       d$date >= d$date[i] - baseline_days & !is.na(d$composite)]
  d$base_n[i] <- length(prior)
  if (length(prior) >= 1) d$base_mean[i] <- mean(prior)
  if (length(prior) >= 2) d$base_sd[i]   <- sd(prior)
  if (length(prior) >= baseline_min_n && !is.na(d$composite[i]))
    d$z[i] <- (d$composite[i] - d$base_mean[i]) / max(d$base_sd[i], sd_floor)
}

d$pct    <- d$composite / max_composite
d$method <- ifelse(d$base_n >= baseline_min_n, "baseline", "early")
d$status <- ifelse(is.na(d$composite), "incomplete",
            ifelse(d$method == "baseline",
              ifelse(d$z >= z_green, "green", ifelse(d$z >= z_red, "yellow", "red")),
              ifelse(d$pct >= pct_green, "green", ifelse(d$pct >= pct_red, "yellow", "red"))))

# ---- Write the report -----------------------------------------------------------
out <- d[c("name", "date", "trained", "reason", "rpe", "load", "load_date",
           items, "sleep_hours", "composite", "base_n", "base_mean", "base_sd",
           "z", "pct", "method", "status")]
out$base_mean <- round(out$base_mean, 2)
out$base_sd   <- round(out$base_sd, 2)
out$z         <- round(out$z, 2)
out$pct       <- round(out$pct, 3)
out$date      <- format(out$date)
out$load_date <- format(out$load_date)

payload <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  source    = basename(input_file),
  settings  = list(session_minutes = session_minutes, baseline_days = baseline_days,
                   baseline_min_n = baseline_min_n, sd_floor = sd_floor,
                   z_green = z_green, z_red = z_red, pct_green = pct_green,
                   pct_red = pct_red, item_max = item_max, max_composite = max_composite),
  rows      = out
)
json <- toJSON(payload, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = NA)
json <- gsub("</", "<\\/", json, fixed = TRUE)   # keep the <script> block intact

if (!file.exists(template_file)) stop("Can't find the report template: ", template_file)
tpl <- paste(readLines(template_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
open_tag <- '<script id="report-data" type="application/json">'
s <- regexpr(open_tag, tpl, fixed = TRUE)
if (s < 0) stop("The template is missing its <script id=\"report-data\"> block.")
head_end <- s + attr(s, "match.length") - 1
rest     <- substring(tpl, head_end + 1)
e        <- regexpr("</script>", rest, fixed = TRUE)
html     <- paste0(substring(tpl, 1, head_end), "\n", json, "\n", substring(rest, e))

con <- file(output_file, open = "w", encoding = "UTF-8")
writeLines(html, con)
close(con)

if (write_processed_csv) {
  csv_file <- sub("\\.html?$", "_data.csv", output_file)
  write.csv(out, csv_file, row.names = FALSE, na = "")
}

# ---- Console summary ----------------------------------------------------------------
latest <- max(d$date)
today  <- d[d$date == latest, ]
cat(sprintf("\nRead %d rows from %s (%d athletes, %s to %s).\n",
            nrow(d), basename(input_file), length(unique(d$name)), min(d$date), latest))
if (dropped) cat(sprintf("Skipped %d row(s) with no name or date.\n", dropped))
if (dupes)   cat(sprintf("Kept the latest of %d duplicate same-day submission(s).\n", dupes))
cat(sprintf("Latest day %s: %d green, %d yellow, %d red, %d incomplete.\n", latest,
            sum(today$status == "green"), sum(today$status == "yellow"),
            sum(today$status == "red"), sum(today$status == "incomplete")))
flag <- today[today$status %in% c("red", "yellow"), ]
if (nrow(flag)) {
  flag <- flag[order(flag$status != "red", flag$composite), ]
  cat("Check in with:\n")
  for (i in seq_len(nrow(flag)))
    cat(sprintf("  %-7s %-24s composite %4.1f  (baseline %s)\n", toupper(flag$status[i]),
                flag$name[i], flag$composite[i],
                ifelse(is.na(flag$base_mean[i]), "n/a", sprintf("%.1f", flag$base_mean[i]))))
}
cat(sprintf("\nReport written to %s\n", normalizePath(output_file)))
