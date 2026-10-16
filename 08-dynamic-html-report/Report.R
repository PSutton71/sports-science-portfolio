# =============================================================================
#  Testing Report
# =============================================================================

library(readxl)
library(jsonlite)
library(dplyr)
library(stringr)

# ── 1. CONFIGURATION ─────────────────────────────────────────────────────────

INPUT_FILE     <- "Blank_Data.xlsx"
TEMPLATE_FILE  <- "report_template.html"
OUTPUT_FILE    <- "report.html"

SHEET_MONTHLY  <- "Monthly"
SHEET_WEEKLY   <- "Weekly"
SHEET_SCHEDULE <- "Schedule"

REPORT_TITLE <- "Testing Report"
TEAM_LABEL   <- "Baseball"

COL_MONTHLY <- list(
  date         = "Date",
  name         = "Name",
  ir_throw     = "IR_Throw",
  ir_nonthrow  = "IR_NonThrow",
  er_throw     = "ER_Throw",
  er_nonthrow  = "ER_NonThrow",
  arc_throw    = "Arc_Throw",
  arc_nonthrow = "Arc_NonThrow"
)

COL_WEEKLY <- list(
  date   = "Date",
  name   = "Name",
  visit  = "Visit",
  pitches = "Pitches",
  
  grip_l1 = "Grip_L1",
  grip_r1 = "Grip_R1",
  grip_l2 = "Grip_L2",
  grip_r2 = "Grip_R2",
  
  er_l1 = "ER_L_1",
  er_r1 = "ER_R_1",
  er_l2 = "ER_L_2",
  er_r2 = "ER_R_2",
  
  ir_l1 = "IR_L_1",
  ir_r1 = "IR_R_1",
  ir_l2 = "IR_L_2",
  ir_r2 = "IR_R_2",
  
  l_shoulder_flexion   = "L_Shoulder_Flexion",
  l_shoulder_extension = "L_Shoulder_Extension",
  l_shoulder_ir        = "L_Shoulder_IR",
  l_shoulder_er        = "L_Shoulder_ER",
  r_shoulder_flexion   = "R_Shoulder_Flexion",
  r_shoulder_extension = "R_Shoulder_Extension",
  r_shoulder_ir        = "R_Shoulder_IR",
  r_shoulder_er        = "R_Shoulder_ER",
  
  cervical_flexion    = "Cervical_Flexion",
  cervical_extension  = "Cervical_Extension",
  l_cervical_rotation = "L_Cervical_Rotation",
  r_cervical_rotation = "R_Cervical_Rotation",
  
  l_sl_balance_variability = "L_SL_Balance_Variability",
  r_sl_balance_variability = "R_SL_Balance_Variability",
  
  toe_touch         = "Toe_Touch",
  reach_back        = "Reach_Back",
  squat_ankle_dorsi = "Squat_Ankle_Dorsi",
  
  l_apley_oh = "L_Apley_OH",
  l_apley_uh = "L_Apley_UH",
  r_apley_oh = "R_Apley_OH",
  r_apley_uh = "R_Apley_UH",
  
  l_torso_rotation = "L_Torso_Rotation",
  r_torso_rotation = "R_Torso_Rotation",
  
  cmj_1 = "CMJ_1",
  cmj_contraction_time_1 = "CMJ_Contraction_Time_1",
  rsi_1 = "RSI_1",
  cmj_2 = "CMJ_2",
  cmj_contraction_time_2 = "CMJ_Contraction_Time_2",
  rsi_2 = "RSI_2",
  
  l_arc = "L_Arc",
  r_arc = "R_Arc",
  er_ir_ratio = "ER_IR_Ratio"
)

COL_SCHEDULE <- list(
  date     = "Date",
  name     = "Name",
  avg_velo = "Avg_Velo",
  max_velo = "Max_Velo",
  disp     = "Disp"
)

THRESH_OK   <- 5
THRESH_WARN <- 10

# ── 2. BASIC FILE CHECKS ─────────────────────────────────────────────────────

if (!file.exists(INPUT_FILE)) {
  stop("Excel file not found: ", INPUT_FILE)
}

if (!file.exists(TEMPLATE_FILE)) {
  stop("Template file not found: ", TEMPLATE_FILE)
}

# ── 3. READ MONTHLY (ROM) DATA ───────────────────────────────────────────────

raw_monthly <- read_excel(INPUT_FILE, sheet = SHEET_MONTHLY)
names(raw_monthly) <- trimws(names(raw_monthly))

monthly <- raw_monthly %>%
  rename(
    date         = all_of(COL_MONTHLY$date),
    name         = all_of(COL_MONTHLY$name),
    ir_throw     = all_of(COL_MONTHLY$ir_throw),
    ir_nonthrow  = all_of(COL_MONTHLY$ir_nonthrow),
    er_throw     = all_of(COL_MONTHLY$er_throw),
    er_nonthrow  = all_of(COL_MONTHLY$er_nonthrow),
    arc_throw    = all_of(COL_MONTHLY$arc_throw),
    arc_nonthrow = all_of(COL_MONTHLY$arc_nonthrow)
  ) %>%
  mutate(
    date     = format(as.Date(date), "%Y-%m-%d"),
    ir_diff  = ir_nonthrow - ir_throw,
    er_diff  = er_nonthrow - er_throw,
    arc_diff = arc_nonthrow - arc_throw
  ) %>%
  arrange(date, name)

cat("Monthly rows:", nrow(monthly), "| Dates:", paste(sort(unique(monthly$date)), collapse = ", "), "\n")

# ── 4. READ WEEKLY DATA ──────────────────────────────────────────────────────

raw_weekly <- read_excel(INPUT_FILE, sheet = SHEET_WEEKLY)
names(raw_weekly) <- trimws(names(raw_weekly))

weekly <- raw_weekly %>%
  rename(
    date   = all_of(COL_WEEKLY$date),
    name   = all_of(COL_WEEKLY$name),
    visit  = all_of(COL_WEEKLY$visit),
    pitches = all_of(COL_WEEKLY$pitches),
    
    grip_l1 = all_of(COL_WEEKLY$grip_l1),
    grip_r1 = all_of(COL_WEEKLY$grip_r1),
    grip_l2 = all_of(COL_WEEKLY$grip_l2),
    grip_r2 = all_of(COL_WEEKLY$grip_r2),
    
    er_l1 = all_of(COL_WEEKLY$er_l1),
    er_r1 = all_of(COL_WEEKLY$er_r1),
    er_l2 = all_of(COL_WEEKLY$er_l2),
    er_r2 = all_of(COL_WEEKLY$er_r2),
    
    ir_l1 = all_of(COL_WEEKLY$ir_l1),
    ir_r1 = all_of(COL_WEEKLY$ir_r1),
    ir_l2 = all_of(COL_WEEKLY$ir_l2),
    ir_r2 = all_of(COL_WEEKLY$ir_r2),
    
    l_shoulder_flexion   = all_of(COL_WEEKLY$l_shoulder_flexion),
    l_shoulder_extension = all_of(COL_WEEKLY$l_shoulder_extension),
    l_shoulder_ir        = all_of(COL_WEEKLY$l_shoulder_ir),
    l_shoulder_er        = all_of(COL_WEEKLY$l_shoulder_er),
    r_shoulder_flexion   = all_of(COL_WEEKLY$r_shoulder_flexion),
    r_shoulder_extension = all_of(COL_WEEKLY$r_shoulder_extension),
    r_shoulder_ir        = all_of(COL_WEEKLY$r_shoulder_ir),
    r_shoulder_er        = all_of(COL_WEEKLY$r_shoulder_er),
    
    cervical_flexion    = all_of(COL_WEEKLY$cervical_flexion),
    cervical_extension  = all_of(COL_WEEKLY$cervical_extension),
    l_cervical_rotation = all_of(COL_WEEKLY$l_cervical_rotation),
    r_cervical_rotation = all_of(COL_WEEKLY$r_cervical_rotation),
    
    l_sl_balance_variability = all_of(COL_WEEKLY$l_sl_balance_variability),
    r_sl_balance_variability = all_of(COL_WEEKLY$r_sl_balance_variability),
    
    toe_touch         = all_of(COL_WEEKLY$toe_touch),
    reach_back        = all_of(COL_WEEKLY$reach_back),
    squat_ankle_dorsi = all_of(COL_WEEKLY$squat_ankle_dorsi),
    
    l_apley_oh = all_of(COL_WEEKLY$l_apley_oh),
    l_apley_uh = all_of(COL_WEEKLY$l_apley_uh),
    r_apley_oh = all_of(COL_WEEKLY$r_apley_oh),
    r_apley_uh = all_of(COL_WEEKLY$r_apley_uh),
    
    l_torso_rotation = all_of(COL_WEEKLY$l_torso_rotation),
    r_torso_rotation = all_of(COL_WEEKLY$r_torso_rotation),
    
    cmj_1 = all_of(COL_WEEKLY$cmj_1),
    cmj_contraction_time_1 = all_of(COL_WEEKLY$cmj_contraction_time_1),
    rsi_1 = all_of(COL_WEEKLY$rsi_1),
    cmj_2 = all_of(COL_WEEKLY$cmj_2),
    cmj_contraction_time_2 = all_of(COL_WEEKLY$cmj_contraction_time_2),
    rsi_2 = all_of(COL_WEEKLY$rsi_2),
    
    l_arc = all_of(COL_WEEKLY$l_arc),
    r_arc = all_of(COL_WEEKLY$r_arc),
    er_ir_ratio = all_of(COL_WEEKLY$er_ir_ratio)
  ) %>%
  mutate(
    date = format(as.Date(date), "%Y-%m-%d"),
    
    left_grip  = ifelse(is.na(grip_l1) & is.na(grip_l2), NA, pmax(grip_l1, grip_l2, na.rm = TRUE)),
    right_grip = ifelse(is.na(grip_r1) & is.na(grip_r2), NA, pmax(grip_r1, grip_r2, na.rm = TRUE)),
    
    er_l = ifelse(is.na(er_l1) & is.na(er_l2), NA, pmax(er_l1, er_l2, na.rm = TRUE)),
    er_r = ifelse(is.na(er_r1) & is.na(er_r2), NA, pmax(er_r1, er_r2, na.rm = TRUE)),
    ir_l = ifelse(is.na(ir_l1) & is.na(ir_l2), NA, pmax(ir_l1, ir_l2, na.rm = TRUE)),
    ir_r = ifelse(is.na(ir_r1) & is.na(ir_r2), NA, pmax(ir_r1, ir_r2, na.rm = TRUE)),
    
    jump_height = ifelse(is.na(cmj_1) & is.na(cmj_2), NA, pmax(cmj_1, cmj_2, na.rm = TRUE)),
    rsi         = ifelse(is.na(rsi_1) & is.na(rsi_2), NA, pmax(rsi_1, rsi_2, na.rm = TRUE))
  ) %>%
  select(
    date, name, visit, pitches,
    
    left_grip, right_grip,
    jump_height, rsi,
    
    er_l, er_r, ir_l, ir_r,
    
    l_shoulder_flexion, r_shoulder_flexion,
    l_shoulder_extension, r_shoulder_extension,
    l_shoulder_ir, r_shoulder_ir,
    l_shoulder_er, r_shoulder_er,
    
    l_arc, r_arc, er_ir_ratio,
    
    cervical_flexion, cervical_extension,
    l_cervical_rotation, r_cervical_rotation,
    
    l_sl_balance_variability, r_sl_balance_variability,
    
    toe_touch, reach_back, squat_ankle_dorsi,
    
    l_apley_oh, r_apley_oh,
    l_apley_uh, r_apley_uh,
    
    l_torso_rotation, r_torso_rotation
  ) %>%
  filter(if_any(-c(date, name, visit, pitches), ~ !is.na(.x))) %>%
  arrange(date, name)

cat("Weekly rows:", nrow(weekly), "| Dates:", paste(sort(unique(weekly$date)), collapse = ", "), "\n")

# ── 5. READ SCHEDULE DATA ────────────────────────────────────────────────────

raw_schedule <- read_excel(INPUT_FILE, sheet = SHEET_SCHEDULE)
names(raw_schedule) <- trimws(names(raw_schedule))

schedule <- raw_schedule %>%
  rename(
    date     = all_of(COL_SCHEDULE$date),
    name     = all_of(COL_SCHEDULE$name),
    avg_velo = all_of(COL_SCHEDULE$avg_velo),
    max_velo = all_of(COL_SCHEDULE$max_velo),
    disp     = all_of(COL_SCHEDULE$disp)
  ) %>%
  mutate(
    date = format(as.Date(date), "%Y-%m-%d")
  ) %>%
  select(date, name, avg_velo, max_velo, disp) %>%
  filter(!is.na(date), !is.na(name)) %>%
  arrange(date, name)

cat("Schedule rows:", nrow(schedule), "| Dates:", paste(sort(unique(schedule$date)), collapse = ", "), "\n")

# ── 6. CONVERT TO JSON ───────────────────────────────────────────────────────

monthly_records <- monthly %>%
  select(
    Date         = date,
    Name         = name,
    IR_Throw     = ir_throw,
    IR_NonThrow  = ir_nonthrow,
    IR_Diff      = ir_diff,
    ER_Throw     = er_throw,
    ER_NonThrow  = er_nonthrow,
    ER_Diff      = er_diff,
    Arc_Throw    = arc_throw,
    Arc_NonThrow = arc_nonthrow,
    Arc_Diff     = arc_diff
  )

weekly_records <- weekly %>%
  select(
    Date = date,
    Name = name,
    Visit = visit,
    Pitches = pitches,
    
    Left_Grip  = left_grip,
    Right_Grip = right_grip,
    Jump_Height = jump_height,
    RSI         = rsi,
    
    Left_ER  = er_l,
    Right_ER = er_r,
    Left_IR  = ir_l,
    Right_IR = ir_r,
    
    Left_Shoulder_Flexion    = l_shoulder_flexion,
    Right_Shoulder_Flexion   = r_shoulder_flexion,
    Left_Shoulder_Extension  = l_shoulder_extension,
    Right_Shoulder_Extension = r_shoulder_extension,
    Left_Shoulder_IR         = l_shoulder_ir,
    Right_Shoulder_IR        = r_shoulder_ir,
    Left_Shoulder_ER         = l_shoulder_er,
    Right_Shoulder_ER        = r_shoulder_er,
    
    L_Arc       = l_arc,
    R_Arc       = r_arc,
    ER_IR_Ratio = er_ir_ratio,
    
    Cervical_Flexion        = cervical_flexion,
    Cervical_Extension      = cervical_extension,
    Left_Cervical_Rotation  = l_cervical_rotation,
    Right_Cervical_Rotation = r_cervical_rotation,
    
    Left_Single_Leg_Balance_Variability  = l_sl_balance_variability,
    Right_Single_Leg_Balance_Variability = r_sl_balance_variability,
    
    Toe_Touch                = toe_touch,
    Reach_Back               = reach_back,
    Squat_Ankle_Dorsiflexion = squat_ankle_dorsi,
    
    Left_Apley_Overhand   = l_apley_oh,
    Right_Apley_Overhand  = r_apley_oh,
    Left_Apley_Underhand  = l_apley_uh,
    Right_Apley_Underhand = r_apley_uh,
    
    Left_Torso_Rotation  = l_torso_rotation,
    Right_Torso_Rotation = r_torso_rotation
  )

schedule_records <- schedule %>%
  select(
    Date = date,
    Name = name,
    Avg_Velo = avg_velo,
    Max_Velo = max_velo,
    Disp     = disp
  )

json_monthly  <- toJSON(monthly_records,  dataframe = "rows", digits = 4, na = "null")
json_weekly   <- toJSON(weekly_records,   dataframe = "rows", digits = 4, na = "null")
json_schedule <- toJSON(schedule_records, dataframe = "rows", digits = 4, na = "null")

# ── 7. READ HTML TEMPLATE ────────────────────────────────────────────────────

html_template <- paste(
  readLines(TEMPLATE_FILE, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)

# ── 8. INJECT DATA INTO TEMPLATE ─────────────────────────────────────────────

html_out <- html_template
html_out <- str_replace_all(html_out, "<<JSON_MONTHLY>>",  as.character(json_monthly))
html_out <- str_replace_all(html_out, "<<JSON_WEEKLY>>",   as.character(json_weekly))
html_out <- str_replace_all(html_out, "<<JSON_SCHEDULE>>", as.character(json_schedule))
html_out <- str_replace_all(html_out, "<<REPORT_TITLE>>",  REPORT_TITLE)
html_out <- str_replace_all(html_out, "<<TEAM_LABEL>>",    TEAM_LABEL)
html_out <- str_replace_all(html_out, "<<THRESH_OK>>",     as.character(THRESH_OK))
html_out <- str_replace_all(html_out, "<<THRESH_WARN>>",   as.character(THRESH_WARN))

# ── 9. WRITE FILE ────────────────────────────────────────────────────────────

writeLines(enc2utf8(html_out), OUTPUT_FILE, useBytes = TRUE)
cat("\nReport written to:", normalizePath(OUTPUT_FILE), "\n")

# ── 10. OPEN REPORT ──────────────────────────────────────────────────────────

browseURL(OUTPUT_FILE)