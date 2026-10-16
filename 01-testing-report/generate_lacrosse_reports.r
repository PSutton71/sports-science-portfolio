# =============================================================================
# OSU LACROSSE REPORT 
# =============================================================================

setwd("C:/Users/paisutt/OneDrive - Oklahoma A and M System/GRIP/MLax/Reports")

required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor",
  "readr", "jsonlite", "base64enc", "plotly", "htmlwidgets"
)

new_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))
options(scipen = 999)

input_file <- "lacrosse_data.xlsx"
template_file <- "osu_lacrosse_template.html"
logo_file <- "image-2.jpg"
phantom_file <- "phantom_pete.jpg"
output_dir <- "Lacrosse_Reports"

if (!file.exists(input_file)) {
  stop("Could not find lacrosse_data.xlsx in the Reports folder.")
}
if (!file.exists(template_file)) {
  stop("Could not find osu_lacrosse_template.html in the Reports folder.")
}
if (!file.exists(logo_file)) {
  stop("Could not find image-2.jpg in the Reports folder.")
}
if (!file.exists(phantom_file)) {
  stop("Could not find phantom_pete.jpg in the Reports folder.")
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Helpers
# =============================================================================

# Reads one sheet, tolerating a missing sheet or missing columns so one
# absent test doesn't stop every report. `cols` lists the (clean_names)
# columns the script uses from that sheet: any that are missing are added
# as empty columns with a warning, and a missing sheet becomes an empty
# table with those columns. Downstream, an absent test just means those
# athletes have no value for it, and every score re-weights around it.
available_sheets <- readxl::excel_sheets(input_file)
data_warnings <- character()

safe_read <- function(sheet, cols) {
  empty <- tibble::as_tibble(
    stats::setNames(rep(list(character()), length(cols)), cols)
  )
  
  if (!sheet %in% available_sheets) {
    msg <- paste0("Sheet '", sheet, "' not found -- its tests will be skipped.")
    warning(msg, call. = FALSE)
    data_warnings <<- c(data_warnings, msg)
    return(empty)
  }
  
  out <- readxl::read_excel(input_file, sheet = sheet, guess_max = 10000) |>
    janitor::clean_names()
  
  missing_cols <- setdiff(cols, names(out))
  if (length(missing_cols)) {
    msg <- paste0(
      "Sheet '", sheet, "' is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      " -- anything using them will be left blank."
    )
    warning(msg, call. = FALSE)
    data_warnings <<- c(data_warnings, msg)
    for (col in missing_cols) out[[col]] <- NA_character_
  }
  
  out
}

num <- function(x) suppressWarnings(as.numeric(x))
clean_name <- function(x) stringr::str_squish(as.character(x))

slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", trimws(x))
  x <- gsub("_+", "_", x)
  gsub("_$", "", x)
}

fmt_num <- function(x, digits = 0) {
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits, big.mark = ","))
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || all(is.na(x)) || all(x == "")) y else x
}

# Replaces literal template fields without allowing replacement content such as
# embedded Plotly JavaScript to be interpreted as a regular-expression replacement.
#
# fixed = TRUE already makes gsub() treat BOTH pattern and replacement as
# literal text, so backslashes in `replacement` need no escaping. Doubling
# them (as the previous version did) corrupted the embedded Plotly library
# and JSON payloads, which are full of legitimate single backslashes.
replace_literal <- function(text, pattern, replacement) {
  replacement <- as.character(replacement)
  
  gsub(
    pattern = pattern,
    replacement = replacement,
    x = text,
    fixed = TRUE
  )
}

validate_output_html <- function(page, report_name) {
  unresolved <- stringr::str_extract_all(page, "\\{\\{[A-Z0-9_]+\\}\\}")[[1]]
  
  if (length(unresolved) > 0) {
    stop(
      "Unreplaced template fields in ", report_name, ": ",
      paste(unique(unresolved), collapse = ", ")
    )
  }
  
  required_text <- c(
    "data:image/jpeg;base64,",
    "const performanceData =",
    "const upperData =",
    "const lowerData =",
    "const dsiExplosivenessData =",
    "Plotly.newPlot"
  )
  
  missing_text <- required_text[
    !vapply(required_text, function(x) grepl(x, page, fixed = TRUE), logical(1))
  ]
  
  if (length(missing_text) > 0) {
    stop(
      "Incomplete report ", report_name, ". Missing: ",
      paste(missing_text, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}

safe_pmax <- function(...) {
  z <- do.call(pmax, c(list(...), na.rm = TRUE))
  z[is.infinite(z)] <- NA_real_
  z
}

safe_pmin <- function(...) {
  z <- do.call(pmin, c(list(...), na.rm = TRUE))
  z[is.infinite(z)] <- NA_real_
  z
}

safe_row_mean <- function(...) {
  z <- rowMeans(cbind(...), na.rm = TRUE)
  z[is.nan(z)] <- NA_real_
  z
}

# Weighted mean that tolerates missing components: any NA input has its
# weight set to 0 for that row, and the remaining weights are implicitly
# renormalized (dividing by their own sum) rather than the whole score
# going NA just because one test wasn't collected. Only returns NA when
# every component is missing -- i.e. there's truly insufficient data.
safe_weighted_mean <- function(values, weights) {
  mat <- do.call(cbind, values)
  w <- matrix(weights, nrow = nrow(mat), ncol = length(weights), byrow = TRUE)
  w[is.na(mat)] <- 0
  mat[is.na(mat)] <- 0
  denom <- rowSums(w)
  ifelse(denom == 0, NA_real_, rowSums(mat * w) / denom)
}

# Percentiles are compressed into a 40-100 floor/ceiling so no athlete's
# report ever shows a 0th-percentile (or otherwise very low, alarming)
# number just from being last on the team -- the *relative* ranking is
# unchanged, only the numeric scale it's expressed on.
# Strength score weights, in order: IMTP, Biodex 60 deg/s, Grip.
# Must sum to 1 (they're renormalized anyway if a test is missing).
STRENGTH_WEIGHTS <- c(imtp = .50, biodex = .30, grip = .20)

percentile_score <- function(x, higher = TRUE) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)
  n <- sum(valid)
  
  if (n == 0) return(out)
  
  if (n == 1 || length(unique(x[valid])) == 1) {
    out[valid] <- 80
    return(out)
  }
  
  r <- rank(if (higher) x[valid] else -x[valid], ties.method = "average")
  out[valid] <- 40 + 60 * (r - 1) / (n - 1)
  out
}

asymmetry_pct <- function(left, right) {
  100 * abs(left - right) / safe_pmax(left, right)
}

# Names the weaker of two labeled raw values (e.g. "left hand"/"right hand").
# Returns "" when either side is missing or they're exactly tied, so
# downstream text can skip the clause entirely rather than print nothing.
weak_side <- function(left, right, left_label, right_label) {
  dplyr::case_when(
    is.na(left) | is.na(right) | left == right ~ "",
    left < right ~ left_label,
    TRUE ~ right_label
  )
}

# Same idea, but from a signed L-relative-to-right index rather than two
# raw values (used for CMJ/DJ, where only the index is reliably available).
# Convention: positive = left greater than right.
weak_side_signed <- function(signed_index, left_label, right_label) {
  dplyr::case_when(
    is.na(signed_index) | signed_index == 0 ~ "",
    signed_index > 0 ~ right_label,
    TRUE ~ left_label
  )
}

asym_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x <= 10 ~ "Green",
    x < 20 ~ "Yellow",
    TRUE ~ "Red"
  )
}

hq_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x >= 60 ~ "Green",
    x >= 50 ~ "Yellow",
    TRUE ~ "Red"
  )
}

# Hip Extension (hamstring-risk) test: two independent pass/fail criteria,
# each with its own yellow buffer, combined via worst_status() into one
# flag for the axis. Force is per-leg (the WEAKER leg is what's tested
# against 3.0 N/kg -- a single strong leg does not offset a weak one).
hip_force_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x >= 3.3 ~ "Green",
    x >= 3.0 ~ "Yellow",
    TRUE ~ "Red"
  )
}

hip_asym_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x <= 10 ~ "Green",
    x <= 15 ~ "Yellow",
    TRUE ~ "Red"
  )
}

neck_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x >= .60 & x <= .74 ~ "Green",
    x >= .50 & x < .60 ~ "Yellow",
    x > .74 & x <= .84 ~ "Yellow",
    TRUE ~ "Red"
  )
}

# Dynamic Strength Index = CMJ Peak Propulsive Force / IMTP Peak Force.
# Thresholds per staff guidance: <0.60 strength dominant, 0.60-0.80
# balanced, >0.80 power dominant / low strength.
classify_dsi <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x < 0.60 ~ "Strength Dominant",
    x <= 0.80 ~ "Balanced",
    TRUE ~ "Power Dominant"
  )
}

# Quadrant classification: DSI determines power-dominant vs. strength-
# dominant (split at 0.70, the center of the "Balanced" band), crossed
# with Wingate fatigue-resistance percentile (holds up well vs. fades
# fast, split at the 50th percentile).
classify_explosiveness <- function(dsi, fatigue_pct) {
  dplyr::case_when(
    is.na(dsi) | is.na(fatigue_pct) ~ "No data",
    dsi >= 0.70 & fatigue_pct >= 50 ~ "Power-Dominant, Holds Up Well",
    dsi >= 0.70 & fatigue_pct < 50 ~ "Power-Dominant, Fades Fast",
    dsi < 0.70 & fatigue_pct >= 50 ~ "Strength-Dominant, Holds Up Well",
    TRUE ~ "Strength-Dominant, Fades Fast"
  )
}

status_value <- function(x) {
  dplyr::case_when(
    x == "Green" ~ 0,
    x == "Yellow" ~ 50,
    x == "Red" ~ 100,
    TRUE ~ NA_real_
  )
}

# Green/Yellow/Red band for a composite readiness SCORE (0-100), matching
# the fixed bands used to color the team Readiness Leaderboard:
# > 80 Green, 70-80 Yellow, < 70 Red (mirrors readinessScoreColor() in the
# template, where exactly 80 is Yellow). Used for upper/lower/overall
# status so the hover labels agree with the bar colors. (Per-axis radar
# statuses still use each metric's own clinical thresholds.)
READINESS_GREEN_MIN <- 80
READINESS_YELLOW_MIN <- 70
readiness_band_status <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "No data",
    x > READINESS_GREEN_MIN ~ "Green",
    x >= READINESS_YELLOW_MIN ~ "Yellow",
    TRUE ~ "Red"
  )
}

worst_status <- function(x) {
  z <- status_value(x)
  if (all(is.na(z))) {
    "No data"
  } else {
    x[which.max(replace(z, is.na(z), -Inf))][1]
  }
}

# Readiness values point outward as they approach the desired range.
# Score 80 is the recommended ring / green threshold.
readiness_scale_asym <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x <= 10 ~ pmin(100, 80 + (10 - x) * 2),
    x < 20 ~ 80 - (x - 10) * 3,
    TRUE ~ pmax(0, 50 - (x - 20) * 2.5)
  )
}

readiness_scale_hq <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x >= 60 ~ pmin(100, 80 + (x - 60) * 1.5),
    x >= 50 ~ 50 + (x - 50) * 3,
    TRUE ~ pmax(0, x)
  )
}

readiness_scale_quad <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x >= 90 ~ pmin(100, 80 + (x - 90) * 2),
    x >= 80 ~ 50 + (x - 80) * 3,
    TRUE ~ pmax(0, x * .625)
  )
}

readiness_scale_neck <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x >= .60 & x <= .74 ~ 100 - abs(x - .67) / .07 * 10,
    x >= .50 & x < .60 ~ 50 + (x - .50) * 300,
    x > .74 & x <= .84 ~ 80 - (x - .74) * 300,
    x < .50 ~ pmax(0, x * 100),
    TRUE ~ pmax(0, 50 - (x - .84) * 200)
  )
}

# Hip Extension force sub-score: x is the WEAKER leg's force in N/kg of
# body mass. 3.0 N/kg is the injury-risk cutoff (score 50), 3.3 N/kg is
# the top of the yellow buffer (score 80) -- mirrors the shape of the
# other readiness_scale_* functions (80 = green threshold, 50 = yellow).
readiness_scale_hip_force <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x >= 3.3 ~ pmin(100, 80 + (x - 3.3) * 40),
    x >= 3.0 ~ 50 + (x - 3.0) * 100,
    TRUE ~ pmax(0, x / 3.0 * 50)
  )
}

# Hip Extension asymmetry sub-score: same shape as readiness_scale_asym,
# but with the 10%/15% buffer specific to this test rather than the 10%/20%
# used for the other asymmetry-based readiness axes.
readiness_scale_hip_asym <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x <= 10 ~ pmin(100, 80 + (10 - x) * 2),
    x < 15 ~ 80 - (x - 10) * 6,
    TRUE ~ pmax(0, 50 - (x - 15) * 2.5)
  )
}

best_each_date <- function(data, value_col, higher = TRUE) {
  if (!nrow(data)) return(data)
  
  data |>
    dplyr::filter(
      !is.na(name), name != "",
      !is.na(date),
      !is.na(.data[[value_col]])
    ) |>
    dplyr::group_by(name, date) |>
    dplyr::slice(if (higher) which.max(.data[[value_col]]) else which.min(.data[[value_col]])) |>
    dplyr::ungroup()
}

latest_best <- function(data, value_col, higher = TRUE) {
  best_each_date(data, value_col, higher) |>
    dplyr::group_by(name) |>
    dplyr::slice_max(date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

best_bilateral_dynamo <- function(data, movement_name, region_name = NULL) {
  z <- data |>
    dplyr::filter(tolower(movement) == tolower(movement_name))
  
  if (!is.null(region_name)) {
    z <- z |>
      dplyr::filter(tolower(body_region) == tolower(region_name))
  }
  
  z |>
    dplyr::filter(
      !is.na(l_max_force_n),
      !is.na(r_max_force_n)
    ) |>
    dplyr::mutate(
      combined_force_n = l_max_force_n + r_max_force_n,
      asymmetry = asymmetry_pct(l_max_force_n, r_max_force_n)
    ) |>
    latest_best("combined_force_n", TRUE)
}

as_json <- function(x) {
  jsonlite::toJSON(
    x,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    digits = 10
  )
}

# =============================================================================
# Read workbook
# =============================================================================

demographics <- safe_read("Demographics", c("name", "position", "height", "weight")) |>
  dplyr::transmute(
    name = clean_name(name),
    position = clean_name(position),
    # The sheet stores height in cm and weight in kg -- convert to inches
    # and pounds here, once, so every downstream use (display, DSI,
    # relative-strength calcs) works in the same units it always has.
    height_in = num(height) / 2.54,
    weight_lb = num(weight) * 2.20462262
  ) |>
  dplyr::filter(!is.na(name), name != "")

if (!nrow(demographics)) {
  stop("The Demographics sheet is missing or has no athletes -- it defines the roster, so reports can't be built without it.")
}

dynamo <- safe_read("Dynamo", c(
  "name", "date", "movement", "type", "body_region",
  "l_max_force_n", "r_max_force_n", "n_max_force_n"
)) |>
  dplyr::mutate(
    name = clean_name(name),
    date = as.Date(date),
    movement = clean_name(movement),
    type = clean_name(type),
    body_region = clean_name(body_region),
    l_max_force_n = num(l_max_force_n),
    r_max_force_n = num(r_max_force_n),
    n_max_force_n = num(n_max_force_n)
  ) |>
  dplyr::filter(!is.na(name), name != "")

cmj_raw <- safe_read("CMJ", c(
  "name", "date", "tags", "jump_height", "peak_propulsive_force",
  "l_r_peak_propulsive_force", "left_force_at_peak_propulsive_force",
  "right_force_at_peak_propulsive_force"
)) |>
  dplyr::mutate(
    name = clean_name(name),
    date = as.Date(date),
    tags = clean_name(tags),
    is_prone = !is.na(tags) & stringr::str_detect(tolower(tags), "prone"),
    jump_height = num(jump_height),
    peak_propulsive_force = num(peak_propulsive_force),
    lr_peak_propulsive_force = num(l_r_peak_propulsive_force),
    left_force_at_peak_propulsive_force = num(left_force_at_peak_propulsive_force),
    right_force_at_peak_propulsive_force = num(right_force_at_peak_propulsive_force)
  ) |>
  # ---------------------------------------------------------------------
# DATA FIX: on Plyo Push-Up trials (rows tagged "Prone"), the two force
# plates were mislabeled at collection -- the column recorded as "Left"
# actually measured the athlete's right hand, and vice versa. Swap the
# per-hand force values (and flip the sign of the L|R ratio, since it
# is defined as left relative to right) back to their correct side for
# every Prone-tagged row, before anything downstream uses them. This
# does not affect the *magnitude* of push-up asymmetry (it's symmetric
# in L and R either way), but it makes the per-hand values themselves
# correct for any left/right-specific use.
# ---------------------------------------------------------------------
dplyr::mutate(
  corrected_left_force = dplyr::if_else(
    is_prone, right_force_at_peak_propulsive_force, left_force_at_peak_propulsive_force
  ),
  corrected_right_force = dplyr::if_else(
    is_prone, left_force_at_peak_propulsive_force, right_force_at_peak_propulsive_force
  ),
  lr_peak_propulsive_force = dplyr::if_else(
    is_prone, -1 * lr_peak_propulsive_force, lr_peak_propulsive_force
  )
) |>
  dplyr::mutate(
    left_force_at_peak_propulsive_force = corrected_left_force,
    right_force_at_peak_propulsive_force = corrected_right_force
  ) |>
  dplyr::select(-corrected_left_force, -corrected_right_force) |>
  dplyr::mutate(
    protocol = dplyr::if_else(is_prone, "Plyometric Push-Up", "CMJ"),
    # Signed (not absolute) L|R index, kept alongside the absolute asymmetry
    # magnitude so downstream text can say *which side* is lower, not just
    # *how much* asymmetry there is. Convention: positive = left greater
    # than right (consistent with the Prone/push-up sign flip above, which
    # only corrects for the swapped force-plate wiring on those rows).
    propulsive_lr_signed = lr_peak_propulsive_force,
    propulsive_asymmetry_pct = abs(lr_peak_propulsive_force),
    pushup_left_n = dplyr::if_else(is_prone, left_force_at_peak_propulsive_force, NA_real_),
    pushup_right_n = dplyr::if_else(is_prone, right_force_at_peak_propulsive_force, NA_real_),
    cmj_left_n = dplyr::if_else(!is_prone, left_force_at_peak_propulsive_force, NA_real_),
    cmj_right_n = dplyr::if_else(!is_prone, right_force_at_peak_propulsive_force, NA_real_)
  ) |>
  dplyr::filter(!is.na(name), name != "")

dj_raw <- safe_read("DJ", c(
  "name", "date", "rsi",
  "left_force_at_peak_landing_force", "right_force_at_peak_landing_force"
)) |>
  dplyr::mutate(
    name = clean_name(name),
    date = as.Date(date),
    rsi = num(rsi),
    dj_left_n = num(left_force_at_peak_landing_force),
    dj_right_n = num(right_force_at_peak_landing_force),
    landing_asymmetry_pct = asymmetry_pct(dj_left_n, dj_right_n)
  ) |>
  dplyr::filter(!is.na(name), name != "")

imtp_raw <- safe_read("IMTP", c("name", "date", "peak_force")) |>
  dplyr::mutate(
    name = clean_name(name),
    date = as.Date(date),
    peak_force = num(peak_force)
  ) |>
  dplyr::filter(!is.na(name), name != "")

biodex <- safe_read("BioDex", c(
  "patient_id", "date", "speed_deg_s",
  "extension_peak_torque_l_n_m", "extension_peak_torque_r_n_m",
  "extension_peak_torque_deficit_percent",
  "flexion_peak_torque_l_n_m", "flexion_peak_torque_r_n_m"
)) |>
  dplyr::mutate(
    name = clean_name(patient_id),
    date = as.Date(date),
    speed = num(speed_deg_s),
    ext_l = num(extension_peak_torque_l_n_m),
    ext_r = num(extension_peak_torque_r_n_m),
    ext_deficit = num(extension_peak_torque_deficit_percent),
    flex_l = num(flexion_peak_torque_l_n_m),
    flex_r = num(flexion_peak_torque_r_n_m),
    hq_left_pct = 100 * flex_l / ext_l,
    hq_right_pct = 100 * flex_r / ext_r,
    quad_symmetry_pct = 100 - abs(ext_deficit),
    combined_peak_torque = ext_l + ext_r + flex_l + flex_r
  ) |>
  dplyr::filter(!is.na(name), name != "", !is.na(speed)) |>
  dplyr::group_by(name, speed) |>
  dplyr::slice_max(date, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

wingate <- safe_read("Wingate", c(
  "participant_id", "peak_power", "peak_power_body_mass", "rate_of_fatigue"
)) |>
  dplyr::mutate(
    name = clean_name(participant_id),
    peak_power = num(peak_power),
    peak_power_bodymass = num(peak_power_body_mass),
    rate_of_fatigue = num(rate_of_fatigue)
  ) |>
  dplyr::filter(!is.na(name), name != "")

# Expected headers after janitor::clean_names():
# Date, Name, Agility 1, Agility 2, Acc 1, Acc 2, Sprint 1, Sprint 2
# become: date, name, agility_1, agility_2, acc_1, acc_2, sprint_1, sprint_2.
speed <- safe_read("Speed", c(
  "date", "name", "agility_1", "agility_2", "acc_1", "acc_2", "sprint_1", "sprint_2"
)) |>
  dplyr::mutate(
    name = clean_name(name),
    date = as.Date(date),
    agility_1 = num(agility_1),
    agility_2 = num(agility_2),
    acc_1 = num(acc_1),
    acc_2 = num(acc_2),
    sprint_1 = num(sprint_1),
    sprint_2 = num(sprint_2),
    best_agility = safe_pmin(agility_1, agility_2),
    best_acceleration = safe_pmin(acc_1, acc_2),
    best_sprint = safe_pmin(sprint_1, sprint_2)
  ) |>
  dplyr::filter(!is.na(name), name != "")

# =============================================================================
# Select best valid data for each athlete
# =============================================================================

cmj <- cmj_raw |>
  dplyr::filter(protocol == "CMJ", !is.na(jump_height)) |>
  latest_best("jump_height", TRUE)

pushup <- cmj_raw |>
  dplyr::filter(protocol == "Plyometric Push-Up", !is.na(peak_propulsive_force)) |>
  latest_best("peak_propulsive_force", TRUE)

dj <- dj_raw |>
  dplyr::filter(!is.na(rsi)) |>
  latest_best("rsi", TRUE)

imtp <- latest_best(imtp_raw, "peak_force", TRUE)

# ---------------------------------------------------------------------
# Verified against the corrected Dynamo export:
#   - Grip Squeeze, Pinch Grip, Trunk Rotation, Hip Extension: all
#     genuinely bilateral now (L Max Force and R Max Force both
#     populated) -> best_bilateral_dynamo() handles these directly.
#   - "Extension" is used as the Movement for BOTH Hip Extension and
#     Neck Extension, so Hip needs a Body Region filter to avoid
#     picking up Neck rows (and vice versa).
#   - Neck (Flexion = Supine, Extension = Prone) is a single-value test
#     whose result is recorded in the N (neutral) Max Force column, not
#     L or R -- unlike every bilateral test above.
# ---------------------------------------------------------------------

grip <- best_bilateral_dynamo(dynamo, "Grip Squeeze")
pinch <- best_bilateral_dynamo(dynamo, "Pinch Grip")
hip <- best_bilateral_dynamo(dynamo, "Extension", "Hip")
trunk <- best_bilateral_dynamo(dynamo, "Rotation", "Trunk")

neck_flex <- dynamo |>
  dplyr::filter(
    tolower(movement) == "flexion",
    tolower(body_region) == "neck",
    !is.na(n_max_force_n)
  ) |>
  latest_best("n_max_force_n", TRUE) |>
  dplyr::transmute(name, neck_flexion_n = n_max_force_n)

neck_ext <- dynamo |>
  dplyr::filter(
    tolower(movement) == "extension",
    tolower(body_region) == "neck",
    !is.na(n_max_force_n)
  ) |>
  latest_best("n_max_force_n", TRUE) |>
  dplyr::transmute(name, neck_extension_n = n_max_force_n)

# Each speed metric is taken from the most recent session where THAT
# metric was recorded -- so a latest session with only agility times
# doesn't wipe out acceleration/sprint from an earlier session.
latest_speed_metric <- function(metric) {
  speed |>
    dplyr::filter(!is.na(date), !is.na(.data[[metric]])) |>
    dplyr::group_by(name) |>
    dplyr::filter(date == max(date)) |>
    dplyr::summarise(!!metric := min(.data[[metric]]), .groups = "drop")
}

speed_latest <- dplyr::tibble(name = unique(speed$name)) |>
  dplyr::left_join(latest_speed_metric("best_acceleration"), by = "name") |>
  dplyr::left_join(latest_speed_metric("best_sprint"), by = "name") |>
  dplyr::left_join(latest_speed_metric("best_agility"), by = "name")

wingate_latest <- wingate |>
  dplyr::group_by(name) |>
  dplyr::summarise(
    wingate_peak_power_kg = max(peak_power_bodymass, na.rm = TRUE),
    fatigue_index = min(rate_of_fatigue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      -name,
      ~ifelse(is.infinite(.x), NA_real_, .x)
    )
  )

slow_biodex <- biodex |>
  dplyr::filter(speed == 60)

# full_join (not inner_join): if only flexion or only extension was
# tested, that value still counts toward the Neck Strength radar axis;
# only the flexion:extension ratio needs both.
neck_data <- neck_flex |>
  dplyr::full_join(neck_ext, by = "name") |>
  dplyr::mutate(neck_ratio = neck_flexion_n / neck_extension_n)

# =============================================================================
# Scores and radar inputs
# =============================================================================

scores <- demographics |>
  dplyr::left_join(
    cmj |>
      dplyr::transmute(
        name,
        cmj_height_m = jump_height,
        cmj_asymmetry_pct = propulsive_asymmetry_pct,
        cmj_lr_signed = propulsive_lr_signed,
        cmj_left_n,
        cmj_right_n,
        dsi_cmj_force_n = peak_propulsive_force
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    pushup |>
      dplyr::transmute(
        name,
        pushup_peak_force_n = peak_propulsive_force,
        pushup_asymmetry_pct = propulsive_asymmetry_pct,
        pushup_left_n,
        pushup_right_n
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    dj |>
      dplyr::transmute(
        name,
        dj_rsi = rsi,
        dj_landing_asymmetry_pct = landing_asymmetry_pct,
        dj_left_n,
        dj_right_n
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    imtp |>
      dplyr::transmute(name, imtp_peak_force_n = peak_force),
    by = "name"
  ) |>
  dplyr::left_join(
    grip |>
      dplyr::transmute(
        name,
        grip_left_n = l_max_force_n,
        grip_right_n = r_max_force_n,
        grip_asymmetry_pct = asymmetry
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    pinch |>
      dplyr::transmute(
        name,
        pinch_left_n = l_max_force_n,
        pinch_right_n = r_max_force_n,
        pinch_asymmetry_pct = asymmetry
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    hip |>
      dplyr::transmute(
        name,
        hip_left_n = l_max_force_n,
        hip_right_n = r_max_force_n,
        hip_asymmetry_pct = asymmetry
      ),
    by = "name"
  ) |>
  dplyr::left_join(
    trunk |>
      dplyr::transmute(
        name,
        trunk_output_n = combined_force_n / 2,
        trunk_left_n = l_max_force_n,
        trunk_right_n = r_max_force_n,
        trunk_asymmetry_pct = asymmetry
      ),
    by = "name"
  ) |>
  dplyr::left_join(neck_data, by = "name") |>
  dplyr::left_join(
    slow_biodex |>
      dplyr::transmute(
        name,
        slow_hq_average_pct = safe_row_mean(hq_left_pct, hq_right_pct),
        slow_quad_symmetry_pct = quad_symmetry_pct,
        biodex60_combined_torque = combined_peak_torque,
        quad_left_torque = ext_l,
        quad_right_torque = ext_r,
        hq_flex_avg_torque = (flex_l + flex_r) / 2,
        hq_ext_avg_torque = (ext_l + ext_r) / 2
      ),
    by = "name"
  ) |>
  dplyr::left_join(speed_latest, by = "name") |>
  dplyr::left_join(wingate_latest, by = "name") |>
  dplyr::mutate(
    body_mass_kg = weight_lb * 0.45359237,
    imtp_relative_force = imtp_peak_force_n / body_mass_kg,
    biodex60_relative_torque = biodex60_combined_torque / body_mass_kg,
    hip_left_n_per_kg = hip_left_n / body_mass_kg,
    hip_right_n_per_kg = hip_right_n / body_mass_kg,
    # The weaker leg is what's tested against the 3.0 N/kg injury-risk
    # cutoff -- a strong leg does not offset a weak one for this test.
    hip_min_force_per_kg = safe_pmin(hip_left_n_per_kg, hip_right_n_per_kg),
    cmj_pct = percentile_score(cmj_height_m, TRUE),
    dj_pct = percentile_score(dj_rsi, TRUE),
    pushup_pct = percentile_score(pushup_peak_force_n, TRUE),
    wingate_power_pct = percentile_score(wingate_peak_power_kg, TRUE),
    fatigue_pct = percentile_score(fatigue_index, FALSE),
    imtp_pct = percentile_score(imtp_relative_force, TRUE),
    grip_left_pct = percentile_score(grip_left_n, TRUE),
    grip_right_pct = percentile_score(grip_right_n, TRUE),
    pinch_left_pct = percentile_score(pinch_left_n, TRUE),
    pinch_right_pct = percentile_score(pinch_right_n, TRUE),
    neck_flex_pct = percentile_score(neck_flexion_n, TRUE),
    neck_ext_pct = percentile_score(neck_extension_n, TRUE),
    trunk_pct = percentile_score(trunk_output_n, TRUE),
    acc_pct = percentile_score(best_acceleration, FALSE),
    sprint_pct = percentile_score(best_sprint, FALSE),
    agility_pct = percentile_score(best_agility, FALSE),
    power_score = safe_row_mean(cmj_pct, dj_pct, pushup_pct, wingate_power_pct),
    # Strength = IMTP 50% / Biodex 60 deg/s 30% / Grip 20% (see
    # STRENGTH_WEIGHTS). Missing tests drop out and the rest renormalize.
    strength_score = safe_weighted_mean(
      list(
        imtp_pct,
        percentile_score(biodex60_relative_torque, TRUE),
        safe_row_mean(grip_left_pct, grip_right_pct)
      ),
      STRENGTH_WEIGHTS
    ),
    speed_score = safe_row_mean(acc_pct, sprint_pct),
    agility_score = agility_pct,
    composite_score = safe_weighted_mean(
      list(power_score, strength_score, speed_score, agility_score),
      c(.30, .30, .20, .20)
    ),
    radar_lower_power = safe_row_mean(cmj_pct, dj_pct, wingate_power_pct),
    radar_pushup = pushup_pct,
    radar_fatigue = fatigue_pct,
    radar_imtp = imtp_pct,
    radar_hand = safe_row_mean(grip_left_pct, grip_right_pct, pinch_left_pct, pinch_right_pct),
    radar_neck = safe_row_mean(neck_flex_pct, neck_ext_pct),
    radar_core = trunk_pct,
    radar_speed = safe_row_mean(acc_pct, sprint_pct),
    radar_agility = agility_pct,
    dsi = dplyr::if_else(
      !is.na(dsi_cmj_force_n) & !is.na(imtp_peak_force_n) & imtp_peak_force_n > 0,
      dsi_cmj_force_n / imtp_peak_force_n,
      NA_real_
    ),
    dsi_status = classify_dsi(dsi),
    explosiveness_quadrant = classify_explosiveness(dsi, fatigue_pct)
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  )

readiness <- scores |>
  dplyr::mutate(
    upper_plyo = readiness_scale_asym(pushup_asymmetry_pct),
    upper_trunk = readiness_scale_asym(trunk_asymmetry_pct),
    upper_neck = readiness_scale_neck(neck_ratio),
    upper_grip = readiness_scale_asym(grip_asymmetry_pct),
    upper_pinch = readiness_scale_asym(pinch_asymmetry_pct),
    lower_cmj = readiness_scale_asym(cmj_asymmetry_pct),
    lower_dj = readiness_scale_asym(dj_landing_asymmetry_pct),
    lower_hq = readiness_scale_hq(slow_hq_average_pct),
    lower_quad = readiness_scale_quad(slow_quad_symmetry_pct),
    # Hip Extension (hamstring-risk) axis: worse of the force sub-score
    # (weaker leg vs. the 3.0 N/kg cutoff) and the asymmetry sub-score,
    # so a pass on one dimension can't mask a fail on the other.
    lower_hip_ext = safe_pmin(
      readiness_scale_hip_force(hip_min_force_per_kg),
      readiness_scale_hip_asym(hip_asymmetry_pct)
    ),
    upper_readiness = safe_weighted_mean(
      list(upper_plyo, upper_trunk, upper_neck, upper_grip, upper_pinch),
      c(.30, .30, .20, .10, .10)
    ),
    lower_readiness = safe_weighted_mean(
      list(lower_cmj, lower_dj, lower_hq, lower_quad, lower_hip_ext),
      c(.20, .20, .20, .20, .20)
    ),
    readiness_score = safe_row_mean(upper_readiness, lower_readiness),
    upper_status = readiness_band_status(upper_readiness),
    # Combined Green/Yellow/Red flag for the Hip Extension radar axis
    # (worse of the force and asymmetry checks). Reused by lower_radar.
    hip_ext_status = vapply(
      seq_len(dplyr::n()),
      function(i) worst_status(c(
        hip_force_status(hip_min_force_per_kg[i]),
        hip_asym_status(hip_asymmetry_pct[i])
      )),
      character(1)
    ),
    lower_status = readiness_band_status(lower_readiness),
    overall_status = readiness_band_status(readiness_score),
    # Which side is weaker for each bilateral readiness metric, for use in
    # both the radar hover data and the athlete-facing readiness write-up.
    weak_plyo = weak_side(pushup_left_n, pushup_right_n, "left hand", "right hand"),
    weak_trunk = weak_side(trunk_left_n, trunk_right_n, "left side", "right side"),
    weak_grip = weak_side(grip_left_n, grip_right_n, "left hand", "right hand"),
    weak_pinch = weak_side(pinch_left_n, pinch_right_n, "left hand", "right hand"),
    weak_cmj = weak_side_signed(cmj_lr_signed, "left leg", "right leg"),
    weak_dj = weak_side(dj_left_n, dj_right_n, "left leg", "right leg"),
    weak_quad = weak_side(quad_left_torque, quad_right_torque, "left leg", "right leg"),
    weak_hip_ext = weak_side(hip_left_n, hip_right_n, "left leg", "right leg")
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  ) |>
  dplyr::mutate(
    raw_lower_power = paste0(
      "CMJ: ", fmt_num(cmj_height_m * 100, 1), " cm · ",
      "DJ RSI: ", fmt_num(dj_rsi, 2), " · ",
      "Wingate: ", fmt_num(wingate_peak_power_kg, 1), " W/kg"
    ),
    raw_pushup = paste0("Push-Up Force: ", fmt_num(pushup_peak_force_n, 0), " N"),
    raw_fatigue = paste0("Fatigue Index: ", fmt_num(fatigue_index, 1), "%"),
    raw_imtp = paste0("Peak Force: ", fmt_num(imtp_peak_force_n, 0), " N"),
    raw_hand = paste0(
      "Grip L: ", fmt_num(grip_left_n, 0), " N · Grip R: ", fmt_num(grip_right_n, 0), " N · ",
      "Pinch L: ", fmt_num(pinch_left_n, 0), " N · Pinch R: ", fmt_num(pinch_right_n, 0), " N"
    ),
    raw_neck = paste0(
      "Flexion: ", fmt_num(neck_flexion_n, 0), " N · Extension: ", fmt_num(neck_extension_n, 0), " N"
    ),
    raw_core = paste0("Left: ", fmt_num(trunk_left_n, 0), " N · Right: ", fmt_num(trunk_right_n, 0), " N"),
    raw_speed = paste0(
      "Acceleration: ", fmt_num(best_acceleration, 2), " s · Sprint: ", fmt_num(best_sprint, 2), " s"
    ),
    raw_agility = paste0("Best Time: ", fmt_num(best_agility, 2), " s")
  )

# =============================================================================
# Data coverage check
# =============================================================================
# Lists, per athlete, which tests had no usable data. Scores are still
# produced -- missing tests simply drop out and the remaining ones are
# re-weighted -- but this shows staff exactly what each score was built
# from. Printed to the console and saved as Data_Coverage.csv.

coverage_checks <- list(
  "Body weight" = ~weight_lb,
  "CMJ" = ~cmj_height_m,
  "Drop Jump" = ~dj_rsi,
  "Plyo Push-Up" = ~pushup_peak_force_n,
  "Wingate" = ~wingate_peak_power_kg,
  "IMTP" = ~imtp_peak_force_n,
  "Biodex 60" = ~biodex60_combined_torque,
  "Grip" = ~grip_left_n,
  "Pinch" = ~pinch_left_n,
  "Neck" = ~dplyr::coalesce(neck_flexion_n, neck_extension_n),
  "Trunk Rotation" = ~trunk_left_n,
  "Hip Extension" = ~hip_left_n,
  "Acceleration" = ~best_acceleration,
  "Sprint" = ~best_sprint,
  "Agility" = ~best_agility
)

data_coverage <- readiness |>
  dplyr::select(name) |>
  dplyr::mutate(
    missing_tests = vapply(seq_len(dplyr::n()), function(i) {
      row <- readiness[i, ]
      miss <- names(coverage_checks)[vapply(
        coverage_checks,
        function(f) is.na(rlang::eval_tidy(rlang::f_rhs(f), row)),
        logical(1)
      )]
      paste(miss, collapse = ", ")
    }, character(1)),
    n_missing = ifelse(missing_tests == "", 0L, lengths(strsplit(missing_tests, ", ")))
  )

readr::write_csv(data_coverage, file.path(output_dir, "Data_Coverage.csv"))

if (length(data_warnings)) {
  message("\nWORKBOOK WARNINGS")
  for (w in data_warnings) message("  - ", w)
}

gaps <- dplyr::filter(data_coverage, n_missing > 0)
if (nrow(gaps)) {
  message("\nDATA GAPS (scores re-weighted around these):")
  for (i in seq_len(nrow(gaps))) {
    message("  - ", gaps$name[i], ": ", gaps$missing_tests[i])
  }
  if ("Body weight" %in% unlist(strsplit(gaps$missing_tests, ", "))) {
    message("  Note: no body weight means IMTP, Biodex and Hip Extension can't be scaled per kg, so those are left blank for that athlete.")
  }
} else {
  message("\nData coverage: every athlete has every test.")
}

performance_radar <- readiness |>
  dplyr::select(
    name,
    radar_lower_power,
    radar_pushup,
    radar_fatigue,
    radar_imtp,
    radar_hand,
    radar_neck,
    radar_core,
    radar_speed,
    radar_agility,
    raw_lower_power,
    raw_pushup,
    raw_fatigue,
    raw_imtp,
    raw_hand,
    raw_neck,
    raw_core,
    raw_speed,
    raw_agility
  ) |>
  tidyr::pivot_longer(
    cols = c(
      radar_lower_power, radar_pushup, radar_fatigue, radar_imtp, radar_hand,
      radar_neck, radar_core, radar_speed, radar_agility
    ),
    names_to = "key",
    values_to = "score"
  ) |>
  dplyr::mutate(
    axis = dplyr::recode(
      key,
      radar_lower_power = "Lower-Body<br>Power",
      radar_pushup = "Plyometric<br>Push-Up",
      radar_fatigue = "Fatigue<br>Resistance",
      radar_imtp = "IMTP",
      radar_hand = "Hand<br>Strength",
      radar_neck = "Neck<br>Strength",
      radar_core = "Core<br>Strength",
      radar_speed = "Field<br>Speed",
      radar_agility = "Agility"
    ),
    raw_display = dplyr::case_when(
      key == "radar_lower_power" ~ raw_lower_power,
      key == "radar_pushup" ~ raw_pushup,
      key == "radar_fatigue" ~ raw_fatigue,
      key == "radar_imtp" ~ raw_imtp,
      key == "radar_hand" ~ raw_hand,
      key == "radar_neck" ~ raw_neck,
      key == "radar_core" ~ raw_core,
      key == "radar_speed" ~ raw_speed,
      key == "radar_agility" ~ raw_agility,
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(name, axis, score, raw_display)

# One row per athlete per upper-body readiness category.
upper_radar <- readiness |>
  dplyr::select(
    name,
    upper_plyo,
    upper_trunk,
    upper_neck,
    upper_grip,
    upper_pinch,
    pushup_asymmetry_pct,
    trunk_asymmetry_pct,
    neck_ratio,
    grip_asymmetry_pct,
    pinch_asymmetry_pct,
    pushup_left_n,
    pushup_right_n,
    trunk_left_n,
    trunk_right_n,
    neck_flexion_n,
    neck_extension_n,
    grip_left_n,
    grip_right_n,
    pinch_left_n,
    pinch_right_n,
    weak_plyo,
    weak_trunk,
    weak_grip,
    weak_pinch
  ) |>
  tidyr::pivot_longer(
    cols = c(upper_plyo, upper_trunk, upper_neck, upper_grip, upper_pinch),
    names_to = "key",
    values_to = "score"
  ) |>
  dplyr::mutate(
    axis = dplyr::recode(
      key,
      upper_plyo = "Plyometric Push-Up<br>Asymmetry",
      upper_trunk = "Trunk Rotation<br>Asymmetry",
      upper_neck = "Neck Flex :<br>Extension",
      upper_grip = "Grip Squeeze<br>Asymmetry",
      upper_pinch = "Pinch Grip<br>Asymmetry"
    ),
    status = dplyr::case_when(
      key == "upper_plyo" ~ asym_status(pushup_asymmetry_pct),
      key == "upper_trunk" ~ asym_status(trunk_asymmetry_pct),
      key == "upper_neck" ~ neck_status(neck_ratio),
      key == "upper_grip" ~ asym_status(grip_asymmetry_pct),
      key == "upper_pinch" ~ asym_status(pinch_asymmetry_pct),
      TRUE ~ "No data"
    ),
    raw1_text = dplyr::case_when(
      key == "upper_plyo" ~ paste0("Left Hand: ", fmt_num(pushup_left_n, 0), " N"),
      key == "upper_trunk" ~ paste0("Left Side: ", fmt_num(trunk_left_n, 0), " N"),
      key == "upper_neck" ~ paste0("Flexion: ", fmt_num(neck_flexion_n, 0), " N"),
      key == "upper_grip" ~ paste0("Left Hand: ", fmt_num(grip_left_n, 0), " N"),
      key == "upper_pinch" ~ paste0("Left Hand: ", fmt_num(pinch_left_n, 0), " N"),
      TRUE ~ ""
    ),
    raw2_text = dplyr::case_when(
      key == "upper_plyo" ~ paste0("Right Hand: ", fmt_num(pushup_right_n, 0), " N"),
      key == "upper_trunk" ~ paste0("Right Side: ", fmt_num(trunk_right_n, 0), " N"),
      key == "upper_neck" ~ paste0("Extension: ", fmt_num(neck_extension_n, 0), " N"),
      key == "upper_grip" ~ paste0("Right Hand: ", fmt_num(grip_right_n, 0), " N"),
      key == "upper_pinch" ~ paste0("Right Hand: ", fmt_num(pinch_right_n, 0), " N"),
      TRUE ~ ""
    ),
    raw_metric_text = dplyr::case_when(
      key == "upper_plyo" ~ paste0("Asymmetry: ", fmt_num(pushup_asymmetry_pct, 1), "%"),
      key == "upper_trunk" ~ paste0("Asymmetry: ", fmt_num(trunk_asymmetry_pct, 1), "%"),
      key == "upper_neck" ~ paste0("Flexion:Extension Ratio: ", fmt_num(neck_ratio, 2)),
      key == "upper_grip" ~ paste0("Asymmetry: ", fmt_num(grip_asymmetry_pct, 1), "%"),
      key == "upper_pinch" ~ paste0("Asymmetry: ", fmt_num(pinch_asymmetry_pct, 1), "%"),
      TRUE ~ ""
    ),
    weak_side = dplyr::case_when(
      key == "upper_plyo" ~ weak_plyo,
      key == "upper_trunk" ~ weak_trunk,
      key == "upper_grip" ~ weak_grip,
      key == "upper_pinch" ~ weak_pinch,
      TRUE ~ ""
    )
  ) |>
  dplyr::select(name, axis, score, status, raw1_text, raw2_text, raw_metric_text, weak_side)

# One row per athlete per lower-body readiness category.
lower_radar <- readiness |>
  dplyr::select(
    name,
    lower_cmj,
    lower_dj,
    lower_hq,
    lower_quad,
    lower_hip_ext,
    cmj_asymmetry_pct,
    dj_landing_asymmetry_pct,
    slow_hq_average_pct,
    slow_quad_symmetry_pct,
    hip_asymmetry_pct,
    hip_min_force_per_kg,
    cmj_left_n,
    cmj_right_n,
    dj_left_n,
    dj_right_n,
    hq_flex_avg_torque,
    hq_ext_avg_torque,
    quad_left_torque,
    quad_right_torque,
    hip_left_n,
    hip_right_n,
    hip_left_n_per_kg,
    hip_right_n_per_kg,
    hip_ext_status,
    weak_cmj,
    weak_dj,
    weak_quad,
    weak_hip_ext
  ) |>
  tidyr::pivot_longer(
    cols = c(lower_cmj, lower_dj, lower_hq, lower_quad, lower_hip_ext),
    names_to = "key",
    values_to = "score"
  ) |>
  dplyr::mutate(
    axis = dplyr::recode(
      key,
      lower_cmj = "CMJ<br>Asymmetry",
      lower_dj = "Drop Jump Landing<br>Asymmetry",
      lower_hq = "H:Q Ratio<br>Strength Balance",
      lower_quad = "Quadriceps<br>Symmetry",
      lower_hip_ext = "Hamstring<br>Force & Symmetry"
    ),
    status = dplyr::case_when(
      key == "lower_cmj" ~ asym_status(cmj_asymmetry_pct),
      key == "lower_dj" ~ asym_status(dj_landing_asymmetry_pct),
      key == "lower_hq" ~ hq_status(slow_hq_average_pct),
      key == "lower_quad" ~ asym_status(100 - slow_quad_symmetry_pct),
      key == "lower_hip_ext" ~ hip_ext_status,
      TRUE ~ "No data"
    ),
    raw1_text = dplyr::case_when(
      key == "lower_cmj" ~ paste0("Left Leg: ", fmt_num(cmj_left_n, 0), " N"),
      key == "lower_dj" ~ paste0("Left Leg: ", fmt_num(dj_left_n, 0), " N"),
      key == "lower_hq" ~ paste0("Hamstring (Flexion): ", fmt_num(hq_flex_avg_torque, 0), " N\u00b7m"),
      key == "lower_quad" ~ paste0("Left Leg: ", fmt_num(quad_left_torque, 0), " N\u00b7m"),
      key == "lower_hip_ext" ~ paste0(
        "Left Leg: ", fmt_num(hip_left_n, 0), " N (", fmt_num(hip_left_n_per_kg, 2), " N/kg)"
      ),
      TRUE ~ ""
    ),
    raw2_text = dplyr::case_when(
      key == "lower_cmj" ~ paste0("Right Leg: ", fmt_num(cmj_right_n, 0), " N"),
      key == "lower_dj" ~ paste0("Right Leg: ", fmt_num(dj_right_n, 0), " N"),
      key == "lower_hq" ~ paste0("Quad (Extension): ", fmt_num(hq_ext_avg_torque, 0), " N\u00b7m"),
      key == "lower_quad" ~ paste0("Right Leg: ", fmt_num(quad_right_torque, 0), " N\u00b7m"),
      key == "lower_hip_ext" ~ paste0(
        "Right Leg: ", fmt_num(hip_right_n, 0), " N (", fmt_num(hip_right_n_per_kg, 2), " N/kg)"
      ),
      TRUE ~ ""
    ),
    raw_metric_text = dplyr::case_when(
      key == "lower_cmj" ~ paste0("Asymmetry: ", fmt_num(cmj_asymmetry_pct, 1), "%"),
      key == "lower_dj" ~ paste0("Asymmetry: ", fmt_num(dj_landing_asymmetry_pct, 1), "%"),
      key == "lower_hq" ~ paste0("H:Q Ratio: ", fmt_num(slow_hq_average_pct, 1), "%"),
      key == "lower_quad" ~ paste0("Symmetry: ", fmt_num(slow_quad_symmetry_pct, 1), "%"),
      key == "lower_hip_ext" ~ paste0(
        "Weaker Leg: ", fmt_num(hip_min_force_per_kg, 2), " N/kg (risk threshold 3.0) \u00b7 ",
        "Asymmetry: ", fmt_num(hip_asymmetry_pct, 1), "% (risk threshold 10%)"
      ),
      TRUE ~ ""
    ),
    weak_side = dplyr::case_when(
      key == "lower_cmj" ~ weak_cmj,
      key == "lower_dj" ~ weak_dj,
      key == "lower_quad" ~ weak_quad,
      key == "lower_hip_ext" ~ weak_hip_ext,
      TRUE ~ ""
    )
  ) |>
  dplyr::select(name, axis, score, status, raw1_text, raw2_text, raw_metric_text, weak_side)

# Validation: every athlete must yield five upper and four lower readiness rows.
upper_counts <- upper_radar |>
  dplyr::count(name, name = "n_upper")

lower_counts <- lower_radar |>
  dplyr::count(name, name = "n_lower")

if (any(upper_counts$n_upper != 5)) {
  stop("Upper-radar construction failed: every athlete must have exactly 5 readiness points.")
}

if (any(lower_counts$n_lower != 5)) {
  stop("Lower-radar construction failed: every athlete must have exactly 5 readiness points.")
}

# =============================================================================
# Athlete-facing individualized notes
# =============================================================================

performance_wording <- c(
  "Lower-Body<br>Power" = "lower-body explosive power",
  "Plyometric<br>Push-Up" = "upper-body explosive pushing force",
  "Fatigue<br>Resistance" = "ability to maintain high-intensity power output",
  "IMTP" = "total-body isometric pulling force",
  "Hand<br>Strength" = "grip and hand strength",
  "Neck<br>Strength" = "neck flexion and extension strength",
  "Core<br>Strength" = "rotational core strength",
  "Field<br>Speed" = "acceleration and sprint speed",
  "Agility" = "change-of-direction speed"
)

readiness_wording <- c(
  "Plyometric Push-Up<br>Asymmetry" = "left-to-right pushing-force balance",
  "Trunk Rotation<br>Asymmetry" = "left-to-right rotational core balance",
  "Neck Flex :<br>Extension" = "neck flexion-to-extension strength balance",
  "Grip Squeeze<br>Asymmetry" = "left-to-right grip-strength balance",
  "Pinch Grip<br>Asymmetry" = "left-to-right pinch-strength balance",
  "CMJ<br>Asymmetry" = "left-to-right force balance during a vertical jump",
  "Drop Jump Landing<br>Asymmetry" = "left-to-right landing-force balance",
  "H:Q Ratio<br>Strength Balance" = "hamstring-to-quadriceps strength ratio",
  "Quadriceps<br>Symmetry" = "left-to-right quadriceps strength balance",
  "Hamstring<br>Force & Symmetry" = "hip extension (hamstring-injury-risk) strength and left-to-right balance"
)

# Broad, one-to-three-word category names used only for the "Development
# Focus" bullets -- deliberately shorter/less specific than
# performance_wording (used for the "Strengths" bullets), per staff
# preference to keep the development callout general rather than
# over-prescribing a specific drill or mechanism.
broad_drivers <- c(
  "Lower-Body<br>Power" = "lower-body power",
  "Plyometric<br>Push-Up" = "upper-body power",
  "Fatigue<br>Resistance" = "fatigue resistance",
  "IMTP" = "strength",
  "Hand<br>Strength" = "grip strength",
  "Neck<br>Strength" = "neck strength",
  "Core<br>Strength" = "core strength",
  "Field<br>Speed" = "speed",
  "Agility" = "agility"
)

# Below this score (on the 40-100 percentile scale), a "lowest" area is
# treated as a genuine development need. At or above it, an athlete's
# bottom two scores are still comparatively their weakest, but not weak
# in any absolute sense, so the note says so instead of implying a real gap.
DEVELOPMENT_LOW_THRESHOLD <- 70

make_performance_notes <- function(athlete_name) {
  x <- performance_radar |>
    dplyr::filter(name == athlete_name, !is.na(score)) |>
    dplyr::arrange(dplyr::desc(score))
  
  if (!nrow(x)) {
    return(list(strengths = c("", ""), development = c("", "")))
  }
  
  top <- head(x, 2)
  bottom <- head(x |> dplyr::arrange(score), 2)
  
  strengths <- vapply(seq_len(nrow(top)), function(i) {
    if (top$score[i] >= 85) {
      paste0("Clear strength in ", performance_wording[[top$axis[i]]], ".")
    } else {
      paste0("Solid team-relative profile in ", performance_wording[[top$axis[i]]], ".")
    }
  }, character(1))
  
  development <- vapply(seq_len(nrow(bottom)), function(i) {
    driver <- broad_drivers[[bottom$axis[i]]]
    if (bottom$score[i] < DEVELOPMENT_LOW_THRESHOLD) {
      paste0("Prioritize development in ", driver, ".")
    } else {
      paste0(
        stringr::str_to_sentence(driver),
        " is comparatively the lowest score here, though still solid overall -- not a true weak point."
      )
    }
  }, character(1))
  
  list(
    strengths = c(strengths, rep("", max(0, 2 - length(strengths))))[1:2],
    development = c(development, rep("", max(0, 2 - length(development))))[1:2]
  )
}

make_readiness_notes <- function(athlete_name) {
  x <- dplyr::bind_rows(upper_radar, lower_radar) |>
    dplyr::filter(name == athlete_name, status %in% c("Yellow", "Red")) |>
    dplyr::mutate(severity = ifelse(status == "Red", 2, 1)) |>
    dplyr::arrange(dplyr::desc(severity), score)
  
  if (!nrow(x)) {
    return("Current upper- and lower-body readiness measures are within the recommended range.")
  }
  
  x <- head(x, 2)
  
  side_clause <- function(i) {
    if (isTRUE(nzchar(x$weak_side[i]))) paste0(" (", x$weak_side[i], " is weaker)") else ""
  }
  
  paste0(
    "Continue developing ", readiness_wording[[x$axis[1]]], side_clause(1), ".",
    if (nrow(x) > 1) {
      paste0(" Prioritize ", readiness_wording[[x$axis[2]]], side_clause(2), ".")
    } else {
      ""
    }
  )
}

# Below this score, the DSI/fatigue-resistance profile is treated as a
# genuine, assertive training priority. At or above it, the language stays
# vague/optional ("may want to emphasize"), since the rest of the athlete's
# profile is strong enough that this isn't a true gap -- per staff
# preference not to flag something as a priority when overall numbers are
# good.
EXPLOSIVENESS_LOW_THRESHOLD <- 55

dsi_wording_strong <- c(
  "Strength Dominant" = "Strength-dominant profile: raw force capacity is ahead of the ability to express it quickly. Ballistic and plyometric work (jump training, medicine ball throws, contrast training) is the priority to convert that strength into faster force production.",
  "Power Dominant" = "Power-dominant profile: explosive output is ahead of maximal-strength capacity. Heavy strength training (near-maximal loads, slower tempos) is the priority to build a stronger force base to convert into power."
)

dsi_wording_soft <- c(
  "Strength Dominant" = "Strength-dominant profile: raw force capacity is a bit ahead of the ability to express it quickly. Adding some ballistic and plyometric work (jump training, medicine ball throws, contrast training) could help convert more of that strength into speed, though the overall profile is already strong.",
  "Power Dominant" = "Power-dominant profile: explosive output is a bit ahead of maximal-strength capacity. Adding some heavy strength work (near-maximal loads, slower tempos) could help build a bit more of a force base, though the overall profile is already strong."
)

dsi_wording_neutral <- c(
  "Balanced" = "Balanced strength-to-power profile, converting isometric strength into fast force production efficiently. General strength-power training should be maintained to keep this balance.",
  "No data" = "Not enough IMTP and CMJ data on file to calculate a Dynamic Strength Index."
)

# Trailing clause appended after "holds up well under repeated effort" or
# "fades faster than teammates under repeated effort" -- see
# make_strength_explosiveness_note(). Split into strong (genuinely low
# fatigue resistance) and soft (comparatively lowest, but not truly low)
# variants, same logic as the DSI wording above.
quadrant_wording_strong <- c(
  "Power-Dominant, Fades Fast" = " -- explosive in a single effort, but anaerobic conditioning is the next priority to hold onto that output across a game.",
  "Strength-Dominant, Fades Fast" = " -- both power conversion and anaerobic conditioning are priorities here."
)

quadrant_wording_soft <- c(
  "Power-Dominant, Fades Fast" = " -- explosive in a single effort; some added anaerobic conditioning could help that output hold up a bit longer across a game.",
  "Strength-Dominant, Fades Fast" = " -- both power conversion and anaerobic conditioning could be worth some added emphasis here."
)

quadrant_wording_holds_up <- c(
  "Power-Dominant, Holds Up Well" = " -- explosive output that also repeats, which is the goal for match performance.",
  "Strength-Dominant, Holds Up Well" = " -- a strong engine that hasn't yet been fully converted into ballistic output; some added conversion work could help, though the overall profile is already solid."
)

make_strength_explosiveness_note <- function(athlete_name) {
  a <- readiness |>
    dplyr::filter(name == athlete_name) |>
    dplyr::slice(1)
  
  if (!nrow(a)) return("")
  
  # DSI sentence: which side of the strength/power relationship is behind
  # determines which raw score gates how assertive the language is --
  # Strength Dominant means *power* is relatively behind, Power Dominant
  # means *strength* is relatively behind.
  dsi_gate_score <- dplyr::case_when(
    a$dsi_status == "Strength Dominant" ~ a$power_score,
    a$dsi_status == "Power Dominant" ~ a$strength_score,
    TRUE ~ NA_real_
  )
  
  dsi_sentence <- if (a$dsi_status %in% names(dsi_wording_neutral)) {
    dsi_wording_neutral[[a$dsi_status]]
  } else if (!is.na(dsi_gate_score) && dsi_gate_score < EXPLOSIVENESS_LOW_THRESHOLD) {
    dsi_wording_strong[[a$dsi_status]]
  } else {
    dsi_wording_soft[[a$dsi_status]]
  }
  
  fatigue_sentence <- if (is.na(a$fatigue_pct)) {
    " There isn't enough Wingate data on file to evaluate fatigue resistance."
  } else if (identical(a$explosiveness_quadrant, "No data")) {
    # Wingate is on file but DSI isn't (missing CMJ or IMTP): still report
    # fatigue resistance, just without the quadrant-specific follow-on.
    paste0(
      " On the Wingate, fatigue resistance ",
      if (a$fatigue_pct >= 50) "holds up well under repeated effort." else "fades faster than teammates under repeated effort."
    )
  } else {
    holds_up <- grepl("Holds Up Well", a$explosiveness_quadrant, fixed = TRUE)
    trailing <- if (holds_up) {
      quadrant_wording_holds_up[[a$explosiveness_quadrant]]
    } else if (!is.na(a$fatigue_pct) && a$fatigue_pct < EXPLOSIVENESS_LOW_THRESHOLD) {
      quadrant_wording_strong[[a$explosiveness_quadrant]]
    } else {
      quadrant_wording_soft[[a$explosiveness_quadrant]]
    }
    paste0(
      " On the Wingate, fatigue resistance ",
      if (holds_up) "holds up well under repeated effort" else "fades faster than teammates under repeated effort",
      trailing
    )
  }
  
  paste0(dsi_sentence, fatigue_sentence)
}

# =============================================================================
# Read template and embed required local assets
# =============================================================================

logo_uri <- paste0("data:image/jpeg;base64,", base64enc::base64encode(logo_file))
phantom_uri <- paste0("data:image/jpeg;base64,", base64enc::base64encode(phantom_file))

plotly_js_dir <- system.file(
  "htmlwidgets/lib/plotlyjs",
  package = "plotly"
)

if (plotly_js_dir == "" || !dir.exists(plotly_js_dir)) {
  stop(
    "Could not locate the Plotly JavaScript folder in the plotly package. ",
    "Reinstall Plotly with install.packages('plotly')."
  )
}

plotly_candidates <- list.files(
  plotly_js_dir,
  pattern = "^plotly.*\\.min\\.js$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (!length(plotly_candidates)) {
  stop(
    "Could not find a minified Plotly JavaScript file in: ",
    plotly_js_dir
  )
}

plotly_js_file <- plotly_candidates[1]

message("Embedding Plotly JavaScript: ", basename(plotly_js_file))

# Read as raw bytes (not text) so nothing about line endings or encoding
# can be altered before it gets base64-encoded below.
plotly_js_raw <- readBin(
  plotly_js_file,
  what = "raw",
  n = file.info(plotly_js_file)$size
)

message("Plotly JS size: ", format(length(plotly_js_raw), big.mark = ","), " bytes")

if (length(plotly_js_raw) < 100000) {
  stop(
    "Plotly JavaScript was unexpectedly small. ",
    "Expected a complete library, but found only ",
    length(plotly_js_raw),
    " bytes."
  )
}

plotly_js_text <- rawToChar(plotly_js_raw)

if (!grepl("newPlot", plotly_js_text, fixed = TRUE)) {
  stop(
    "The selected Plotly JavaScript file does not appear to contain Plotly.newPlot(): ",
    plotly_js_file
  )
}

# IMPORTANT: the library is embedded as base64, not as raw JavaScript text,
# inside a plain <script> tag. Browsers close a <script> element on the
# first literal "</script" byte sequence they see, REGARDLESS of whether
# that sequence is inside a string, a comment, or otherwise meaningless to
# the JavaScript parser. Plotly's minified bundle contains SVG/export
# sanitization code that includes exactly that substring, which silently
# truncated the embedded library and is why the report rendered but every
# chart said "Plotly did not load in this report." Base64 text can never
# contain "<", so encoding first makes this class of bug impossible.
plotly_js_b64 <- base64enc::base64encode(plotly_js_raw)

template <- paste(
  readLines(template_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

# =============================================================================
# Render self-contained reports
# =============================================================================

render_athlete <- function(athlete_name) {
  a <- readiness |>
    dplyr::filter(name == athlete_name) |>
    dplyr::slice(1)
  
  if (!nrow(a)) {
    stop("No readiness record found for: ", athlete_name)
  }
  
  p_notes <- make_performance_notes(athlete_name)
  r_note <- make_readiness_notes(athlete_name)
  se_note <- make_strength_explosiveness_note(athlete_name)
  
  dsi_explosiveness_row <- list(list(
    name = athlete_name,
    dsi = if (is.na(a$dsi)) NA else round(a$dsi, 2),
    dsi_status = a$dsi_status,
    fatigue_pct = if (is.na(a$fatigue_pct)) NA else round(a$fatigue_pct, 0),
    quadrant = a$explosiveness_quadrant,
    wingate_peak_power = if (is.na(a$wingate_peak_power_kg)) NA else round(a$wingate_peak_power_kg, 1),
    fatigue_decline = if (is.na(a$fatigue_index)) NA else round(a$fatigue_index, 1)
  ))
  
  replacements <- list(
    "{{PAGE_TITLE}}" = paste0(athlete_name, " | OSU Lacrosse Performance"),
    "{{PLOTLY_JS_B64}}" = plotly_js_b64,
    "{{LOGO_URI}}" = logo_uri,
    "{{PHANTOM_URI}}" = phantom_uri,
    "{{ATHLETE_NAME}}" = athlete_name,
    "{{POSITION}}" = ifelse(is.na(a$position), "", a$position),
    "{{HEIGHT}}" = ifelse(is.na(a$height_in), "", {
      ft <- floor(a$height_in / 12)
      inch <- round(a$height_in %% 12)
      # handle rounding up to 12 inches (e.g. 71.6 -> 5'12" should read 6'0")
      carry <- inch == 12
      ft <- ft + as.numeric(carry)
      inch <- ifelse(carry, 0, inch)
      paste0(ft, "'", inch, "\"")
    }),
    "{{WEIGHT}}" = ifelse(is.na(a$weight_lb), "", paste0(round(a$weight_lb), " lb")),
    "{{OVERALL_SCORE}}" = fmt_num(a$composite_score),
    "{{POWER_SCORE}}" = fmt_num(a$power_score),
    "{{STRENGTH_SCORE}}" = fmt_num(a$strength_score),
    "{{SPEED_SCORE}}" = fmt_num(a$speed_score),
    "{{AGILITY_SCORE}}" = fmt_num(a$agility_score),
    "{{UPPER_SCORE}}" = fmt_num(a$upper_readiness),
    "{{LOWER_SCORE}}" = fmt_num(a$lower_readiness),
    "{{STRENGTH_NOTE_1}}" = p_notes$strengths[1] %||% "",
    "{{STRENGTH_NOTE_2}}" = p_notes$strengths[2] %||% "",
    "{{DEVELOPMENT_NOTE_1}}" = p_notes$development[1] %||% "",
    "{{DEVELOPMENT_NOTE_2}}" = p_notes$development[2] %||% "",
    "{{READINESS_NOTE}}" = r_note,
    "{{STRENGTH_EXPLOSIVENESS_NOTE}}" = se_note,
    "{{PERFORMANCE_RADAR_JSON}}" = as_json(performance_radar |> dplyr::filter(name == athlete_name)),
    "{{UPPER_RADAR_JSON}}" = as_json(upper_radar |> dplyr::filter(name == athlete_name)),
    "{{LOWER_RADAR_JSON}}" = as_json(lower_radar |> dplyr::filter(name == athlete_name)),
    "{{DSI_EXPLOSIVENESS_JSON}}" = as_json(dsi_explosiveness_row),
    "{{TEAM_SUMMARY_JSON}}" = "[]",
    "{{TEAM_UPPER_JSON}}" = "[]",
    "{{TEAM_LOWER_JSON}}" = "[]"
  )
  
  page <- template
  for (key in names(replacements)) {
    page <- replace_literal(page, key, as.character(replacements[[key]]))
  }
  
  out <- file.path(output_dir, paste0(slug(athlete_name), "_Report.html"))
  validate_output_html(page, basename(out))
  writeLines(page, out, useBytes = TRUE)
  message("Created: ", out)
}

render_team <- function() {
  team_summary <- readiness |>
    dplyr::select(
      name,
      position,
      power_score,
      strength_score,
      speed_score,
      agility_score,
      upper_readiness,
      lower_readiness,
      readiness_score,
      overall_status,
      dsi,
      dsi_status,
      wingate_power_pct,
      fatigue_pct,
      wingate_peak_power_kg,
      fatigue_index,
      explosiveness_quadrant
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          power_score, strength_score, speed_score, agility_score,
          upper_readiness, lower_readiness, readiness_score,
          wingate_power_pct, fatigue_pct, wingate_peak_power_kg, fatigue_index
        ),
        ~round(.x, 1)
      ),
      dsi = round(dsi, 2),
      # Recompute from the ROUNDED score the template colors the bar by,
      # so e.g. 80.04 (shown as 80.0, Yellow bar) isn't labeled Green.
      overall_status = readiness_band_status(readiness_score)
    )
  
  replacements <- list(
    "{{PAGE_TITLE}}" = "OSU Lacrosse | Team Dashboard",
    "{{PLOTLY_JS_B64}}" = plotly_js_b64,
    "{{LOGO_URI}}" = logo_uri,
    "{{PHANTOM_URI}}" = phantom_uri,
    "{{ATHLETE_NAME}}" = "",
    "{{POSITION}}" = "",
    "{{HEIGHT}}" = "",
    "{{WEIGHT}}" = "",
    "{{OVERALL_SCORE}}" = "",
    "{{POWER_SCORE}}" = "",
    "{{STRENGTH_SCORE}}" = "",
    "{{SPEED_SCORE}}" = "",
    "{{AGILITY_SCORE}}" = "",
    "{{UPPER_SCORE}}" = "",
    "{{LOWER_SCORE}}" = "",
    "{{STRENGTH_NOTE_1}}" = "",
    "{{STRENGTH_NOTE_2}}" = "",
    "{{DEVELOPMENT_NOTE_1}}" = "",
    "{{DEVELOPMENT_NOTE_2}}" = "",
    "{{READINESS_NOTE}}" = "",
    "{{STRENGTH_EXPLOSIVENESS_NOTE}}" = "",
    "{{PERFORMANCE_RADAR_JSON}}" = "[]",
    "{{UPPER_RADAR_JSON}}" = "[]",
    "{{LOWER_RADAR_JSON}}" = "[]",
    "{{DSI_EXPLOSIVENESS_JSON}}" = "[]",
    "{{TEAM_SUMMARY_JSON}}" = as_json(team_summary),
    "{{TEAM_UPPER_JSON}}" = as_json(upper_radar),
    "{{TEAM_LOWER_JSON}}" = as_json(lower_radar)
  )
  
  page <- template
  for (key in names(replacements)) {
    page <- replace_literal(page, key, as.character(replacements[[key]]))
  }
  
  out <- file.path(output_dir, "Team_Dashboard.html")
  validate_output_html(page, basename(out))
  writeLines(page, out, useBytes = TRUE)
  message("Created: ", out)
}

# =============================================================================
# Menu
# =============================================================================

athletes <- sort(unique(readiness$name))
if (!length(athletes)) {
  stop("No athletes found in Demographics.")
}

all_option <- length(athletes) + 1L

cat("\nOSU LACROSSE REPORT GENERATOR\n")
cat(strrep("=", 38), "\n", sep = "")

for (i in seq_along(athletes)) {
  cat(sprintf("%2d = %s\n", i, athletes[i]))
}

cat(sprintf("%2d = ALL ATHLETES + TEAM DASHBOARD\n\n", all_option))

choice <- suppressWarnings(as.integer(readline("Enter a number: ")))

if (is.na(choice)) {
  stop("Enter a menu number.")
}

if (choice == all_option) {
  for (athlete_name in athletes) {
    render_athlete(athlete_name)
  }
  render_team()
} else if (choice %in% seq_along(athletes)) {
  render_athlete(athletes[choice])
} else {
  stop("That menu number is not available.")
}

cat("\nFinished. Open the HTML files in Lacrosse_Reports.\n")