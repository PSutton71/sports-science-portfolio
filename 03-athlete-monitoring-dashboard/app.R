# ======================================================================
# Athlete Performance Dashboard 
# ----------------------------------------------------------------------
# Features:
# - Imports multi-sheet Excel data (GPS, survey, demographics).
# - Cleans dates and athlete names into consistent formats.
# - Provides team-level, positional, and athlete-level views.
# - Includes ACWR and training monotony visualizations.
# - Offers time-series plots and raw-data tables per athlete.
# ======================================================================

# ---- 0. Libraries -----------------------------------------------------

pkgs <- c(
  "ggplot2", "shiny", "readxl", "dplyr",
  "tidyr", "shinyWidgets", "DT", "base64enc")
invisible(lapply(pkgs, library, character.only = TRUE))

# Optional: rsconnect to deploy to shinyapps.io. Edit file name. 
# library(rsconnect)
# rsconnect::deployApp("path/to/your/app")

# ---- 1. Global Settings & Helper Functions ----------------------------

# Data file & sheet names (template parameters)
DATA_FILE   <- "Example Polar Data.xlsx"  # <-- replace file as needed, keep sheet names the same
SHEETS_USED <- c("GPS Polar Data", "Survey Data", "Demographics")

# Colors Selection (based on team colors)
col_primary   <- "#000000"
col_accent    <- "#F56312"
col_background <- "#FFFFFF"

# Logos (based on team logos). Store in 'www' folder within wd.
logo_left_path  <- "www/Logo1.jpg"   
logo_right_path <- "www/Image1.png"

# Encode logos as base64 only if files exist
img_left  <- if (file.exists(logo_left_path))  dataURI(file = logo_left_path)  else NULL
img_right <- if (file.exists(logo_right_path)) dataURI(file = logo_right_path) else NULL

# Helper: normalize Name and Date columns in a data frame
normalize_names_dates <- function(df) {
  if ("Name" %in% names(df)) {
    df$Name <- trimws(as.character(df$Name))
  }
  if ("Date" %in% names(df)) {
    if (is.numeric(df$Date)) {
      df$Date <- as.Date(df$Date, origin = "1899-12-30")
    } else {
      df$Date <- as.Date(df$Date)
    }
  }
  df
}

# Helper: get numeric metric columns, excluding common ID/date fields
get_numeric_metrics <- function(df) {
  exclude_cols <- c("Date", "Name", "ID")
  metrics <- names(df)[sapply(df, is.numeric)]
  setdiff(metrics, exclude_cols)
}

get_sheet_dates <- function(sheet_name) {
  dates <- data_list[[sheet_name]]$Date
  sort(unique(na.omit(dates)), decreasing = TRUE)
}

# ---- 2. Load & Prepare Data (Global) ----------------------------------

# Read all sheets and normalize basic fields
data_list <- lapply(SHEETS_USED, function(sheet) {
  df <- read_excel(DATA_FILE, sheet = sheet)
  normalize_names_dates(df)
})
names(data_list) <- SHEETS_USED

# Metrics per sheet
metrics_list <- lapply(data_list, get_numeric_metrics)

# All unique athletes (from any sheet with Name)
all_athletes <- unique(unlist(lapply(data_list, function(df) {
  if ("Name" %in% names(df)) df$Name else NULL
})))
all_athletes <- sort(na.omit(all_athletes))

# All available dates across sheets for date-range defaults
unique_date_vector <- sort(
  unique(na.omit(
    do.call(c, lapply(data_list, function(df) {
      if ("Date" %in% names(df)) df$Date else NULL
    }))
  )),
  decreasing = TRUE
)

# GPS sheet contains ACWR and monotony metrics
gps_sheet_name <- SHEETS_USED[1]  
gps_data <- data_list[[gps_sheet_name]]

# ACWR dates where ACWR is present
acwr_dates <- sort(unique(na.omit(gps_data$Date[gps_data$ACWR > 0])))
first_acwr_date <- if (length(acwr_dates)) min(acwr_dates) else NA

# Monotony dates (where monotony metric exists); adjust column name if needed
monot_col <- "TD_Monotony"
monot_date_vector <- sort(
  unique(na.omit(gps_data$Date[!is.na(gps_data[[monot_col]])])),
  decreasing = TRUE
)


# ======================================================================
# 3. UI
# ======================================================================

ui <- fluidPage(
  # Global styles
  tags$head(
    tags$style(HTML("
      body, label, input, button, select {font-family: Arial;}
      .nav-tabs > li.active > a {background-color: #F56312; color: white;}
      .nav-tabs > li > a {color: #F56312;}
      .shiny-output-error {color: #F56312;}
      #team1_names {
        font-family: Arial, sans-serif;
        text-align: left;
        white-space: pre-wrap;
        margin-left: auto;
        margin-right: auto;
        width: 100%;
        padding: 0;
        background-color: white;
        border: none;
      }
    "))
  ),
  
  titlePanel("Athlete Performance Dashboard"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # ---- TEAM TAB -----------------------------------------------------
    tabPanel(
      "Team",
      # Team averages over time
      fluidRow(
        column(
          12,
          tags$h4(style = "text-align: center;", "Team Analysis"),
          selectInput("avg_sheet", "Sheet", choices = SHEETS_USED, selected = SHEETS_USED[1]),
          selectInput("avg_metric", "Metric", choices = NULL),
          dateRangeInput(
            "avg_daterange", "Date Range",
            start = min(unique_date_vector),
            end   = max(unique_date_vector)
          ),
          plotOutput("avg_plot", height = "600px")
        )
      ),
      
      # Positional analysis
      fluidRow(
        column(12, tags$h4(style = "text-align: center;", "Positional Analysis"))
      ),
      fluidRow(
        column(
          4,
          selectInput("ta_sheet", "Sheet", choices = SHEETS_USED, selected = gps_sheet_name),
          selectInput("ta_metric", "Metric", choices = NULL),
          selectInput("ta_position", "Position", choices = NULL),
          selectInput("ta_date", "Date", choices = NULL)
        ),
        column(
          8,
          div(style = "text-align: center; margin-top: 60px; width: 90%;"),
          tableOutput("all_position_averages")
        )
      ),
      fluidRow(
        column(12, plotOutput("ta_pos_plot", height = "500px"))
      ),
      
      # Team ACWR snapshot
      fluidRow(
        column(
          12,
          selectInput("acwr_date", "Date", choices = acwr_dates, selected = first_acwr_date),
          plotOutput("acwr_plot", height = "500px")
        )
      ),
      
      # Monotony view (team or athlete)
      fluidRow(
        column(
          4,
          dateRangeInput(
            "monot_daterange", "Monotony Date Range",
            start = min(monot_date_vector),
            end   = max(monot_date_vector)
          )
        ),
        column(
          4,
          pickerInput(
            "monot_athlete",
            "Athlete / Team",
            choices = c("Team", all_athletes),
            options = list(`live-search` = TRUE),
            multiple = FALSE
          )
        )
      ),
      fluidRow(
        column(12, plotOutput("monot_plot", height = "500px"))
      )
    ),
    
    # ---- ATHLETE TAB --------------------------------------------------
    tabPanel(
      "Athlete",
      fluidRow(
        column(
          2,
          align = "center",
          if (!is.null(img_left)) tags$img(src = img_left, height = "150px", width = "auto")
        ),
        column(
          4,
          br(), br(),
          align = "right",
          uiOutput("athlete_demographics")
        ),
        column(
          6,
          style = "display: flex; justify-content: flex-end; align-items: flex-start;",
          if (!is.null(img_right)) {
            tagList(br(), tags$img(src = img_right, height = "150px", width = "auto"))
          }
        )
      ),
      fluidRow(
        column(
          4,
          pickerInput(
            "athlete_select", "Athlete",
            choices = all_athletes,
            options = list(`live-search` = TRUE),
            multiple = FALSE
          )
        ),
        column(
          4,
          pickerInput(
            "athlete_metrics", "Select 1-2 Metrics",
            choices = NULL,
            options = list(`live-search` = TRUE),
            multiple = TRUE
          )
        ),
        column(
          4,
          dateRangeInput(
            "athlete_daterange", "Date Range",
            start = min(unique_date_vector),
            end   = max(unique_date_vector)
          )
        )
      ),
      fluidRow(
        column(12, plotOutput("athlete_timeplot", height = "500px"))
      ),
      hr(),
      fluidRow(
        column(12, DTOutput("athlete_table"))
      ),
      
      # Athlete-specific ACWR time series
      fluidRow(
        column(
          4,
          pickerInput(
            "acwr_athlete", "Athlete",
            choices = NULL,
            options = list(`live-search` = TRUE)
          )
        ),
        column(
          4,
          dateRangeInput(
            "acwr_athlete_daterange", "ACWR Date Range",
            start = min(unique_date_vector),
            end   = max(unique_date_vector)
          )
        )
      ),
      fluidRow(
        column(12, plotOutput("acwr_athlete_plot", height = "400px"))
      )
    ),
    
    # ---- DAY-BY-DAY TAB ----------------------------------------------
    tabPanel(
      "Day by Day",
      fluidRow(
        column(
          2,
          align = "center",
          if (!is.null(img_left)) tags$img(src = img_left, height = "200px", width = "auto")
        )),
      fluidRow(
        column(
          12,
          selectInput("team_sheet1", "Sheet for Plot 1", choices = SHEETS_USED, selected = SHEETS_USED[1]),
          selectInput("team_metric1", "Metric", choices = NULL),
          selectInput("date1", "Date", choices = NULL),
          plotOutput("plot1", height = "600px")
        )
      ),
      fluidRow(
        column(
          12,
          selectInput("team_sheet2", "Sheet for Plot 2", choices = SHEETS_USED, selected = SHEETS_USED[1]),
          selectInput("team_metric2", "Metric", choices = NULL),
          selectInput("date2", "Date", choices = NULL),
          plotOutput("plot2", height = "600px")
        )
      )
    )
  )
)


# ======================================================================
# 4. SERVER
# ======================================================================

server <- function(input, output, session) {
  
  # ---- TEAM TAB: metric/date dropdowns --------------------------------
  
  observeEvent(input$team_sheet1, {
    updateSelectInput(
      session, "team_metric1",
      choices  = metrics_list[[input$team_sheet1]],
      selected = metrics_list[[input$team_sheet1]][1]
    )
    sheet_dates <- get_sheet_dates(input$team_sheet1)
    updateSelectInput(
      session, "date1",
      choices  = sheet_dates,
      selected = if (length(sheet_dates)) sheet_dates[1] else NULL
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$team_sheet2, {
    updateSelectInput(
      session, "team_metric2",
      choices  = metrics_list[[input$team_sheet2]],
      selected = metrics_list[[input$team_sheet2]][1]
    )
    sheet_dates <- get_sheet_dates(input$team_sheet2)
    updateSelectInput(
      session, "date2",
      choices  = sheet_dates,
      selected = if (length(sheet_dates)) sheet_dates[1] else NULL
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$avg_sheet, {
    updateSelectInput(
      session, "avg_metric",
      choices  = metrics_list[[input$avg_sheet]],
      selected = metrics_list[[input$avg_sheet]][1]
    )
  }, ignoreNULL = FALSE)
  
  # ---- TEAM TAB: average over time ------------------------------------
  
  output$avg_plot <- renderPlot({
    req(input$avg_sheet, input$avg_metric, input$avg_daterange)
    df <- data_list[[input$avg_sheet]]
    
    # If no Type column, default to "Practice"
    if (!"Type" %in% names(df)) {
      df$Type <- "Practice"
    }
    
    df_filtered <- df %>%
      filter(Date >= input$avg_daterange[1], Date <= input$avg_daterange[2]) %>%
      group_by(Date, Type) %>%
      summarise(
        Avg = mean(.data[[input$avg_metric]], na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(df_filtered, aes(x = Date, y = Avg, fill = Type)) +
      geom_col(alpha = 0.9) +
      geom_text(
        aes(label = round(Avg, 0)),
        vjust = -0.3, color = col_primary, size = 3, family = "Arial"
      ) +
      scale_fill_manual(values = c("Practice" = col_primary, "Game" = col_accent)) +
      theme_minimal() +
      labs(
        title = NULL,
        x     = NULL,
        y     = paste("Average", input$avg_metric)
      ) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, color = col_primary, size = 14),
        axis.text.y = element_text(color = col_primary, size = 10),
        axis.title.y = element_text(color = col_primary, size = 14),
        legend.position = "top",
        legend.text = element_text(color = col_primary, size = 12)
      ) +
      guides(fill = guide_legend(title = NULL))
  })
  
  # ---- TEAM TAB: positional analysis ----------------------------------
  
  observeEvent(input$ta_sheet, {
    updateSelectInput(session, "ta_metric", choices = metrics_list[[input$ta_sheet]])
    sheet_dates <- get_sheet_dates(input$ta_sheet)
    updateSelectInput(session, "ta_date", choices = sheet_dates)
  })
  
  observe({
    demo <- data_list[["Demographics"]]
    if (!is.null(demo) && "Position" %in% names(demo)) {
      positions <- sort(unique(na.omit(demo$Position)))
      updateSelectInput(session, "ta_position", choices = positions)
    }
  })
  
  output$ta_pos_plot <- renderPlot({
    req(input$ta_sheet, input$ta_metric, input$ta_position, input$ta_date)
    df   <- data_list[[input$ta_sheet]]
    demo <- data_list[["Demographics"]]
    
    demo <- normalize_names_dates(demo)
    demo$Position <- trimws(as.character(demo$Position))
    
    ta_name_vec <- unique(demo$Name[demo$Position == input$ta_position])
    
    plot_df <- df %>%
      filter(Date == as.Date(input$ta_date), Name %in% ta_name_vec)
    
    if (nrow(plot_df) == 0 || !(input$ta_metric %in% names(df))) {
      ggplot() +
        annotate(
          "text", x = 1, y = 1,
          label = "No data for position / date / metric selection.",
          size = 5
        ) +
        theme_void()
    } else {
      plot_df <- plot_df %>%
        group_by(Name) %>%
        summarise(
          Value = mean(.data[[input$ta_metric]], na.rm = TRUE),
          .groups = "drop"
        )
      group_mean <- mean(plot_df$Value, na.rm = TRUE)
      
      ggplot(plot_df, aes(x = reorder(Name, Value), y = Value, fill = Value)) +
        geom_col(alpha = 0.9) +
        geom_text(
          aes(label = round(Value, 1)),
          vjust = -0.3, color = col_primary, size = 4, family = "Arial"
        ) +
        scale_fill_gradient(low = col_primary, high = col_accent) +
        theme_minimal() +
        labs(
          title = paste(input$ta_position, "on", input$ta_date, "-", input$ta_metric),
          x     = "Athlete",
          y     = input$ta_metric
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, color = col_primary, size = 13),
          legend.position = "none"
        ) +
        geom_hline(yintercept = group_mean, color = "grey40", linetype = "dashed")
    }
  })
  
  output$all_position_averages <- renderTable({
    req(input$ta_sheet, input$ta_metric, input$ta_date)
    df   <- data_list[[input$ta_sheet]]
    demo <- data_list[["Demographics"]]
    
    df   <- normalize_names_dates(df)
    demo <- normalize_names_dates(demo)
    demo$Position <- trimws(as.character(demo$Position))
    
    joined <- left_join(df, demo[, c("Name", "Position")], by = "Name")
    filtered <- joined %>%
      filter(Date == as.Date(input$ta_date))
    
    filtered %>%
      group_by(Position) %>%
      summarise(
        `Average` = mean(.data[[input$ta_metric]], na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      arrange(Position)
  })
  
  # ---- TEAM TAB: ACWR snapshot ----------------------------------------
  
  output$acwr_plot <- renderPlot({
    req(input$acwr_date)
    df <- gps_data %>%
      normalize_names_dates() %>%
      filter(Date == as.Date(input$acwr_date), !is.na(ACWR)) %>%
      select(Name, ACWR)
    
    acwr_color <- function(x) {
      if (is.na(x) || x < 0.8 || x > 1.5) {
        "#b71c1c"      # red zone
      } else if (x >= 1.3 && x <= 1.5) {
        "#e3b506"      # yellow zone
      } else if (x >= 0.8 && x < 1.3) {
        "#238b45"      # green zone
      } else {
        "grey"
      }
    }
    
    df$Color <- vapply(df$ACWR, acwr_color, character(1))
    
    ggplot(df, aes(x = Name, y = ACWR, fill = Color)) +
      geom_col() +
      geom_text(
        aes(label = round(ACWR, 2)),
        vjust = 1.5, color = "black", size = 4, family = "Arial"
      ) +
      scale_fill_identity() +
      labs(
        title    = "Acute:Chronic Workload Ratio",
        subtitle = "Green: 0.8–1.3 | Yellow: 1.3–1.5 | Red: <0.8 or >1.5",
        y        = "Workload Ratio",
        x        = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x  = element_text(angle = 60, hjust = 1, size = 10, color = col_primary),
        plot.title   = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.text.y  = element_text(size = 10, color = col_primary)
      )
  })
  
  # ---- TEAM TAB: Monotony ---------------------------------------------
  
  dist_data <- reactive({
    req(input$monot_daterange, input$monot_athlete)
    df <- gps_data %>%
      normalize_names_dates() %>%
      filter(Date >= input$monot_daterange[1],
             Date <= input$monot_daterange[2])
    
    if (input$monot_athlete == "Team") {
      df %>%
        group_by(Date) %>%
        summarise(
          TotalDist = mean(`Total distance [m]`, na.rm = TRUE),
          Monotony  = mean(.data[[monot_col]], na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(Name = "Team")
    } else {
      df %>%
        filter(Name == input$monot_athlete) %>%
        group_by(Date, Name) %>%
        summarise(
          TotalDist = mean(`Total distance [m]`, na.rm = TRUE),
          Monotony  = mean(.data[[monot_col]], na.rm = TRUE),
          .groups   = "drop"
        )
    }
  })
  
  mono_data <- reactive({
    req(input$monot_daterange, input$monot_athlete)
    df <- gps_data %>%
      normalize_names_dates() %>%
      filter(Date >= input$monot_daterange[1],
             Date <= input$monot_daterange[2])
    
    if (input$monot_athlete == "Team") {
      df <- df %>%
        group_by(Date) %>%
        summarise(
          Monotony = mean(.data[[monot_col]], na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        mutate(Name = "Team")
    } else {
      df <- df %>%
        filter(Name == input$monot_athlete) %>%
        group_by(Date, Name) %>%
        summarise(
          Monotony = mean(.data[[monot_col]], na.rm = TRUE),
          .groups  = "drop"
        )
    }
    
    df %>%
      filter(!is.na(Monotony)) %>%
      mutate(
        MonotCategory = case_when(
          Monotony >= 1.5 & Monotony <= 2.0 ~ "Moderate",
          Monotony > 2.0                    ~ "High",
          TRUE                              ~ "Normal"
        )
      )
  })
  
  output$monot_plot <- renderPlot({
    df_dist <- dist_data()
    df_mono <- mono_data()
    validate(need(nrow(df_mono) > 0, "No monotony data for this selection."))
    
    max_dist <- max(df_dist$TotalDist, na.rm = TRUE)
    max_mono <- max(df_mono$Monotony,  na.rm = TRUE)
    ratio    <- ifelse(is.na(max_mono) || max_mono == 0, 1, max_dist / max_mono)
    
    ggplot() +
      geom_col(
        data = df_dist,
        aes(x = Date, y = TotalDist),
        fill = col_primary, alpha = 0.8
      ) +
      geom_text(
        data = df_dist,
        aes(
          x     = Date,
          y     = TotalDist,
          label = round(TotalDist / 1000, 1)
        ),
        vjust  = -0.5,
        color  = "black",
        size   = 3.5,
        family = "Arial"
      ) +
      geom_line(
        data = df_mono,
        aes(x = Date, y = Monotony * ratio),
        color = col_primary,
        linetype = "dotted",
        size = 1.2
      ) +
      geom_point(
        data = df_mono,
        aes(x = Date, y = Monotony * ratio, color = MonotCategory),
        size = 2
      ) +
      scale_y_continuous(
        name = "Total distance [km]",
        sec.axis = sec_axis(~ . / ratio, name = "TD Monotony")
      ) +
      scale_color_manual(
        values = c(
          "Normal"   = "grey60",
          "Moderate" = "#e3b506",
          "High"     = "#b71c1c"
        ),
        name = "Monotony zone"
      ) +
      theme_minimal() +
      labs(
        title = paste0("Distance + Monotony: ", unique(df_mono$Name)),
        x     = NULL
      ) +
      theme(
        axis.title.y.left  = element_text(size = 14, color = col_primary),
        axis.title.y.right = element_text(size = 14, color = col_primary),
        axis.text.x        = element_text(size = 12, color = col_primary),
        axis.text.y        = element_text(size = 12, color = col_primary),
        legend.position    = "top"
      )
  })
  
  # ---- DAY-BY-DAY TAB: roster & plots ---------------------------------
  
  output$team1_names <- renderText({
    req(input$team_sheet1)
    df <- data_list[[input$team_sheet1]]
    if (!("Name" %in% names(df))) return("No data available")
    names_vec <- df %>%
      pull(Name) %>%
      unique() %>%
      na.omit() %>%
      sort()
    paste(names_vec, collapse = "\n")
  })
  
  get_team_data <- function(sheet, metric, date = NULL) {
    df <- data_list[[sheet]]
    if (!metric %in% names(df)) return(NULL)
    df <- normalize_names_dates(df)
    if (!is.null(date)) {
      date <- as.Date(date)
      df <- df %>% filter(Date == date)
    }
    df %>%
      select(Date, Name, !!sym(metric)) %>%
      filter(!is.na(.data[[metric]]))
  }
  
  plot_scatter_team <- function(sheet, metric, date = NULL) {
    df <- get_team_data(sheet, metric, date)
    if (is.null(df) || nrow(df) == 0) {
      return(
        ggplot() +
          annotate(
            "text", x = 0.5, y = 0.5,
            label = "No data for selected metric",
            size = 6, hjust = 0.5, vjust = 0.5
          ) +
          theme_void()
      )
    }
    df <- df %>% mutate(x_jitter = as.numeric(as.factor(Name)) + runif(n(), -0.3, 0.3))
    mean_val <- mean(df[[metric]], na.rm = TRUE)
    sd_val   <- sd(df[[metric]],   na.rm = TRUE)
    annotation_text <- paste0(
      "Mean: ", round(mean_val, 2), "\n",
      "SD: ",   round(sd_val,   2)
    )
    
    ggplot(df, aes(x = x_jitter, y = !!sym(metric))) +
      geom_point(color = col_accent, alpha = 1, size = 3) +
      geom_hline(yintercept = mean_val,         color = col_primary, linetype = "solid",  size = 1, alpha = 0.4) +
      geom_hline(yintercept = mean_val + sd_val, color = col_primary, linetype = "dashed", size = 1, alpha = 0.2) +
      geom_hline(yintercept = mean_val - sd_val, color = col_primary, linetype = "dashed", size = 1, alpha = 0.2) +
      geom_text(
        aes(label = round(!!sym(metric), 2)),
        hjust = 0.5, vjust = -0.75, color = "#A9A9A9", size = 4, family = "Arial"
      ) +
      annotate(
        "text",
        x     = max(df$x_jitter) + 0.5,
        y     = max(df[[metric]], na.rm = TRUE),
        label = annotation_text,
        color = "#A9A9A9", size = 4, hjust = 0.6, vjust = 0.5, family = "Arial"
      ) +
      scale_x_continuous(
        breaks = seq_along(unique(df$Name)),
        labels = unique(df$Name)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
      theme_minimal() +
      theme(
        panel.border       = element_rect(color = "grey", fill = NA, size = 1),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y        = element_text(size = 13),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.major.x = element_blank()
      ) +
      labs(y = metric, title = NULL)
  }
  
  output$plot1 <- renderPlot({
    req(input$team_sheet1, input$team_metric1, input$date1)
    plot_scatter_team(input$team_sheet1, input$team_metric1, input$date1)
  })
  
  output$plot2 <- renderPlot({
    req(input$team_sheet2, input$team_metric2, input$date2)
    plot_scatter_team(input$team_sheet2, input$team_metric2, input$date2)
  })
  
  # ---- ATHLETE TAB: data & plots --------------------------------------
  
  observe({
    all_metrics <- unique(unlist(metrics_list))
    updatePickerInput(session, "athlete_metrics", choices = all_metrics)
  }, priority = 1)
  
  observe({
    updatePickerInput(session, "athlete_select", choices = all_athletes)
  }, priority = 1)
  
  # Athlete demographics
  output$athlete_demographics <- renderUI({
    req(input$athlete_select)
    demo <- data_list[["Demographics"]]
    demo <- normalize_names_dates(demo)
    row  <- demo %>% filter(Name == input$athlete_select)
    if (nrow(row) == 0) {
      row <- gps_data %>%
        normalize_names_dates() %>%
        filter(Name == input$athlete_select)
    }
    if (nrow(row) == 0) return("Selected athlete data unavailable")
    
    pos    <- if (!is.null(row$Position[1]) && !is.na(row$Position[1])) row$Position[1] else NA
    year   <- if (!is.null(row$Year[1])     && !is.na(row$Year[1]))     row$Year[1]     else NA
    height <- if (!is.null(row$Height[1])   && !is.na(row$Height[1]))   row$Height[1]   else NA
    weight <- if (!is.null(row$Weight[1])   && !is.na(row$Weight[1]))   row$Weight[1]   else NA
    
    HTML(paste0(
      "<b>", input$athlete_select, "</b><br/>",
      "<span style='font-size:16px; color:black;'>",
      if (!is.na(pos))    paste0("Position: ", pos, "<br/>") else "",
      if (!is.na(year))   paste0("Year: ", year, "<br/>")    else "",
      if (!is.na(height)) paste0("Height: ", height, "<br/>") else "",
      if (!is.na(weight)) paste0("Weight: ", weight, "<br/>") else "",
      "</span>"
    ))
  })
  
  # Combine athlete data across sheets
  athlete_data <- reactive({
    req(input$athlete_select, input$athlete_daterange)
    df_list <- lapply(data_list, function(df) {
      if ("Name" %in% names(df)) df %>% filter(Name == input$athlete_select) else NULL
    })
    df_all <- bind_rows(df_list)
    df_all <- normalize_names_dates(df_all)
    df_all <- df_all %>%
      filter(Date >= input$athlete_daterange[1],
             Date <= input$athlete_daterange[2])
    if (!is.null(input$athlete_metrics) && length(input$athlete_metrics) > 0) {
      df_all <- df_all %>%
        select(Date, Name, all_of(input$athlete_metrics)) %>%
        filter(rowSums(is.na(select(., all_of(input$athlete_metrics)))) < length(input$athlete_metrics))
    } else {
      df_all <- df_all %>% select(Date, Name)
    }
    df_all %>% arrange(Date)
  })
  
  # Athlete time-series plot
  output$athlete_timeplot <- renderPlot({
    df <- athlete_data()
    validate(
      need(nrow(df) > 0, "No data in selected date range."),
      need(length(input$athlete_metrics) >= 1, "Select 1–2 metrics to display.")
    )
    
    metrics_sel <- input$athlete_metrics
    if (length(metrics_sel) > 2) metrics_sel <- metrics_sel[1:2]
    
    df_long <- df %>%
      select(Date, all_of(metrics_sel)) %>%
      pivot_longer(cols = all_of(metrics_sel), names_to = "Metric", values_to = "Value")
    
    if (length(metrics_sel) == 1) {
      ggplot(df_long, aes(x = Date, y = Value)) +
        geom_line(color = col_accent, size = 1) +
        geom_point(color = col_primary, size = 2) +
        geom_smooth(method = "lm", color = col_primary, se = FALSE, linetype = "dashed") +
        theme_minimal() +
        labs(title = NULL, y = metrics_sel[1], x = NULL)
    } else {
      vals1 <- df[[metrics_sel[1]]]
      vals2 <- df[[metrics_sel[2]]]
      ratio <- max(vals1, na.rm = TRUE) / max(vals2, na.rm = TRUE)
      
      df_long <- df_long %>%
        mutate(
          ScaledValue = ifelse(Metric == metrics_sel[2], Value * ratio, Value)
        )
      
      ggplot(df_long, aes(x = Date)) +
        # Metric 1
        geom_line(
          data = df_long %>% filter(Metric == metrics_sel[1]),
          aes(y = Value, color = Metric),
          size = 1
        ) +
        geom_point(
          data = df_long %>% filter(Metric == metrics_sel[1]),
          aes(y = Value, color = Metric),
          size = 2
        ) +
        geom_smooth(
          data = df_long %>% filter(Metric == metrics_sel[1]),
          aes(y = Value),
          method = "lm", color = col_primary, se = FALSE, linetype = "dashed"
        ) +
        # Metric 2 (scaled)
        geom_line(
          data = df_long %>% filter(Metric == metrics_sel[2]),
          aes(y = ScaledValue, color = Metric),
          size = 1
        ) +
        geom_point(
          data = df_long %>% filter(Metric == metrics_sel[2]),
          aes(y = ScaledValue, color = Metric),
          size = 2
        ) +
        geom_smooth(
          data = df_long %>% filter(Metric == metrics_sel[2]),
          aes(y = ScaledValue),
          method = "lm", color = col_accent, se = FALSE, linetype = "dashed"
        ) +
        scale_color_manual(
          values = setNames(c(col_primary, col_accent), metrics_sel),
          name   = NULL
        ) +
        scale_y_continuous(
          name     = metrics_sel[1],
          sec.axis = sec_axis(~ . / ratio, name = metrics_sel[2])
        ) +
        theme_minimal() +
        theme(
          legend.position      = "top",
          axis.title.y.left    = element_text(size = 14, color = col_primary, margin = margin(r = 15)),
          axis.title.y.right   = element_text(size = 14, color = col_primary, margin = margin(l = 15)),
          axis.title.x         = element_blank(),
          axis.text.x          = element_text(size = 12, color = col_primary),
          axis.text.y          = element_text(size = 12, color = col_primary)
        )
    }
  })
  
  # Athlete table
  output$athlete_table <- renderDT({
    df <- athlete_data()
    req(nrow(df) > 0)
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "t",
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "$(thead).find('th').css('text-align', 'left');",
          "}"
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns    = names(df),
        `text-align` = "left"
      )
  })
  
  # ---- ATHLETE TAB: ACWR time series ----------------------------------
  
  observe({
    names_gps <- if ("Name" %in% names(gps_data)) gps_data$Name else NULL
    demo      <- data_list[["Demographics"]]
    names_demo <- if (!is.null(demo) && "Name" %in% names(demo)) demo$Name else NULL
    athletes_acwr <- sort(unique(na.omit(c(names_gps, names_demo))))
    updatePickerInput(session, "acwr_athlete", choices = athletes_acwr)
  }, priority = 1)
  
  acwr_athlete_data <- reactive({
    req(input$acwr_athlete, input$acwr_athlete_daterange)
    df <- gps_data %>%
      normalize_names_dates() %>%
      filter(
        Name == input$acwr_athlete,
        Date >= input$acwr_athlete_daterange[1],
        Date <= input$acwr_athlete_daterange[2],
        !is.na(ACWR)
      )
    df
  })
  
  output$acwr_athlete_plot <- renderPlot({
    df <- acwr_athlete_data()
    validate(need(nrow(df) > 0, "No ACWR data for this athlete and date range."))
    
    ggplot(df, aes(x = Date, y = ACWR, group = 1)) +
      geom_line(color = col_primary, size = 1.2) +
      geom_point(color = col_primary, size = 2) +
      scale_y_continuous(limits = c(0, 2), expand = expansion(mult = 0.05)) +
      geom_hline(yintercept = 1.3, color = "#e3b506", linetype = "dashed", size = 1) +
      geom_hline(yintercept = 1.5, color = "#b71c1c", linetype = "dashed", size = 1) +
      geom_hline(yintercept = 0.8, color = "#b71c1c", linetype = "dashed", size = 1) +
      theme_minimal() +
      labs(
        title = "Acute:Chronic Workload Ratio",
        x     = NULL,
        y     = NULL
      ) +
      theme(
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 14, color = col_primary),
        axis.text.y = element_text(size = 14, color = col_primary)
      )
  })
}

# ---- 5. RUN APP -------------------------------------------------------------

shinyApp(ui = ui, server = server)
