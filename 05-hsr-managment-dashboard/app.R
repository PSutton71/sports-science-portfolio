library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(lubridate)
library(stringr)

# ===== USER SETTINGS =====
datafile   <- "Example Data.xlsx"
gps_sheet  <- "GPS Data"
demo_sheet <- "Demographics"

# ===== LOAD GPS DATA =====
gps_raw <- read_excel(
  datafile,
  sheet = gps_sheet,
  col_types = "text",
  na = "NA"
)

gps_data <- gps_raw |>
  mutate(
    Date = case_when(
      str_detect(Date, "^[0-9]+$") ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
      str_detect(Date, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$") ~ mdy(Date),
      str_detect(Date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") ~ ymd(Date),
      TRUE ~ NA_Date_
    ),
    Name = as.character(Name),
    Type = as.character(Type),
    HSR  = suppressWarnings(as.numeric(HSR))
  ) |>
  filter(
    !is.na(Date),
    !is.na(Name),
    !is.na(Type),
    !is.na(HSR)
  )

gps_raw |>
  select(Date, Type) |>
  filter(Type == "Practice") |>
  tail(20)

gps_data |>
  filter(Type == "Practice") |>
  count(Date, sort = TRUE)

# ===== LOAD DEMOGRAPHICS DATA =====
demo_data <- read_excel(datafile, sheet = demo_sheet) |>
  mutate(
    Name = as.character(.data$Name),
    Position = as.character(.data$Position)
  ) |>
  select(Name, Position) |>
  distinct()

# ===== JOIN POSITION ONTO GPS =====
gps_data <- gps_data |>
  left_join(demo_data, by = "Name") |>
  filter(
    Type %in% c("Game", "Practice"),
    !is.na(Position)
  )

# ===== UI =====
ui <- fluidPage(
  titlePanel("HSR Session Dashboard"),
  
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId = "session_type",
        label   = "Session Type",
        choices = c("Game", "Practice"),
        selected = "Game"
      )
    ),
    column(
      width = 4,
      selectInput(
        inputId = "session_date",
        label   = "Session Date",
        choices = NULL
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      h4("Position Summary"),
      DTOutput("position_table")
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      h4("Game Day High-Speed Running"),
      plotOutput("hsr_plot", height = "650px")
    )
  ),
  br(),
)

# ===== SERVER =====
server <- function(input, output, session) {
  
  observe({
    available_dates <- gps_data |>
      filter(Type == input$session_type) |>
      distinct(Date) |>
      arrange(desc(Date)) |>
      pull(Date)
    
    updateSelectInput(
      session,
      inputId = "session_date",
      choices = available_dates,
      selected = if (length(available_dates) > 0) available_dates[1] else NULL
    )
  })
  
  selected_day_data <- reactive({
    req(input$session_type, input$session_date)
    
    df <- gps_data |>
      mutate(Date = as.Date(Date)) |>
      filter(
        Type == input$session_type,
        Date == as.Date(input$session_date)
      )
    
    validate(
      need(nrow(df) > 0, "No rows found for that selected session/date.")
    )
    
    df
  })
  
  position_summary <- reactive({
    df <- selected_day_data()
    req(nrow(df) > 0)
    
    pos_tbl <- df |>
      group_by(Position) |>
      summarise(
        N = n(),
        Mean_HSR = mean(HSR, na.rm = TRUE),
        SD_HSR   = sd(HSR, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        `Mean ± SD` = ifelse(
          is.na(SD_HSR),
          sprintf("%.1f ± NA", Mean_HSR),
          sprintf("%.1f ± %.1f", Mean_HSR, SD_HSR)
        )
      ) |>
      select(Position, N, `Mean ± SD`)
    
    team_tbl <- df |>
      summarise(
        Position = "Team",
        N = n(),
        Mean_HSR = mean(HSR, na.rm = TRUE),
        SD_HSR   = sd(HSR, na.rm = TRUE)
      ) |>
      mutate(
        `Mean ± SD` = ifelse(
          is.na(SD_HSR),
          sprintf("%.1f ± NA", Mean_HSR),
          sprintf("%.1f ± %.1f", Mean_HSR, SD_HSR)
        )
      ) |>
      select(Position, N, `Mean ± SD`)
    
    bind_rows(pos_tbl, team_tbl)
  })
  
  plot_data <- reactive({
    df <- selected_day_data() |>
      filter(
        !is.na(Name),
        !is.na(Position),
        !is.na(HSR)
      )
    
    validate(
      need(nrow(df) > 0, "No valid athlete rows remain after filtering Name, Position, and HSR.")
    )
    
    pos_ref <- df |>
      group_by(Position) |>
      summarise(
        Pos_Mean = mean(HSR, na.rm = TRUE),
        Pos_SD   = ifelse(n() > 1, sd(HSR, na.rm = TRUE), 0),
        .groups = "drop"
      )
    
    validate(
      need(nrow(pos_ref) > 0, "No positional summary could be created for this session.")
    )
    
    position_order <- intersect(c("Attack", "Midfield", "Defense"), unique(df$Position))
    
    validate(
      need(length(position_order) > 0, "No valid positions found for this selected session.")
    )
    
    df_plot <- df |>
      left_join(pos_ref, by = "Position") |>
      mutate(
        Position = factor(Position, levels = position_order),
        z_score = ifelse(
          is.na(Pos_SD) | Pos_SD == 0,
          0,
          (HSR - Pos_Mean) / Pos_SD
        ),
        abs_z = abs(z_score),
        flag_group = case_when(
          abs_z <= 1.0 ~ "Within 1 SD",
          abs_z <= 2.0 ~ "1 to 2 SD",
          abs_z > 2.0  ~ "More than 2 SD",
          TRUE ~ "Within 1 SD"
        )
      ) |>
      filter(!is.na(Position)) |>
      arrange(Position, desc(HSR), Name) |>
      mutate(
        bar_id = row_number(),
        Name = factor(Name, levels = Name)
      )
    
    validate(
      need(nrow(df_plot) > 0, "No rows available to plot for this selected session.")
    )
    
    df_plot
  })
  
  output$position_table <- renderDT({
    datatable(
      position_summary(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "t",
        ordering = FALSE
      )
    )
  })
  
  output$hsr_plot <- renderPlot({
    df_plot <- plot_data()
    req(nrow(df_plot) > 0)
    
    bg_df <- df_plot |>
      group_by(Position) |>
      summarise(
        xmin = min(bar_id) - 0.5,
        xmax = max(bar_id) + 0.5,
        pos_mean = unique(Pos_Mean)[1],
        .groups = "drop"
      ) |>
      mutate(
        fill_bg = case_when(
          Position == "Attack"   ~ "#FDEBEC",
          Position == "Midfield" ~ "#FFF8DB",
          Position == "Defense"  ~ "#EAF2FB",
          TRUE ~ "grey95"
        ),
        label_x = (xmin + xmax) / 2
      )
    
    y_top <- max(df_plot$HSR, na.rm = TRUE)
    label_y <- y_top * 1.08
    
    ggplot(df_plot, aes(x = bar_id, y = HSR)) +
      geom_rect(
        data = bg_df |> filter(Position == "Attack"),
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "#FDEBEC",
        alpha = 0.45
      ) +
      geom_rect(
        data = bg_df |> filter(Position == "Midfield"),
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "#FFF8DB",
        alpha = 0.45
      ) +
      geom_rect(
        data = bg_df |> filter(Position == "Defense"),
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "#EAF2FB",
        alpha = 0.45
      ) +
      geom_segment(
        data = bg_df,
        aes(
          x = xmin,
          xend = xmax,
          y = pos_mean,
          yend = pos_mean
        ),
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.8,
        color = "grey35"
      ) +
      geom_text(
        data = bg_df,
        aes(x = label_x, y = label_y, label = Position),
        inherit.aes = FALSE,
        fontface = "bold",
        size = 4.2,
        color = "grey25"
      ) +
      geom_col(
        aes(fill = flag_group),
        alpha = 0.95,
        width = 0.72
      ) +
      geom_text(
        aes(label = round(HSR, 0)),
        vjust = -0.35,
        size = 3
      ) +
      scale_x_continuous(
        breaks = df_plot$bar_id,
        labels = df_plot$Name,
        expand = expansion(mult = c(0.01, 0.02))
      ) +
      scale_fill_manual(
        values = c(
          "Within 1 SD" = "black",
          "1 to 2 SD" = "#E3B505",
          "More than 2 SD" = "#B71C1C"
        ),
        name = "Flag"
      ) +
      labs(
        title = paste(
          "HSR for",
          input$session_type,
          "on",
          format(as.Date(input$session_date), "%b %d, %Y")
        ),
        subtitle = "Bars flagged by distance from same-day positional mean; dashed line = positional mean",
        x = NULL,
        y = "HSR"
      ) +
      coord_cartesian(ylim = c(0, y_top * 1.12)) +
      theme_minimal(base_family = "Helvetica") +
      theme(
        plot.title = element_text(face = "bold", size = 15, colour = "grey20"),
        plot.subtitle = element_text(size = 11, colour = "grey35"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
}
  
shinyApp(ui = ui, server = server)