library(arrow)
library(duckdb)
library(shiny)
library(bslib)
library(tidyverse)
library(glue)
library(plotly)

# Proportion of viewport height for plot and table (must sum to <= 100)
PLOT_VH <- 45

QUANTILE <- 50
REFRAC <- "mid"

data_sources <- tibble(
  name = c(
    "filter1-rtGates",
    "filter1-manualGates"
  ),
  dirs = c(
    "/home/chrisbee/mnt/pico/seaflow/seasnakemake-pipeline-2026-04/seasnakemake-realtime-gating",
    "/home/chrisbee/mnt/pico/seaflow/seasnakemake-pipeline-2026-04/seasnakemake"
  )
)

read_parquet_duckdb <- function(path) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbGetQuery(con, glue("SELECT * FROM read_parquet('{path}')")) |>
    as_tibble()
}

read_sfl_table <- function(db_file) {
  popcycle::get_sfl_table(db_file, outlier_join = TRUE) |>
    as_tibble() |>
    select(
      date,
      lat,
      lon,
      conductivity,
      salinity,
      ocean_tmp,
      par,
      stream_pressure,
      event_rate,
      flag
    ) |>
    rename(
      time = date
    ) |>
    mutate(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    ) |>
    arrange(time)
}

read_stat_file <- function(stat_file) {
  read_parquet_duckdb(stat_file) |>
    filter(quantile == QUANTILE) |>
    select(
      time,
      pop,
      opp_evt_ratio,
      abundance,
      glue("diam_{REFRAC}_med"),
      flag
    ) |>
    rename(
      diameter = glue("diam_{REFRAC}_med")
    ) |>
    mutate(time = lubridate::ymd_hms(time, quiet = TRUE)) |>
    arrange(time)
}

#' Find all <cruise> stat parquet and outlier db files
list_data_source_files <- function(data_sources) {
  pmap(data_sources, function(name, dirs) {
    stat_files <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.stat\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )

    outlier_dbs <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.outlier\\.db$",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(stat_files) == 0 && length(outlier_dbs) == 0) {
      return(tibble(
        name = character(),
        cruise = character(),
        stat_file = character(),
        outlier_db = character()
      ))
    }

    stat_tbl <- tibble(
      name = name,
      cruise = basename(dirname(stat_files)),
      stat_file = stat_files
    )

    outlier_tbl <- tibble(
      name = name,
      cruise = basename(dirname(outlier_dbs)),
      outlier_db = outlier_dbs
    )

    full_join(stat_tbl, outlier_tbl, by = c("name", "cruise"))
  }) |>
    list_rbind()
}

all_data_source_files <- list_data_source_files(data_sources)

ui <- page_sidebar(
  title = "SeaFlow Inspect",
  sidebar = sidebar(
    selectInput("cruise", "Cruise", choices = sort(unique(all_data_source_files$cruise))),
    selectInput("variation", "Variation", choices = NULL),
    checkboxGroupInput("exclude_flags", "Exclude flags", choices = NULL)
  ),
  card(
    card_header("Stat"),
    layout_columns(
      col_widths = c(4, 8),
      div(
        selectInput("stat_pop", "Population", choices = NULL),
        selectInput("stat_metric", "Metric", choices = c(
          "OPP/EVT ratio" = "opp_evt_ratio",
          "Abundance" = "abundance",
          "Diameter" = "diameter"
        ))
      ),
      plotlyOutput("stat_plot", height = paste0(PLOT_VH, "vh"))
    )
  ),
  card(
    card_header("SFL"),
    layout_columns(
      col_widths = c(4, 8),
      selectInput("sfl_metric", "Metric", choices = c(
        "Latitude" = "lat",
        "Longitude" = "lon",
        "Conductivity" = "conductivity",
        "Salinity" = "salinity",
        "Ocean temperature" = "ocean_tmp",
        "PAR" = "par",
        "Stream pressure" = "stream_pressure",
        "Event rate" = "event_rate"
      )),
      plotlyOutput("sfl_plot", height = paste0(PLOT_VH, "vh"))
    )
  )
)

server <- function(input, output, session) {
  # Update variation choices whenever cruise changes
  observe({
    files <- all_data_source_files |>
      filter(cruise == input$cruise) |>
      filter(!is.na(stat_file))
    choices <- setNames(files$name, files$name)
    updateSelectInput(session, "variation", choices = choices)
  })

  selected_files <- reactive({
    req(input$cruise, input$variation)
    files <- all_data_source_files |>
      filter(cruise == input$cruise, name == input$variation)

    validate(need(nrow(files) > 0, "No files found for selected cruise and variation."))
    files |> slice(1)
  })

  # Read selected stat file into a tibble
  stat_data <- reactive({
    req(!is.na(selected_files()$stat_file[[1]]))
    df <- read_stat_file(selected_files()$stat_file[[1]])
    print(head(df, 2))
    print(inherits(df$time, "POSIXt"))
    print(inherits(df$time, "POSIXct"))
    print(typeof(df$time))
    df
  })

  # Read selected outlier database SFL table
  sfl_data <- reactive({
    req(!is.na(selected_files()$outlier_db[[1]]))
    read_sfl_table(selected_files()$outlier_db[[1]])
  })

  # Update population choices when data changes, preserving selection if possible
  observe({
    pops <- sort(unique(stat_data()$pop))
    selected <- if (input$stat_pop %in% pops) input$stat_pop else pops[[1]]
    updateSelectInput(session, "stat_pop", choices = pops, selected = selected)
  })

  # Update available flag values for selected variation
  observe({
    flags_stat <- stat_data() |> pull(flag)
    flags_sfl <- if (!is.na(selected_files()$outlier_db[[1]])) sfl_data() |> pull(flag) else numeric()
    flags <- sort(unique(c(flags_stat, flags_sfl))) |> as.character()

    selected <- intersect(input$exclude_flags, flags)
    updateCheckboxGroupInput(session, "exclude_flags", choices = flags, selected = selected)
  })

  # Shared filtered stat data for table and plot
  filtered_stat_data <- reactive({
    req(input$stat_pop)
    df <- stat_data() |> filter(pop == input$stat_pop)

    if (!is.null(input$exclude_flags) && length(input$exclude_flags) > 0) {
      df <- df |> filter(!(as.character(flag) %in% input$exclude_flags))
    }

    print(head(df, 2))
    print(inherits(df$time, "POSIXt"))
    print(inherits(df$time, "POSIXct"))
    print(typeof(df$time))

    df
  })

  filtered_sfl_data <- reactive({
    df <- sfl_data()

    if (!is.null(input$exclude_flags) && length(input$exclude_flags) > 0) {
      df <- df |> filter(!(as.character(flag) %in% input$exclude_flags))
    }

    df
  })

  shared_x_range <- reactive({
    df <- filtered_sfl_data()
    validate(need(nrow(df) > 0, "No SFL rows available for current filters."))
    c(min(df$time, na.rm = TRUE), max(df$time, na.rm = TRUE))
  })

  output$stat_plot <- renderPlotly({
    req(input$stat_metric)
    df <- filtered_stat_data()
    metric_label <- names(which(c(
      "OPP/EVT ratio" = "opp_evt_ratio",
      "Abundance" = "abundance",
      "Diameter" = "diameter"
    ) == input$stat_metric))

    validate(need(nrow(df) > 0, "No rows to plot for current filters."))

    plot_ly(
      data = df,
      x = ~time,
      y = as.formula(paste0("~", input$stat_metric)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 6)
    ) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        yaxis = list(title = metric_label)
      )
  })

  output$sfl_plot <- renderPlotly({
    req(input$sfl_metric)
    df <- filtered_sfl_data()

    validate(need(nrow(df) > 0, "No SFL rows to plot for current filters."))

    plot_ly(
      data = df,
      x = ~time,
      y = as.formula(paste0("~", input$sfl_metric)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 6)
    ) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        yaxis = list(title = input$sfl_metric)
      )
  })
}

shinyApp(ui, server)