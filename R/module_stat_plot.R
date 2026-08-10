
# Stat plot module
# -----------------------------------------------------------------------------
statPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Stat"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::div(
        shiny::selectInput(ns("pop"), "Population", choices = NULL),
        shiny::selectInput(ns("metric"), "Metric", choices = c(
          "Abundance" = "abundance",
          "Diameter" = "diameter",
          "forward-scatter" = "fsc_med",
          "chlorophyll" = "chl_med",
          "phycoerythrin" = "pe_med",
          "Count" = "n_count",
          "OPP/EVT ratio" = "opp_evt_ratio"
        )),
        shiny::checkboxInput(ns("aggregate_hourly"), "Aggregate by hour", value = FALSE)
      ),
      plotly::plotlyOutput(ns("plot"), height = paste0(STAT_PLOT_VH, "vh"))
    )
  )
}

statPlotServer <- function(
  id,
  stat_data,
  filtered_stat_data,
  x_range,
  selected_x
) {
  moduleServer(
    id,
    function(input, output, session) {
      selected_vline_shapes <- reactive({
        build_selected_vline_shapes(selected_x())
      })

      # Update population choices when data changes, preserving selection if possible
      observe({
        pops <- sort(unique(stat_data()$pop))
        selected <- if (input$pop %in% pops) input$pop else pops[[1]]
        updateSelectInput(session, "pop", choices = pops, selected = selected)
      })

      # Filter for selected population
      pop_stat_data <- reactive({
        req(input$pop)
        filtered_stat_data() |> filter(pop == input$pop)
      })

      # Optionally collapse to one row per hour. n_count is summed since it's
      # a count of particles seen per timepoint; every other metric is
      # medianed across the hour.
      plot_stat_data <- reactive({
        df <- pop_stat_data()
        if (!isTRUE(input$aggregate_hourly)) {
          return(df)
        }
        df |>
          mutate(time = lubridate::floor_date(time, unit = "hour")) |>
          group_by(time, pop) |>
          summarise(
            across(
              c(opp_evt_ratio, abundance, fsc_med, chl_med, pe_med, diameter),
              ~median(.x, na.rm = TRUE)
            ),
            n_count = sum(n_count, na.rm = TRUE),
            .groups = "drop"
          )
      })

      output$plot <- renderPlotly({
        req(input$metric)
        df <- plot_stat_data()
        metric_label <- names(which(c(
          "OPP/EVT ratio" = "opp_evt_ratio",
          "Abundance" = "abundance",
          "Diameter" = "diameter",
          "Count" = "n_count"
        ) == input$metric))

        validate(need(nrow(df) > 0, "No rows to plot for current filters."))

        marker_size <- if (isTRUE(input$aggregate_hourly)) 6 else 3

        plot_ly(
          data = df,
          x = ~time,
          y = as.formula(paste0("~", input$metric)),
          type = "scatter",
          mode = "markers",
          marker = list(size = marker_size)
        ) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            yaxis = list(title = metric_label),
            shapes = selected_vline_shapes()
          )
      })
    }
  )
}