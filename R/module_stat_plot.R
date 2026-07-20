
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
        ))
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

      output$plot <- renderPlotly({
        req(input$metric)
        df <- pop_stat_data()
        metric_label <- names(which(c(
          "OPP/EVT ratio" = "opp_evt_ratio",
          "Abundance" = "abundance",
          "Diameter" = "diameter",
          "Count" = "n_count"
        ) == input$metric))

        validate(need(nrow(df) > 0, "No rows to plot for current filters."))

        plot_ly(
          data = df,
          x = ~time,
          y = as.formula(paste0("~", input$metric)),
          type = "scatter",
          mode = "markers",
          marker = list(size = 3)
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