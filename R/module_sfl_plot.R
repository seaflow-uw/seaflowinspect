# SFL plot module
# -----------------------------------------------------------------------------
sflPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("SFL"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::selectInput(ns("metric"), "Metric", choices = c(
        "PAR" = "par",
        "Ocean temperature" = "ocean_tmp",
        "Salinity" = "salinity",
        "Conductivity" = "conductivity",
        "Latitude" = "lat",
        "Longitude" = "lon",
        "Stream pressure" = "stream_pressure",
        "Event rate" = "event_rate"
      )),
      plotly::plotlyOutput(ns("plot"), height = paste0(SFL_PLOT_VH, "vh"))
    )
  )
}

sflPlotServer <- function(id, sfl_data, x_range, selected_x) {
  moduleServer(
    id,
    function(input, output, session) {

      selected_vline_shapes <- reactive({
        build_selected_vline_shapes(selected_x())
      })

      output$plot <- renderPlotly({
        req(input$metric)

        validate(need(nrow(sfl_data()) > 0, "No SFL rows to plot for current filters."))

        plot_ly(
          data = sfl_data(),
          x = ~time,
          y = as.formula(paste0("~", input$metric)),
          type = "scatter",
          mode = "markers",
          marker = list(size = 3)
        ) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            yaxis = list(title = input$metric),
            shapes = selected_vline_shapes()
          )
      })
    }
  )
}