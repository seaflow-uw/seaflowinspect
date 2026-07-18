# Time navigator plot module
# -----------------------------------------------------------------------------
# Shows every distinct time point present in the stat table, independent of
# the population/metric selected in the Stat plot, so a time point can be
# selected regardless of which tab is currently active.
timeNavPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    fill = FALSE,
    bslib::card_header("Time Navigator"),
    plotly::plotlyOutput(ns("plot"), height = paste0(TIME_NAV_PLOT_VH, "vh"))
  )
}

timeNavPlotServer <- function(id, stat_data, x_range, selected_x_val) {
  moduleServer(
    id,
    function(input, output, session) {
      time_nav_click_event <- reactive({
        plotly::event_data(
          "plotly_click",
          source = session$ns("plot_click"),
          priority = "event"
        )
      })

      observeEvent(time_nav_click_event(), {
        click <- time_nav_click_event()
        if (!is.null(click) && nrow(click) > 0 && !is.null(click$x)) {
          parsed_click_x <- normalize_click_time(click$x[[1]])
          if (!is.null(parsed_click_x)) {
            selected_x_val(parsed_click_x)
          }
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      selected_vline_shapes <- reactive({
        build_selected_vline_shapes(selected_x_val())
      })

      time_points <- reactive({
        stat_data() |>
          dplyr::distinct(time) |>
          dplyr::arrange(time)
      })

      output$plot <- renderPlotly({
        df <- time_points()
        validate(need(nrow(df) > 0, "No rows to plot for current filters."))

        p <- plot_ly(
          data = df,
          x = ~time,
          y = 0,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8, symbol = "line-ns-open", color = "#1f77b4"),
          hovertemplate = "%{x}<extra></extra>",
          source = session$ns("plot_click")
        ) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            yaxis = list(
              title = "",
              visible = FALSE,
              fixedrange = TRUE,
              range = c(-1, 1)
            ),
            margin = list(t = 10, b = 40, l = 40, r = 20),
            shapes = selected_vline_shapes()
          ) |>
          plotly::config(displayModeBar = FALSE)

        plotly::event_register(p, "plotly_click")
      })
    }
  )
}
