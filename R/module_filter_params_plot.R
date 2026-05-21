filterParamsPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Filter Params"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::div(),
      plotly::plotlyOutput(ns("plot"), height = paste0(FILTER_PARAMS_PLOT_VH, "vh"))
    )
  )
}

filterParamsServer <- function(id, filter_params_data, x_range, selected_x) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        df <- filter_params_data()


        required_cols <- c("start_date", "end_date", "beads_fsc_small", "beads_D1", "beads_D2")
        validate(need(all(required_cols %in% names(df)), "Filter params table missing required columns."))

        make_segment_plot <- function(col_name, y_title, color) {
          plot_df <- df |>
            mutate(y_value = .data[[col_name]]) |>
            filter(!is.na(start_date), !is.na(end_date), !is.na(y_value))

          p <- plot_ly(plot_df) |>
            add_segments(
              x = ~start_date,
              xend = ~end_date,
              y = ~y_value,
              yend = ~y_value,
              name = y_title,
              legendgroup = y_title,
              showlegend = FALSE,
              hovertemplate = paste0(
                "Start: %{x}<br>",
                "End: %{xend}<br>",
                y_title,
                ": %{y}<extra></extra>"
              ),
              line = list(width = 3, color = color)
            )

          vx <- selected_x()
          if (!is.null(vx) && nrow(plot_df) > 0) {
            y_min <- min(plot_df$y_value, na.rm = TRUE)
            y_max <- max(plot_df$y_value, na.rm = TRUE)
            if (is.finite(y_min) && is.finite(y_max)) {
              if (y_min == y_max) {
                y_min <- y_min - 0.5
                y_max <- y_max + 0.5
              }
              p <- p |>
                add_segments(
                  data = tibble::tibble(x0 = vx, x1 = vx, y0 = y_min, y1 = y_max),
                  x = ~x0,
                  xend = ~x1,
                  y = ~y0,
                  yend = ~y1,
                  inherit = FALSE,
                  showlegend = FALSE,
                  hoverinfo = "skip",
                  line = list(color = "#d62728", width = 1.5, dash = "dot")
                )
            }
          }

          p |>
            layout(yaxis = list(title = y_title))
        }

        p1 <- make_segment_plot("beads_fsc_small", "FSC", "#1f77b4")
        p2 <- make_segment_plot("beads_D1", "D1", "#ff7f0e")
        p3 <- make_segment_plot("beads_D2", "D2", "#2ca02c")

        subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            xaxis2 = list(range = x_range()),
            xaxis3 = list(range = x_range())
          )
      })
    }
  )
}