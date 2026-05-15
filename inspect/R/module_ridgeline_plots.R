ridgelinePlotUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Ridgeline Plot"),
    shiny::selectInput(
      ns("x_var"),
      "X value",
      choices = c("fsc_small", "pe", "chl_small", "Qc"),
      selected = "fsc_small"
    ),
    shiny::plotOutput(ns("plot"), height = "80vh")
  )
}

ridgelinePlotServer <- function(id, gridded_df, grid_bins_df) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- shiny::renderPlot({
        req(gridded_df())
        req(grid_bins_df())
        x_var <- req(input$x_var)
        x_coord_col <- paste0(x_var, "_coord")

        validate(need(
          x_coord_col %in% names(gridded_df()),
          paste("Gridded data is missing expected coordinate column", x_coord_col)
        ))

        plot_data <- gridded_df() |>
          # Collapse grid dimensions other than the selected x coordinate.
          group_by(date, pop, .data[[x_coord_col]]) |>
          summarise(
            n = sum(n),
            Qc_sum = sum(Qc_sum),
            .groups = "drop"
          ) |>
          filter(pop != "unknown")
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            x = .data[[x_coord_col]],
            y = date,
            height = Qc_sum,
            group = interaction(date, pop),
            fill = pop
          )
        ) +
          ggridges::geom_ridgeline(scale = 100, alpha = 0.6) +
          ggplot2::labs(x = x_var)
      })
    }
  )
}