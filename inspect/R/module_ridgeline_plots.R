ridgelinePlotUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Ridgeline Plot"),
    bslib::layout_columns(
      shiny::selectInput(
        ns("x_var"),
        "X value",
        choices = c("fsc_small", "pe", "chl_small", "Qc"),
        selected = "fsc_small"
      ),
      shiny::selectInput(
        ns("height_var"),
        "Height value",
        choices = c("n", "Qc_sum"),
        selected = "Qc_sum"
      )
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
        height_var <- req(input$height_var)
        x_coord_col <- paste0(x_var, "_coord")

        validate(need(
          x_coord_col %in% names(gridded_df()),
          paste("Gridded data is missing expected coordinate column", x_coord_col)
        ))
        validate(need(
          height_var %in% c("n", "Qc_sum"),
          paste("Unsupported ridgeline height value", height_var)
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
            height = .data[[height_var]],
            group = interaction(date, pop),
            fill = pop
          )
        ) +
          ggridges::geom_ridgeline(scale = 100, alpha = 0.6) +
          ggplot2::labs(x = x_var, height = height_var)
      })
    }
  )
}