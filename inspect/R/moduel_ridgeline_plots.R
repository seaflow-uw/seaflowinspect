ridgelinePlotUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Ridgeline Plot"),
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
        plot_data <- gridded_df() |>
          group_by(date, pop, fsc_small_coord) |>
          summarise(
            n = sum(n),
            Qc_sum = sum(Qc_sum),
            .groups = "drop"
          ) |>
          filter(pop != "unknown")
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            x = fsc_small_coord,
            y = date,
            height = Qc_sum,
            group = interaction(date, pop),
            fill = pop
          )
        ) +
          ggridges::geom_ridgeline(scale=100, alpha=0.6)
      })
    }
  )
}