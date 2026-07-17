#' Combine the gating and gating_plan tables into one table of active gating
#' parameters, joining on gating.id == gating_plan.gating_id.
combine_gating_and_plan <- function(gating, plan) {
  dplyr::left_join(plan, gating, by = c("gating_id" = "id"))
}

vctGatingTableUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Active gating parameters"),
    DT::DTOutput(ns("gating_table")),
    bslib::card_header("Associated polygon parameters"),
    DT::DTOutput(ns("poly_table"))
  )
}

vctGatingTableServer <- function(id, gating_params) {
  moduleServer(
    id,
    function(input, output, session) {
      combined_gating <- reactive({
        gp <- gating_params()
        combine_gating_and_plan(gp$gating, gp$plan)
      })

      gating_table_data <- reactive({
        cg <- combined_gating()
        validate(need(nrow(cg) > 0, "No active gating parameters found for selected time."))

        cg |>
          dplyr::mutate(
            dplyr::across(
              c(start_date, end_date),
              ~ ifelse(
                is.na(.x),
                NA_character_,
                format(as.POSIXct(.x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
              )
            )
          )
      })

      output$gating_table <- DT::renderDT({
        DT::datatable(
          as.data.frame(gating_table_data()),
          rownames = FALSE,
          selection = list(mode = "single", target = "row"),
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }, server = FALSE)

      selected_gating_row <- reactive({
        selected_row <- input$gating_table_rows_selected
        if (length(selected_row) != 1) {
          return(NULL)
        }

        cg <- combined_gating()
        validate(need(nrow(cg) > 0, "No active gating parameters found for selected time."))

        selected_idx <- selected_row[[1]]
        validate(need(nrow(cg) >= selected_idx, "Selected gating row is no longer available."))
        cg[selected_idx, , drop = FALSE]
      })

      output$poly_table <- DT::renderDT({
        gp <- gating_params()
        selected_gating <- selected_gating_row()
        poly_rows <- gp$poly

        if (!is.null(selected_gating)) {
          poly_rows <- poly_rows |>
            dplyr::filter(pop == selected_gating$pop[[1]])
        }

        validate(need(nrow(poly_rows) > 0, "No polygon parameters found for the selected gating row."))

        DT::datatable(
          as.data.frame(poly_rows),
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }, server = FALSE)
    }
  )
}
