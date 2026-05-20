griddedScatter3dUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Gridded 3D Scatter"),
    bslib::layout_columns(
      shiny::selectInput(
        ns("x_var"),
        "X value",
        choices = c("fsc_small", "pe", "chl_small", "Qc", "diam"),
        selected = "fsc_small"
      ),
      shiny::selectInput(
        ns("y_var"),
        "Y value",
        choices = c("chl_small", "pe", "fsc_small", "Qc", "diam"),
        selected = "chl_small"
      ),
      shiny::selectInput(
        ns("z_var"),
        "Z value",
        choices = c("pe", "Qc", "diam", "fsc_small", "chl_small"),
        selected = "pe"
      ),
      shiny::selectInput(
        ns("size_var"),
        "Marker size",
        choices = c("n", "Qc_sum"),
        selected = "n"
      ),
      shiny::checkboxInput(
        ns("x_log"),
        "Log X axis",
        value = TRUE
      ),
      shiny::checkboxInput(
        ns("y_log"),
        "Log Y axis",
        value = TRUE
      ),
      shiny::checkboxInput(
        ns("z_log"),
        "Log Z axis",
        value = TRUE
      )
    ),
    shiny::uiOutput(ns("pop_filter_ui")),
    shiny::textOutput(ns("active_hour_text")),
    plotly::plotlyOutput(ns("plot"), height = "70vh")
  )
}

griddedScatter3dServer <- function(id, gridded_df, selected_x, active_tab = NULL, active_tab_value = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      scatter_is_active <- shiny::reactive({
        if (is.null(active_tab)) {
          return(TRUE)
        }

        shiny::req(!is.null(active_tab_value))
        identical(active_tab(), active_tab_value)
      })

      gridded_plot_df <- shiny::reactive({
        df <- gridded_df()
        shiny::validate(shiny::need(nrow(df) > 0, "No gridded data available."))
        shiny::validate(shiny::need("date" %in% names(df), "Gridded data is missing 'date' column."))

        df |>
          dplyr::mutate(date = as.POSIXct(date, tz = "UTC"))
      })

      available_pops <- shiny::reactive({
        df <- gridded_plot_df()
        shiny::validate(shiny::need("pop" %in% names(df), "Gridded data is missing 'pop' column."))
        sort(unique(df$pop))
      })

      output$pop_filter_ui <- shiny::renderUI({
        pops <- available_pops()
        current <- isolate(input$pop_filter)
        valid <- intersect(current, pops)
        default_selected <- setdiff(pops, "unknown")
        if (length(default_selected) == 0) {
          default_selected <- pops
        }
        selected <- if (!is.null(current)) valid else default_selected

        shiny::checkboxGroupInput(
          session$ns("pop_filter"),
          "Populations",
          choices = pops,
          selected = selected,
          inline = TRUE
        )
      })

      selected_hour <- shiny::reactive({
        shiny::req(scatter_is_active())
        ts <- selected_x()
        shiny::validate(shiny::need(!is.null(ts), "Select a Stat x time point to view the corresponding gridded hour."))
        lubridate::floor_date(ts, unit = "hour")
      })

      output$active_hour_text <- shiny::renderText({
        shiny::req(scatter_is_active())
        hour_ts <- selected_hour()
        glue::glue("Displaying gridded hour: {format(hour_ts, '%Y-%m-%d %H:%M:%S %Z')}")
      })

      scatter_plot_data <- shiny::reactive({
        shiny::req(scatter_is_active())
        df <- gridded_plot_df()
        x_var <- shiny::req(input$x_var)
        y_var <- shiny::req(input$y_var)
        z_var <- shiny::req(input$z_var)
        x_log <- isTRUE(input$x_log)
        y_log <- isTRUE(input$y_log)
        z_log <- isTRUE(input$z_log)
        size_var <- shiny::req(input$size_var)
        selected_pops <- input$pop_filter
        hour_ts <- selected_hour()
        hour_end <- hour_ts + lubridate::hours(1)
        required_cols <- c(x_var, y_var, z_var, size_var, "pop", "date")

        shiny::validate(shiny::need(all(required_cols %in% names(df)), "Gridded data is missing one or more selected columns."))
        shiny::validate(shiny::need(length(selected_pops) > 0, "Select at least one population."))

        plot_data <- df |>
          dplyr::filter(
            date >= hour_ts,
            date < hour_end,
            pop %in% selected_pops
          ) |>
          dplyr::filter(
            !is.na(.data[[x_var]]),
            !is.na(.data[[y_var]]),
            !is.na(.data[[z_var]]),
            !is.na(.data[[size_var]])
          )

        if (x_log) {
          plot_data <- plot_data |>
            dplyr::filter(.data[[x_var]] > 0)
        }

        if (y_log) {
          plot_data <- plot_data |>
            dplyr::filter(.data[[y_var]] > 0)
        }

        if (z_log) {
          plot_data <- plot_data |>
            dplyr::filter(.data[[z_var]] > 0)
        }

        shiny::validate(shiny::need(nrow(plot_data) > 0, "No gridded data available for the selected hour and filters."))
        plot_data
      })

      output$plot <- plotly::renderPlotly({
        x_var <- shiny::req(input$x_var)
        y_var <- shiny::req(input$y_var)
        z_var <- shiny::req(input$z_var)
        x_log <- isTRUE(input$x_log)
        y_log <- isTRUE(input$y_log)
        z_log <- isTRUE(input$z_log)
        size_var <- shiny::req(input$size_var)
        plot_data <- scatter_plot_data()

        size_values <- plot_data[[size_var]]
        size_range <- range(size_values, na.rm = TRUE)
        if (diff(size_range) == 0) {
          marker_sizes <- rep(10, length(size_values))
        } else {
          marker_sizes <- scales::rescale(size_values, to = c(3, 18), from = size_range)
        }

        plot_data$marker_size <- marker_sizes
        plot_data$hover_text <- paste(
          "pop:", plot_data$pop,
          paste0(x_var, ": ", signif(plot_data[[x_var]], 5)),
          paste0(y_var, ": ", signif(plot_data[[y_var]], 5)),
          paste0(z_var, ": ", signif(plot_data[[z_var]], 5)),
          paste0(size_var, ": ", signif(plot_data[[size_var]], 5)),
          sep = "<br>"
        )

        plotly::plot_ly(
          data = plot_data,
          x = as.formula(paste0("~", x_var)),
          y = as.formula(paste0("~", y_var)),
          z = as.formula(paste0("~", z_var)),
          color = ~pop,
          colors = "Set2",
          type = "scatter3d",
          mode = "markers",
          marker = list(
            size = ~marker_size,
            opacity = 0.75,
            line = list(width = 0)
          ),
          hovertemplate = "%{text}<extra></extra>",
          text = ~hover_text
        ) |>
          plotly::layout(
            scene = list(
              aspectmode = "cube",
              xaxis = list(title = x_var, type = if (x_log) "log" else "linear"),
              yaxis = list(title = y_var, type = if (y_log) "log" else "linear"),
              zaxis = list(title = z_var, type = if (z_log) "log" else "linear")
            ),
            legend = list(title = list(text = "Population"))
          )
      })
    }
  )
}
