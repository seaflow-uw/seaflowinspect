ridgelinePlotUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Ridgeline Plot"),
    bslib::layout_columns(
      shiny::selectInput(
        ns("x_var"),
        "X value",
        choices = c("fsc_small", "pe", "chl_small", "Qc", "diam"),
        selected = "fsc_small"
      ),
      shiny::selectInput(
        ns("height_var"),
        "Height value",
        choices = c("n", "Qc_sum"),
        selected = "Qc_sum"
      ),
      shiny::numericInput(
        ns("scale"),
        "Scale",
        value = 100,
        min = 1
      )
    ),
    shiny::uiOutput(ns("pop_filter_ui")),
    shiny::checkboxInput(
      ns("selected_hour_only"),
      "Limit to selected hour",
      value = TRUE
    ),
    shiny::textOutput(ns("active_time_range_text")),
    shiny::uiOutput(ns("time_range_ui")),
    shiny::plotOutput(ns("plot"), height = "40vh"),
    plotly::plotlyOutput(ns("plotly_plot"), height = "50vh")
  )
}

# Ridgeline module server.
#
# This module can gate its expensive gridded-data outputs on the active top-
# level tab to preserve lazy loading. To enable that behavior, the containing
# tabset must expose its selected tab as a Shiny input, and the call site must
# pass both:
# - `active_tab`: a reactive returning the current tabset value, such as
#   `reactive(input$main_tab)`
# - `active_tab_value`: the stable internal value for the Ridgeline tab, such as
#   `"ridgeline"`
#
# The UI should set an explicit tab `value`, and that value must match the
# `active_tab_value` argument passed at the server call site. Prefer this
# stable internal value over the display label, since labels may change during
# refactors.
#
# If `active_tab` is omitted, the module behaves as always-active and no lazy
# loading guard is applied.
ridgelinePlotServer <- function(id, gridded_df, grid_bins_df, selected_x, active_tab = NULL, active_tab_value = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      # Hidden outputs may still initialize once during app startup, so use an
      # explicit active-tab gate to keep gridded parquet reads deferred until
      # the Ridgeline tab is actually shown.
      ridgeline_is_active <- reactive({
        if (is.null(active_tab)) {
          return(TRUE)
        }

        req(!is.null(active_tab_value))
        identical(active_tab(), active_tab_value)
      })

      gridded_plot_df <- reactive({
        df <- gridded_df()
        validate(need(nrow(df) > 0, "No gridded data available."))
        validate(need("date" %in% names(df), "Gridded data is missing 'date' column."))

        df |>
          dplyr::mutate(date = as.POSIXct(date, tz = "UTC"))
      })

      available_time_range <- reactive({
        df <- gridded_plot_df()
        validate(need(any(!is.na(df$date)), "Gridded data date values are missing."))
        c(
          min(df$date, na.rm = TRUE),
          max(df$date, na.rm = TRUE) + lubridate::hours(1)
        )
      })

      manual_time_range <- reactive({
        time_range <- available_time_range()
        current_value <- input$time_range

        if (length(current_value) != 2) {
          return(time_range)
        }

        c(
          max(as.POSIXct(current_value[[1]], tz = "UTC"), time_range[[1]]),
          min(as.POSIXct(current_value[[2]], tz = "UTC"), time_range[[2]])
        )
      })

      effective_time_range <- reactive({
        if (!isTRUE(input$selected_hour_only)) {
          return(manual_time_range())
        }

        ts <- selected_x()
        validate(need(!is.null(ts), "Select a Stat x time point to use the selected hour range."))

        available_range <- available_time_range()
        selected_hour <- lubridate::floor_date(ts, unit = "hour")
        selected_hour_end <- selected_hour + lubridate::hours(1)

        c(
          max(selected_hour, available_range[[1]]),
          min(selected_hour_end, available_range[[2]])
        )
      })

      available_pops <- reactive({
        df <- gridded_plot_df()
        validate(need(nrow(df) > 0, "No gridded data available."))
        validate(need("pop" %in% names(df), "Gridded data is missing 'pop' column."))
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

      debounced_effective_time_range <- shiny::debounce(
        reactive({
          req(ridgeline_is_active())
          effective_time_range()
        }),
        millis = 250
      )

      output$active_time_range_text <- shiny::renderText({
        req(ridgeline_is_active())

        if (!isTRUE(input$selected_hour_only)) {
          time_range <- manual_time_range()
          return(glue::glue(
            "Using manual time range: {format(time_range[[1]], '%Y-%m-%d %H:%M:%S %Z')} to {format(time_range[[2]], '%Y-%m-%d %H:%M:%S %Z')}"
          ))
        }

        ts <- selected_x()
        if (is.null(ts)) {
          return("Selected-hour mode is on. Select a Stat x time point to define the active range.")
        }

        time_range <- effective_time_range()
        glue::glue(
          "Using selected hour: {format(time_range[[1]], '%Y-%m-%d %H:%M:%S %Z')} to {format(time_range[[2]], '%Y-%m-%d %H:%M:%S %Z')}"
        )
      })

      output$time_range_ui <- shiny::renderUI({
        req(ridgeline_is_active())

        time_range <- available_time_range()
        current_value <- isolate(input$time_range)

        if (length(current_value) != 2) {
          current_value <- time_range
        } else {
          current_value <- c(
            max(as.POSIXct(current_value[[1]], tz = "UTC"), time_range[[1]]),
            min(as.POSIXct(current_value[[2]], tz = "UTC"), time_range[[2]])
          )
        }

        shiny::div(
          style = if (isTRUE(input$selected_hour_only)) {
            "opacity: 0.5; pointer-events: none;"
          },
          shiny::sliderInput(
            session$ns("time_range"),
            "Time range",
            min = time_range[[1]],
            max = time_range[[2]],
            value = current_value,
            timeFormat = "%Y-%m-%d %H:%M",
            timezone = "UTC",
            width = "100%"
          )
        )
      })

      ridgeline_plot_data <- reactive({
        req(ridgeline_is_active())
        req(gridded_df())
        req(grid_bins_df())

        df <- gridded_plot_df()
        x_var <- req(input$x_var)
        height_var <- req(input$height_var)
        selected_pops <- input$pop_filter
        time_range <- debounced_effective_time_range()

        validate(need(
          x_var %in% names(df),
          paste("Gridded data is missing expected column", x_var)
        ))
        validate(need(
          height_var %in% c("n", "Qc_sum"),
          paste("Unsupported ridgeline height value", height_var)
        ))
        validate(need(length(selected_pops) > 0, "Select at least one population."))
        validate(need(length(time_range) == 2, "Select a valid ridgeline time range."))

        df <- df |>
          dplyr::filter(
            date >= time_range[[1]],
            date < time_range[[2]],
            pop %in% selected_pops
          )

        validate(need(nrow(df) > 0, "No gridded data available for the selected filters."))

        df |>
          dplyr::group_by(date, pop, .data[[x_var]]) |>
          dplyr::summarise(
            n = sum(n),
            Qc_sum = sum(Qc_sum),
            .groups = "drop"
          )
      })

      output$plot <- shiny::renderPlot({
        x_var <- req(input$x_var)
        height_var <- req(input$height_var)
        scale <- req(input$scale)
        plot_data <- ridgeline_plot_data()

        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            x = .data[[x_var]],
            y = date,
            height = .data[[height_var]],
            group = interaction(date, pop),
            fill = pop
          )
        ) +
          ggridges::geom_ridgeline(scale = scale, alpha = 0.6, color = NA) +
          ggplot2::scale_x_log10() +
          ggplot2::labs(x = x_var, height = height_var)
      })

      output$plotly_plot <- plotly::renderPlotly({
        x_var <- req(input$x_var)
        height_var <- req(input$height_var)
        plot_data <- ridgeline_plot_data() |>
          dplyr::arrange(pop, date, .data[[x_var]])

        validate(need(nrow(plot_data) > 0, "No gridded data available for the selected filters."))

        pops <- sort(unique(plot_data$pop))
        base_colors <- RColorBrewer::brewer.pal(max(3, min(8, length(pops))), "Set2")
        pop_colors <- stats::setNames(
          grDevices::colorRampPalette(base_colors)(length(pops)),
          pops
        )

        p <- plotly::plot_ly(type = "scatter3d", mode = "lines")

        for (pop_name in pops) {
          pop_data <- plot_data |>
            dplyr::filter(pop == .env[["pop_name"]])
          pop_dates <- unique(pop_data$date)
          show_legend <- TRUE

          for (date_value in pop_dates) {
            trace_data <- pop_data |>
              dplyr::filter(date == .env[["date_value"]])

            p <- p |>
              plotly::add_trace(
                data = trace_data,
                x = as.formula(paste0("~", x_var)),
                y = ~date,
                z = as.formula(paste0("~", height_var)),
                type = "scatter3d",
                mode = "lines",
                name = pop_name,
                legendgroup = pop_name,
                showlegend = show_legend,
                line = list(color = unname(pop_colors[[pop_name]]))
              )

            show_legend <- FALSE
          }
        }

        p |>
          plotly::layout(
            scene = list(
              xaxis = list(title = x_var, type = "log", autorange = "reversed"),
              yaxis = list(title = "Date", autorange = "reversed"),
              zaxis = list(title = height_var)
            ),
            legend = list(title = list(text = "Population"))
          )
      })
    }
  )
}