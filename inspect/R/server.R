server <- function(input, output, session) {
  exclude_flags_selected <- reactiveVal(c("1", "2", "3"))
  selected_stat_x <- reactiveVal(NULL)

  clicked_stat_x <- reactive({
    x <- selected_stat_x()
    if (is.null(x)) {
      return(NULL)
    }

    if (inherits(x, "POSIXt")) {
      return(as.POSIXct(x))
    }

    parsed <- suppressWarnings(lubridate::ymd_hms(as.character(x), quiet = TRUE))
    if (all(is.na(parsed))) {
      parsed <- suppressWarnings(lubridate::ymd_hm(as.character(x), quiet = TRUE))
    }
    if (all(is.na(parsed))) {
      return(NULL)
    }

    parsed[[1]]
  })

  clicked_stat_vline <- reactive({
    x <- clicked_stat_x()
    if (is.null(x)) {
      return(NULL)
    }

    list(list(
      type = "line",
      x0 = x,
      x1 = x,
      y0 = 0,
      y1 = 1,
      xref = "x",
      yref = "paper",
      line = list(color = "#d62728", width = 1.5, dash = "dot")
    ))
  })

  # Keep server-side flag selection state in sync with user interaction.
  observeEvent(input$exclude_flags, ignoreInit = TRUE, {
    exclude_flags_selected(as.character(input$exclude_flags))
  })

  # Update variation choices whenever cruise changes
  observe({
    files <- all_data_source_files |>
      filter(cruise == input$cruise) |>
      filter(!is.na(stat_file))
    choices <- setNames(files$name, files$name)
    updateSelectInput(session, "variation", choices = choices)
  })

  selected_files <- reactive({
    req(input$cruise, input$variation)
    files <- all_data_source_files |>
      filter(cruise == input$cruise, name == input$variation)

    validate(need(nrow(files) > 0, "No files found for selected cruise and variation."))
    files |> slice(1)
  })

  # Read selected stat file into a tibble
  stat_data <- reactive({
    req(!is.na(selected_files()$stat_file[[1]]))
    read_stat_file(selected_files()$stat_file[[1]])
  })

  # Read selected outlier database SFL table
  sfl_data <- reactive({
    req(!is.na(selected_files()$outlier_db[[1]]))
    read_sfl_table(selected_files()$outlier_db[[1]])
  })

  # Read selected filter parameters and cap final plan end_date at the
  # last available SFL timestamp.
  filter_params_data <- reactive({
    req(!is.na(selected_files()$outlier_db[[1]]))
    sfl_df <- sfl_data()

    read_filter_params(
      selected_files()$outlier_db[[1]],
      max_date = max(sfl_df$time, na.rm = TRUE)
    )
  })

  # Update population choices when data changes, preserving selection if possible
  observe({
    pops <- sort(unique(stat_data()$pop))
    selected <- if (input$stat_pop %in% pops) input$stat_pop else pops[[1]]
    updateSelectInput(session, "stat_pop", choices = pops, selected = selected)
  })

  # Update available flag values for selected variation
  observe({
    flags_stat <- stat_data() |> pull(flag)
    flags_sfl <- if (!is.na(selected_files()$outlier_db[[1]])) sfl_data() |> pull(flag) else numeric()
    flags <- sort(unique(c(flags_stat, flags_sfl))) |> as.character()

    selected <- intersect(exclude_flags_selected(), flags)
    exclude_flags_selected(selected)
    updateCheckboxGroupInput(session, "exclude_flags", choices = flags, selected = selected)
  })

  filtered_stat_data <- reactive({
    req(input$stat_pop)
    df <- stat_data() |> filter(pop == input$stat_pop)

    if (!is.null(input$exclude_flags) && length(input$exclude_flags) > 0) {
      df <- df |> filter(!(as.character(flag) %in% input$exclude_flags))
    }

    df
  })

  filtered_sfl_data <- reactive({
    df <- sfl_data()

    if (!is.null(input$exclude_flags) && length(input$exclude_flags) > 0) {
      df <- df |> filter(!(as.character(flag) %in% input$exclude_flags))
    }

    df
  })

  shared_x_range <- reactive({
    df <- filtered_sfl_data()
    validate(need(nrow(df) > 0, "No SFL rows available for current filters."))
    c(min(df$time, na.rm = TRUE), max(df$time, na.rm = TRUE))
  })

  output$stat_plot <- renderPlotly({
    req(input$stat_metric)
    df <- filtered_stat_data()
    metric_label <- names(which(c(
      "OPP/EVT ratio" = "opp_evt_ratio",
      "Abundance" = "abundance",
      "Diameter" = "diameter"
    ) == input$stat_metric))

    validate(need(nrow(df) > 0, "No rows to plot for current filters."))

    p <- plot_ly(
      data = df,
      x = ~time,
      y = as.formula(paste0("~", input$stat_metric)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 3),
      source = "stat_plot_click"
    ) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        yaxis = list(title = metric_label),
        shapes = clicked_stat_vline()
      )

    # We intentionally accept an occasional startup warning from
    # plotly::event_data("plotly_click", source = "stat_plot_click"):
    # "...event tied a source ID ... is not registered".
    #
    # During initial reactive churn, event_data() can execute before client-side
    # event registration has fully settled for this source. This is benign,
    # click interactivity works after first render, and no incorrect data is
    # produced. This is a timing warning, not a logic error.
    #
    # We will not suppress or fix this warning, as robust suppression or
    # internal-state workarounds add maintenance complexity and couple us to
    # non-public internals. For this project, keeping code simple is preferred.
    plotly::event_register(p, "plotly_click")
  })

  stat_click_event <- reactive({
    plotly::event_data(
      "plotly_click",
      source = "stat_plot_click",
      priority = "event"
    )
  })

  observeEvent(stat_click_event(), {
    click <- stat_click_event()
    if (!is.null(click) && nrow(click) > 0 && !is.null(click$x)) {
      selected_stat_x(click$x[[1]])
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$selected_stat_x <- renderText({
    x <- selected_stat_x()
    if (is.null(x)) {
      "None"
    } else if (inherits(x, "POSIXt")) {
      format(x, "%Y-%m-%d %H:%M:%S %Z")
    } else {
      as.character(x)
    }
  })

  output$sfl_plot <- renderPlotly({
    req(input$sfl_metric)
    df <- filtered_sfl_data()

    validate(need(nrow(df) > 0, "No SFL rows to plot for current filters."))

    plot_ly(
      data = df,
      x = ~time,
      y = as.formula(paste0("~", input$sfl_metric)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 3)
    ) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        yaxis = list(title = input$sfl_metric),
        shapes = clicked_stat_vline()
      )
  })

  output$filter_params_plot <- renderPlotly({
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

      vx <- clicked_stat_x()
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
        xaxis = list(title = "Time", range = shared_x_range()),
        xaxis2 = list(range = shared_x_range()),
        xaxis3 = list(range = shared_x_range())
      )
  })
}
