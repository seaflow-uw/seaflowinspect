server <- function(input, output, session) {
  exclude_flags_selected <- reactiveVal(c("1", "2", "3"))

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

    plot_ly(
      data = df,
      x = ~time,
      y = as.formula(paste0("~", input$stat_metric)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 3)
    ) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        yaxis = list(title = metric_label)
      )
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
        yaxis = list(title = input$sfl_metric)
      )
  })

  output$filter_params_legend <- renderUI({
    legend_items <- list(
      list(label = "beads_fsc_small", color = "#1f77b4"),
      list(label = "beads_D1", color = "#ff7f0e"),
      list(label = "beads_D2", color = "#2ca02c")
    )

    tags$div(
      style = "padding-top: 0.5rem;",
      tags$h5("Legend"),
      lapply(legend_items, function(item) {
        tags$div(
          style = "display:flex; align-items:center; margin-bottom:0.5rem;",
          tags$span(
            style = paste0(
              "display:inline-block; width:18px; height:4px; margin-right:8px; background:",
              item$color,
              ";"
            )
          ),
          tags$span(item$label)
        )
      })
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

      plot_ly(plot_df) |>
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
        ) |>
        layout(yaxis = list(title = y_title))
    }

    p1 <- make_segment_plot("beads_fsc_small", "beads_fsc_small", "#1f77b4")
    p2 <- make_segment_plot("beads_D1", "beads_D1", "#ff7f0e")
    p3 <- make_segment_plot("beads_D2", "beads_D2", "#2ca02c")

    subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) |>
      layout(
        xaxis = list(title = "Time", range = shared_x_range()),
        xaxis2 = list(range = shared_x_range()),
        xaxis3 = list(range = shared_x_range())
      )
  })
}
