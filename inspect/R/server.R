server <- function(input, output, session) {
  exclude_flags_selected <- reactiveVal(c("1", "2", "3"))
  clear_stat_time_selection <- reactiveVal(FALSE)

  # Clear selected Stat point when cruise changes
  observeEvent(input$cruise, ignoreInit = TRUE, {
    clear_stat_time_selection(TRUE)
  })

  # Keep server-side flag selection state in sync with user interaction.
  observeEvent(input$exclude_flags, ignoreInit = TRUE, ignoreNULL = FALSE, {
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

  # Reactive expression for the currently selected files based on cruise and variation inputs
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

  # Stat data filtered according to user-selected flags to exclude
  filtered_stat_data <- reactive({
    df <- stat_data()

    if (!is.null(exclude_flags_selected()) && length(exclude_flags_selected()) > 0) {
      df <- df |> filter(!(as.character(flag) %in% exclude_flags_selected()))
    }

    df
  })

  # Create a reactive expression for the vertical line indicating the selected time point
  clicked_stat_vline <- reactive({
    x <- selected_stat_x()
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

  # Read selected outlier database SFL table
  sfl_data <- reactive({
    req(!is.na(selected_files()$outlier_db[[1]]))
    read_sfl_table(selected_files()$outlier_db[[1]])
  })

  # SFL data filtered according to user-selected flags to exclude
  filtered_sfl_data <- reactive({
    df <- sfl_data()

    if (!is.null(exclude_flags_selected()) && length(exclude_flags_selected()) > 0) {
      df <- df |> filter(!(as.character(flag) %in% exclude_flags_selected()))
    }

    df
  })

  # Update available flag values for selected variation
  observe({
    flags_stat <- stat_data() |> pull(flag)
    flags_sfl <- sfl_data() |> pull(flag)
    flags <- sort(unique(c(flags_stat, flags_sfl))) |> as.character()

    selected <- intersect(exclude_flags_selected(), flags)
    updateCheckboxGroupInput(session, "exclude_flags", choices = flags, selected = selected)
  })

  # Calculate the shared x-axis range across all plots based on the filtered
  # SFL data.
  shared_x_range <- reactive({
    df <- filtered_sfl_data()
    validate(need(nrow(df) > 0, "No SFL rows available for current filters."))
    c(min(df$time, na.rm = TRUE), max(df$time, na.rm = TRUE))
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

  # Bead filter params for selected time point.
  active_bead_filter_params <- reactive({
    ts <- selected_stat_x()
    req(!is.null(ts))

    fp <- filter_params_data()
    validate(need(
      all(c(
        "start_date", "end_date", "beads_fsc_small", "beads_D1", "beads_D2",
        "notch_small_D1", "offset_small_D1", "notch_small_D2", "offset_small_D2",
        "notch_large_D1", "offset_large_D1", "notch_large_D2", "offset_large_D2",
        "width"
      ) %in% names(fp)),
      "Filter params are missing required columns for bead guide lines."
    ))

    fp |>
      dplyr::filter(start_date <= ts, is.na(end_date) | ts < end_date) |>
      dplyr::slice_tail(n = 1)
  })

  # Read selected gating parameters and cap final plan end_date at the
  # last available SFL timestamp.
  gating_params_data <- reactive({
    req(!is.na(selected_files()$outlier_db[[1]]))
    sfl_df <- sfl_data()

    read_gating_params(
      selected_files()$outlier_db[[1]],
      max_date = max(sfl_df$time, na.rm = TRUE)
    )
  })

  # Gating params for selected time point.
  active_gating_params <- reactive({
    ts <- selected_stat_x()
    req(!is.null(ts))

    gp <- gating_params_data()

    # Get gating params for selected stat point
    gp$gating <- gp$gating |>
      dplyr::filter(start_date <= ts, is.na(end_date) | ts < end_date)
    if (length(unique(gp$gating$id)) > 1) {
      stop(glue::glue("Multiple active gating parameter sets found for selected Stat point (timestamp {ts})."))
    }
    gp$gating <- gp$gating

    # Filter poly down to those matching id
    gp$poly <- gp$poly |>
      dplyr::filter(gating_id %in% gp$gating$id)

    gp
  })

  bead_evt_data <- reactive({
    req(!is.na(selected_files()$bead_file[[1]]))
    read_bead_sample(selected_files()$bead_file[[1]])
  })

  # Filter bead data to selected hour
  time_filtered_bead_evt_data <- reactive({
    df <- bead_evt_data()
    validate(need("time" %in% names(df), "Bead event data is missing 'time' column."))
    validate(need(inherits(df$time, "POSIXt"), "Bead event time must be POSIXt."))

    selected_timestamp <- selected_stat_x()
    selected_timestamp_hour <- if (!is.null(selected_timestamp)) lubridate::floor_date(selected_timestamp, unit = "hour") else NULL
    if (!is.null(selected_timestamp_hour)) {
      df <- df |> filter(lubridate::floor_date(time, unit = "hour") == selected_timestamp_hour)
    } else {
      df <- df |> filter(FALSE)
    }

    df
  })

  # OPP data for time filtered bead data
  time_filtered_bead_opp_data <- reactive({
    df <- time_filtered_bead_evt_data()

    fp <- active_bead_filter_params()
    validate(need(nrow(fp) > 0, "No active filter parameters found for the selected Stat point."))
    # filter_evt expects filter params columns to have dot separators instead
    # of underscores.
    fp <- fp |>
      dplyr::rename_with(~gsub("_", ".", .x, fixed = TRUE))

    # Filter params have been filtered down to one quantile. Add rows for the
    # other quantiles for compatilibity with the popcycle::filter_evt function,
    # which expects all quantiles to be present in params.
    fp <- dplyr::bind_rows(list(fp, fp, fp))
    fp$quantile <- popcycle:::QUANTILES

    df <- popcycle::filter_evt(df, fp) |>
      dplyr::filter(.data[[glue::glue("q{QUANTILE}")]] == TRUE)
  })

  vct_data <- reactive({
    req(!is.na(selected_files()$vct_dir[[1]]))
    req(!is.null(selected_stat_x()))
    read_vct_parquet(selected_files()$vct_dir[[1]], selected_stat_x())
  })

  output$bead_evt_hex_plot <- renderPlot({
    df <- time_filtered_bead_evt_data()

    validate(need(!is.null(selected_stat_x()), "Click a point in the Stat plot to view bead events."))
    validate(need(nrow(df) > 0, "No bead events found for the selected hour."))
    validate(need(
      all(c("fsc_small", "chl_small", "pe", "D1", "D2") %in% names(df)),
      "Bead event data is missing one or more required columns: fsc_small, chl_small, pe, D1, D2."
    ))

    if (!requireNamespace("hexbin", quietly = TRUE)) {
      validate(need(FALSE, "Package 'hexbin' is required for geom_hex. Install it to display this plot."))
    }

    panel_levels <- c("chl_small", "pe", "D1", "D2")
    panel_labels <- c(
      "chl_small vs fsc_small",
      "pe vs fsc_small",
      "D1 vs fsc_small",
      "D2 vs fsc_small"
    )

    # Apply EVT filters
    if (input$alignment_filter) {
      width <- active_bead_filter_params()$width[[1]]
      df <- df |> filter((df$D2 < df$D1 + width) & (df$D1 < df$D2 + width))
    }

    plot_df <- df |>
      dplyr::select(fsc_small, chl_small, pe, D1, D2) |>
      tidyr::pivot_longer(
        cols = c(chl_small, pe, D1, D2),
        names_to = "y_var",
        values_to = "y_value"
      ) |>
      dplyr::mutate(
        panel = factor(
          y_var,
          levels = panel_levels,
          labels = panel_labels
        )
      ) |>
      dplyr::filter(!is.na(fsc_small), !is.na(y_value))

    validate(need(nrow(plot_df) > 0, "No bead rows available to plot after filtering missing values."))

    fp <- active_bead_filter_params()

    vline_df <- tibble::tibble()
    hline_df <- tibble::tibble()
    abline_segment_df <- tibble::tibble()
    if (nrow(fp) > 0) {
      if (!is.na(fp$beads_fsc_small[[1]])) {
        vline_df <- tibble::tibble(
          panel = factor(panel_labels, levels = panel_labels),
          xintercept = fp$beads_fsc_small[[1]]
        )
      }

      hline_df <- tibble::tibble(
        panel = factor(c("D1 vs fsc_small", "D2 vs fsc_small"), levels = panel_labels),
        yintercept = c(fp$beads_D1[[1]], fp$beads_D2[[1]])
      ) |>
        dplyr::filter(!is.na(yintercept))

      abline_df <- tibble::tibble(
        panel = factor(
          c(
            "D1 vs fsc_small", "D2 vs fsc_small",
            "D1 vs fsc_small", "D2 vs fsc_small"
          ),
          levels = panel_labels
        ),
        slope = c(
          fp$notch_small_D1[[1]],
          fp$notch_small_D2[[1]],
          fp$notch_large_D1[[1]],
          fp$notch_large_D2[[1]]
        ),
        intercept = c(
          fp$offset_small_D1[[1]],
          fp$offset_small_D2[[1]],
          fp$offset_large_D1[[1]],
          fp$offset_large_D2[[1]]
        ),
        line_kind = c("small", "small", "large", "large")
      ) |>
        dplyr::filter(!is.na(slope), is.finite(slope), !is.na(intercept), is.finite(intercept))

      panel_ranges <- plot_df |>
        dplyr::filter(panel %in% c("D1 vs fsc_small", "D2 vs fsc_small")) |>
        dplyr::group_by(panel) |>
        dplyr::summarise(
          x_min = min(fsc_small, na.rm = TRUE),
          x_max = max(fsc_small, na.rm = TRUE),
          .groups = "drop"
        )

      abline_segment_df <- abline_df |>
        dplyr::inner_join(panel_ranges, by = "panel") |>
        dplyr::group_by(panel) |>
        dplyr::group_modify(~{
          panel_lines <- .x
          small <- panel_lines |> dplyr::filter(line_kind == "small")
          large <- panel_lines |> dplyr::filter(line_kind == "large")
          if (nrow(small) != 1 || nrow(large) != 1) {
            return(tibble::tibble())
          }

          x_min <- panel_lines$x_min[[1]]
          x_max <- panel_lines$x_max[[1]]
          m_small <- small$slope[[1]]
          b_small <- small$intercept[[1]]
          m_large <- large$slope[[1]]
          b_large <- large$intercept[[1]]

          x_int <- if (isTRUE(all.equal(m_small, m_large))) {
            NA_real_
          } else {
            (b_large - b_small) / (m_small - m_large)
          }

          split_x <- if (is.na(x_int)) {
            x_min
          } else {
            min(max(x_int, x_min), x_max)
          }

          tibble::tibble(
            line_kind = c("small", "large"),
            slope = c(m_small, m_large),
            intercept = c(b_small, b_large),
            x_start = c(x_min, split_x),
            x_end = c(split_x, x_max)
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::filter(x_end > x_start) |>
        dplyr::mutate(
          y_start = slope * x_start + intercept,
          y_end = slope * x_end + intercept
        )
    }

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = fsc_small, y = y_value)) +
      ggplot2::geom_hex(bins = BEAD_HEXBIN_BINS) +
      ggplot2::scale_fill_viridis_c(trans = "log10", name = "Count") +
      ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
      ggplot2::labs(
        x = "FSC Small",
        y = NULL,
        title = "Bead Subsample EVT",
        subtitle = "Filtered to the hour of the selected Stat point"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    if (nrow(vline_df) > 0) {
      p <- p +
        ggplot2::geom_vline(
          data = vline_df,
          ggplot2::aes(xintercept = xintercept),
          inherit.aes = FALSE,
          color = "#d62728",
          linetype = "dashed",
          linewidth = 0.6
        )
    }

    if (nrow(hline_df) > 0) {
      p <- p +
        ggplot2::geom_hline(
          data = hline_df,
          ggplot2::aes(yintercept = yintercept),
          inherit.aes = FALSE,
          color = "#d62728",
          linetype = "dashed",
          linewidth = 0.6
        )
    }

    if (nrow(abline_segment_df) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = abline_segment_df,
          ggplot2::aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
          inherit.aes = FALSE,
          color = "#cc00cc",
          linewidth = 0.7,
          show.legend = FALSE
        )
    }

    p
  })

  output$bead_opp_hex_plot <- renderPlot({
    df <- time_filtered_bead_opp_data()

    validate(need(!is.null(selected_stat_x()), "Click a point in the Stat plot to view bead events."))
    validate(need(nrow(df) > 0, "No bead events found for the selected hour."))
    validate(need(
      all(c("fsc_small", "chl_small", "pe", "D1", "D2") %in% names(df)),
      "Bead event data is missing one or more required columns: fsc_small, chl_small, pe, D1, D2."
    ))

    if (!requireNamespace("hexbin", quietly = TRUE)) {
      validate(need(FALSE, "Package 'hexbin' is required for geom_hex. Install it to display this plot."))
    }

    panel_levels <- c("chl_small", "pe", "D1", "D2")
    panel_labels <- c(
      "chl_small vs fsc_small",
      "pe vs fsc_small",
      "D1 vs fsc_small",
      "D2 vs fsc_small"
    )

    plot_df <- df |>
      dplyr::select(fsc_small, chl_small, pe, D1, D2) |>
      tidyr::pivot_longer(
        cols = c(chl_small, pe, D1, D2),
        names_to = "y_var",
        values_to = "y_value"
      ) |>
      dplyr::mutate(
        panel = factor(
          y_var,
          levels = panel_levels,
          labels = panel_labels
        )
      ) |>
      dplyr::filter(!is.na(fsc_small), !is.na(y_value))

    validate(need(nrow(plot_df) > 0, "No bead rows available to plot after filtering missing values."))

    fp <- active_bead_filter_params()

    vline_df <- tibble::tibble()
    hline_df <- tibble::tibble()
    abline_segment_df <- tibble::tibble()
    if (nrow(fp) > 0) {
      if (!is.na(fp$beads_fsc_small[[1]])) {
        vline_df <- tibble::tibble(
          panel = factor(panel_labels, levels = panel_labels),
          xintercept = fp$beads_fsc_small[[1]]
        )
      }

      hline_df <- tibble::tibble(
        panel = factor(c("D1 vs fsc_small", "D2 vs fsc_small"), levels = panel_labels),
        yintercept = c(fp$beads_D1[[1]], fp$beads_D2[[1]])
      ) |>
        dplyr::filter(!is.na(yintercept))

      abline_df <- tibble::tibble(
        panel = factor(
          c(
            "D1 vs fsc_small", "D2 vs fsc_small",
            "D1 vs fsc_small", "D2 vs fsc_small"
          ),
          levels = panel_labels
        ),
        slope = c(
          fp$notch_small_D1[[1]],
          fp$notch_small_D2[[1]],
          fp$notch_large_D1[[1]],
          fp$notch_large_D2[[1]]
        ),
        intercept = c(
          fp$offset_small_D1[[1]],
          fp$offset_small_D2[[1]],
          fp$offset_large_D1[[1]],
          fp$offset_large_D2[[1]]
        ),
        line_kind = c("small", "small", "large", "large")
      ) |>
        dplyr::filter(!is.na(slope), is.finite(slope), !is.na(intercept), is.finite(intercept))

      panel_ranges <- plot_df |>
        dplyr::filter(panel %in% c("D1 vs fsc_small", "D2 vs fsc_small")) |>
        dplyr::group_by(panel) |>
        dplyr::summarise(
          x_min = min(fsc_small, na.rm = TRUE),
          x_max = max(fsc_small, na.rm = TRUE),
          .groups = "drop"
        )

      abline_segment_df <- abline_df |>
        dplyr::inner_join(panel_ranges, by = "panel") |>
        dplyr::group_by(panel) |>
        dplyr::group_modify(~{
          panel_lines <- .x
          small <- panel_lines |> dplyr::filter(line_kind == "small")
          large <- panel_lines |> dplyr::filter(line_kind == "large")
          if (nrow(small) != 1 || nrow(large) != 1) {
            return(tibble::tibble())
          }

          x_min <- panel_lines$x_min[[1]]
          x_max <- panel_lines$x_max[[1]]
          m_small <- small$slope[[1]]
          b_small <- small$intercept[[1]]
          m_large <- large$slope[[1]]
          b_large <- large$intercept[[1]]

          x_int <- if (isTRUE(all.equal(m_small, m_large))) {
            NA_real_
          } else {
            (b_large - b_small) / (m_small - m_large)
          }

          split_x <- if (is.na(x_int)) {
            x_min
          } else {
            min(max(x_int, x_min), x_max)
          }

          tibble::tibble(
            line_kind = c("small", "large"),
            slope = c(m_small, m_large),
            intercept = c(b_small, b_large),
            x_start = c(x_min, split_x),
            x_end = c(split_x, x_max)
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::filter(x_end > x_start) |>
        dplyr::mutate(
          y_start = slope * x_start + intercept,
          y_end = slope * x_end + intercept
        )
    }

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = fsc_small, y = y_value)) +
      ggplot2::geom_hex(bins = BEAD_HEXBIN_BINS) +
      ggplot2::scale_fill_viridis_c(trans = "log10", name = "Count") +
      ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
      ggplot2::labs(
        x = "FSC Small",
        y = NULL,
        title = "Bead Subsample OPP",
        subtitle = "Filtered to the hour of the selected Stat point"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    if (nrow(vline_df) > 0) {
      p <- p +
        ggplot2::geom_vline(
          data = vline_df,
          ggplot2::aes(xintercept = xintercept),
          inherit.aes = FALSE,
          color = "#d62728",
          linetype = "dashed",
          linewidth = 0.6
        )
    }

    if (nrow(hline_df) > 0) {
      p <- p +
        ggplot2::geom_hline(
          data = hline_df,
          ggplot2::aes(yintercept = yintercept),
          inherit.aes = FALSE,
          color = "#d62728",
          linetype = "dashed",
          linewidth = 0.6
        )
    }

    if (nrow(abline_segment_df) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = abline_segment_df,
          ggplot2::aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
          inherit.aes = FALSE,
          color = "#cc00cc",
          linewidth = 0.7,
          show.legend = FALSE
        )
    }

    p
  })

  sflPlotServer(
    "sfl_plot",
    filtered_sfl_data,
    clicked_stat_vline,
    shared_x_range
  )

  stat_plot_stat <- statPlotServer(
    "stat_plot",
    stat_data,
    filtered_stat_data,
    clicked_stat_vline,
    shared_x_range,
    clear_time_selection = clear_stat_time_selection
  )
  selected_stat_x <- stat_plot_stat$selected_stat_x

  output$selected_stat_x <- renderText({
    x <- selected_stat_x()
    if (is.null(x)) {
      "None"
    } else {
      format(x, "%Y-%m-%d %H:%M:%S %Z")
    }
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

      vx <- selected_stat_x()
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

  # output$vct_plot <- renderPlot({
  #   df <- vct_data()
  #   validate(need(nrow(df) > 0, "No VCT data for selected hour."))
  #   plot_vct_cytogram(df, x = "fsc_small", y = "pe")
  #   # popcycle::plot_vct_cytogram(
  #   #   df,
  #   #   para.x = "fsc_small",
  #   #   para.y = "pe"
  #   # )
  # })

  output$vct_plot_pe_fsc_small <- renderPlot({
    df <- vct_data()
    validate(need(nrow(df) > 0, "No VCT data for selected hour."))

    gp <- active_gating_params()
    validate(need(nrow(gp$gating) > 0, "No active gating parameters found for selected Stat point."))
    validate(need(nrow(gp$poly) > 0, "No active gating polygon parameters found for selected Stat point."))

    plot_vct_cytogram(df, x = "fsc_small", y = "pe", gating_params = gp)
  })

  output$vct_plot_chl_small_fsc_small <- renderPlot({
    df <- vct_data()
    validate(need(nrow(df) > 0, "No VCT data for selected hour."))

    gp <- active_gating_params()
    validate(need(nrow(gp$gating) > 0, "No active gating parameters found for selected Stat point."))
    validate(need(nrow(gp$poly) > 0, "No active gating polygon parameters found for selected Stat point."))

    plot_vct_cytogram(df, x = "fsc_small", y = "chl_small", gating_params = gp)
  })

  output$vct_plot_pe_chl_small <- renderPlot({
    df <- vct_data()
    validate(need(nrow(df) > 0, "No VCT data for selected hour."))

    gp <- active_gating_params()
    validate(need(nrow(gp$gating) > 0, "No active gating parameters found for selected Stat point."))
    validate(need(nrow(gp$poly) > 0, "No active gating polygon parameters found for selected Stat point."))

    plot_vct_cytogram(df, x = "chl_small", y = "pe", gating_params = gp)
  })
}
