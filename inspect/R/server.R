server <- function(input, output, session) {
  exclude_flags_selected <- reactiveVal(c("1", "2", "3"))
  clear_stat_time_selection <- reactiveVal(FALSE)
  selected_x_val <- reactiveVal(NULL)
  selected_x <- reactive(selected_x_val())
  sfl_only_n <- reactiveVal(NULL)  # only in SFl, i.e. dropped from OPP or VCT

  # Clear selected Stat point when cruise changes
  observeEvent(input$cruise, ignoreInit = TRUE, {
    print("Cruise changed, clearing selected time")
    clear_stat_time_selection(TRUE)
  })

  # Keep server-side flag selection state in sync with user interaction.
  observeEvent(input$exclude_flags, ignoreInit = TRUE, ignoreNULL = FALSE, {
    selected <- if (is.null(input$exclude_flags)) character() else as.character(input$exclude_flags)
    if (!identical(exclude_flags_selected(), selected)) {
      exclude_flags_selected(selected)
    }
  })

  # Update variation choices whenever cruise changes
  observe({
    files <- all_data_source_files |>
      filter(cruise == input$cruise) |>
      filter(!is.na(stat_file)) |>
      distinct(name, default)
    choices <- setNames(files$name, files$name)
    default_choice <- files |>
      filter(default) |>
      slice(1) |>
      pull(name)

    current_choice <- input$variation
    selected_choice <- NULL
    if (length(current_choice) == 1 && current_choice %in% files$name) {
      selected_choice <- current_choice
    } else if (length(default_choice) > 0) {
      selected_choice <- default_choice[[1]]
    } else if (length(files$name) > 0) {
      selected_choice <- files$name[[1]]
    }

    updateSelectInput(session, "variation", choices = choices, selected = selected_choice)
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

  # Update available flag values when the underlying data changes. This avoids
  # writing the checkbox state back to the input on every user toggle.
  observeEvent(list(stat_data(), sfl_data()), ignoreInit = FALSE, {
    flags_stat <- stat_data() |> pull(flag)
    flags_sfl <- sfl_data() |> pull(flag)
    flags <- sort(unique(c(flags_stat, flags_sfl))) |> as.character()

    selected <- intersect(exclude_flags_selected(), flags)
    exclude_flags_selected(selected)
    updateCheckboxGroupInput(session, "exclude_flags", choices = flags, selected = selected)
  })

  # Calculate number of rows in SFL that are not in Stat (i.e. dropped from OPP or VCT)
  observe({
    sfl_df <- sfl_data()
    stat_df <- stat_data()
    sfl_time <- setdiff(sfl_df$time, stat_df$time)
    sfl_only_n(length(sfl_time))
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
    ts <- selected_x()
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
    ts <- selected_x()
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

    selected_timestamp <- selected_x()
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

  empty_vct_data <- function() {
    tibble::tibble(
      time = as.POSIXct(character()),
      pop = character(),
      D1 = numeric(),
      D2 = numeric(),
      fsc_small = numeric(),
      chl_small = numeric(),
      pe = numeric(),
      diameter = numeric(),
      qc = numeric(),
      filter_id = numeric(),
      gating_id = numeric()
    )
  }

  vct_data <- reactive({
    req(!is.na(selected_files()$vct_dir[[1]]))
    req(!is.null(selected_x()))
    vct_scope <- if (is.null(input$gating_vct_scope)) "point" else input$gating_vct_scope

    data <- tryCatch(
      read_vct_parquet(selected_files()$vct_dir[[1]], selected_x(), scope = vct_scope),
      vct_no_files = function(e) empty_vct_data(),
      vct_no_match = function(e) empty_vct_data()
    )
    data
  })

  output$gating_vct_scope_text <- renderText({
    ts <- selected_x()
    scope <- if (is.null(input$gating_vct_scope)) "point" else input$gating_vct_scope

    if (is.null(ts)) {
      return("No Stat x selected.")
    }

    if (identical(scope, "hour")) {
      hour_ts <- lubridate::floor_date(ts, unit = "hour")
      return(glue::glue(
        "Displaying selected hour: {format(hour_ts, '%Y-%m-%d %H:%M:%S %Z')}"
      ))
    }

    glue::glue(
      "Displaying selected 3-min time point: {format(ts, '%Y-%m-%d %H:%M:%S %Z')}"
    )
  })

  gridded_data <- reactive({
    req(!is.na(selected_files()$grid_gridded_file[[1]]))
    grid <- grid_data()
    validate(need(nrow(grid) > 0, "Gridded data file contains no rows."))
    gridded <- read_grid_parquet(selected_files()$grid_gridded_file[[1]])
    # Replace grid bin numbers with bin starts
    gridded <- gridded |>
      dplyr::mutate(
        fsc_small = grid$fsc_small[gridded$fsc_small_coord],
        chl_small = grid$chl_small[gridded$chl_small_coord],
        pe = grid$pe[gridded$pe_coord],
        Qc = grid$Qc[gridded$Qc_coord]
      ) |>
      dplyr::select(-ends_with("_coord"))

    # Note for conversion from carbon per cell Qc to equivalent spherical diameter
    # Menden-Deuer, S. & Lessard, E. J. Carbon to volume relationships for dinoflagellates, diatoms, and other protist plankton. Limnol. Oceanogr. 45, 569–579 (2000).
    d <- 0.261
    e <- 0.860 # < 3000 µm3 
    gridded <- gridded |>
      mutate(diam = round(2 * (3 / (4 * base::pi) * (Qc / d)^(1 / e))^(1 / 3), 5))

    gridded
  })

  grid_data <- reactive({
    req(!is.na(selected_files()$grid_grid_file[[1]]))
    read_grid_bins_parquet(selected_files()$grid_grid_file[[1]])
  })

  sflPlotServer(
    "sfl_plot",
    filtered_sfl_data,
    shared_x_range,
    selected_x
  )

  statPlotServer(
    "stat_plot",
    stat_data,
    filtered_stat_data,
    shared_x_range,
    clear_time_selection = clear_stat_time_selection,
    selected_x_val = selected_x_val
  )

  # Show selected time point as text timestamp
  output$selected_x <- renderText({
    x <- selected_x()
    if (is.null(x)) {
      "None"
    } else {
      format(x, "%Y-%m-%d %H:%M:%S %Z")
    }
  })

  # Show exclusive filtered SFL rows
  output$sfl_only_n <- renderText({
    n <- sfl_only_n()
    if (is.null(n)) {
      "None"
    } else {
      format(n, big.mark = ",")
    }
  })

  filterParamsServer(
    "filter_params_plot",
    filter_params_data,
    shared_x_range,
    selected_x
  )

  beadFilterPlotServer(
    "bead_filter_plot",
    time_filtered_bead_evt_data,
    time_filtered_bead_opp_data,
    active_bead_filter_params
  )

  vctGatingPlotServer(
    "vct_plot_pe_fsc_small",
    vct_data,
    active_gating_params,
    show_gating_order = reactive(isTRUE(input$gating_show_gating_order)),
    x = "fsc_small",
    y = "pe"
  )
  vctGatingPlotServer(
    "vct_plot_chl_small_fsc_small",
    vct_data,
    active_gating_params,
    show_gating_order = reactive(isTRUE(input$gating_show_gating_order)),
    x = "fsc_small",
    y = "chl_small"
  )
  vctGatingPlotServer(
    "vct_plot_pe_chl_small",
    vct_data,
    active_gating_params,
    show_gating_order = reactive(isTRUE(input$gating_show_gating_order)),
    x = "chl_small",
    y = "pe"
  )
  vctGatingTableServer(
    "vct_gating_table",
    active_gating_params
  )

  ridgelinePlotServer(
    "ridgeline_plot",
    gridded_data,
    grid_data,
    selected_x,
    active_tab = reactive(input$main_tab),
    active_tab_value = "ridgeline"
  )
}
