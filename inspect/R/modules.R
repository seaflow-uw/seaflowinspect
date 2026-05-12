# SFL plot module
# -----------------------------------------------------------------------------
sflPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("SFL"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::selectInput(ns("metric"), "Metric", choices = c(
        "PAR" = "par",
        "Ocean temperature" = "ocean_tmp",
        "Salinity" = "salinity",
        "Conductivity" = "conductivity",
        "Latitude" = "lat",
        "Longitude" = "lon",
        "Stream pressure" = "stream_pressure",
        "Event rate" = "event_rate"
      )),
      plotly::plotlyOutput(ns("plot"), height = paste0(SFL_PLOT_VH, "vh"))
    )
  )
}

sflPlotServer <- function(id, sfl_data, stat_vline, x_range) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        req(input$metric)

        validate(need(nrow(sfl_data()) > 0, "No SFL rows to plot for current filters."))

        plot_ly(
          data = sfl_data(),
          x = ~time,
          y = as.formula(paste0("~", input$metric)),
          type = "scatter",
          mode = "markers",
          marker = list(size = 3)
        ) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            yaxis = list(title = input$metric),
            shapes = stat_vline()
          )
      })
    }
  )
}

# Stat plot module
# -----------------------------------------------------------------------------
statPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Stat"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::div(
        shiny::selectInput(ns("pop"), "Population", choices = NULL),
        shiny::selectInput(ns("metric"), "Metric", choices = c(
          "Abundance" = "abundance",
          "Diameter" = "diameter",
          "OPP/EVT ratio" = "opp_evt_ratio"
        ))
      ),
      plotly::plotlyOutput(ns("plot"), height = paste0(STAT_PLOT_VH, "vh"))
    )
  )
}

statPlotServer <- function(
  id,
  stat_data,
  filtered_stat_data,
  stat_vline,
  x_range,
  clear_time_selection
) {
  normalize_click_time <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (inherits(x, "POSIXt")) {
      return(lubridate::with_tz(as.POSIXct(x), tzone = "UTC"))
    }

    if (is.numeric(x)) {
      return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }

    parsed <- suppressWarnings(lubridate::ymd_hms(as.character(x), quiet = TRUE))
    if (all(is.na(parsed))) {
      parsed <- suppressWarnings(lubridate::ymd_hm(as.character(x), quiet = TRUE))
    }
    if (all(is.na(parsed))) {
      return(NULL)
    }

    lubridate::with_tz(parsed[[1]], tzone = "UTC")
  }

  moduleServer(
    id,
    function(input, output, session) {
      # Manage the user-selected x value
      selected_stat_x <- reactiveVal(NULL)

      stat_click_event <- reactive({
        plotly::event_data(
          "plotly_click",
          source = session$ns("plot_click"),
          priority = "event"
        )
      })

      observeEvent(stat_click_event(), {
        click <- stat_click_event()
        if (!is.null(click) && nrow(click) > 0 && !is.null(click$x)) {
          parsed_click_x <- normalize_click_time(click$x[[1]])
          if (!is.null(parsed_click_x)) {
            selected_stat_x(parsed_click_x)
          }
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Clear time selection if requested
      observeEvent(clear_time_selection(), ignoreInit = TRUE, {
        req(isTRUE(clear_time_selection()))
        selected_stat_x(NULL)
        clear_time_selection(FALSE)
      })

      # Update population choices when data changes, preserving selection if possible
      observe({
        pops <- sort(unique(stat_data()$pop))
        selected <- if (input$pop %in% pops) input$pop else pops[[1]]
        updateSelectInput(session, "pop", choices = pops, selected = selected)
      })

      # Filter for selected population
      pop_stat_data <- reactive({
        req(input$pop)
        filtered_stat_data() |> filter(pop == input$pop)
      })

      output$plot <- renderPlotly({
        req(input$metric)
        df <- pop_stat_data()
        metric_label <- names(which(c(
          "OPP/EVT ratio" = "opp_evt_ratio",
          "Abundance" = "abundance",
          "Diameter" = "diameter"
        ) == input$metric))

        validate(need(nrow(df) > 0, "No rows to plot for current filters."))

        p <- plot_ly(
          data = df,
          x = ~time,
          y = as.formula(paste0("~", input$metric)),
          type = "scatter",
          mode = "markers",
          marker = list(size = 3),
          source = session$ns("plot_click")
        ) |>
          layout(
            xaxis = list(title = "Time", range = x_range()),
            yaxis = list(title = metric_label),
            shapes = stat_vline()
          )

        # We intentionally accept an occasional startup warning from
        # plotly::event_data("plotly_click", source = session$ns("plot_click")):
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

      list(
        selected_stat_x = reactive(selected_stat_x())
      )
    }
  )
}