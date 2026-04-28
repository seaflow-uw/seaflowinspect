ui <- page_sidebar(
  title = "SeaFlow Inspect",
  sidebar = sidebar(
    selectInput("cruise", "Cruise", choices = sort(unique(all_data_source_files$cruise))),
    selectInput("variation", "Variation", choices = NULL),
    checkboxGroupInput("exclude_flags", "Exclude flags", choices = NULL)
  ),
  card(
    card_header("SFL"),
    layout_columns(
      col_widths = c(3, 9),
      selectInput("sfl_metric", "Metric", choices = c(
        "PAR" = "par",
        "Ocean temperature" = "ocean_tmp",
        "Salinity" = "salinity",
        "Conductivity" = "conductivity",
        "Latitude" = "lat",
        "Longitude" = "lon",
        "Stream pressure" = "stream_pressure",
        "Event rate" = "event_rate"
      )),
      plotlyOutput("sfl_plot", height = paste0(PLOT_VH, "vh"))
    )
  ),
  card(
    card_header("Stat"),
    layout_columns(
      col_widths = c(3, 9),
      div(
        selectInput("stat_pop", "Population", choices = NULL),
        selectInput("stat_metric", "Metric", choices = c(
          "Abundance" = "abundance",
          "Diameter" = "diameter",
          "OPP/EVT ratio" = "opp_evt_ratio"
        ))
      ),
      plotlyOutput("stat_plot", height = paste0(PLOT_VH, "vh"))
    )
  ),
  card(
    card_header("Filter Params"),
    layout_columns(
      col_widths = c(3, 9),
      uiOutput("filter_params_legend"),
      plotlyOutput("filter_params_plot", height = "60vh")
    )
  )
)
