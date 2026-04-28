ui <- bslib::page_sidebar(
  title = "SeaFlow Inspect",
  sidebar = bslib::sidebar(
    shiny::selectInput("cruise", "Cruise", choices = sort(unique(all_data_source_files$cruise))),
    shiny::selectInput("variation", "Variation", choices = NULL),
    shiny::checkboxGroupInput("exclude_flags", "Exclude flags", choices = NULL)
  ),
  bslib::navset_tab(
    bslib::nav_panel(
      "Summary",
      bslib::card(
        bslib::card_header("SFL"),
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::selectInput("sfl_metric", "Metric", choices = c(
            "PAR" = "par",
            "Ocean temperature" = "ocean_tmp",
            "Salinity" = "salinity",
            "Conductivity" = "conductivity",
            "Latitude" = "lat",
            "Longitude" = "lon",
            "Stream pressure" = "stream_pressure",
            "Event rate" = "event_rate"
          )),
          plotly::plotlyOutput("sfl_plot", height = paste0(SFL_PLOT_VH, "vh"))
        )
      ),
      bslib::card(
        bslib::card_header("Stat"),
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::div(
            shiny::selectInput("stat_pop", "Population", choices = NULL),
            shiny::selectInput("stat_metric", "Metric", choices = c(
              "Abundance" = "abundance",
              "Diameter" = "diameter",
              "OPP/EVT ratio" = "opp_evt_ratio"
            ))
          ),
          plotly::plotlyOutput("stat_plot", height = paste0(STAT_PLOT_VH, "vh"))
        )
      ),
      bslib::card(
        bslib::card_header("Filter Params"),
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::uiOutput("filter_params_legend"),
          plotly::plotlyOutput("filter_params_plot", height = paste0(FILTER_PARAMS_PLOT_VH, "vh"))
        )
      )
    ),
    bslib::nav_panel(
      "Context",
      bslib::card(
        bslib::card_header("Contextual Plots"),
        shiny::p("Context plots will appear here.")
      )
    )
  )
)
