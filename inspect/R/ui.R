ui <- bslib::page_sidebar(
  title = "SeaFlow Inspect",
  sidebar = bslib::sidebar(
    shiny::selectInput("cruise", "Cruise", choices = sort(unique(all_data_source_files$cruise))),
    shiny::selectInput("variation", "Variation", choices = NULL),
    shiny::checkboxGroupInput("exclude_flags", "Exclude flags", choices = NULL),
    shiny::hr(),
    bslib::card(
      bslib::card_header("Selection State"),
      shiny::strong("Stat x:"),
      shiny::textOutput("selected_stat_x"),
    )
  ),
  bslib::navset_tab(
    bslib::nav_panel(
      "Summary",
      sflPlotUI("sfl_plot"),
      statPlotUI("stat_plot"),
      bslib::card(
        bslib::card_header("Filter Params"),
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::div(),
          plotly::plotlyOutput("filter_params_plot", height = paste0(FILTER_PARAMS_PLOT_VH, "vh"))
        )
      )
    ),
    bslib::nav_panel(
      "Filter",
      bslib::card(
        bslib::card_header("Bead Subsample (Selected Stat Hour)"),
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::div(
            shiny::p("Select filters to apply to bead EVT plot."),
            shiny::checkboxInput("alignment_filter", "EVT Alignment Filter", value = TRUE),
          ),
          shiny::plotOutput("bead_evt_hex_plot", height = paste0(FILTER_PLOT_VH, "vh")),
          shiny::div(),  # placeholder
          shiny::plotOutput("bead_opp_hex_plot", height = paste0(FILTER_PLOT_VH, "vh"))
        )
      )
    ),
    bslib::nav_panel(
      "Gating",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("pe vs fsc_small"),
          shiny::plotOutput("vct_plot_pe_fsc_small", height = "40vh")
        ),
        bslib::card(
          bslib::card_header("chl_small vs fsc_small"),
          shiny::plotOutput("vct_plot_chl_small_fsc_small", height = "40vh")
        ),
        bslib::card(
          bslib::card_header("pe vs chl_small"),
          shiny::plotOutput("vct_plot_pe_chl_small", height = "40vh")
        )
      )
    )
  )
)
