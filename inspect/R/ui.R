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
      shiny::textOutput("selected_x")
    ),
    shiny::hr(),
    bslib::card(
      bslib::card_header("SFL-only rows"),
      shiny::textOutput("sfl_only_n")
    )
  ),
  bslib::navset_tab(
    id = "main_tab",
    bslib::nav_panel(
      "Summary",
      sflPlotUI("sfl_plot"),
      statPlotUI("stat_plot"),
      filterParamsPlotUI("filter_params_plot")
    ),
    bslib::nav_panel(
      "Filter",
      beadFilterPlotUI("bead_filter_plot")
    ),
    bslib::nav_panel(
      "Gating",
      bslib::card(
        bslib::card_header("Displayed VCT data"),
        shiny::radioButtons(
          "gating_vct_scope",
          "Display scope",
          choices = c(
            "Exact time point" = "point",
            "Hour" = "hour"
          ),
          selected = "point",
          inline = TRUE
        ),
        shiny::textOutput("gating_vct_scope_text")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        vctGatingPlotUI("vct_plot_pe_fsc_small", x = "fsc_small", y = "pe"),
        vctGatingPlotUI("vct_plot_chl_small_fsc_small", x = "fsc_small", y = "chl_small"),
        vctGatingPlotUI("vct_plot_pe_chl_small", x = "chl_small", y = "pe")
      )
    ),
    bslib::nav_panel(
      "Ridgeline",
      # Keep this internal value in sync with the active_tab_value passed to
      # ridgelinePlotServer() so the module can defer gridded-data reads until
      # the tab is actually active.
      value = "ridgeline",
      ridgelinePlotUI("ridgeline_plot")
    )
  )
)
