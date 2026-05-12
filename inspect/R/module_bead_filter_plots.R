# Bead filter plots module
# -----------------------------------------------------------------------------

BEAD_FILTER_PANEL_LEVELS <- c("chl_small", "pe", "D1", "D2")
BEAD_FILTER_PANEL_LABELS <- c(
  "chl_small vs fsc_small",
  "pe vs fsc_small",
  "D1 vs fsc_small",
  "D2 vs fsc_small"
)

prepare_bead_plot_df <- function(data, filter_params, apply_alignment_filter = FALSE) {
  if (apply_alignment_filter) {
    width <- filter_params$width[[1]]
    data <- data |> dplyr::filter((D2 < D1 + width) & (D1 < D2 + width))
  }

  data |>
    dplyr::select(fsc_small, chl_small, pe, D1, D2) |>
    tidyr::pivot_longer(
      cols = c(chl_small, pe, D1, D2),
      names_to = "y_var",
      values_to = "y_value"
    ) |>
    dplyr::mutate(
      panel = factor(
        y_var,
        levels = BEAD_FILTER_PANEL_LEVELS,
        labels = BEAD_FILTER_PANEL_LABELS
      )
    ) |>
    dplyr::filter(!is.na(fsc_small), !is.na(y_value))
}

build_bead_guide_data <- function(plot_df, filter_params) {
  vline_df <- tibble::tibble()
  hline_df <- tibble::tibble()
  abline_segment_df <- tibble::tibble()

  if (nrow(filter_params) == 0) {
    return(list(
      vline_df = vline_df,
      hline_df = hline_df,
      abline_segment_df = abline_segment_df
    ))
  }

  if (!is.na(filter_params$beads_fsc_small[[1]])) {
    vline_df <- tibble::tibble(
      panel = factor(BEAD_FILTER_PANEL_LABELS, levels = BEAD_FILTER_PANEL_LABELS),
      xintercept = filter_params$beads_fsc_small[[1]]
    )
  }

  hline_df <- tibble::tibble(
    panel = factor(c("D1 vs fsc_small", "D2 vs fsc_small"), levels = BEAD_FILTER_PANEL_LABELS),
    yintercept = c(filter_params$beads_D1[[1]], filter_params$beads_D2[[1]])
  ) |>
    dplyr::filter(!is.na(yintercept))

  abline_df <- tibble::tibble(
    panel = factor(
      c(
        "D1 vs fsc_small", "D2 vs fsc_small",
        "D1 vs fsc_small", "D2 vs fsc_small"
      ),
      levels = BEAD_FILTER_PANEL_LABELS
    ),
    slope = c(
      filter_params$notch_small_D1[[1]],
      filter_params$notch_small_D2[[1]],
      filter_params$notch_large_D1[[1]],
      filter_params$notch_large_D2[[1]]
    ),
    intercept = c(
      filter_params$offset_small_D1[[1]],
      filter_params$offset_small_D2[[1]],
      filter_params$offset_large_D1[[1]],
      filter_params$offset_large_D2[[1]]
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

  list(
    vline_df = vline_df,
    hline_df = hline_df,
    abline_segment_df = abline_segment_df
  )
}

build_bead_filter_plot <- function(data, filter_params, title, empty_message, apply_alignment_filter = FALSE) {
  validate(need(nrow(data) > 0, empty_message))
  validate(need(
    all(c("fsc_small", "chl_small", "pe", "D1", "D2") %in% names(data)),
    "Bead event data is missing one or more required columns: fsc_small, chl_small, pe, D1, D2."
  ))

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    validate(need(FALSE, "Package 'hexbin' is required for geom_hex. Install it to display this plot."))
  }

  plot_df <- prepare_bead_plot_df(
    data,
    filter_params = filter_params,
    apply_alignment_filter = apply_alignment_filter
  )

  validate(need(nrow(plot_df) > 0, "No bead rows available to plot after filtering missing values."))

  guide_data <- build_bead_guide_data(plot_df, filter_params)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = fsc_small, y = y_value)) +
    ggplot2::geom_hex(bins = BEAD_HEXBIN_BINS) +
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "Count") +
    ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
    ggplot2::labs(
      x = "FSC",
      y = NULL,
      title = title,
      subtitle = "Filtered to selected hour"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (nrow(guide_data$vline_df) > 0) {
    p <- p +
      ggplot2::geom_vline(
        data = guide_data$vline_df,
        ggplot2::aes(xintercept = xintercept),
        inherit.aes = FALSE,
        color = "#d62728",
        linetype = "dashed",
        linewidth = 0.6
      )
  }

  if (nrow(guide_data$hline_df) > 0) {
    p <- p +
      ggplot2::geom_hline(
        data = guide_data$hline_df,
        ggplot2::aes(yintercept = yintercept),
        inherit.aes = FALSE,
        color = "#d62728",
        linetype = "dashed",
        linewidth = 0.6
      )
  }

  if (nrow(guide_data$abline_segment_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = guide_data$abline_segment_df,
        ggplot2::aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
        inherit.aes = FALSE,
        color = "#cc00cc",
        linewidth = 0.7,
        show.legend = FALSE
      )
  }

  p
}

beadFilterPlotUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Bead Subsample (Selected Stat Hour)"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      shiny::div(
        shiny::p("Select filters to apply to bead EVT plot."),
        shiny::checkboxInput(ns("alignment_filter"), "EVT Alignment Filter", value = TRUE),
      ),
      shiny::plotOutput(ns("evt_plot"), height = paste0(FILTER_PLOT_VH, "vh")),
      shiny::div(),  # placeholder
      shiny::plotOutput(ns("opp_plot"), height = paste0(FILTER_PLOT_VH, "vh"))
    )
  )
}

beadFilterPlotServer <- function(id, evt_df, opp_df, filter_params) {
  moduleServer(
    id,
    function(input, output, session) {
      output$evt_plot <- renderPlot({
        build_bead_filter_plot(
          data = evt_df(),
          filter_params = filter_params(),
          title = "Bead Subsample EVT",
          empty_message = "No bead EVT data for selected time.",
          apply_alignment_filter = isTRUE(input$alignment_filter)
        )
      })

      output$opp_plot <- renderPlot({
        build_bead_filter_plot(
          data = opp_df(),
          filter_params = filter_params(),
          title = "Bead Subsample OPP",
          empty_message = "No bead OPP data for selected time."
        )

      })
    }
  )
}