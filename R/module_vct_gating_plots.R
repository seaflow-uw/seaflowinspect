# VCT gating plots module
# -----------------------------------------------------------------------------

vctGatingPlotUI <- function(id, x, y) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(glue::glue("{y} vs {x}")),
    shiny::plotOutput(ns(glue::glue("plot")), height = "40vh")
  )
}

vctGatingPlotServer <- function(id, vct_data, gating_params, show_gating_order, x, y) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        df <- vct_data()
        validate(need(nrow(df) > 0, "No VCT data for the selected time point."))

        gp <- gating_params()
        validate(need(nrow(gp$gating) > 0, "No active gating parameters found for selected time."))
        validate(need(nrow(gp$poly) > 0, "No active gating polygon parameters found for selected time."))

        plot_vct_cytogram(
          df,
          x = x,
          y = y,
          xlim = c(1, 10^3.5),
          ylim = c(1, 10^3.5),
          gating_params = gp,
          show_gating_order = isTRUE(show_gating_order())
        )
      })
    }
  )
}

#' Plot cytogram with particles colored by population.
#'
#' @param df VCT data frame.
#' @param x Channel to use as x axis. Can be either a parameter name like "fsc_small" or a tidy-select expression.
#' @param y Channel to use as y axis. Can be either a parameter name like "chl_small" or a tidy-select expression.
#' @param transform Log transformation for both x- and y-axis"
#' @param xlim limits for x-axis.
#' @param ylim limits for y-axis.
#' @return None
#' @usage plot_vct_cytogram(df, x = "fsc_small", y = "chl_small")
#' @export plot_vct_cytogram
plot_vct_cytogram <- function(df, x = "fsc_small", y = "chl_small",
                              transform = TRUE, xlim = NULL, ylim = NULL,
                              gating_params = NULL,
                              show_gating_order = TRUE) {
  pop_colors <- c(
    unknown="grey",
    beads="red3",
    prochloro=viridis::viridis(4)[1],
    synecho=viridis::viridis(4)[2],
    picoeuk=viridis::viridis(4)[3],
    croco=viridis::viridis(4)[4]
  )
  if (!any(names(df) == "pop")) {
    df[, "pop"] <- "unknown"
  }
  df$pop <- factor(df$pop, levels = names(pop_colors))

  p <- df |>
    ggplot2::ggplot() +
    ggplot2::stat_bin_2d(
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        fill = pop,
        alpha = ggplot2::after_stat(count)
      ),
      colour = NA,
      bins = VCT_CYTOGRAM_BINS,
      show.legend = TRUE
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = pop_colors) +
    ggplot2::scale_alpha_continuous(range = c(0.3, 1)) +
    ggplot2::guides(
      alpha = "none",
      fill = ggplot2::guide_legend(override.aes = list(size = 2, alpha = 0.5), title = "population")
    )

  if (transform) {
    p <- p +
      ggplot2::scale_y_continuous(trans="log10") +
      ggplot2::scale_x_continuous(trans="log10") +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim)
  } else {
    p <- p +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim)
  }

  # Draw gating polygons if provided
  if (!is.null(gating_params)) {
    if (!all(c("gating", "poly") %in% names(gating_params))) {
      stop("gating_params must be a list containing 'gating' and 'poly' data frames.")
    }
    gating <- gating_params$gating
    poly <- gating_params$poly
    # Iterate over each row in gating, selecting poly rows with matching
    # gating_id and pop. If the gating polygon is appropriate for the current x
    # and y channels, draw it on the plot.
    for (i in seq_len(nrow(gating))) {
      gating_id <- gating$id[i]
      pop <- gating$pop[i]
      gating_channels <- c(gating$channel1[i], gating$channel2[i])
      if (any(is.na(gating_channels))) {
        next
      }
      if (!(setequal(gating_channels, c(x, y)))) {
        next
      }

      poly_subset <- poly |> dplyr::filter(gating_id == .env[["gating_id"]], pop == .env[["pop"]])
      if (nrow(poly_subset) > 0) {
        p <- p + ggplot2::geom_polygon(
          data = poly_subset,
          ggplot2::aes(x = .data[[x]], y = .data[[y]]),
          fill = NA,
          color = pop_colors[pop],
          linewidth = 0.5
        )

        if (isTRUE(show_gating_order) && "pop_order" %in% names(gating)) {
          label_data <- poly_subset |>
            dplyr::summarise(
              !!x := mean(.data[[x]], na.rm = TRUE),
              !!y := mean(.data[[y]], na.rm = TRUE)
            ) |>
            dplyr::mutate(pop_order = gating$pop_order[[i]])

          p <- p + ggplot2::geom_text(
            data = label_data,
            ggplot2::aes(x = .data[[x]], y = .data[[y]], label = pop_order),
            color = pop_colors[pop],
            size = 5,
            fontface = "bold",
            show.legend = FALSE,
            na.rm = TRUE
          )
        }

        if (isTRUE(show_gating_order) && "point_order" %in% names(poly_subset)) {
          p <- p + ggplot2::geom_text(
            data = poly_subset,
            ggplot2::aes(x = .data[[x]], y = .data[[y]], label = point_order),
            color = pop_colors[pop],
            size = 3,
            vjust = -0.6,
            show.legend = FALSE,
            na.rm = TRUE
          )
        }
      }
    }
  }

  p
}