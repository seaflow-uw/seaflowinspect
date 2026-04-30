
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
                              gating_params = NULL) {
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

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
        x = !!x,
        y = !!y,
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
      ggplot2::scale_y_continuous(trans="log10", limits=ylim) +
      ggplot2::scale_x_continuous(trans="log10", limits=xlim)
  } else {
    p <- p +
      ggplot2::scale_y_continuous(limits=ylim) + ggplot2::scale_x_continuous(limits=xlim)
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
      if (!(setequal(gating_channels, c(rlang::as_string(x), rlang::as_string(y))))) {
        next
      }

      poly_subset <- poly |> dplyr::filter(gating_id == .env[["gating_id"]], pop == .env[["pop"]])
      if (nrow(poly_subset) > 0) {
        p <- p + ggplot2::geom_polygon(
          data = poly_subset,
          ggplot2::aes(x = !!x, y = !!y),
          fill = NA,
          color = pop_colors[pop],
          linewidth = 0.5
        )
      }
    }
  }

  p
}