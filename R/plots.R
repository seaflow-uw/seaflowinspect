#' Normalize a plotly click x value (POSIXt, numeric epoch, or string) to a
#' single UTC POSIXct, or NULL if it cannot be parsed.
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

#' Create a vertical line shape for the selected x value
build_selected_vline_shapes <- function(x) {
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
}