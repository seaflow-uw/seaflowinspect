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