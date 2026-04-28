#' Find all <cruise> stat parquet and outlier db files
list_data_source_files <- function(data_sources) {
  purrr::pmap(data_sources, function(name, dirs) {
    stat_files <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.stat\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )

    outlier_dbs <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.outlier\\.db$",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(stat_files) == 0 && length(outlier_dbs) == 0) {
      return(tibble::tibble(
        name = character(),
        cruise = character(),
        stat_file = character(),
        outlier_db = character()
      ))
    }

    stat_tbl <- tibble::tibble(
      name = name,
      cruise = basename(dirname(stat_files)),
      stat_file = stat_files
    )

    outlier_tbl <- tibble::tibble(
      name = name,
      cruise = basename(dirname(outlier_dbs)),
      outlier_db = outlier_dbs
    )

    dplyr::full_join(stat_tbl, outlier_tbl, by = c("name", "cruise"))
  }) |>
    purrr::list_rbind()
}

all_data_source_files <- list_data_source_files(data_sources)
