#' Find all <cruise> stat parquet, outlier db, bead sample parquet, and VCT dirs
list_data_source_files <- function(data_sources) {
  parse_cruise_from_filename <- function(paths) {
    sub("\\..*$", "", basename(paths))
  }

  #' OPP or VCT files are in folders named <cruise>_opp or <cruise>_vct
  parse_cruise_from_folder <- function(paths) {
    sub("_(opp|vct)$", "", basename(paths))
  }

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

    bead_files <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.beads-sample-.*\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )

    opp_files <- list.files(
      file.path(dirs, "results"),
      pattern = "\\.opp\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )

    vct_dirs <- list.files(
      file.path(dirs, "results"),
      pattern = "_vct$",
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = TRUE
    )

    if (length(stat_files) == 0 && length(outlier_dbs) == 0 && length(bead_files) == 0 && length(vct_dirs) == 0) {
      return(tibble::tibble(
        name = character(),
        cruise = character(),
        stat_file = character(),
        outlier_db = character(),
        bead_file = character(),
        vct_dir = character()
      ))
    }

    stat_tbl <- tibble::tibble(
      name = name,
      cruise = parse_cruise_from_filename(stat_files),
      stat_file = stat_files
    )

    outlier_tbl <- tibble::tibble(
      name = name,
      cruise = parse_cruise_from_filename(outlier_dbs),
      outlier_db = outlier_dbs
    )

    bead_tbl <- tibble::tibble(
      name = name,
      cruise = parse_cruise_from_filename(bead_files),
      bead_file = bead_files
    )

    vct_tbl <- tibble::tibble(
      name = name,
      cruise = parse_cruise_from_folder(vct_dirs),
      vct_dir = vct_dirs
    )

    dplyr::full_join(stat_tbl, outlier_tbl, by = c("name", "cruise")) |>
      dplyr::full_join(bead_tbl, by = c("name", "cruise")) |>
      dplyr::full_join(vct_tbl, by = c("name", "cruise"))
  }) |>
    purrr::list_rbind()
}

all_data_source_files <- list_data_source_files(data_sources)
