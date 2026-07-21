#' Find all <cruise> outlier db, bead sample parquet, and VCT dirs
list_data_source_files <- function(data_sources) {
  parse_cruise_from_filename <- function(paths) {
    sub("\\..*$", "", basename(paths))
  }

  #' OPP or VCT files are in folders named <cruise>_opp or <cruise>_vct
  parse_cruise_from_folder <- function(paths) {
    sub("_(opp|vct)$", "", basename(paths))
  }

  assert_unique_per_cruise <- function(paths, cruise_parser, source_type, name, dir) {
    if (length(paths) <= 1) {
      return(invisible(NULL))
    }

    cruises <- cruise_parser(paths)
    duplicate_cruises <- unique(cruises[duplicated(cruises)])

    if (length(duplicate_cruises) == 0) {
      return(invisible(NULL))
    }

    duplicate_paths <- paths[cruises %in% duplicate_cruises]
    stop(
      paste0(
        "Expected at most one ", source_type,
        " per cruise in data source '", name,
        "' (dir '", dir,
        "'), but found duplicates for cruise(s): ",
        paste(duplicate_cruises, collapse = ", "),
        ". Matching paths: ",
        paste(duplicate_paths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  purrr::pmap(data_sources, function(name, dir, default) {
    outlier_dbs <- list.files(
      dir,
      pattern = "\\.outlier\\.db$",
      recursive = TRUE,
      full.names = TRUE
    )

    bead_files <- list.files(
      dir,
      pattern = "\\.beads-sample-.*\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )

    vct_dirs <- list.files(
      dir,
      pattern = "_vct$",
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = TRUE
    )

    # Find gridded data files. The "grid" parent dir can occur at varying
    # depths under dir, so list.files' pattern (which only matches basename)
    # can't select for it directly -- filter the candidate paths afterward.
    candidate_grid_files <- list.files(
      dir,
      pattern = "\\.grid_bins\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
    grid_files <- grep("/grid/.*\\.grid_bins\\.parquet$", candidate_grid_files, value = TRUE)

    candidate_gridded_files <- list.files(
      dir,
      pattern = "\\.hourly_gridded\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
    gridded_files <- grep("/grid/.*\\.hourly_gridded\\.parquet$", candidate_gridded_files, value = TRUE)

    # Make sure we only find one unambiguous file per cruise for each data type,
    # within each data source.
    assert_unique_per_cruise(outlier_dbs, parse_cruise_from_filename, "outlier db", name, dir)
    assert_unique_per_cruise(bead_files, parse_cruise_from_filename, "bead file", name, dir)
    assert_unique_per_cruise(vct_dirs, parse_cruise_from_folder, "vct dir", name, dir)
    assert_unique_per_cruise(grid_files, parse_cruise_from_filename, "grid bins file", name, dir)
    assert_unique_per_cruise(gridded_files, parse_cruise_from_filename, "grid gridded file", name, dir)

    if (length(outlier_dbs) == 0 && length(bead_files) == 0 && length(vct_dirs) == 0) {
      return(tibble::tibble(
        name = character(),
        default = logical(),
        cruise = character(),
        outlier_db = character(),
        bead_file = character(),
        vct_dir = character()
      ))
    }

    outlier_tbl <- tibble::tibble(
      name = name,
      default = default,
      cruise = parse_cruise_from_filename(outlier_dbs),
      outlier_db = outlier_dbs
    )

    bead_tbl <- tibble::tibble(
      name = name,
      default = default,
      cruise = parse_cruise_from_filename(bead_files),
      bead_file = bead_files
    )

    vct_tbl <- tibble::tibble(
      name = name,
      default = default,
      cruise = parse_cruise_from_folder(vct_dirs),
      vct_dir = vct_dirs
    )

    grid_tbl <- tibble::tibble(
      name = name,
      default = default,
      cruise = parse_cruise_from_filename(grid_files),
      grid_file = grid_files
    )
    gridded_tbl <- tibble::tibble(
      name = name,
      default = default,
      cruise = parse_cruise_from_filename(gridded_files),
      gridded_file = gridded_files
    )

    dplyr::full_join(outlier_tbl, bead_tbl, by = c("name", "default", "cruise")) |>
      dplyr::full_join(vct_tbl, by = c("name", "default", "cruise")) |>
      dplyr::full_join(grid_tbl, by = c("name", "default", "cruise")) |>
      dplyr::full_join(gridded_tbl, by = c("name", "default", "cruise"))
  }) |>
    purrr::list_rbind()
}

all_data_source_files <- list_data_source_files(data_sources)
