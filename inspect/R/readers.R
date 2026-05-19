read_parquet_duckdb <- function(path, select_cols = NULL, where_clause = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  quoted_path <- gsub("'", "''", path, fixed = TRUE)

  select_sql <- "*"
  if (!is.null(select_cols) && length(select_cols) > 0) {
    select_sql <- paste(DBI::dbQuoteIdentifier(con, select_cols), collapse = ", ")
  }

  where_sql <- ""
  if (!is.null(where_clause) && nzchar(where_clause)) {
    where_sql <- glue::glue(" WHERE {where_clause}")
  }

  query <- glue::glue("SELECT {select_sql} FROM read_parquet('{quoted_path}'){where_sql}")

  DBI::dbGetQuery(con, query) |>
    tibble::as_tibble()
}

read_sfl_table <- function(db_file) {
  popcycle::get_sfl_table(db_file, outlier_join = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select(
      date,
      lat,
      lon,
      conductivity,
      salinity,
      ocean_tmp,
      par,
      stream_pressure,
      event_rate,
      flag
    ) |>
    dplyr::rename(
      time = date
    ) |>
    dplyr::mutate(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    ) |>
    dplyr::arrange(time)
}

read_stat_file <- function(stat_file) {
  read_parquet_duckdb(stat_file) |>
    dplyr::filter(quantile == QUANTILE) |>
    dplyr::select(
      time,
      pop,
      opp_evt_ratio,
      abundance,
      glue("diam_{REFRAC}_med"),
      flag
    ) |>
    dplyr::rename(
      diameter = glue("diam_{REFRAC}_med")
    ) |>
    dplyr::mutate(time = lubridate::ymd_hms(time, quiet = TRUE)) |>
    dplyr::arrange(time)
}

read_filter_params <- function(db_file, max_date = NULL) {
  normalize_datetime <- function(x) {
    if (inherits(x, "POSIXt")) {
      return(as.POSIXct(x))
    }

    if (is.numeric(x)) {
      return(lubridate::as_datetime(x))
    }

    out <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
    if (all(is.na(out))) {
      out <- suppressWarnings(lubridate::ymd_hm(x, quiet = TRUE))
    }
    out
  }

  fp <- popcycle::get_filter_table(db_file) |>
    dplyr::filter(quantile == QUANTILE) |>
    dplyr::select(-date) |>
    tibble::as_tibble()
  plan <- popcycle::get_filter_plan_table(db_file) |>
    dplyr::mutate(start_date = normalize_datetime(start_date)) |>
    dplyr::arrange(start_date) |>
    dplyr::mutate(end_date = dplyr::lead(start_date)) |>
    tibble::as_tibble()

  if (!is.null(max_date) && nrow(plan) > 0) {
    last_idx <- nrow(plan)
    if (is.na(plan$end_date[[last_idx]])) {
      plan$end_date[[last_idx]] <- max_date
    }
  }

  out <- dplyr::left_join(plan, fp, by = c("filter_id" = "id"))

  out
}

read_gating_params <- function(db_file, max_date = NULL) {
  normalize_datetime <- function(x) {
    if (inherits(x, "POSIXt")) {
      return(as.POSIXct(x))
    }

    if (is.numeric(x)) {
      return(lubridate::as_datetime(x))
    }

    out <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
    if (all(is.na(out))) {
      out <- suppressWarnings(lubridate::ymd_hm(x, quiet = TRUE))
    }
    out
  }

  gating_tbl <- popcycle::get_gating_table(db_file) |>
    dplyr::select(!date) |>
    tibble::as_tibble()
  poly_tbl <- popcycle::get_poly_table(db_file) |>
    tibble::as_tibble()
  plan_tbl <- popcycle::get_gating_plan_table(db_file) |>
    dplyr::mutate(start_date = normalize_datetime(start_date)) |>
    dplyr::arrange(start_date) |>
    dplyr::mutate(end_date = dplyr::lead(start_date)) |>
    tibble::as_tibble()
  if (!is.null(max_date) && nrow(plan_tbl) > 0) {
    last_idx <- nrow(plan_tbl)
    if (is.na(plan_tbl$end_date[[last_idx]])) {
      plan_tbl$end_date[[last_idx]] <- max_date
    }
  }

  gating_tbl <- plan_tbl |>
    dplyr::left_join(gating_tbl, by = c("gating_id" = "id")) |>
    dplyr::rename(id = gating_id)

  list(
    gating = gating_tbl,
    poly = poly_tbl
  )
}

read_bead_sample <- function(bead_file) {
  read_parquet_duckdb(bead_file) |>
    dplyr::rename(time = date) |>
    dplyr::select(-file_id)
}

parse_vct_file_hour <- function(vct_file) {
  ts_txt <- sub("\\..*$", "", basename(vct_file))
  ts_rfc3339 <- sub(
    "^(.+T\\d{2})-(\\d{2})-(\\d{2})([+-]\\d{2})-(\\d{2})$",
    "\\1:\\2:\\3\\4:\\5",
    ts_txt
  )

  parsed <- suppressWarnings(lubridate::ymd_hms(ts_rfc3339, quiet = TRUE))
  if (all(is.na(parsed))) {
    return(as.POSIXct(NA))
  }

  # Canonicalize to UTC so filename hours with different offsets can be
  # compared deterministically against selected UTC hour values.
  lubridate::with_tz(parsed[[1]], tzone = "UTC")
}

resolve_vct_file_for_hour <- function(vct_dir, hour) {
  vct_files <- list.files(vct_dir, pattern = "\\.vct\\.parquet$", full.names = TRUE)
  if (length(vct_files) == 0) {
    rlang::abort(
      glue::glue("No VCT parquet files found in directory: {vct_dir}"),
      class = "vct_no_files"
    )
  }

  target_hour <- lubridate::floor_date(
    lubridate::with_tz(as.POSIXct(hour), tzone = "UTC"),
    unit = "hour"
  )
  file_hours <- vapply(vct_files, parse_vct_file_hour, FUN.VALUE = as.POSIXct(NA), USE.NAMES = FALSE)
  matches <- vct_files[!is.na(file_hours) & file_hours == target_hour]

  if (length(matches) == 0) {
    rlang::abort(
      glue::glue("No VCT file matches selected UTC hour {format(target_hour, '%Y-%m-%dT%H:%M:%SZ')} in {vct_dir}"),
      class = "vct_no_match"
    )
  }

  if (length(matches) > 1) {
    rlang::abort(
      glue::glue("Multiple VCT files match selected UTC hour {format(target_hour, '%Y-%m-%dT%H:%M:%SZ')} in {vct_dir}"),
      class = "vct_multiple_matches"
    )
  }

  matches[[1]]
}

read_vct_parquet <- function(vct_dir, dt, scope = c("point", "hour")) {
  scope <- match.arg(scope)
  vct_file <- resolve_vct_file_for_hour(vct_dir, dt)
  cols <- c(
    "date",
    glue::glue("pop_q{QUANTILE}"),
    "D1",
    "D2",
    "fsc_small",
    "chl_small",
    "pe",
    glue::glue("diam_{REFRAC}_q{QUANTILE}"),
    glue::glue("Qc_{REFRAC}_q{QUANTILE}"),
    "filter_id",
    "gating_id"
  )

  data <- read_parquet_duckdb(
    vct_file,
    select_cols = cols,
    where_clause = glue::glue("q{QUANTILE} = TRUE")
  ) |>
    dplyr::rename(
      time = date,
      pop = glue::glue("pop_q{QUANTILE}"),
      diameter = glue::glue("diam_{REFRAC}_q{QUANTILE}"),
      qc = glue::glue("Qc_{REFRAC}_q{QUANTILE}")
    )

  if (scope == "point") {
    data <- data |>
      dplyr::filter(time == dt)
  }

  data
}

read_grid_parquet <- function(grid_file) {
  message(glue::glue("Reading grid parquet with DuckDB: {grid_file}"))
  data <- read_parquet_duckdb(grid_file)
  message(glue::glue("Finished reading grid parquet: {grid_file}"))
  data
}

read_grid_bins_parquet <- function(grid_bins_file) {
  read_parquet_duckdb(grid_bins_file)
}
