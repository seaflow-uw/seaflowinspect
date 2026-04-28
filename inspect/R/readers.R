read_parquet_duckdb <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, glue::glue("SELECT * FROM read_parquet('{path}')")) |>
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

  out <- dplyr::left_join(plan, fp, by = c("filter_id" = "id"))

  if (!is.null(max_date) && nrow(out) > 0) {
    last_idx <- nrow(out)
    if (is.na(out$end_date[[last_idx]])) {
      out$end_date[[last_idx]] <- max_date
    }
  }

  out
}
