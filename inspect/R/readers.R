read_parquet_duckdb <- function(path) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbGetQuery(con, glue("SELECT * FROM read_parquet('{path}')")) |>
    as_tibble()
}

read_sfl_table <- function(db_file) {
  popcycle::get_sfl_table(db_file, outlier_join = TRUE) |>
    as_tibble() |>
    select(
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
    rename(
      time = date
    ) |>
    mutate(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    ) |>
    arrange(time)
}

read_stat_file <- function(stat_file) {
  read_parquet_duckdb(stat_file) |>
    filter(quantile == QUANTILE) |>
    select(
      time,
      pop,
      opp_evt_ratio,
      abundance,
      glue("diam_{REFRAC}_med"),
      flag
    ) |>
    rename(
      diameter = glue("diam_{REFRAC}_med")
    ) |>
    mutate(time = lubridate::ymd_hms(time, quiet = TRUE)) |>
    arrange(time)
}

read_filter_params <- function(db_file, max_date = NULL) {
  fp <- popcycle::get_filter_table(db_file) |>
    filter(quantile == QUANTILE) |>
    select(-date) |>
    tibble::as_tibble()
  plan <- popcycle::get_filter_plan_table(db_file) |>
    mutate(start_date = lubridate::ymd_hms(start_date, quiet = TRUE)) |>
    arrange(start_date) |>
    mutate(end_date = dplyr::lead(start_date)) |>
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
