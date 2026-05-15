# App configuration
SFL_PLOT_VH <- 20
STAT_PLOT_VH <- 20
FILTER_PARAMS_PLOT_VH <- 20
FILTER_PLOT_VH <- 40
BEAD_HEXBIN_BINS <- 110
VCT_CYTOGRAM_BINS <- 140
CYTOGRAM_POINT_LIMIT <- 40000
QUANTILE <- 50
REFRAC <- "mid"

# Data source roots are loaded from config/data_sources.csv.
# Override path with env var SEAFLOWINSPECT_DATA_SOURCES_FILE when deploying.
DATA_SOURCES_FILE <- Sys.getenv(
  "SEAFLOWINSPECT_DATA_SOURCES_FILE",
  unset = file.path("config", "data_sources.csv")
)

if (!file.exists(DATA_SOURCES_FILE)) {
  stop(glue::glue("Data sources config not found: {DATA_SOURCES_FILE}"))
}

data_sources_raw <- readr::read_csv(DATA_SOURCES_FILE, show_col_types = FALSE)

required_cols <- c("name", "dirs", "default")
if (!all(required_cols %in% names(data_sources_raw))) {
  stop("Data sources config must include columns: name, dirs, default")
}

data_sources_raw <- data_sources_raw |>
  dplyr::mutate(
    default = tolower(as.character(default)) %in% c("true", "t", "1", "yes", "y")
  )

data_sources <- data_sources_raw |>
  dplyr::select(name, dirs, default)
