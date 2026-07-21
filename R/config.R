# App configuration
SFL_PLOT_VH <- 20
STAT_PLOT_VH <- 20
TIME_NAV_PLOT_VH <- 5
FILTER_PARAMS_PLOT_VH <- 20
FILTER_PLOT_VH <- 40
BEAD_HEXBIN_BINS <- 110
VCT_CYTOGRAM_BINS <- 140
CYTOGRAM_POINT_LIMIT <- 40000
QUANTILE <- 50
REFRAC <- "mid"

# Data set roots are loaded from config/data_sets.csv.
# Override path with env var SEAFLOWINSPECT_DATA_SETS_FILE when deploying.
DATA_SETS_FILE <- Sys.getenv(
  "SEAFLOWINSPECT_DATA_SETS_FILE",
  unset = file.path("config", "data_sets.csv")
)

if (!file.exists(DATA_SETS_FILE)) {
  stop(glue::glue("Data sets config not found: {DATA_SETS_FILE}"))
}

data_sets_raw <- readr::read_csv(DATA_SETS_FILE, show_col_types = FALSE)

required_cols <- c("name", "dir", "default")
if (!all(required_cols %in% names(data_sets_raw))) {
  stop("Data sets config must include columns: name, dir, default")
}

data_sets_raw <- data_sets_raw |>
  dplyr::mutate(
    default = tolower(as.character(default)) %in% c("true", "t", "1", "yes", "y")
  )

data_sets <- data_sets_raw |>
  dplyr::select(name, dir, default)
