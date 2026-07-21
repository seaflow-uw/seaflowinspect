A project to explore SeaFlow data through interactive visualizations.

## Install

Install the popcycle R package ([https://github.com/seaflow-uw/popcycle](https://github.com/seaflow-uw/popcycle)). Then
install the dependencies for this app with `Rscript install-deps.R`. This script will install the `remotes` package if needed and then use `remotes::install_deps()` to install dependencies.

## Configure data sets

Copy `config/data_sets.template.csv` to e.g. `config/data_sets.csv` and edit to point to the SeaFlow data sets you want to inspect. You can place this file elsewhere and reference its path with the environment variable `SEAFLOWINSPECT_DATA_SETS_FILE`. There are three columns: `name`, `dir`, and `default`. `name` is the label for a data set, `dir` is the path to the data set, and `default` should be `TRUE` for the the default data set to load and FALSE for others. Each data set dir should contain the following files or folders, where `CRUISE` is a placeholder for cruise names:


### `CRUISE.outlier.db`

**required**

A Popcycle SQLite database file. It should contain populated tables up through gating. This data is used as input for the global "Time Navigator" plot, the "SFL" plot, the "Stat" plot, the "Filter Params" plot, and the gating polygons and tables in the "Gating" tab.

### `CRUISE_vct`

VCT directory with hourly VCT Parquet files. This data is used to show true OPP in the "Filter" tab cytograms and to show population-labeled cytograms in the "Gating" tab.

### `CRUISE.beads-sample-*.parquet`

Subsampled bead particles for an entire cruise. This is usually the file used as input to bead finding with beadclust. This data is used to generate plots in the "Filter" tab.

### `CRUISE.grid_bins.parquet`, `CRUISE.hourly_gridded.parquet`

Gridded data files. These must be in a subdirectory named `grid` (there are other forms of these files in our snakemake pipeline in various subdirectories). This data is used as input to the "Gridded 3D" and "Ridgeline" tabs.

## Run

`Rscript runInspect.R`

If you have placed your copy of `config/data_sets.template.csv` in a non-standard location (not `config/data_sets.csv`), you can use it by setting the env var `SEAFLOWINSPECT_DATA_SETS_FILE`.

`SEAFLOWINSPECT_DATA_SETS_FILE=/path/to/data_sets.csv runInspect.R`.
