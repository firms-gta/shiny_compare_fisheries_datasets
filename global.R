require(parallel)
require(here)
require(futile.logger)
require(markdown)
require(plotly)
require(leaflet.extras)
source(here::here('install.R'))
flog.info("Loading libraries")
flog.info("All libraries loaded successfully.")
futile.logger::flog.info("Turn s2 off for sf => avoid issues")
sf::sf_use_s2(FALSE)
futile.logger::flog.info("Load functions")
source(here::here('R/download_data.R'))
source(here::here('R/load_data.R'))
source(here::here('R/load_spatial_data.R'))
source(here::here('R/load_default_dataset.R'))
source(here::here('R/load_grouped_data.R'))
source(here::here('R/load_filters_combinations.R'))
source(here::here('R/update_current_filters.R'))
source(here::here('R/list_areas_within_wkt.R'))
source(here::here('R/verify_filesize.R'))
source(here::here('R/apply_filters.R'))
# Initialize variables and reactive values and default WKT for mapping
flog.info("Initialize variables")
reset_all <- FALSE
spatial_processing_mode <- "sf" # "QGIS"
all_wkt <- ""
within_areas <- NULL
flog.info("Initialize reactive values")

whole_dataset <- reactiveVal()
whole_filtered_df <- reactiveVal()
filtered_default_df <- reactiveVal()
main_df <- reactiveVal()
plot_df <- reactiveVal()
map_df <- reactiveVal()

current_wkt <- reactiveVal()
last_wkt <- reactiveVal()
map_wkt <- reactiveVal()
current_selection_footprint_wkt <- reactiveVal()

current_dataset <- reactiveVal()
current_species <- reactiveVal()
current_source_authority <- reactiveVal()
current_gear_type <- reactiveVal()
current_year <- reactiveVal()
current_fishing_fleet <- reactiveVal()
current_unit <- reactiveVal()
current_gridtype <- reactiveVal()
switch_unit <- reactiveVal(TRUE)
flog.info("Reactive values initialized successfully.")
# mode="DOI" | mode="gpkg" | mode="postgres" | mode="QS" | mode="parquet"
mode="DOI"

flog.info("Loading data with mode: %s", mode)
flog.info("Loading ist of data from other script : list_dataframes.")
source(here::here("create_or_load_default_dataset.R"))
whole_dataset(list_dataframes$whole_group_df)
filters_combinations <- list_dataframes$filters_combinations
list_values_dimensions <- list_dataframes$list_values_dimensions
# flog.info("nrow(whole_group_df) %s: ",nrow(whole_dataset()))
flog.info("All data succesfully loaded")
df_distinct_geom <-  list_dataframes$df_distinct_geom
all_polygons <- list_dataframes$all_polygons
all_polygons_footprint <- list_dataframes$all_polygons_footprint
list_default_filters <- list_dataframes$list_default_filters
init_whole_default_df <- list_dataframes$init_whole_default_df

update_current_filters(list_filters_values = list_default_filters)
current_wkt(list_default_filters$wkt)
last_wkt(list_default_filters$wkt)
all_wkt <- st_as_text(st_union(df_distinct_geom))
new_wkt <- all_wkt
target_wkt <- list_default_filters$wkt
within_areas <- list_default_filters$within_areas

default_footprint <- list_dataframes$default_footprint
default_df <- list_dataframes$default_df

whole_filtered_df(init_whole_default_df)
current_selection_footprint_wkt(default_footprint)
filtered_default_df(default_df)


flog.info("########################## LOAD MODULES")
# Source external R scripts for additional functionalities
flog.info("External R scripts sourced successfully.")
flog.info("Loading modules.")
load_ui_modules <- function() {
  ui_files <- c("modules/map_leaflet.R","modules/timeSeries.R","modules/pie_bar_charts.R","modules/timeSeries_per_geartype.R","modules/about.R")
  lapply(ui_files, function(file) {
    source(here::here(file))
    flog.info(paste("Loaded UI module:", file))
  })
}
load_ui_modules()
flog.info("########################## All Modules loaded")

flog.info("########################## START UI")
source(here::here("ui.R"))
flog.info("########################## START SERVER")
source(here::here("server.R"))
flog.info("########################## END GLOBAL")