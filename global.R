# rm(list = ls())
dir <- getwd()
require(parallel)
require(here)
require(futile.logger)
source(here::here('install.R'))
flog.info("Loading libraries")
flog.info("All libraries loaded successfully.")
spatial_processing_mode <- "sf" # "QGIS"
sf::sf_use_s2(FALSE)

# loadSupport(  ) ??
futile.logger::flog.info("Load zenodo download function")
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
source(here::here("create_or_load_default_dataset.R"))
# Initialize variables and reactive values and default WKT for mapping
reset_all <- FALSE

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

# mode="DOI" | mode="gpkg" | mode="postgres" | mode="RDS" | mode="parquet"
mode="DOI"

flog.info("Loading data with mode: %s", mode)
# ########################################################## Load data from a list of DOIs ########################################################## 
# list_DOIs <-"data/DOI.csv"
# DOIs <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
# list_dataframes <- load_data(mode=mode)
whole_dataset(list_dataframes$whole_group_df)
filters_combinations <- list_dataframes$filters_combinations
list_values_dimensions <- list_dataframes$list_values_dimensions
# flog.info("nrow(whole_group_df) %s: ",nrow(whole_dataset()))
# whole_dataset(df_sf)
flog.info("All data succesfully loaded")
setwd(dir)

# main_df(whole_dataset())
# whole_map_df <- whole_dataset() %>%  dplyr::group_by(geom_wkt, dataset, measurement_unit) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(wkt="geom_wkt",crs=4326)
# 
# whole_plot_df <- whole_dataset()  %>% 
#   dplyr::group_by(dataset, year, gridtype, measurement_unit) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()


flog.info("Load spatial filter data")
df_distinct_geom <-  load_spatial_data(df_sf=whole_dataset(), mode=mode)
all_polygons <- df_distinct_geom %>% st_combine() # %>% st_simplify() 
all_polygons_footprint <- all_polygons %>% st_as_text()
  
# possible_values / selected_values / current_values
flog.info("Set values of filters : list distinct values in the main dataset for each dimension")
all_wkt <- st_as_text(st_union(df_distinct_geom))
new_wkt <- all_wkt

flog.info("Set filters values to be applied by default (before user selection)")
# flog.info("Spatial filter :main WKT : %s", current_wkt())
default_dataset <- c('global_catch_tunaatlasird_level2_1164128',
                     'global_catch_tunaatlasird_level2_14184244',
                     'global_nominal_catch_firms_level0_public_11410529') # c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
default_species <- c('YFT') # c('YFT','SKJ','BET','SBF','ALB')
default_year <- c(seq(1:10)+2010) # c(seq(min(list_values_dimensions$year):max(list_values_dimensions$year))+min(list_values_dimensions$year)-2) | c(seq(1950:2021)+1949) | c(seq((max(list_values_dimensions$year)-10):max(list_values_dimensions$year))+max(list_values_dimensions$year)-11)
default_gear_type <- c('1.1','1.2') #  c('01.1','01.2')
default_unit <- c('t')
default_source_authority <- unique(list_values_dimensions$source_authority)
default_gridtype <- list_values_dimensions$gridtype # c("1deg_x_1deg")
default_fishing_fleet <- c('EUFRA','EUESP')
flog.info("Default filters values set.")
list_default_filters = list("dataset"=default_dataset,
                           "species"=default_species,
                           "year"=default_year,
                           "gear_type"=default_gear_type,
                           "unit"=default_unit,
                           "source_authority"=default_source_authority,
                           "gridtype"=default_gridtype,
                           "fishing_fleet"=default_fishing_fleet)
update_current_filters(list_filters_values = list_default_filters)

flog.info("Keeping tracks of current selected values for filters to faster data loading.")
target_wkt <- "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))"
# target_wkt <- "POLYGON ((-10.195313 49.15297,33.222656 49.15297,33.222656 35.46067,-10.195313 35.46067,-10.195313 49.15297))"
current_wkt(target_wkt)
last_wkt(target_wkt)
current_selection <- st_sf(st_as_sfc(target_wkt, crs = 4326))
# current_areas ?
within_areas <- process_list_areas(df_distinct_geom, wkt=target_wkt, list_gridtype=default_gridtype) 

# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

flog.info("Load default dataset!!")
# add parameter = list of values ?
init_whole_default_df <- load_default_dataset(df=list_dataframes$whole_group_df, 
                                              filename="data/default_df.parquet",
                                              list_filters=list_default_filters
                                              )
whole_filtered_df(init_whole_default_df)

# add function to calculate the footprint of a df ?
default_footprint <- init_whole_default_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
  dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
  st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine() %>% st_as_text() # %>% st_simplify()
# flog.info("Current footprint for filters is %s: ",whole_footprint)
current_selection_footprint_wkt(default_footprint)

default_df <- init_whole_default_df  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
filtered_default_df(default_df)
flog.info("########################## DEFAULT FILTERED DATA LOADED")

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