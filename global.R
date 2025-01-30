rm(list = ls())
dir <- getwd()
require(parallel)
require(here)
require(futile.logger)
source(here::here('install.R'))
flog.info("Loading libraries")
flog.info("All libraries loaded successfully.")
sf::sf_use_s2(FALSE)
spatial_processing_mode <- "sf" # "QGIS"

# loadSupport(  ) ??
futile.logger::flog.info("Load zenodo download function")
source(here::here('R/download_data.R'))
source(here::here('R/load_data.R'))
source(here::here('R/load_spatial_data.R'))
source(here::here('R/load_default_dataset.R'))
source(here::here('R/load_grouped_data.R'))
source(here::here('R/load_filters_combinations.R'))
source(here::here('R/list_areas_within_wkt.R'))
source(here::here('R/verify_filesize.R'))

# Initialize reactive values and default WKT for mapping
flog.info("Initialize reactive values")
reset_all <- FALSE

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
########################################################## Load data from a list of DOIs ########################################################## 
list_DOIs <-"data/DOI.csv"
DOI <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
df_sf <- load_data(mode=mode)
flog.info("All data succesfully loaded")

setwd(dir)

flog.info("Loading / storing aggregated data")
whole_group_df <- load_grouped_data(filename = "whole_group_df.parquet")
# whole_dataset(df_sf)
whole_dataset(whole_group_df)
flog.info("nrow(whole_group_df) %s: ",nrow(whole_group_df))

# main_df(whole_group_df)
# whole_map_df <- whole_group_df %>%  dplyr::group_by(geom_wkt, dataset, measurement_unit) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(wkt="geom_wkt",crs=4326)
# 
# whole_plot_df <- whole_group_df  %>% 
#   dplyr::group_by(dataset, year, gridtype, measurement_unit) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()

flog.info("Load non spatial filters combinations")
filters_combinations <- load_filters_combinations(filename = "data/filters_combinations.parquet")

flog.info("Load spatial filter data")
df_distinct_geom <-  load_spatial_data(mode=mode,df_sf=whole_group_df)
all_polygons <- df_distinct_geom %>% st_combine() 
all_polygons_footprint <- all_polygons %>% st_as_text()
  

flog.info("Set values of filters : list distinct values in the main dataset for each dimension")
all_wkt <- st_as_text(st_union(df_distinct_geom))
new_wkt <- all_wkt
flog.info("Storing all possible values for retained filters : list distinct values in the main dataset for each dimension")
# target_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
target_dataset <- unique(filters_combinations$dataset) #  %>% arrange(desc(dataset))
target_species <- unique(filters_combinations$species) # %>% arrange(desc(species))
target_year <-  unique(filters_combinations$year)  # %>% arrange(desc(year))
target_gear_type <-  unique(filters_combinations$gear_type)# %>% arrange(desc(gear_type))
target_measurement_unit <- unique(filters_combinations$measurement_unit) # %>% arrange(desc(measurement_unit))
target_source_authority <- unique(filters_combinations$source_authority)
target_gridtype <- unique(df_distinct_geom$gridtype) 
target_fishing_fleet <-  unique(filters_combinations$fishing_fleet)

flog.info("Set filters values to be applied by default (before user selection)")
# flog.info("Spatial filter :main WKT : %s", current_wkt())
# default_species <- c('YFT','SKJ','BET','SBF','ALB')
# default_dataset <- c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
default_dataset <- c('global_catch_tunaatlasird_level2','global_nominal_catch_firms_level0_public')
default_species <- c('YFT')
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-2)
default_year <- c(seq(1:10)+2010)
# default_year <- c(seq(1950:2021)+1949)
# default_year <- c(seq((max(target_year)-10):max(target_year))+max(target_year)-11)
# default_gear <- c('01.1','01.2')
default_gear_type <- c('1.1','1.2')
default_unit <- c('t')
default_source_authority <- unique(target_source_authority)
# default_gridtype <- c("1deg_x_1deg")
default_gridtype <- target_gridtype
default_fishing_fleet <- c('EUFRA','EUESP')
flog.info("Default filters values set.")


flog.info("Keeping tracks of current selected values for filters to faster data loading.")
target_wkt <- "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))"
# target_wkt <- "POLYGON ((-10.195313 49.15297,33.222656 49.15297,33.222656 35.46067,-10.195313 35.46067,-10.195313 49.15297))"
current_wkt(target_wkt)
last_wkt(target_wkt)
current_selection <- st_sf(st_as_sfc(target_wkt, crs = 4326))
current_species(default_species)
current_dataset(default_dataset)
current_source_authority(default_source_authority)
current_gear_type(default_gear_type)
current_year(default_year)
current_fishing_fleet(default_fishing_fleet)
current_unit(default_unit)
current_gridtype(default_gridtype)


# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

flog.info("Load default dataset!!")

within_areas <- process_list_areas(df_distinct_geom, wkt=target_wkt, list_gridtype=default_gridtype) 

init_whole_default_df <- load_default_dataset(df=whole_group_df, filename="default_df.parquet")
whole_filtered_df(init_whole_default_df)

default_footprint <- init_whole_default_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
  dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
  st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine() %>% st_as_text() # %>% st_simplify()
# flog.info("Current footprint for filters is %s: ",whole_footprint)
current_selection_footprint_wkt(default_footprint)

default_df <- init_whole_default_df  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
filtered_default_df(default_df)

# flog.info("nrow(df_sf) %s: ",nrow(df_sf))
rm(df_sf)

#---------------------------------------------------------------------------------------
source(here::here('modules/map_leaflet.R'))

# Source external R scripts for additional functionalities
# source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i2_SpeciesByGear.R")
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
flog.info("Modules loaded")




flog.info("########################## END GLOBAL")
flog.info("########################## START UI")
source(here::here("ui.R"))
flog.info("########################## START SERVER")
source(here::here("server.R"))


