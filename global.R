rm(list = ls()) 
require(here)
source(here::here('install.R'))
# Log the successful loading of libraries
flog.info("All libraries loaded successfully.")

# Initialize reactive values and default WKT for mapping
main_wkt <- reactiveVal()
switch_unit <- reactiveVal(TRUE)
flog.info("Reactive values initialized successfully.")
initial_data <- reactiveVal()

mode="gpkg"
mode="postgres"
mode="RDS"
mode="parquet"

flog.info("Loading data ")
load_data <- function(mode="parquet") {
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  
  if(mode=="gpkg"){
    flog.info("Loading main data from %s file",mode)
    gpkg_file <- "~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/Global_Tuna_Atlas.gpkg"
    # st_write(loaded_data,gpkg_file,layer = "public.shinycatch",delete_dsn = TRUE)
    con <- dbConnect(RSQLite::SQLite(), dbname = gpkg_file)
    # result <- dbSendQuery(con, "ALTER TABLElihinycatch RENAME to public.shinycatch;")
    
    res <- dbSendQuery(con, "select load_extension('/usr/lib/x86_64-linux-gnu/mod_spatialite.so');")
    res <-st_read(con,query="select sqlite_version(), spatialite_version();")
    dbListTables(con)
  }else if(mode=="RDS"){
    flog.info("Loading main data from %s file",mode)
    # try(loaded_data <- readRDS("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/shinycatch.rds"))
    # try(loaded_data <- readRDS(here::here("shinycatch.RDS")))
    loaded_data <- readRDS(here::here("shinycatch.RDS"))
    class(loaded_data)
    class(loaded_data$geom)
    # change geometry type to "POLYGON" and turn geom colum into eWKT to save and read it by using parquet or feather data format
    transform_df_sf <- st_as_sf(loaded_data) %>% st_cast("POLYGON") %>% as.data.frame() %>% dplyr::mutate(geom=st_as_text(st_sfc(geom),EWKT = TRUE))
    class(transform_df_sf)
    colnames(transform_df_sf)
    class(transform_df_sf$geom)
    saveRDS(transform_df_sf, "shinycatch.RDS") 
    # saveRDS(loaded_data, "shinycatch.RDS")
    
    #save and read the data frame by using parquet and feather data formats
    feather::write_feather(transform_df_sf,"gta.feather")
    df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom", crs = 4326)
    
    arrow::write_parquet(transform_df_sf, "gta.parquet")
    reloaded_data = arrow::read_parquet("gta.parquet") %>% st_as_sf(wkt="geom", crs = 4326)
    # simple_df <- df_sf %>% as.data.frame
    # saveRDS(simple_df, "shinycatch.RDS")
  }else if(mode=="feather"){
    # feather::write_feather(loaded_data,"gta.feather")
    # rm(df_feather)
    # df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom", crs = 4326)
    # class(df_feather)
    # tt <- df_parquet %>% filter(!is.na(geom)) %>% st_as_sf(wkt="geom", crs = 4326)
    # class(df_parquet)
  }else if(mode=="parquet"){
    if(!file.exists("gta.parquet")){
      loaded_data <- load_data(mode="postgres")
      arrow::write_parquet(loaded_data, "gta.parquet")
    }
    loaded_data <- arrow::read_parquet("gta.parquet") 
    # tt <- df_parquet %>% filter(!is.na(geom)) %>% st_as_sf(wkt="geom", crs = 4326)
    # class(df_parquet)
  }else if(mode=="postgres"){
    # Database connection setup
    flog.info("Loading main data from %s database",mode)
    try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))
    flog.info("Loading main data file")
    db_host <- Sys.getenv("DB_HOST")
    db_port <- as.integer(Sys.getenv("DB_PORT"))
    db_name <- Sys.getenv("DB_NAME")
    db_user <- Sys.getenv("DB_USER_READONLY")
    db_password <- Sys.getenv("DB_PASSWORD")
    
    con <- dbConnect(RPostgreSQL::PostgreSQL(), host=db_host, port=db_port, dbname=db_name, user=db_user, password=db_password)
    # loaded_data <- st_read(con, query="SELECT * FROM public.shinycatch ;")
    query <- paste0("SELECT ogc_fid,dataset,year,month,source_authority,species,fishing_fleet,gear_type, measurement_value,measurement_unit,count,gridtype,fishing_mode, codesource_area,ST_AsText(ST_GeometryN(geom, 1)) AS geom 
                              FROM public.shinycatch;")
    loaded_data <- dbGetQuery(con,query )
    qs::qsave(loaded_data,"newshinypublic.qs")
    #save and read the data frame by using parquet and feather data formats
    loaded_data
  }else{
    flog.info("No data loaded !!")
  }
  return(loaded_data)
}
df_sf <- load_data(mode="parquet")
flog.info("Data succesfully loaded")


# flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
df_distinct_geom <- df_sf %>% as.data.frame() %>% dplyr::group_by(codesource_area,gridtype,geom) %>% filter(!is.na(gridtype))  %>% filter(!is.na(geom)) %>% 
  dplyr::summarise(ogc_fid = first(ogc_fid)) %>% ungroup() %>% st_as_sf(wkt="geom",crs=4326) 
class(df_distinct_geom)

default_wkt <- st_as_text(st_as_sfc(st_bbox(df_distinct_geom)))
# main_wkt(default_wkt)
new_wkt <- default_wkt


if(!exists("df_sf")){
  flog.info("Try  if a default file for filters is pre-calculated")
  # df_sf <- readRDS(here::here("data/shinycatch.rds"))
}
#

flog.info("Check what are the existing / possible combinations between dimension values (to adapt the values of filters dynamically)")
if(!file_exists("filters_combinations.parquet")){
  filters_combinations <- df_sf  %>% st_drop_geometry() %>%  dplyr::group_by(species, year, gear_type, fishing_fleet) %>% dplyr::summarise(count = n())
  flog.info("Filter combinations retrieved and stored.")
  arrow::write_parquet(filters_combinations, "filters_combinations.parquet")
}else{
  flog.info("Try  if a default file for filters is pre-calculated")
  filters_combinations <- arrow::read_parquet("filters_combinations.parquet") 
}


flog.info("Set values of filters : list distinct values in the main dataset for each dimension")
target_wkt <- "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))"
# target_wkt <- "POLYGON ((-10.195313 49.15297,33.222656 49.15297,33.222656 35.46067,-10.195313 35.46067,-10.195313 49.15297))"
main_wkt(target_wkt)
# flog.info("Spatial filter :main WKT : %s", main_wkt())

# target_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
target_dataset <- unique(df_sf$dataset)
# target_species <-  dbGetQuery(con,"SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_species <-  unique(filters_combinations$species)
# target_year <-  dbGetQuery(con,"SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_year <-  unique(filters_combinations$year)
# target_gear <-  dbGetQuery(con,"SELECT DISTINCT(gear_type) as gear FROM public.shinycatch ORDER BY gear_type;")
target_gear_type <-  unique(filters_combinations$gear_type)
# target_ocean <- st_read(pool, "SELECT DISTINCT(ocean) as ocean FROM public.shinycatch ORDER BY ocean;")
# target_unit <-  dbGetQuery(con,"SELECT DISTINCT(measurement_unit) AS unit FROM public.shinycatch ORDER BY unit;")
target_measurement_unit <-  unique(df_sf$measurement_unit)
target_gridtype <-   unique(df_sf$gridtype) 
# df_sf %>% group_by(geom_id)
target_flag <-  unique(filters_combinations$fishing_fleet)
# target_fishing_mode <- dbGetQuery(con, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

flog.info("Set filters values to be seflected by default")

# default_species <- c('YFT','SKJ','BET','SBF','ALB')
default_species <- c('YFT')
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-2)
default_year <- c(seq(1950:2021)+1949)
# default_year <- c(seq((max(target_year)-10):max(target_year))+max(target_year)-11)
# default_gear <- c('01.1','01.2')
default_gear_type <- unique(target_gear_type)
# default_dataset <- c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
default_dataset <- unique(target_dataset)
default_unit <- c('t','no')
# default_unit <- unique(target_unit$unit)
default_gridtype <- c("1deg_x_1deg")
# default_area <- unique(target_area$gridtype)
default_fishing_fleet <- target_flag
flog.info("Default filter values set.")

# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

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

initial_data(df_sf)
rm(df_sf)

flog.info("########################## End GLOBAL")
flog.info("########################## START UI")
source(here::here("ui.R"))
flog.info("########################## START GLOBAL")
source(here::here("server.R"))