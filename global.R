rm(list = ls()) 
source(here::here('install.R'))
# Log the successful loading of libraries
flog.info("All libraries loaded successfully.")

# Initialize reactive values and default WKT for mapping
# bbox <- 'POLYGON ((-180 -60, 180 -60, 180 70, -180 70, -180 -60))'
# bbox <- 'POLYGON ((10.01953 -28.76766, 10.01953 5.266008, 66.09375 5.266008, 66.09375 -28.76766, 10.01953 -28.76766))'
bbox <- 'POLYGON ((-124.1016 -51.17934, -124.1016 49.15297, 207.0703 49.15297, 207.0703 -51.17934, -124.1016 -51.17934))'
wkt <- reactiveVal(bbox)
switch_unit <- reactiveVal(TRUE)
query_all_datasets <- reactiveVal()
list_areas  <- reactiveVal()
# data_map <- reactiveVal()
flog.info("Reactive values initialized successfully.")
initial_data <- reactiveVal()

mode="gpkg"
mode="postgres"
mode="RDS"
mode="parquet"

flog.info("Loading data ")
load_data <- function() {
  loaded_data <- list()
  flog.info("Loading dataset: %s", mode)
  
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
    
    #save and read the data frame by using parquet and feather data formats
    feather::write_feather(transform_df_sf,"gta.feather")
    df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom")
    
    arrow::write_parquet(transform_df_sf, "gta.parquet")
    reloaded_data = arrow::read_parquet("gta.parquet") %>% st_as_sf(wkt="geom")
    # simple_df <- df_sf %>% as.data.frame
    # saveRDS(simple_df, "shinycatch.RDS")
  }else if(mode=="parquet"){
    # feather::write_feather(loaded_data,"gta.feather")
    # rm(df_feather)
    # df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom")
    # class(df_feather)
    # arrow::write_parquet(loaded_data, "gta.parquet")
    loaded_data <- arrow::read_parquet("gta.parquet") 
    # tt <- df_parquet %>% filter(!is.na(geom)) %>% st_as_sf(wkt="geom")
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
    loaded_data <- dbGetQuery(con, "SELECT ogc_fid,dataset,year,month,species,fishing_fleet,gear_type, measurement_value,measurement_unit,count,gridtype,fishing_mode, codesource_area, geom_id,ST_AsText(ST_GeometryN(geom, 1)) AS geom FROM public.shinycatch;")
    # saveRDS(loaded_data, "shinycatch.RDS")
    #save and read the data frame by using parquet and feather data formats
  }else{
    flog.info("No data loaded !!")
  }
  return(loaded_data)
}
flog.info("Data succesfully loaded")
df_sf <- load_data()
rm(loaded_data)

# flog.info(df_sf)

flog.info("Store the list of distinct area_id in the dataset loaded")
list_area_id <- df_sf %>% as.data.frame() %>% dplyr::group_by(codesource_area) %>% dplyr::summarise(ogc_fid = first(ogc_fid))
df_distinct_geom <- df_sf %>% dplyr::select(ogc_fid,codesource_area,geom) %>% 
  dplyr::right_join(list_area_id,by=c("ogc_fid","codesource_area")) %>% filter(!is.na(geom))  %>% st_as_sf(wkt="geom", crs = 4326) # %>% filter(!st_is_empty(.)) # %>% dplyr::select(codesource_area)

if(!exists("df_sf")){
  flog.info("Try  if a default file for filters is pre-calculated")
  df_sf <- readRDS(here::here("data/shinycatch.rds"))
}
#

flog.info("Check what are the existing / possible combinations between dimension values (to adapt the values of filters dynamically)")
filters_combinations <- df_sf  %>% st_drop_geometry() %>%  dplyr::group_by(species, year, gear_type, fishing_fleet) %>% dplyr::summarise(count = n())
flog.info("Filter combinations retrieved and stored.")

flog.info("Set values of filters : list distinct values in the main dataset for each dimension")
# target_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
target_dataset <- unique(df_sf$dataset)
# target_species <-  dbGetQuery(con,"SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_species <-  unique(df_sf$species)
# target_year <-  dbGetQuery(con,"SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_year <-  unique(df_sf$year)
# target_gear <-  dbGetQuery(con,"SELECT DISTINCT(gear_type) as gear FROM public.shinycatch ORDER BY gear_type;")
target_gear_type <-  unique(df_sf$gear_type)
# target_ocean <- st_read(pool, "SELECT DISTINCT(ocean) as ocean FROM public.shinycatch ORDER BY ocean;")
# target_unit <-  dbGetQuery(con,"SELECT DISTINCT(measurement_unit) AS unit FROM public.shinycatch ORDER BY unit;")
target_measurement_unit <-  unique(df_sf$measurement_unit)
# target_area <-  dbGetQuery(con,"SELECT DISTINCT(ST_Area(geom)) AS area FROM public.shinycatch ORDER BY area DESC;")
# target_area <-    tibble(wkb=unique(df_sf$geom)) %>% st_read()
target_gridtype <-   unique(df_sf$gridtype) 
# df_sf %>% group_by(geom_id)
target_flag <-  unique(df_sf$fishing_fleet)
# target_fishing_mode <- dbGetQuery(con, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

flog.info("Set filters values to be seflected by default")
default_wkt <- st_as_text(st_as_sfc(st_bbox(df_distinct_geom)))
# wkt(default_wkt)
# default_species <- c('YFT','SKJ','BET','SBF','ALB')
default_species <- c('YFT','SKJ')
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
default_year <- c(seq((max(target_year)-10):max(target_year))+max(target_year)-11)
# default_gear <- c('01.1','01.2')
default_gear_type <- unique(target_gear_type)
default_dataset <- c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
# default_dataset <- unique(target_dataset$dataset)
default_unit <- c('t','no')
# default_unit <- unique(target_unit$unit)
default_gridtype <- c("1deg_x_1deg","5deg_x_5deg")
# default_area <- unique(target_area$gridtype)
default_fishing_fleet <- target_flag
flog.info("Default filter values set.")

# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

#---------------------------------------------------------------------------------------
source(here::here('modules/map_leaflet.R'))

# Source external R scripts for additional functionalities
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i1_SpeciesByOcean.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i2_SpeciesByGear.R")
flog.info("External R scripts sourced successfully.")
flog.info("Loading modules.")
load_ui_modules <- function() {
  ui_files <- c("modules/map_leaflet.R")
  lapply(ui_files, function(file) {
    source(here::here(file))
    flog.info(paste("Loaded UI module:", file))
  })
}
load_ui_modules()
flog.info("Modules loaded")


initial_data(df_sf)
rm(df_sf)
source(here::here("ui.R"))
source(here::here("server.R"))