source(here::here('install.R'))
# Log the successful loading of libraries
flog.info("All libraries loaded successfully.")

# Source external R scripts for additional functionalities
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i1_SpeciesByOcean.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i2_SpeciesByGear.R")
flog.info("External R scripts sourced successfully.")

# Initialize reactive values and default WKT for mapping
new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt)
switch_unit <- reactiveVal(TRUE)
query_metadata <- reactiveVal()
query_all_datasets <- reactiveVal()
flog.info("Reactive values initialized successfully.")

# Database connection setup


# Retrieve distinct values from the database for dropdown filters
# target_dataset <- dbGetQuery(con, "SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")
# target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
# target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
# target_flag <- dbGetQuery(con, "SELECT DISTINCT(fishing_fleet) FROM public.shinycatch ORDER BY fishing_fleet;")
# target_gridtype <- dbGetQuery(con, "SELECT DISTINCT(gridtype) FROM public.shinycatch ORDER BY gridtype;")
# target_gear_type <- dbGetQuery(con, "SELECT DISTINCT(gear_type) FROM public.shinycatch ORDER BY gear_type;")
# target_measurement_unit <- dbGetQuery(con, "SELECT DISTINCT(measurement_unit) FROM public.shinycatch ORDER BY measurement_unit;")
# target_fishing_mode <- dbGetQuery(con, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")
#
# saveRDS(list(target_dataset = target_dataset,
#              target_species = target_species,
#              target_year = target_year,
#              target_flag = target_flag,
#              target_gridtype = target_gridtype,
#              target_gear_type = target_gear_type,
#              target_measurement_unit = target_measurement_unit,
#              target_fishing_mode = target_fishing_mode),
#         "data/target.rds")
# load_target_data <- function(file_path) {
#   target_data <- readRDS(file_path)
#   list2env(target_data, .GlobalEnv)
# }
#
# # Call the function to load data
# load_target_data("data/target.rds")
# flog.info("Database queries for filter options completed successfully.")
#
# # Set default values for the UI filters
# default_species <- 'YFT'
# default_year <- c(seq(min(target_year$year), max(target_year$year)) + min(target_year$year) - 1)
# default_gear_type <- unique(target_gear_type$gear_type)
# default_dataset <- unique(target_dataset$dataset)
# default_unit <- c('t','no')
# default_gridtype <- unique(target_gridtype$gridtype)
# default_fishing_fleet <- unique(target_flag$fishing_fleet)
# flog.info("Default filter values set.")

# mode="gpkg"
mode="postgres"
if(mode=="gpkg"){
  gpkg_file <- "~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/Global_Tuna_Atlas.gpkg"
  # df_sf <- readRDS("public_public.shinycatch.RDS")
  # st_write(df_sf,gpkg_file,layer = "public.shinycatch",delete_dsn = TRUE)
  
  con <- dbConnect(RSQLite::SQLite(), dbname = gpkg_file)
  # result <- dbSendQuery(con, "ALTER TABLElihinycatch RENAME to public.shinycatch;")
  
  res <- dbSendQuery(con, "select load_extension('/usr/lib/x86_64-linux-gnu/mod_spatialite.so');")
  res <-st_read(con,query="select sqlite_version(), spatialite_version();")
  dbListTables(con)
}else{
  # source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
  try(dotenv::load_dot_env("~/blue-cloud-dataspace/GlobalFisheriesAtlas/shiny_compare_tunaatlas_datasests/connection_tunaatlas_inv.txt"))
  
  db_host <- Sys.getenv("DB_HOST")
  db_port <- as.integer(Sys.getenv("DB_PORT"))
  db_name <- Sys.getenv("DB_NAME")
  db_user <- Sys.getenv("DB_USER_READONLY")
  db_password <- Sys.getenv("DB_PASSWORD")
  
  con <- dbConnect(RPostgreSQL::PostgreSQL(), host=db_host, port=db_port, dbname=db_name, user=db_user, password=db_password)
  flog.info("Database connection established.")
}

flog.info("Reading big data")

df_sf <- readRDS("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/shinycatch.RDS")

flog.info("Big data read")

# df_sf <- readRDS("~/blue-cloud-dataspace/tunaatlas_pie_map_shiny/tunaatlas_pie_map_shiny/data/datasf.rds")



# target_dataset <- st_read(con, query="SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
target_dataset <- unique(df_sf$dataset)
# target_species <-  st_read(con, query="SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_species <-  unique(df_sf$species)
# target_year <-  st_read(con, query="SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_year <-  unique(df_sf$year)
# target_gear <-  st_read(con, query="SELECT DISTINCT(gear_type) as gear FROM public.shinycatch ORDER BY gear_type;")
target_gear_type <-  unique(df_sf$gear_type)
# target_ocean <- st_read(pool, "SELECT DISTINCT(ocean) as ocean FROM public.shinycatch ORDER BY ocean;")
# target_unit <-  st_read(con, query="SELECT DISTINCT(measurement_unit) AS unit FROM public.shinycatch ORDER BY unit;")
target_measurement_unit <-  unique(df_sf$measurement_unit)
# target_area <-  st_read(con, query="SELECT DISTINCT(ST_Area(geom)) AS area FROM public.shinycatch ORDER BY area DESC;")
# target_area <-    tibble(wkb=unique(df_sf$geom)) %>% st_read()
target_gridtype <-   unique(df_sf$gridtype)
# df_sf %>% group_by(geom_id)
target_flag <-  unique(df_sf$fishing_fleet)

# default_species <- c('YFT','SKJ','BET','SBF','ALB')
default_species <- c('YFT','SKJ')
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
default_year <- c(seq((max(target_year)-10):max(target_year))+max(target_year)-11)
# default_gear <- c('01.1','01.2')
default_gear_type <- unique(target_gear_type)
default_dataset <- c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
# default_dataset <- unique(target_dataset$dataset)
default_unit <-  c('t')
# default_unit <- unique(target_unit$unit)
default_gridtype <- c("1deg_x_1deg","5deg_x_5deg")
# default_area <- unique(target_area$gridtype)
default_fishing_fleet <- target_flag

#check what are existing / possible combinations between dimension values
# filters_combinations <-  st_read(con, query="SELECT species, year, gear_type as gear FROM public.shinycatch GROUP BY species, year,gear_type;")
# df_sf %>% group_by(species, year,gear_type)
# toto


# Log and retrieve combinations for filters
filters_combinations <- dbGetQuery(con, "SELECT species, year, gear_type, fishing_fleet FROM shinycatch GROUP BY species, year, gear_type, fishing_fleet;")
flog.info("Filter combinations retrieved and stored.")

# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

