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
try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))

db_host <- Sys.getenv("DB_HOST")
db_port <- as.integer(Sys.getenv("DB_PORT"))
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER_READONLY")
db_password <- Sys.getenv("DB_PASSWORD")

con <- dbConnect(RPostgreSQL::PostgreSQL(), host=db_host, port=db_port, dbname=db_name, user=db_user, password=db_password)
flog.info("Database connection established.")

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
load_target_data <- function(file_path) {
  target_data <- readRDS(file_path)
  list2env(target_data, .GlobalEnv)
}

# Call the function to load data
load_target_data("data/target.rds")
flog.info("Database queries for filter options completed successfully.")

# Set default values for the UI filters
default_species <- 'YFT'
default_year <- c(seq(min(target_year$year), max(target_year$year)) + min(target_year$year) - 1)
default_gear_type <- unique(target_gear_type$gear_type)
default_dataset <- unique(target_dataset$dataset)
default_unit <- c('t','no')
default_gridtype <- unique(target_gridtype$gridtype)
default_fishing_fleet <- unique(target_flag$fishing_fleet)
flog.info("Default filter values set.")

# Log and retrieve combinations for filters
filters_combinations <- dbGetQuery(con, "SELECT species, year, gear_type, fishing_fleet FROM public.shinycatch GROUP BY species, year, gear_type, fishing_fleet;")
flog.info("Filter combinations retrieved and stored.")

# Logging the successful execution of the script up to this point
flog.info("Initial setup and data retrieval completed successfully.")

