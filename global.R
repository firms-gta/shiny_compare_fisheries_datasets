rm(list = ls())
dir <- getwd()
require(parallel)
require(here)
require(zen4R)
require(futile.logger)
source(here::here('install.R'))
flog.info("Loading libraries")
flog.info("All libraries loaded successfully.")

DOI <- readr::read_csv("data/DOI.csv") %>% dplyr::mutate(identifier="",title="")

options(timeout = 60000) # Global timeout for downloads
zenodo <- ZenodoManager$new()

# Function to verify file size (if available in DOI)
verify_file <- function(filepath, expected_size) {
  file.info(filepath)$size == expected_size
}

futile.logger::flog.info("Load zenodo download function")
extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  dir <- getwd()
  if (!dir.exists(data_dir)) dir.create(data_dir)
  setwd(data_dir)
  
  success <- FALSE
  attempts <- 3
  attempt <- 1
  record_id <- gsub(".*\\.", "",doi)
  
  while (!success && attempt <= attempts) {
    tryCatch({
      # export DCMI metadata from Zenodo ??
      zen4R::export_zenodo(doi = doi, filename = paste0("metadata_",record_id), format = "DublinCore")
      # Download the specific file
      # zen4R::download_zenodo(doi = resource, files = file, path = dirname(path), sandbox = if(!is.null(software)) software$sandbox else FALSE)
      zen4R::download_zenodo(doi = doi, files = filename, parallel_handler = parLapply, cl = makeCluster(12))
      
      # Check if the file was downloaded
      if (file.exists(filename)) {
        success <- TRUE
        message(sprintf("File '%s' downloaded successfully", filename))
      } else {
        message(sprintf("File '%s' was not downloaded completely, retrying...", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed for file '%s': %s", attempt, filename, e$message))
    })
    attempt <- attempt + 1
  }
  setwd(dir)
}

# Initialize reactive values and default WKT for mapping
flog.info("Initialize reactive values")
main_wkt <- reactiveVal()
switch_unit <- reactiveVal(TRUE)
initial_data <- reactiveVal()
flog.info("Reactive values initialized successfully.")

mode="DOI"
mode="gpkg"
mode="postgres"
mode="RDS"
mode="parquet"
mode="DOI"
# mode="postgres"

flog.info("Loading data ")
load_data <- function(mode="DOI"){
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  if(mode=="DOI"){
    setwd("data")
    if(!file.exists("gta_dois.parquet")){
      # Use the function with lapply for each DOI
      df_dois <-lapply(1:nrow(DOI), function(i) {
        this_doi <- DOI$DOI[i]
        record_id <- gsub(".*\\.", "",DOI$DOI[i])
        this_rec <- zenodo$getRecordById(record_id)
        # this_rec <- zenodo$getRecordByConceptDOI(this_doi)
        # this_rec <- zenodo$getRecordById("10037645")
        DOI$identifier[i] <- gsub("urn:","",this_rec$metadata$related_identifiers[[1]]$identifier)
        DOI$title[i] <- gsub("urn:","",this_rec$metadata$title)
        write_csv(x = DOI,file = "DOIs_enriched.csv")
        filepath <- paste0("data/", DOI$Filename[i])
        filename <- gsub("\\..*", "",DOI$Filename[i])
        file_mime=gsub(".*\\.", "",DOI$Filename[i])
        newname <- paste0(filename,"_",record_id,".",file_mime)
        if (!file.exists(newname) && file_mime =="zip") {
          flog.info("######################### CSV => ZIP DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = DOI$DOI[i], filename=DOI$Filename[i],data_dir = ".")
          unzip(zipfile = DOI$Filename[i],files = c(paste0(filename,".csv")), exdir=".",overwrite = TRUE)
          file.rename(from = paste0(filename,".csv"),to = newname)
          } else if (!file.exists(newname) && file_mime =="csv") {
            flog.info("######################### CSV FILE DONT EXIST")
            flog.info("Loading dataset: %s Zenodo record", record_id)
            extract_zenodo_metadata(doi = DOI$DOI[i], filename=gsub(" ","%20", DOI$Filename[i]),data_dir = ".")
            file.rename(from = DOI$Filename[i],to = newname)
          }else if (!file.exists(newname) && file_mime =="qs") {
            flog.info("######################### QS FILE DONT EXIST")
            flog.info("Loading dataset: %s Zenodo record", record_id)
            extract_zenodo_metadata(doi = DOI$DOI[i], filename=gsub(" ","%20", DOI$Filename[i]),data_dir = ".")
            file.rename(from = DOI$Filename[i],to = newname)
            flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
            if(!file.exists("gta_geom.RDS")){
              df_distinct_geom <- qread(newname) %>%
                dplyr::select(geographic_identifier, GRIDTYPE) %>% 
                dplyr::mutate(ogc_fid = 1) %>% 
                dplyr::rename(codesource_area=geographic_identifier,gridtype=GRIDTYPE,geom=geom_wkt) %>%
                mutate(ogc_fid=row_number(codesource_area)) %>% 
                dplyr::group_by(codesource_area,gridtype,geom) %>% dplyr::summarise(count = sum(ogc_fid)) %>% ungroup() %>%  st_set_crs(4326)
              #%>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom),EWKT = TRUE)) %>% dplyr::as_tibble() # st_as_sf(wkt="geom_wkt", crs=4326)

                            saveRDS(df_distinct_geom, "gta_geom.RDS")   
            }
          }
        flog.info("Dataset  %s downloaded successfully from Zenodo.", newname)
        
        this_df <- switch (file_mime,
                           "csv" =  read.csv(newname),
                           "zip" =  read.csv(newname),
                           "qs" =  qread(newname) %>% dplyr::as_data_frame()
        )
        
        # print(colnames(this_df))
        
        if(any(grepl("geographic_identifier",colnames(this_df)))){
          flog.info("Renaming geographic_identifier column")
          this_df <- this_df %>% 
            dplyr::rename(codesource_area=geographic_identifier)
        }
        
        if(any(grepl("flag",colnames(this_df)))){
          flog.info("Renaming Flag column")
          this_df <- this_df %>% 
            dplyr::rename(fishing_fleet=flag,gear_type=gear,fishing_mode=schooltype_label,measurement_unit=catchunit,measurement_value=value)
        }
        if(any(grepl("fishingfleet",colnames(this_df)))){
          flog.info("Renaming fishingfleet / gear / schooltype / unit / value columns")
          this_df <- this_df %>% 
            dplyr::rename(fishing_fleet=fishingfleet,gear_type=gear,fishing_mode=schooltype,measurement_unit=unit,measurement_value=value)
          }          
        this_df <- this_df %>% 
          dplyr::select(c("source_authority","fishing_fleet","time_start","time_end","codesource_area","gear_type","species","fishing_mode","measurement_unit","measurement_value"))  %>%
          mutate(dataset=gsub(file_mime,"",filename), year=year(time_start))  %>%
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Tons', 't')) %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Number of fish', 'no'))  %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='NO', 'no'))  %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))
        })
      loaded_data <- do.call(rbind, df_dois)
      
      flog.info("Add spatial geometries for both nominal and gridded catches")
      
      df_distinct_geom_spatial <- readRDS("gta_geom.RDS") %>% dplyr::select(-c(count)) 
      
      df_distinct_geom_nominal <- sf::read_sf("cl_nc_areas_simplfied.gpkg") %>% 
        dplyr::rename('codesource_area'= code)   %>% 
        dplyr::mutate(geom=st_buffer(st_centroid(st_boundary(geom)),dist=1000000),'gridtype'="nominal")  %>% 
        # dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom)),EWKT = TRUE) %>% 
        dplyr::select(codesource_area,gridtype)
      
      df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
        dplyr::mutate('ogc_fid'= row_number(codesource_area)) 

      df_distinct_geom_light <- df_distinct_geom %>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom))) %>% 
        st_drop_geometry()  %>% dplyr::as_data_frame()
        
      flog.info("Left join with spatial geometries for both nominal and gridded catches")
      loaded_data <- loaded_data %>% 
         dplyr::left_join((df_distinct_geom_light), by=c('codesource_area'))

      flog.info("Write all binded dataframes into a parquet file")
      arrow::write_parquet(loaded_data, "gta_dois.parquet")
      
    }
    #read all DOIs data from parquet file
    loaded_data <- arrow::read_parquet("gta_dois.parquet")
    }else if(mode=="gpkg"){
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
    # change geometry type to "POLYGON" and turn geom colum into eWKT to save and read it by using parquet or feather data format
    transform_df_sf <- st_as_sf(loaded_data) %>% st_cast("POLYGON") %>% as.data.frame() %>% dplyr::mutate(geom=st_as_text(st_sfc(geom),EWKT = TRUE))
    saveRDS(transform_df_sf, "shinycatch.RDS")

    #save and read the data frame by using parquet and feather data formats
    feather::write_feather(transform_df_sf,"gta.feather")
    df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom", crs = 4326)

    arrow::write_parquet(transform_df_sf, "gta.parquet")
    reloaded_data = arrow::read_parquet("gta.parquet") %>% st_as_sf(wkt="geom", crs = 4326)
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
    query <- paste0("SELECT ogc_fid,dataset,year,month,species,fishing_fleet,gear_type, measurement_value,measurement_unit,count,gridtype,fishing_mode, codesource_area, geom_id,ST_AsText(ST_GeometryN(geom, 1)) AS geom
                              FROM public.shinycatch ;")
    loaded_data <- dbGetQuery(con,query )
    qs::qsave(loaded_data,"newshinypublic.qs")
    #save and read the data frame by using parquet and feather data formats
  }else{
    flog.info("No data loaded !!")
  }
  return(loaded_data)
}
flog.info("Data succesfully loaded")
df_sf <- load_data(mode=mode) 
setwd(dir)

flog.info("Load spatial data")

if(mode!="DOI"){
  flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
  df_distinct_geom <- df_sf %>% as.data.frame() %>% dplyr::group_by(codesource_area,gridtype,geom) %>%
    filter(!is.na(gridtype)) %>% filter(!is.na(geom)) %>%
    dplyr::summarise(ogc_fid = first(ogc_fid)) %>% ungroup() %>% st_as_sf(wkt="geom",crs=4326) 
}else{
  # saveRDS(df_distinct_geom, "gta_geom.RDS")
  df_distinct_geom_spatial <- readRDS("data/gta_geom.RDS") %>% dplyr::select(-c(count)) 
  
  df_distinct_geom_nominal <- sf::read_sf("data/cl_nc_areas_simplfied.gpkg") %>% 
    dplyr::rename('codesource_area'= code)   %>% 
    dplyr::mutate(geom=st_buffer(st_centroid(st_boundary(geom)),dist=1000000),'gridtype'="nominal")  %>% 
    # dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom)),EWKT = TRUE) %>% 
    dplyr::select(codesource_area,gridtype)
  
  df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
    dplyr::mutate('ogc_fid'= row_number(codesource_area)) 
  
    # df_distinct_geom_nominal <- read.csv("data/cl_nc_areas.csv") %>% sf::st_as_sf(wkt="geom_wkt",crs=4326)   %>% 
  #   dplyr::mutate('geom'=st_bbox(),'codesource_area'=geographic_identifier)
  # arrow::write_parquet(df_distinct_geom, "gta_geom.parquet")
  
}


default_wkt <- st_as_text(st_as_sfc(st_bbox(df_distinct_geom)))
# main_wkt(default_wkt)
new_wkt <- default_wkt


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
current_selection <- st_sf(st_as_sfc(target_wkt, crs = 4326))

# main_wkt(default_wkt)

# flog.info("Spatial filter :main WKT : %s", main_wkt())

# target_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
target_dataset <- unique(df_sf$dataset) #  %>% arrange(desc(dataset))
# target_species <-  dbGetQuery(con,"SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_species <- unique(filters_combinations$species) # %>% arrange(desc(species))
# target_year <-  dbGetQuery(con,"SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_year <-  unique(filters_combinations$year)  # %>% arrange(desc(year))
# target_gear <-  dbGetQuery(con,"SELECT DISTINCT(gear_type) as gear FROM public.shinycatch ORDER BY gear_type;")
target_gear_type <-  unique(filters_combinations$gear_type)# %>% arrange(desc(gear_type))
target_measurement_unit <- unique(df_sf$measurement_unit) # %>% arrange(desc(measurement_unit))
target_source_authority <- unique(df_sf$source_authority)
target_gridtype <- unique(df_distinct_geom$gridtype) 
# df_sf %>% group_by(geom_id)
target_flag <-  unique(filters_combinations$fishing_fleet)
# target_fishing_mode <- dbGetQuery(con, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

flog.info("Set filters values to be seflected by default")

# default_species <- c('YFT','SKJ','BET','SBF','ALB')
default_species <- c('YFT')
# default_species <- target_species
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-2)
# default_year <- c(seq(1950:2021)+1949)
# default_year <- c(seq((max(target_year)-10):max(target_year))+max(target_year)-11)
# default_gear <- c('01.1','01.2')
default_gear_type <- target_gear_type
# default_dataset <- c('global_catch_ird_level2','global_catch_5deg_1m_firms_level1')
default_dataset <- target_dataset
# default_unit <- c('t','no')
default_unit <- target_measurement_unit
default_source_authority <- unique(target_source_authority)
# default_gridtype <- c("1deg_x_1deg")
default_gridtype <- target_gridtype
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
# rm(df_sf)

flog.info("########################## End GLOBAL")
flog.info("########################## START UI")
source(here::here("ui.R"))
flog.info("########################## START GLOBAL")
source(here::here("server.R"))
