load_data <- function(mode="DOI"){
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  if(mode=="DOI"){
    setwd("data")
    source(here::here("R/download_and_process_zenodo_data.R"))
    gc()
    loaded_data <- download_and_process_zenodo_data()
  } else if(mode=="gpkg"){
    flog.info("Loading main data from %s file",mode)
    gpkg_file <- "~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/Global_Tuna_Atlas.gpkg"
    # st_write(loaded_data,gpkg_file,layer = "public.shinycatch",delete_dsn = TRUE)
    con <- dbConnect(RSQLite::SQLite(), dbname = gpkg_file)
    # result <- dbSendQuery(con, "ALTER TABLElihinycatch RENAME to public.shinycatch;")
    
    res <- dbSendQuery(con, "select load_extension('/usr/lib/x86_64-linux-gnu/mod_spatialite.so');")
    res <-st_read(con,query="select sqlite_version(), spatialite_version();")
    dbListTables(con)
  } else if(mode=="QS"){
    flog.info("Loading main data from %s file",mode)
    # try(loaded_data <- qs::qread("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps/shinycatch.qs"))
    # try(loaded_data <- qs::qread(here::here("shinycatch.qs")))
    loaded_data <- qs::qread(here::here("shinycatch.qs"))
    # change geometry type to "POLYGON" and turn geom colum into eWKT to save and read it by using parquet or feather data format
    transform_df_sf <- st_as_sf(loaded_data) %>% st_cast("POLYGON") %>% as.data.frame() %>% dplyr::mutate(geom=st_as_text(st_sfc(geom),EWKT = TRUE))
    qs::qsave(transform_df_sf, "shinycatch.qs")
    
    #save and read the data frame by using parquet and feather data formats
    feather::write_feather(transform_df_sf,"gta.feather")
    # df_feather <- feather::read_feather("gta.feather") %>% st_as_sf(wkt="geom", crs = 4326)
    
    arrow::write_parquet(transform_df_sf, "gta.parquet")
    # reloaded_data = arrow::read_parquet("gta.parquet") %>% st_as_sf(wkt="geom", crs = 4326)
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
  df_distinct_geom_light <- qs::qread(here::here("data/df_distinct_geom_light.csv"))
  loaded_data <- loaded_data %>%
    dplyr::left_join((df_distinct_geom_light), by=c('codesource_area'))
  rm(df_distinct_geom_light)
  gc()
  flog.info("Loading / storing aggregated data with dimensions only needed by filters")
  whole_group_df <- load_grouped_data(df_sf=loaded_data, filename = "whole_group_df.parquet")
  #whole group_df cannot be used as it now excludes geom_wkt which is not in the groupping
  gc()
  
  flog.info("Load non spatial filters combinations  & List all values for non spatial filters")
  list_filters <- load_filters_combinations(df_sf=loaded_data, filename = "filters_combinations.parquet")
  filters_combinations <- list_filters$filters_combinations
  list_values_dimensions <- list_filters$list_values_dimensions
  rm(list_filters)
  
  flog.info("Load spatial filter data")
  df_distinct_geom <-  load_spatial_data(df_sf=loaded_data, mode=mode)
  all_polygons <- df_distinct_geom %>% st_combine() # %>% st_simplify() 
  all_polygons_footprint <- all_polygons %>% st_as_text()
  
  
  # possible_values / selected_values / current_values
  flog.info("Set values of filters : list distinct values in the main dataset for each dimension")
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
  target_wkt <- "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))"
  current_selection <- st_sf(st_as_sfc(target_wkt, crs = 4326))
  # current_areas ?
  within_areas <- process_list_areas(df_distinct_geom, wkt=target_wkt, list_gridtype=default_gridtype) 
  
  list_default_filters = list("dataset"=default_dataset,
                              "species"=default_species,
                              "year"=default_year,
                              "gear_type"=default_gear_type,
                              "unit"=default_unit,
                              "source_authority"=default_source_authority,
                              "gridtype"=default_gridtype,
                              "fishing_fleet"=default_fishing_fleet,
                              "target_wkt" = "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))",
                              "within_areas" = within_areas
                              # target_wkt <- "POLYGON ((-10.195313 49.15297,33.222656 49.15297,33.222656 35.46067,-10.195313 35.46067,-10.195313 49.15297))"
                              )
  flog.info("Keeping tracks of current selected values for filters to faster data loading.")

  # Logging the successful execution of the script up to this point
  flog.info("Initial setup and data retrieval completed successfully.")
  
  flog.info("Load default dataset!!")
  # add parameter = list of values ?
  init_whole_default_df <- load_default_dataset(df=loaded_data,
                                                filename="default_df.parquet",
                                                list_filters=list_default_filters)
  # whole_filtered_df(init_whole_default_df)
  
  # add function to calculate the footprint of a df ?
  default_footprint <- init_whole_default_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
    st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine() %>% st_as_text() # %>% st_simplify()
  # flog.info("Current footprint for filters is %s: ",whole_footprint)
  # current_selection_footprint_wkt(default_footprint)
  
  default_df <- init_whole_default_df  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
  # filtered_default_df(default_df)
  flog.info("########################## DEFAULT FILTERED DATA LOADED")
  
  
  
  flog.info("Returns a list of dataframes")
  list_df = list(
    "whole_group_df" = whole_group_df,
    "filters_combinations" = filters_combinations,
    "list_values_dimensions" = list_values_dimensions,
    "df_distinct_geom" = df_distinct_geom,
    "all_polygons" = all_polygons,
    "all_polygons_footprint" = all_polygons_footprint,
    "list_default_filters"=list_default_filters,
    "init_whole_default_df"=init_whole_default_df,
    "default_footprint"=default_footprint,
    "default_df"=default_df
  )
  rm(loaded_data)
  gc()
  rm(whole_group_df)
  gc()
  return(list_df)
}