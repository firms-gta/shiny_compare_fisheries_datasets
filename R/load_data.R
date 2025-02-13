load_data <- function(mode="DOI"){
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  if(mode=="DOI"){
    setwd("data")
    source(here::here("R/download_and_process_zenodo_data.R"))
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
  
  flog.info("Loading / storing aggregated data with dimensions only needed by filters")
  whole_group_df <- load_grouped_data(df_sf=loaded_data, filename = "whole_group_df.parquet")
  
  whole_group_df <- whole_group_df %>%
    dplyr::left_join((df_distinct_geom_light), by=c('codesource_area'))
  
  flog.info("Load non spatial filters combinations  & List all values for non spatial filters")
  list_filters <- load_filters_combinations(df_sf=whole_group_df, filename = "filters_combinations.parquet")
  filters_combinations <- list_filters$filters_combinations
  list_values_dimensions <- list_filters$list_values_dimensions
  rm(list_filters)
  flog.info("Returns a list of dataframes")
  list_df = list(
    "whole_group_df" = whole_group_df,
    "filters_combinations" = filters_combinations,
    "list_values_dimensions" = list_values_dimensions
  )
  rm(loaded_data)
  gc()
  source(here::here("R/clean_and_convert_csv.R"))
  clean_and_convert_csv(here::here("data"))
  return(list_df)
}