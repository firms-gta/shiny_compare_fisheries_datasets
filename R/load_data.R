load_data <- function(mode="DOI"){
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  if(mode=="DOI"){
    setwd("data")
    if(!file.exists("gta_dois.parquet")){
      require(zen4R)
      zenodo <- ZenodoManager$new()
      # Use the function with lapply for each DOI
      df_dois <-lapply(1:nrow(DOIs), function(i) {
        this_doi <- DOIs$DOI[i]
        record_id <- gsub(".*\\.", "",DOIs$DOI[i])
        this_rec <- zenodo$getRecordById(record_id)
        # this_rec <- zenodo$getRecordByConceptDOI(this_doi)
        # this_rec <- zenodo$getRecordById("10037645")
        DOIs$identifier[i] <- gsub("urn:","",this_rec$metadata$related_identifiers[[1]]$identifier)
        DOIs$title[i] <- gsub("urn:","",this_rec$metadata$title)
        readr::write_csv(x = DOIs,file = "DOIs_enriched.csv") 
        filepath <- paste0("data/", DOIs$Filename[i])
        filename <- gsub("\\..*", "",DOIs$Filename[i])
        file_mime=gsub(".*\\.", "",DOIs$Filename[i])
        newname <- paste0(filename,"_",record_id,".",file_mime)
        if (!file.exists(newname) && file_mime =="zip") {
          flog.info("######################### CSV => ZIP DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = DOIs$DOI[i], filename=DOIs$Filename[i],data_dir = ".")
          unzip(zipfile = DOIs$Filename[i],files = c(paste0(filename,".csv")), exdir=".",overwrite = TRUE)
          file.rename(from = paste0(filename,".csv"),to = newname)
        } else if (!file.exists(newname) && file_mime =="csv") {
          flog.info("######################### CSV FILE DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = DOIs$DOI[i], filename=gsub(" ","%20", DOIs$Filename[i]),data_dir = ".")
          file.rename(from = DOIs$Filename[i],to = newname)
        }else if (!file.exists(newname) && file_mime =="qs") {
          flog.info("######################### QS FILE DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = DOIs$DOI[i], filename=gsub(" ","%20", DOIs$Filename[i]),data_dir = ".")
          file.rename(from = DOIs$Filename[i],to = newname)
          flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
        }
        flog.info("Dataset  %s downloaded successfully from Zenodo.", newname)
        
        this_df <- switch (file_mime,
                           "csv" =  read.csv(newname),
                           "zip" =  read.csv(newname),
                           "qs" =  qread(newname) %>% dplyr::mutate(gear_type = gsub("0","",gear_type)) %>% dplyr::as_data_frame()
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
          mutate(dataset=gsub(paste0(".",file_mime),"",newname), year=year(time_start))  %>%
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Tons', 't')) %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Number of fish', 'no'))  %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='NO', 'no'))  %>% 
          mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))
      })
      loaded_data <- do.call(rbind, df_dois)
      
      flog.info("Add spatial geometries for both nominal and gridded catches")
      df_distinct_geom <-  load_spatial_data(df_sf=loaded_data, mode=mode)
      
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
  
  flog.info("Loading / storing aggregated data with dimensions only needed by filters")
  whole_group_df <- load_grouped_data(df_sf=loaded_data, filename = "whole_group_df.parquet")
  
  flog.info("Load non spatial filters combinations  & List all values for non spatial filters")
  list_filters <- load_filters_combinations(df_sf=whole_group_df, filename = "filters_combinations.parquet")
  filters_combinations <- list_filters$filters_combinations
  list_values_dimensions <- list_filters$list_values_dimensions
  
  flog.info("Load spatial filter data")
  df_distinct_geom <-  load_spatial_data(df_sf=whole_group_df, mode=mode)
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
  list_default_filters = list("dataset"=default_dataset,
                              "species"=default_species,
                              "year"=default_year,
                              "gear_type"=default_gear_type,
                              "unit"=default_unit,
                              "source_authority"=default_source_authority,
                              "gridtype"=default_gridtype,
                              "fishing_fleet"=default_fishing_fleet,
                              "target_wkt" = "POLYGON ((-53.789063 21.616579,98.964844 21.616579,98.964844 -35.746512,-53.789063 -35.746512,-53.789063 21.616579))"
                              # target_wkt <- "POLYGON ((-10.195313 49.15297,33.222656 49.15297,33.222656 35.46067,-10.195313 35.46067,-10.195313 49.15297))"
                              )
  flog.info("Keeping tracks of current selected values for filters to faster data loading.")

  current_selection <- st_sf(st_as_sfc(target_wkt, crs = 4326))
  # current_areas ?
  within_areas <- process_list_areas(df_distinct_geom, wkt=target_wkt, list_gridtype=default_gridtype) 
  
  # Logging the successful execution of the script up to this point
  flog.info("Initial setup and data retrieval completed successfully.")
  
  flog.info("Load default dataset!!")
  # add parameter = list of values ?
  init_whole_default_df <- load_default_dataset(df=whole_group_df,
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
  
  return(list_df)
}