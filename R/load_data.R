load_data <- function(mode="DOI"){
  loaded_data <- list()
  flog.info("Loading dataset: %s format", mode)
  if(mode=="DOI"){
    setwd("data")
    if(!file.exists("gta_dois.parquet")){
      # Use the function with lapply for each DOI
      df_dois <-lapply(1:nrow(list_DOIs), function(i) {
        this_doi <- list_DOIs$DOI[i]
        record_id <- gsub(".*\\.", "",list_DOIs$DOI[i])
        this_rec <- zenodo$getRecordById(record_id)
        # this_rec <- zenodo$getRecordByConceptDOI(this_doi)
        # this_rec <- zenodo$getRecordById("10037645")
        list_DOIs$identifier[i] <- gsub("urn:","",this_rec$metadata$related_identifiers[[1]]$identifier)
        list_DOIs$title[i] <- gsub("urn:","",this_rec$metadata$title)
        write_csv(x = list_DOIs,file = "DOIs_enriched.csv")
        filepath <- paste0("data/", list_DOIs$Filename[i])
        filename <- gsub("\\..*", "",list_DOIs$Filename[i])
        file_mime=gsub(".*\\.", "",list_DOIs$Filename[i])
        newname <- paste0(filename,"_",record_id,".",file_mime)
        if (!file.exists(newname) && file_mime =="zip") {
          flog.info("######################### CSV => ZIP DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = list_DOIs$DOI[i], filename=list_DOIs$Filename[i],data_dir = ".")
          unzip(zipfile = list_DOIs$Filename[i],files = c(paste0(filename,".csv")), exdir=".",overwrite = TRUE)
          file.rename(from = paste0(filename,".csv"),to = newname)
        } else if (!file.exists(newname) && file_mime =="csv") {
          flog.info("######################### CSV FILE DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = list_DOIs$DOI[i], filename=gsub(" ","%20", list_DOIs$Filename[i]),data_dir = ".")
          file.rename(from = list_DOIs$Filename[i],to = newname)
        }else if (!file.exists(newname) && file_mime =="qs") {
          flog.info("######################### QS FILE DONT EXIST")
          flog.info("Loading dataset: %s Zenodo record", record_id)
          extract_zenodo_metadata(doi = list_DOIs$DOI[i], filename=gsub(" ","%20", list_DOIs$Filename[i]),data_dir = ".")
          file.rename(from = list_DOIs$Filename[i],to = newname)
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
        dplyr::mutate(geom=st_buffer(st_centroid(geom),dist=1),'gridtype'="nominal")  %>% 
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
  
  
  flog.info("Loading / storing aggregated data with dimensions only needed by filters")
  whole_group_df <- load_grouped_data(df_sf=loaded_data, filename = "whole_group_df.parquet")
  
  return(whole_group_df)
}