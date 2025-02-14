download_and_process_zenodo_data <- function() {
  list_DOIs <- here::here("data/DOI.csv")
  DOIs <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
  if(!file.exists(here::here("data/gta_dois.parquet"))){
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
        if(!file.exists(here::here("data/gta_geom.qs"))){
          df_distinct_geom <- qread(newname) %>%
            dplyr::select(geographic_identifier, GRIDTYPE) %>% 
            dplyr::mutate(ogc_fid = 1) %>% 
            dplyr::rename(codesource_area=geographic_identifier,gridtype=GRIDTYPE,geom=geom_wkt) %>%
            mutate(ogc_fid=row_number(codesource_area)) %>% 
            dplyr::group_by(codesource_area,gridtype,geom) %>% dplyr::summarise(count = sum(ogc_fid)) %>% ungroup() %>%  st_set_crs(4326)
          #%>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom),EWKT = TRUE)) %>% dplyr::as_tibble() # st_as_sf(wkt="geom_wkt", crs=4326)
          qs::qsave(df_distinct_geom, here::here("data/gta_geom.qs"))   
        }
      }
      source("https://raw.githubusercontent.com/firms-gta/tunaatlas_pie_map_shiny/refs/heads/main/download_GTA_data.R")
      flog.info("Dataset  %s downloaded successfully from Zenodo or retrieved", newname)
      
      this_df <- switch (file_mime,
                         "csv" =  read.csv(newname),
                         "zip" =  read.csv(newname),
                         "qs" =  qread(newname) %>% dplyr::mutate(gear_type = gsub("0","",gear_type)) %>% dplyr::as_data_frame()
      )
      
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
    gc()
    flog.info("Add spatial geometries for both nominal and gridded catches")
    
    df_distinct_geom_spatial <- qs::qread(here::here("data/gta_geom.qs")) %>% dplyr::select(-c(count)) 
    
    flog.info("Add spatial geometries 1")
    
    # https://github.com/fdiwg/fdi-codelists/raw/main/global/firms/gta/cl_nc_areas.csv
    df_distinct_geom_nominal <- sf::read_sf("cl_nc_areas_simplfied.gpkg") %>% 
      dplyr::rename('codesource_area'= code)   %>% 
      dplyr::mutate(geom=st_buffer(st_centroid(geom),dist=1),'gridtype'="nominal")  %>% 
      # dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom)),EWKT = TRUE) %>% 
      dplyr::select(codesource_area,gridtype)
    flog.info("Add spatial geometries 2")
    
    df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
      dplyr::mutate('ogc_fid'= row_number(codesource_area)) 
    rm(df_distinct_geom_nominal)
    gc()
    flog.info("Add spatial geometries 3")
    
    df_distinct_geom_light <- df_distinct_geom %>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom))) %>% 
      st_drop_geometry()  %>% dplyr::as_data_frame()
    qs::qsave(df_distinct_geom_light, here::here("data/df_distinct_geom_light.csv"))
    rm(df_distinct_geom)
    gc()
    flog.info("Left join with spatial geometries for both nominal and gridded catches")
    # loaded_data <- loaded_data %>% 
    #   dplyr::left_join((df_distinct_geom_light %>% dplyr::select(-geom_wkt)), by=c('codesource_area'))
    # loaded_data$geom_wkt <- loaded_data$codesource_area #hot fix for now # removed as too big
    gc()
    flog.info("Write all binded dataframes into a parquet file")
    arrow::write_parquet(loaded_data, here::here("data/gta_dois.parquet"))
    rm(loaded_data)
    gc()
    rm(list = ls())
    gc()
  }
  #read all DOIs data from parquet file
  loaded_data <- arrow::read_parquet(here::here("data/gta_dois.parquet"))
  return(loaded_data)}