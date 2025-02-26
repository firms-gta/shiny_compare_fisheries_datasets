download_and_process_zenodo_data <- function() {
  sf::sf_use_s2(FALSE)
  lapply(c("here", "readr", "arrow", "qs", "sf", "dplyr", 
           "zen4R", "futile.logger", "lubridate", "stringr"), 
         function(pkg) {
           if (!requireNamespace(pkg, quietly = TRUE)) {
             install.packages(pkg)
           }
           library(pkg, character.only = TRUE)
         })
  
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
      readr::write_csv(x = DOIs,file = here::here("data/DOIs_enriched.csv")) 
      filepath <- here::here("data", DOIs$Filename[i])
      filename <- gsub("\\..*", "",DOIs$Filename[i])
      file_mime=gsub(".*\\.", "",DOIs$Filename[i])
      newname <- here::here("data", paste0(filename,"_",record_id,".",file_mime))
      DATA_DIR <- here::here("data")  # Utilisation exclusive de here
      
      if (!dir.exists(DATA_DIR)) {
        dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
      }
      
      if (file_mime == "zip") {
        flog.info("######################### CSV => ZIP DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)
        
        zip_original <- file.path(DATA_DIR, DOIs$Filename[i])  # Nom fourni par Zenodo
        zip_renamed <- file.path(DATA_DIR, paste0(tools::file_path_sans_ext(DOIs$Filename[i]), "_", record_id, ".zip"))  # Format attendu
        target_csv <- file.path(DATA_DIR, paste0(tools::file_path_sans_ext(DOIs$Filename[i]), "_", record_id, ".csv"))  # 🔹 Initialisation ici !
        
        # Vérifier les fichiers existants
        files_in_data <- list.files(DATA_DIR, full.names = TRUE)
        flog.info("Contenu du dossier DATA_DIR : %s", paste(files_in_data, collapse = ", "))
        
        if (file.exists(zip_renamed)) {
          flog.info("ZIP file already exists with expected name: %s", zip_renamed)
        } else if (file.exists(zip_original)) {
          flog.info("ZIP file found with original name: %s, renaming to %s", zip_original, zip_renamed)
          file.rename(zip_original, zip_renamed)
        } else {
          flog.info("Downloading dataset: %s Zenodo record", record_id)
          download_data(doi = DOIs$DOI[i], filename = DOIs$Filename[i], data_dir = DATA_DIR)
          
          if (file.exists(zip_original)) {
            flog.info("Download successful, renaming file: %s -> %s", zip_original, zip_renamed)
            file.rename(zip_original, zip_renamed)
          } else if (!file.exists(zip_renamed)) {
            flog.error("Download failed, ZIP file not found after download!")
            stop("ZIP file not found after download.")
          }
        }
        
        # 🔹 Vérifier l'existence du CSV après extraction
        if (file.exists(target_csv)) {
          flog.info("Target CSV already exists: %s", target_csv)
        } else {
          flog.info("Extracting ZIP file: %s", zip_renamed)
          unzip(zipfile = zip_renamed, exdir = DATA_DIR, overwrite = TRUE)
          
          extracted_files <- list.files(DATA_DIR, full.names = TRUE)
          flog.info("Extracted files: %s", paste(extracted_files, collapse = ", "))
          
          extracted_csv <- file.path(DATA_DIR, paste0(tools::file_path_sans_ext(DOIs$Filename[i]), ".csv"))
          if (file.exists(extracted_csv)) {
            flog.info("Renaming extracted CSV from %s to %s", extracted_csv, target_csv)
            file.rename(extracted_csv, target_csv)
          } else {
            flog.warn("Extraction failed, file %s does not exist!", extracted_csv)
          }
        }
      } else if (!file.exists(newname) && file_mime == "csv") {
        flog.info("######################### CSV FILE DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)
        
        download_data(doi = DOIs$DOI[i], filename = gsub(" ","%20", DOIs$Filename[i]), data_dir = DATA_DIR)
        
        from_path <- file.path(DATA_DIR, DOIs$Filename[i])
        to_path <- newname
        
        if (file.exists(from_path)) {
          flog.info("Copying file from %s to %s", from_path, to_path)
          file.copy(from = from_path, to = to_path, overwrite = TRUE)
          file.remove(from_path)
        } else {
          flog.warn("File %s does not exist!", from_path)
        }
        
      } else if (!file.exists(newname) && file_mime == "qs") {
        flog.info("######################### QS FILE DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)
        
        download_data(doi = DOIs$DOI[i], filename = gsub(" ","%20", DOIs$Filename[i]), data_dir = DATA_DIR)
        
        from_path <- file.path(DATA_DIR, DOIs$Filename[i])
        to_path <- newname
        
        if (file.exists(from_path)) {
          flog.info("Copying QS file from %s to %s", from_path, to_path)
          file.copy(from = from_path, to = to_path, overwrite = TRUE)
          file.remove(from_path)
        } else {
          flog.warn("File %s does not exist!", from_path)
        }
        
        flog.info("Store distinct geometries in the dedicated sf object 'df_distinct_geom' to perform faster spatial analysis")
        
      }
      
      flog.info("Dataset %s downloaded successfully from Zenodo or retrieved", newname)
      # Correction pour éviter de lire un ZIP comme un CSV
      this_df <- switch(file_mime,
                        "csv" = read.csv(newname),
                        "zip" = read.csv(target_csv),  # On lit le CSV extrait et renommé
                        "qs" = qread(newname) %>% dplyr::mutate(gear_type = gsub("0","",gear_type)) %>% dplyr::as_tibble()
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
    arrow::write_parquet(loaded_data, here::here("data/gta_dois.parquet"))
    rm(loaded_data)
    gc()
    rm(list = ls())
    gc()
  }
    if(!file.exists(here::here("data/gta_geom.qs"))){
      df_distinct_geom <- qread(here::here("data/global_catch_tunaatlasird_level2_14184244.qs")) %>%
        dplyr::select(geographic_identifier, GRIDTYPE) %>% 
        dplyr::mutate(ogc_fid = 1) %>% 
        dplyr::rename(codesource_area=geographic_identifier,gridtype=GRIDTYPE,geom=geom_wkt) %>%
        mutate(ogc_fid=row_number(codesource_area)) %>% 
        dplyr::group_by(codesource_area,gridtype,geom) %>% dplyr::summarise(count = sum(ogc_fid)) %>% ungroup() %>%  st_set_crs(4326)
      #%>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom),EWKT = TRUE)) %>% dplyr::as_tibble() # st_as_sf(wkt="geom_wkt", crs=4326)
      qs::qsave(df_distinct_geom, here::here("data/gta_geom.qs"))   
    }
  source(here::here("R/hotfix.R"))
  #read all DOIs data from parquet file
  loaded_data <- arrow::read_parquet(here::here("data/gta_dois.parquet"))
  
  return(loaded_data)
}
