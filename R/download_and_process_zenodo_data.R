download_and_process_zenodo_data <- function() {
  sf::sf_use_s2(FALSE)
  lapply(c("here", "readr", "arrow", "qs", "sf", "dplyr", 
           "zen4R", "parallel","futile.logger", "lubridate", "stringr"), 
         function(pkg) {
           if (!requireNamespace(pkg, quietly = TRUE)) {
             install.packages(pkg)
           }
           library(pkg, character.only = TRUE)
         })
  
  if(!file.exists(here::here("data/gta_dois.parquet"))){
    require(zen4R)
    zenodo <- ZenodoManager$new()
    list_DOIs <- here::here("data/DOI.csv")
    DOIs <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
    # Use the function with lapply for each DOI
    df_dois <-lapply(1:nrow(DOIs), function(i) {
      this_doi <- DOIs[i,]
      this_df <- NULL
      record_id <- gsub(".*\\.", "",this_doi$DOI)
      this_doi$filepath <- here::here("data", this_doi$Filename)
      this_doi$filename <- gsub("\\..*", "",this_doi$Filename)
      this_doi$file_mime <-  gsub(".*\\.", "",this_doi$Filename)
      this_doi$code <- paste0(this_doi$filename,"_",record_id)
      this_doi$newname <- here::here("data", paste0(this_doi$code,".",this_doi$file_mime))
      this_rec <- zenodo$getRecordById(record_id)
      flog.info("Harvesting metadata %s:", record_id)
      
      this_doi$record_id <- record_id
      # this_doi$svg_badge <- gsub(pattern = "11410529",replacement = record_id,"https://zenodo.org/badge/DOI/10.5281/zenodo.11410529.svg")
      this_doi$Rmd_badge <- gsub(pattern = "11410529",replacement = record_id,"[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11410529.svg)](https://doi.org/10.5281/zenodo.11410529)")
      this_doi$URL <- paste0("https://doi.org/",this_doi$DOI)
      # this_doi$Citation <- print(RefManageR::GetBibEntryWithDOI(this_doi$URL), .opts = list(style = "text", bib.style = "authoryear"))
      this_doi$bibfile <- this_rec$exportAs(format="BibTeX", filename=here::here(paste0("data/",record_id)), append_format = TRUE)
      lines <- readLines(here::here(paste0("data/",basename(this_doi$bibfile))))
      lines <- gsub(pattern = "@dataset",replacement = "@misc",lines)
      writeLines(lines, here::here(paste0("data/",basename(this_doi$bibfile))))
      this_citation <-   RefManageR::ReadBib(here::here(paste0("data/",basename(this_doi$bibfile))), check = FALSE)
      # BibOptions(check.entries = FALSE, style = "markdown", bib.style = "alphabetic", cite.style = 'alphabetic')
      BibOptions(check.entries = FALSE, style = "markdown", cite.style = "authoryear", bib.style = "numeric")
      # this_citation <-   RefManageR::GetBibEntryWithDOI(this_doi$URL)
      # this_doi$bibentry <- RefManageR::GetBibEntryWithDOI(this_doi$URL)
      # dd= print(RefManageR::GetBibEntryWithDOI(this_doi$URL), .opts = list(style = "text", bib.style = "authoryear"))
      # this_doi$Citation <- print(RefManageR::GetBibEntryWithDOI(this_doi$URL), .opts = list(style = "text", bib.style = "authoryear"))
      this_doi$Citation <- RefManageR::Cite(this_citation)
      # this_doi$Citation <-print(this_citation, .opts = list(style = "text", bib.style = "authoryear"))
      # this_doi$Citation <-   capture.output(this_citation)
      # RefManageR::PrintBibliography(this_citation, .opts = list(bib.style = "apa", sorting = ""))
      
      
      
      # <a href="https://doi.org/10.5281/zenodo.11410529"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.11410529.svg" alt="DOI"></a>
      
      # this_doi$URL <- paste0('<a href="',this_url,'">',this_url,'</a>')
      # this_rec <- zenodo$getRecordByConceptDOI(this_doi)
      if(!is.null(this_rec$metadata$related_identifiers[[1]]$identifier)){
        this_doi$identifier <- gsub("urn:","",this_rec$metadata$related_identifiers[[1]]$identifier)
      }
      if(!is.null(this_rec$metadata$title)){
        this_doi$title <- gsub("urn:","",this_rec$metadata$title)
      }
      
      flog.info("Downloading dataset")
      
      DATA_DIR <- here::here("data")  # Utilisation exclusive de here


      if (!dir.exists(DATA_DIR)) {
        dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
      }
      if (this_doi$file_mime == "zip") {
        flog.info("######################### CSV => ZIP DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)
        zip_path <- file.path(DATA_DIR, this_doi$Filename)
        extracted_csv <- file.path(DATA_DIR, paste0(this_doi$filename, ".csv"))
        target_csv <- file.path(DATA_DIR, paste0(this_doi$filename, "_", record_id, ".csv"))
        # Vérifier si le CSV extrait existe déjà avant de télécharger le ZIP
        if (!file.exists(target_csv)) {
          if (!file.exists(zip_path)) {
            flog.info("Downloading dataset: %s Zenodo record", record_id)
            download_data(doi = DOIs$DOI[i], filename = this_doi$Filename, data_dir = DATA_DIR)
          } else {
            flog.info("ZIP file already exists: %s", zip_path)
          }

          flog.info("Unzipping file: %s", zip_path)
          unzip(zipfile = zip_path, exdir = DATA_DIR, overwrite = TRUE)

          extracted_files <- list.files(DATA_DIR, full.names = TRUE)
          flog.info("Extracted files: %s", paste(extracted_files, collapse = ", "))

          if (file.exists(extracted_csv)) {
            flog.info("Renaming extracted CSV from %s to %s", extracted_csv, target_csv)
            if (file.rename(from = extracted_csv, to = target_csv)) {
              flog.info("File successfully moved to %s", target_csv)
              # file.remove(zip_path)  # Supprimer le ZIP si nécessaire
            } else {
              flog.warn("Failed to move extracted CSV to %s", target_csv)
            }
          } else {
            flog.warn("Extraction failed, file %s does not exist!", extracted_csv)
          }
        } else {
          flog.info("Target CSV already exists: %s. Skipping extraction.", target_csv)
        }

      } else if (!file.exists(this_doi$newname) && this_doi$file_mime == "csv") {
        flog.info("######################### CSV FILE DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)

        download_data(doi = DOIs$DOI[i], filename = gsub(" ","%20", this_doi$Filename), data_dir = DATA_DIR)

        from_path <- file.path(DATA_DIR, this_doi$Filename)
        to_path <- this_doi$newname

        if (file.exists(from_path)) {
          flog.info("Copying file from %s to %s", from_path, to_path)
          file.copy(from = from_path, to = to_path, overwrite = TRUE)
          file.remove(from_path)
        } else {
          flog.warn("File %s does not exist!", from_path)
        }

      } else if (!file.exists(this_doi$newname) && this_doi$file_mime == "qs") {
        flog.info("######################### QS FILE DONT EXIST")
        flog.info("Loading dataset: %s Zenodo record", record_id)

        download_data(doi = DOIs$DOI[i], filename = gsub(" ","%20", this_doi$Filename), data_dir = DATA_DIR)

        from_path <- file.path(DATA_DIR, this_doi$Filename)
        to_path <- this_doi$newname

        if (file.exists(from_path)) {
          flog.info("File does exist, copying QS file from %s to %s", from_path, to_path)
          file.copy(from = from_path, to = to_path, overwrite = TRUE)
          file.remove(from_path)
        } else {
          flog.warn("File %s does not exist!", from_path)
        }

        flog.info("Store distinct geometries in the dedicated sf object 'df_distinct_geom' to perform faster spatial analysis")

      }

      flog.info("Dataset %s downloaded successfully from Zenodo or retrieved", this_doi$newname)
      # Correction pour éviter de lire un ZIP comme un CSV
      this_df <- switch(this_doi$file_mime,
                        "csv" = read.csv(this_doi$newname,colClasses=c('character'),stringsAsFactors = FALSE),
                        "zip" = read.csv(target_csv,colClasses=c('character'),stringsAsFactors = FALSE),  # On lit le CSV extrait et renommé
                        # "qs" = qread(this_doi$newname) %>% dplyr::mutate(gear_type = gsub("0","",gear_type)) %>% dplyr::as_tibble()
                        "qs" = qread(this_doi$newname) %>% dplyr::as_tibble()
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
        mutate(dataset=gsub(paste0(".",this_doi$file_mime),"",this_doi$newname), year=year(time_start))  %>%
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Tons', 't')) %>%
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='Number of fish', 'no'))  %>%
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='NO', 'no'))  %>%
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) %>%
        dplyr::mutate(measurement_value=as.numeric(measurement_value),codesource_area=as.character(codesource_area))

      flog.info("All datasets have been downloaded")
      this_list <-list("metadata"=this_doi,"data"=this_df)
    })
    
    loaded_metadata <- do.call(rbind, lapply(df_dois, function(l) l[[1]]))
    readr::write_csv(x = loaded_metadata,file = here::here("data/DOIs_enriched.csv"))
    list_bib <- lapply(1:nrow(loaded_metadata), function(i) {
       RefManageR::ReadBib(loaded_metadata$bibfile[i])
    })
    biblio <<- do.call("c", list_bib)
    
    loaded_data <- do.call(rbind, lapply(df_dois, function(l) l[[2]]))
    gc()
    arrow::write_parquet(loaded_data, here::here("data/gta_dois.parquet"))
    rm(loaded_data)
    gc()
    rm(list = ls())
    gc()
  }
  
  #read all DOIs data from parquet file
  loaded_data <- arrow::read_parquet(here::here("data/gta_dois.parquet"))
  
  return(loaded_data)
}