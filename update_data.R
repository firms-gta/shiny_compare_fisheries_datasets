require(zen4R)
require(readr)
DOI <- read_csv("data/DOI.csv")

options(timeout = 6000) # Global timeout for downloads

library(curl)
library(zen4R)

extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  # Ensure the data directory exists
  dir <- getwd()
  if (!dir.exists(data_dir)) dir.create(data_dir)
  setwd(data_dir)
  
  success <- FALSE
  attempts <- 2
  attempt <- 1
  
  while (!success && attempt <= attempts) {
    tryCatch({
      # Attempt download with zen4R
      zen4R::export_zenodo(doi = doi, filename = "zenodoDublincore", format = "DublinCore")
      zen4R::download_zenodo(doi = doi, files = filename)
      
      # Check if the file was downloaded
      if (file.exists(filename)) {
        success <- TRUE
        message(sprintf("File '%s' downloaded successfully with zen4R", filename))
      } else {
        message(sprintf("File '%s' was not downloaded completely, retrying...", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed for file '%s': %s", attempt, filename, e$message))
    })
    attempt <- attempt + 1
  }
  
  # Fallback with curl if zen4R fails after 3 attempts
  if (!success) {
    message(sprintf("zen4R failed after %d attempts. Trying with curl...", attempts))
    tryCatch({
      # Attempt download with curl
      url <- paste0("https://doi.org/", doi)
      curl::curl_download(url = url, destfile = paste0(data_dir, "/", filename))
      
      # Check if the file was downloaded
      if (file.exists(paste0(data_dir, "/", filename))) {
        success <- TRUE
        message(sprintf("File '%s' downloaded successfully with curl", filename))
      } else {
        message(sprintf("File '%s' could not be downloaded with curl", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Curl download failed for file '%s': %s", filename, e$message))
    })
  }
  
  # Stop the entire process if download was unsuccessful
  if (!success) {
    stop(sprintf("Failed to download file '%s' after multiple attempts. Stopping process.", filename))
  }
  
  setwd(dir)
}

# Use the function with lapply for each DOI
lapply(1:nrow(DOI), function(i) {
  filepath <- paste0("data/", DOI$Filename[i])
  if (!file.exists(filepath)) {
    extract_zenodo_metadata(doi = DOI$DOI[i], filename = DOI$Filename[i])
  }
})
