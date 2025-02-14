extract_zenodo_metadata <- function(doi, filename, data_dir = here::here("data")) {
  options(timeout = 60000) # Global timeout for downloads
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  success <- FALSE
  attempts <- 1
  attempt <- 1
  record_id <- gsub(".*\\.", "", doi)
  destfile <- file.path(data_dir, filename)
  
  while (!success && attempt <= attempts) {
    tryCatch({
      message(sprintf("Attempt %d: Exporting metadata for DOI %s", attempt, doi))
      zen4R::export_zenodo(doi = doi, filename = here::here(data_dir, paste0("metadata_", record_id)), format = "DublinCore")
      
      message(sprintf("Attempt %d: Downloading file '%s'", attempt, filename))
      zen4R::download_zenodo(doi = doi, files = filename, parallel_handler = parLapply, cl = makeCluster(12))
      
      if (file.exists(destfile)) {
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
  
  # Fallback to downloader if zen4R fails
  if (!success) {
    message(sprintf("All attempts with zen4R failed. Trying downloader::download for file '%s'", filename))
    raw_url <- paste0("https://zenodo.org/record/", record_id, "/files/", filename, "?download=1")
    encoded_url <- URLencode(raw_url)
    
    tryCatch({
      downloader::download(encoded_url, destfile, mode = "wb")
      if (file.exists(destfile)) {
        message(sprintf("File '%s' downloaded successfully via downloader", filename))
      } else {
        stop(sprintf("File '%s' could not be downloaded via downloader", filename))
      }
    }, error = function(e) {
      stop(sprintf("Failed to download file '%s' from DOI: %s using all methods", filename, doi))
    })
  }
}

