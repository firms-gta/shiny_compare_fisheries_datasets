extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  dir <- getwd()
  require(zen4R)
  options(timeout = 60000) # Global timeout for downloads
  zenodo <- ZenodoManager$new()
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
