require(zen4R)
require(readr)
DOI <- read_csv("data/DOI.csv")

options(timeout = 6000) # Global timeout for downloads

extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  dir <- getwd()
  if (!dir.exists(data_dir)) dir.create(data_dir)
  setwd(data_dir)
  
  success <- FALSE
  attempts <- 1
  attempt <- 1
  
  while (!success && attempt <= attempts) {
    tryCatch({
      zen4R::export_zenodo(doi = doi, filename = "zenodoDublincore", format = "DublinCore")
      
      zen4R::download_zenodo(doi = doi, files = filename)
      
      # Check if the file was downloaded
      if (file.exists(filename)) {
        success <- TRUE
        message(sprintf("File '%s' downloaded successfully", filename))
      } else {
        message(sprintf("File '%s' was not downloaded completely, retrying...", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed for file '%s': %s", attempt, filename, e$message))

      
      # stop("Stopped because the file cannot be downloaded entirely")
    })
    # attempt <- attempt + 1
    message("Downloading with curl")
    system("curl -L -o global_catch_firms_level0_harmonized.csv 'https://zenodo.org/records/11460074/files/global_catch_firms_level0_harmonized.csv?download=1'")
    message("Downloaded with curl")
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
