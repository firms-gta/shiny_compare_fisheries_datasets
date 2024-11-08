# URL of the first ZIP file to download
zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
zip_destfile <- "cl_areal_grid.zip"
csv_file_in_zip <- "data/cl_areal_grid.csv" # Specify expected CSV file inside

# Download and unzip the ZIP file only if the CSV doesn't already exist
if (!file.exists(csv_file_in_zip)) {
  if (!file.exists(zip_destfile)) {
    download.file(zip_url, zip_destfile, method = "auto")
    message("ZIP file downloaded.")
  } else {
    message("ZIP file already exists. Skipping download.")
  }
  unzip(zip_destfile, exdir = "data")
} else {
  message("CSV file already exists. Skipping download and unzip.")
}


# URL of the first ZIP file to download
csv_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_nc_areas.csv"
csv_destfile <- "cl_nc_areas.csv" # Specify expected CSV file inside

# Download the CSV file if it doesn't already exist
if (!file.exists(csv_destfile)) {
  download.file(csv_url, csv_destfile, method = "auto")
} else {
  message("CSV file already exists. Skipping download.")
}
