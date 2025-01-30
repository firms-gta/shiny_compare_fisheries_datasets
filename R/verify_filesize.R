# Function to verify file size (if available in DOI)
verify_file <- function(filepath, expected_size) {
  file.info(filepath)$size == expected_size
}