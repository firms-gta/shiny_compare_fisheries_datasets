load_default_dataset <- function(df, filename,list_filters) {
  
# Check if the file was downloaded
if (file.exists(filename)) {
  flog.info("Reading default parquet dataset (pre_filtered): %s", filename)
  updates <- qs::qread(here::here(file.path("data",filename)))  
  
} else {
  
  updates <- apply_filters(df=df, list_filters=list_filters)
  qs::qsave(updates, here::here(file.path("data",filename)))
}
  return(updates)
}
