load_default_dataset <- function(df, filename,list_filters) {
  
# Check if the file was downloaded
if (file.exists(filename)) {
  flog.info("Reading default parquet dataset (pre_filtered): %s", filename)
  init_whole_default_df <- arrow::read_parquet(filename)  
  
} else {
  
  updates <- apply_filters(df=df, list_filters=list_filters)
  init_whole_default_df <- updates$whole_filtered_df
  arrow::write_parquet(init_whole_default_df, filename)
}
  return(updates)
}