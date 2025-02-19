
load_grouped_data <- function(df_sf, filename) {
  
if (file.exists(filename)) {
  flog.info("Reading default parquet dataset (whole_group_df.parquet): %s", filename)
  whole_group_df <- arrow::read_parquet(filename)  
  
} else {
  flog.info("writting  default parquet dataset (pre_filtered): %s", filename)
  whole_group_df <- df_sf  %>% filter(!is.na(geom_wkt))  %>%    
    dplyr::group_by(codesource_area, gridtype, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
  
  arrow::write_parquet(whole_group_df, filename)
  
}
  return(whole_group_df)
}