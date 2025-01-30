
load_grouped_data <- function(filename) {
  
if (file.exists("data/whole_group_df.parquet")) {
  flog.info("Reading default parquet dataset (whole_group_df.parquet): %s", filename)
  whole_group_df <- arrow::read_parquet("data/whole_group_df.parquet")  
  
} else {
  flog.info("writting  default parquet dataset (pre_filtered): %s", filename)
  whole_group_df <- df_sf  %>% filter(!is.na(geom_wkt))  %>%    
    dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
  
  arrow::write_parquet(whole_group_df, "data/whole_group_df.parquet")
  
}
  return(whole_group_df)
}