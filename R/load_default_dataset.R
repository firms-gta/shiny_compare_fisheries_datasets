load_default_dataset <- function(df, filename,list_filters) {
  
# Check if the file was downloaded
if (file.exists(filename)) {
  flog.info("Reading default parquet dataset (pre_filtered): %s", filename)
  init_whole_default_df <- arrow::read_parquet(filename)  
  
} else {
  flog.info("writting  default parquet dataset (pre_filtered): %s", filename)
  init_whole_default_df <- df  %>% filter(!is.na(geom_wkt)) %>%  
    dplyr::filter(
      # codesource_area %in% within_areas,
      dataset %in% list_filters$dataset,
      species %in% list_filters$species,
      source_authority %in% list_filters$source_authority,
      gear_type %in% list_filters$gear_type,
      year %in% list_filters$year,
      fishing_fleet %in% list_filters$fishing_fleet,
      measurement_unit %in% list_filters$unit
    ) %>% 
    dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() 
  arrow::write_parquet(init_whole_default_df, filename)
}
  return(init_whole_default_df)
}