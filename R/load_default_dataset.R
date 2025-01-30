load_default_dataset <- function(df, filename) {
  
# Check if the file was downloaded
if (file.exists(filename)) {
  flog.info("Reading default parquet dataset (pre_filtered): %s", filename)
  init_whole_default_df <- arrow::read_parquet(filename)  
  
} else {
  flog.info("writting  default parquet dataset (pre_filtered): %s", filename)
  init_whole_default_df <- df  %>% filter(!is.na(geom_wkt)) %>%  
    dplyr::filter(
      # codesource_area %in% within_areas,
      dataset %in% default_dataset,
      species %in% default_species,
      source_authority %in% default_source_authority,
      gear_type %in% default_gear_type,
      year %in% default_year,
      fishing_fleet %in% default_fishing_fleet,
      measurement_unit %in% default_unit
    ) %>% 
    dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() 
  arrow::write_parquet(init_whole_default_df, filename)
}
  
  return(init_whole_default_df)
}