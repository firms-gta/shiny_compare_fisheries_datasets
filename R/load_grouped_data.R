load_grouped_data <- function(df_sf, filename) {
  
if (file.exists(here::here(file.path("data",filename)))) {
  flog.info("Reading default parquet dataset (whole_group_df.parquet): %s", filename)
  whole_group_df <- qs::qread(here::here(file.path("data",filename)))  
  
} else {
  flog.info("writting  default parquet dataset (pre_filtered): %s", filename)
  whole_group_df <- df_sf  %>% 
    dplyr::group_by(codesource_area, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>%   
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE))  %>% ungroup() %>% 
    filter(!is.na(codesource_area))  %>%    
    dplyr::left_join((df_distinct_geom %>% as.tibble() %>% dplyr::select(-c(geom,ogc_fid))), by=c('codesource_area'))
  
  qs::qsave(whole_group_df, here::here(file.path("data",filename)))
  
}
  return(whole_group_df)
}

