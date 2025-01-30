load_filters_combinations <- function(filename) {
  
  flog.info("Check what are the existing / possible combinations between dimension values (to adapt the values of filters dynamically)")
  if(!file_exists(filename)){
    filters_combinations <- df_sf  %>% st_drop_geometry() %>% 
      dplyr::group_by(dataset,species, year, gear_type, measurement_unit, source_authority, fishing_fleet) %>% dplyr::summarise(count = n())
    flog.info("Filter combinations retrieved and stored.")
    arrow::write_parquet(filters_combinations, filename)
  }else{
    flog.info("Try  if a default file for filters is pre-calculated")
    filters_combinations <- arrow::read_parquet(filename)
  }
  
  return(filters_combinations)
}