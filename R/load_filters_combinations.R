load_filters_combinations <- function(df_sf,filename) {
  
  flog.info("Check what are the existing / possible combinations between dimension values (to adapt the values of filters dynamically)")
  if(!file.exists(here::here(file.path("data",filename)))){
    filters_combinations <- df_sf  %>% st_drop_geometry() %>% 
      dplyr::group_by(dataset,gridtype,species, year, gear_type, measurement_unit, source_authority, fishing_fleet) %>% dplyr::summarise(count = n())
    flog.info("Filter combinations retrieved and stored.")
    qs::qsave(filters_combinations, here::here(file.path("data",filename)))
  }else{
    flog.info("Try  if a default file for filters is pre-calculated")
    filters_combinations <- qs::qread(here::here(file.path("data",filename)))
  }
  
  flog.info("Storing all possible values for retained filters : list distinct values in the main dataset for each dimension")
  # target_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
  # list_values_dataset <- unique(filters_combinations$dataset) #  %>% arrange(desc(dataset))
  # list_values_species <- unique(filters_combinations$species) # %>% arrange(desc(species))
  # list_values_year <-  unique(filters_combinations$year)  # %>% arrange(desc(year))
  # list_values_gear_type <-  unique(filters_combinations$gear_type)# %>% arrange(desc(gear_type))
  # list_values_measurement_unit <- unique(filters_combinations$measurement_unit) # %>% arrange(desc(measurement_unit))
  # list_values_source_authority <- unique(filters_combinations$source_authority)
  # list_values_gridtype <- unique(filters_combinations$gridtype) 
  # list_values_fishing_fleet <-  unique(filters_combinations$fishing_fleet)
  
  list_values_dimensions = list(
    "dataset" = unique(basename(filters_combinations$dataset)),
    "species" = unique(filters_combinations$species),
    "year" = unique(filters_combinations$year),
    "gear_type" = unique(filters_combinations$gear_type),
    "measurement_unit" = unique(filters_combinations$measurement_unit),
    "source_authority" = unique(filters_combinations$source_authority),
    "gridtype" = unique(filters_combinations$gridtype) ,
    "fishing_fleet" = unique(filters_combinations$fishing_fleet)
    )
  
  
  flog.info("Returns a list of dataframe + velues for dimensions")
  list_filters = list(
    "filters_combinations_df" = filters_combinations,
    "list_values_dimensions" = list_values_dimensions
  )
  
  return(list_filters)
}