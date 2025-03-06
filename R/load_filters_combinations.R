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
  
  list_values_dimensions = list(
    "dataset" = unique(basename(filters_combinations$dataset)),
    "species" = unique(basename(filters_combinations$species)),
    "year" = unique(filters_combinations$year),
    "gear_type" = unique(basename(filters_combinations$gear_type)),
    "measurement_unit" = unique(filters_combinations$measurement_unit),
    "source_authority" = unique(filters_combinations$source_authority),
    "gridtype" = unique(filters_combinations$gridtype) ,
    "fishing_fleet" = unique(filters_combinations$fishing_fleet)
  )
  
  flog.info("Storing all possible values for retained filters : list distinct values in the main dataset for each dimension")
  # list_values_dataset <- dbGetQuery(con,"SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")  %>% distinct(dataset) %>% select(dataset) %>% unique()
  # list_values_dataset <- unique(filters_combinations$dataset) #  %>% arrange(desc(dataset))
  # list_values_species <- unique(filters_combinations$species) # %>% arrange(desc(species))
  # list_values_year <-  unique(filters_combinations$year)  # %>% arrange(desc(year))
  # list_values_gear_type <-  unique(filters_combinations$gear_type)# %>% arrange(desc(gear_type))
  # list_values_measurement_unit <- unique(filters_combinations$measurement_unit) # %>% arrange(desc(measurement_unit))
  # list_values_source_authority <- unique(filters_combinations$source_authority)
  # list_values_gridtype <- unique(filters_combinations$gridtype) 
  # list_values_fishing_fleet <-  unique(filters_combinations$fishing_fleet)
  
  # time_series_ids <- c(1,2,3,4,5)
  # names(time_series_ids) <- c('a','b','c','d','e')
  if(!file.exists(here::here(file.path("data","codelist_species.qs")))){
    load_codelists(list_values_dimensions,list_dimensions=c("species"))
  }
  codelist_species <- qs::qread(here::here(file.path("data","codelist_species.qs")))
  species_ids <- codelist_species$code
  names(species_ids) <- codelist_species$taxon_scientific_name
  
  if(!file.exists(here::here(file.path("data","codelist_gear.qs")))){
    load_codelists(list_values_dimensions,list_dimensions=c("species"))
  }
  codelist_gear <- qs::qread(here::here(file.path("data","codelist_gear.qs")))
  gear_ids <- codelist_gear$code
  names(gear_ids) <- codelist_gear$label
  
  if(!file.exists(here::here(file.path("data","codelist_source_authority.qs")))){
    load_codelists(list_values_dimensions,list_dimensions=c("source_authority"))
  }
  codelist_source_authority <- qs::qread(here::here(file.path("data","codelist_source_authority.qs")))
  source_authority_ids <- codelist_source_authority$code
  names(source_authority_ids) <- codelist_source_authority$label
  
  if(!file.exists(here::here(file.path("data","codelist_fishing_fleet.qs")))){
    load_codelists(list_values_dimensions,list_dimensions=c("fishing_fleet"))
  }
  codelist_fishing_fleet <- qs::qread(here::here(file.path("data","codelist_fishing_fleet.qs")))
  fishing_fleet_ids <- codelist_fishing_fleet$code
  names(fishing_fleet_ids) <- codelist_fishing_fleet$label
  
  
  list_values_dimensions = list(
    "dataset" = unique(basename(filters_combinations$dataset)),
    "species" = species_ids,
    "year" = unique(filters_combinations$year),
    "gear_type" = gear_ids,
    "measurement_unit" = unique(filters_combinations$measurement_unit),
    "source_authority" = source_authority_ids,
    "gridtype" = unique(filters_combinations$gridtype) ,
    "fishing_fleet" = fishing_fleet_ids
    )

  
  flog.info("Returns a list of dataframe + velues for dimensions")
  list_filters = list(
    "filters_combinations_df" = filters_combinations,
    "list_values_dimensions" = list_values_dimensions
  )
  
  return(list_filters)
}
