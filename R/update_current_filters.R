update_current_filters <- function(list_filters_values) {
  current_species(list_filters_values$species)
  current_dataset(list_filters_values$dataset)
  current_source_authority(list_filters_values$source_authority)
  current_gear_type(list_filters_values$gear_type)
  current_year(list_filters_values$year)
  current_fishing_fleet(list_filters_values$fishing_fleet)
  current_unit(list_filters_values$unit)
  current_gridtype(list_filters_values$gridtype)
}
