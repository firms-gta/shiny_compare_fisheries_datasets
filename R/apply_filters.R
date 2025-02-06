apply_filters <- function(df, list_filters, wkt,within_areas) {
  
  
    new_df <- df  %>% filter(!is.na(geom_wkt)) %>%  
      dplyr::filter(
        # codesource_area %in% within_areas,
        dataset %in% list_filters$dataset,
        species %in% list_filters$species,
        source_authority %in% list_filters$source_authority,
        gear_type %in% list_filters$gear_type,
        year %in% list_filters$year,
        fishing_fleet %in% list_filters$fishing_fleet,
        measurement_unit %in% list_filters$unit,
        gridtype %in% list_filters$gridtype
      ) %>% 
      dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    
    
    flog.info("Footprint of all grouped filtered data")
    new_df_footprint <- new_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() #%>% st_simplify() 
    # current_selection_footprint_wkt(this_footprint)
    
    if(wkt == all_wkt){
      default_df <- new_df
      }else{
        default_df <- new_df %>% filter(!is.na(geom_wkt)) %>% 
        dplyr::filter(codesource_area %in% within_areas)
    }
    
    # flog.info("Returns a list of dataframes")
    list_df = list(
      "whole_filtered_df" = new_df,
      "current_selection_footprint_wkt" = new_df_footprint,
      "filtered_default_df" = default_df
    )
    
    
    
    # 
    # flog.info("Replacing filtered dataset with the new one")
    # whole_filtered_df(tmp_main_df)
    # 
    # if(wkt == all_wkt){
    #   main_data <- whole_filtered_df()
    # }else{
    #   main_data <- whole_filtered_df()           
    #   default_df <- main_data %>% filter(!is.na(geom_wkt)) %>% 
    #     dplyr::filter(codesource_area %in% within_areas)
    #   filtered_default_df(default_df)
    #   main_data <- filtered_default_df()
    # }
    # 
  return(list_df)
}