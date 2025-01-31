apply_filters <- function(df, list_filters,wkt) {
  
    full_df <- df  %>% filter(!is.na(geom_wkt)) %>%  
      dplyr::filter(
        # codesource_area %in% within_areas,
        dataset %in% list_filters$dataset,
        species %in% list_filters$species,
        source_authority %in% list_filters$source_authority,
        gear_type %in% list_filters$gear_type,
        year %in% list_filters$year,
        fishing_fleet %in% list_filters$fishing_fleet,
        measurement_unit %in% list_filters$unit,
        gridtype %in% input$gridtype
      ) %>% 
      dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    
    
    flog.info("Footprint of all grouped filtered data")
    full_df_footprint <- full_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() #%>% st_simplify() 
    # current_selection_footprint_wkt(this_footprint)
    
    
    
    if(wkt == all_wkt){
      main_data <- full_df
      
    }else{
      main_data <- full_df           
      default_df <- full_df %>% filter(!is.na(geom_wkt)) %>% 
        dplyr::filter(codesource_area %in% within_areas)
      filtered_default_df(default_df)
      main_data <- filtered_default_df()
    }
    
    flog.info("Returns a list of dataframes")
    list_df = list(
      "whole_filtered_df" = full_df,
      "current_selection_footprint_wkt" = full_df_footprint,
      "filtered_default_df" = default_df
    )
    
  return(full_df)
}

# main_data <- whole_dataset()

# tmp_main_df <- main_data  %>% filter(!is.na(geom_wkt)) %>%
  # dplyr::filter(
    # dataset %in% input$dataset,
    # species %in% input$species,
    # source_authority %in% input$source_authority,
    # gear_type %in% input$gear_type,
    # year %in% input$year,
    # fishing_fleet %in% input$fishing_fleet,
    # measurement_unit %in% input$unit,
    # gridtype %in% input$gridtype
  )
# %>%
#   dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, year, measurement_unit) %>%
#   # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
# 
# flog.info("Footprint of all grouped filtered data")
# this_footprint <- tmp_main_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>%
#   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
#   st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() #%>% st_simplify() 
# current_selection_footprint_wkt(this_footprint)
# 
# whole_filtered_df(tmp_main_df)
# 
# if(wkt == all_wkt){
#   main_data <- whole_filtered_df()
#   
# }else{
#   main_data <- whole_filtered_df()           
#   default_df <- main_data %>% filter(!is.na(geom_wkt)) %>% 
#     dplyr::filter(codesource_area %in% within_areas)
#   filtered_default_df(default_df)
#   main_data <- filtered_default_df()
# }
