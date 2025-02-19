apply_filters <- function(df, list_filters) {
  
  wkt <- list_filters$wkt
  within_areas <- list_filters$within_areas
  
  flog.info("Applying all non spatial filters")
  new_df <- df  %>% filter(!is.na(codesource_area)) %>%  
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
      dplyr::group_by(codesource_area, gridtype, dataset, source_authority, species, gear_type, fishing_fleet, year, measurement_unit) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    
    
    flog.info("Footprint of all grouped (non spatial) filtered data")
    flog.info("Check number of rows of main df")
    if(nrow(new_df)!=0){
      flog.info("Check number of rows of main df : %s", nrow(new_df))
      df_distinct_geom_light <- qs::qread("data/df_distinct_geom_light.qs")
      new_df_footprint <- new_df%>%
        dplyr::left_join((df_distinct_geom_light ), by=c('codesource_area'))  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% 
        st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() #%>% st_simplify() 
      rm(df_distinct_geom_light)
      if(is.null(within_areas) || wkt == all_wkt){
        # if(wkt == all_wkt){
          flog.info("there is no spatial filter")
        default_df <- new_df
      }else{
        flog.info("there is a spatial filter")
        default_df <- new_df %>% filter(!is.na(codesource_area)) %>% 
        dplyr::filter(codesource_area %in% within_areas)
        
        if(nrow(default_df)==0){
          flog.info("nrow is null")
          
          showModal(modalDialog(
            title = "Warning",
            "No data left with current filters, back to default filters!",
            easyClose = TRUE,
            footer = NULL
          ))
          default_df <- new_df
        }
        
      }
    } else {
      showModal(modalDialog(
        title = "Warning",
        "No data left with current filters, back to default filters!",
        easyClose = TRUE,
        footer = NULL
      ))
      new_df <- df
      default_df <- filtered_default_df()
      new_df_footprint <- current_selection_footprint_wkt()
    }
    
    # flog.info("Returns a list of dataframes")
    list_df = list(
      "whole_filtered_df" = new_df,
      "current_selection_footprint_wkt" = new_df_footprint,
      "filtered_default_df" = default_df
    )
    return(list_df)
}