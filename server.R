server <- function(input, output, session) {
  
  
  ########################################################## Dynamic filters ########################################################## 
  
  
  # change <- reactive({
  #   unlist(strsplit(paste(c(input$species,input$year,input$gear_type),collapse="|"),"|",fixed=TRUE))
  # })
  
  # observeEvent(input$yourWKT,{
  #   updateSelectInput(session = session,
  #                     inputId = "yourWKT",
  #                     selected = current_wkt())
  # })
  
  # observeEvent(updated_current_wkt$updated_current_wkt(), {
  #   req(current_wkt())
  #   if(updated_current_wkt$updated_current_wkt() != current_wkt()){
  #     current_wkt(updated_current_wkt$updated_current_wkt())
  #     submitTrigger(TRUE)
  #   }
  # })
  
  
  
  
  # observeEvent(input$species,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1])
  #   updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
  #   updateSelectInput(session,"gear_type",choices = unique(temp$gear_type),selected=unique(temp$gear_type))
  # }
  # )
  
  
  # observeEvent(input$gear_type,{
  #   if(!'All' %in% input$gear_type) {
  #     updateSelectInput(session,"gear_type",choices = unique(temp$gear_type),selected=unique(temp$gear_type))
  #   }
  # }
  # )
  # if All is not in selection, filter to selected continents
  
  
  # observeEvent(input$year,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])
  #   updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  # }
  # )
  
  
  
  observeEvent(input$resetWkt, {
    last_wkt(current_wkt())
    current_wkt(all_wkt)
  },
  ignoreInit = TRUE)
  
  observeEvent(current_wkt(),{
    shinyjs::click(id = "submit")
  },
  ignoreInit = TRUE)
  
  observeEvent(input$resetAllFilters, {
    
    flog.info("Action Button => Reset all filters !")
    
    reset_all_filters <- TRUE
    # updateTextInput(session, "polygon", value = all_wkt)
    
    # current_dataset(target_dataset)
    updatePickerInput(session,"dataset",selected=target_dataset)
    
    # current_species(target_species)
    updatePickerInput(session,"species",selected=target_species,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,
                                       yes = "color: rgba(119, 119, 119, 0.5);",
                                       no = "")
                      ))
    
    # current_gear_type(target_gear_type)
    updatePickerInput(session,"gear_type",selected=target_gear_type)
    
    # current_year(target_year)
    updatePickerInput(session,"year",selected=target_year)
    
    # current_fishing_fleet(target_fishing_fleet)
    updatePickerInput(session,"fishing_fleet",selected=target_fishing_fleet)
    
    # current_unit(target_measurement_unit)
    updatePickerInput(session,"unit",selected=target_measurement_unit)
    
    # current_source_authority(target_source_authority)
    updatePickerInput(session,"source_authority",selected=target_source_authority)
    
    # current_gridtype(target_gridtype)
    updatePickerInput(session,"gridtype",selected=target_gridtype)
    
    # main_df <- default_df
    # main_df(whole_dataset())
    # plot_df(whole_plot_df)
    # map_df(whole_map_df)
    # shinyjs::click(id = "submit")
    
  },
  ignoreInit = TRUE)
  
  observeEvent(input$switched, {
    if(switch_unit()){switch_unit(FALSE)}else{switch_unit(TRUE)}
  })
  
  
  flog.info("##########################################################")
  flog.info("Click on submit => subset data with selected filters")
  main_df <- eventReactive(input$submit, {
    # main_df <- observeEvent(current_wkt() , {
    
    flog.info("Set WKT !!")
    req(current_wkt())
    wkt <- current_wkt()
    map_wkt(wkt)
    # flog.info("Define the rules to update the spatial filtering of list of areas within the WKT !!")
    if( (wkt != last_wkt() && !(wkt == all_wkt && all(input$gridtype == current_gridtype()))) ||
        (wkt==last_wkt() && !all(input$gridtype == current_gridtype())) || 
        !all(input$gridtype == current_gridtype()) ){
      flog.info("************ YES UPDATE within_areas ***************************** !!")
      within_areas <- process_list_areas(df_distinct_geom, wkt=current_wkt(), list_gridtype=input$gridtype) 
    }else{
      flog.info("************ NO UPDATE within_areas ***************************** !!")
    }
    
    flog.info("Check if filters have been updated")
    if(all(input$dataset == current_dataset()) && 
       all(input$species == current_species()) && 
       all(input$source_authority == current_source_authority()) && 
       all(input$gear_type == current_gear_type()) && 
       all(input$year == current_year()) && 
       all(input$fishing_fleet == current_fishing_fleet()) && 
       all(input$fishing_fleet == current_fishing_fleet()) && 
       all(input$gridtype == current_gridtype())
    ){
      flog.info("--------------------------------------------")
      flog.info("USE CASE 1: Non spatial filters => same / not updated")
      flog.info("USE CASE 1: Check if the Spatial filters / WKT has been updated")
      
      flog.info("--------------------------------------------")
      if(wkt == last_wkt()){
        flog.info("--------------------------------------------")
        flog.info("USE CASE 1: Spatial filters => same WKT as previous one => Nothing to do !! Exactly the same dataset !!")
        flog.info("--------------------------------------------")
        main_df <- filtered_default_df()
      }else{
        if(wkt==all_wkt) {
          flog.info("--------------------------------------------")
          flog.info("USE CASE 1: Spatial filters => none / loading whole dataset !!")
          flog.info("--------------------------------------------")
          main_df <- whole_filtered_df()
        }else{
          flog.info("--------------------------------------------")
          flog.info("USE CASE 1: Spatial filters => new WKT applied to filter the whole dataset !!")
          flog.info("--------------------------------------------")
          flog.info("Listing remaining areas within the new WKT: %s", wkt)
          main_data <- whole_filtered_df()
          main_df<- main_data %>% filter(!is.na(geom_wkt)) %>% 
            dplyr::filter(codesource_area %in% within_areas)
          }
      } 
    }else if (length(setdiff(input$dataset,current_dataset())) == 0 && 
              length(setdiff(input$species,current_species())) == 0 && 
              length(setdiff(input$source_authority,current_source_authority())) == 0 && 
              length(setdiff(input$gear_type,current_gear_type())) == 0 && 
              length(setdiff(input$year,current_year())) == 0 && 
              length(setdiff(input$fishing_fleet,current_fishing_fleet())) == 0 && 
              length(setdiff(input$unit,current_unit())) == 0 && 
              length(setdiff(input$gridtype,current_gridtype())) == 0  
              ) {
      flog.info("--------------------------------------------")
      flog.info("USE CASE 2: Non spatial filters updated =>  subset of previous ones without additionnal filters, just refining current data")
      flog.info("--------------------------------------------")
     
      flog.info("Starting from previous dataset to refine / subset it with new values")
      main_data <- whole_filtered_df()
      
      tmp_main_df <- main_data  %>% filter(!is.na(geom_wkt)) %>%
        dplyr::filter(
          dataset %in% input$dataset,
          species %in% input$species,
          source_authority %in% input$source_authority,
          gear_type %in% input$gear_type,
          year %in% input$year,
          fishing_fleet %in% input$fishing_fleet,
          measurement_unit %in% input$unit,
          gridtype %in% input$gridtype
        )
      # if(length(setdiff(input$gridtype,current_gridtype())) != 0){
      #   # flog.info("Current footprint for filters is %s: ",whole_footprint)
      #   # within_areas
      #   # tmp_main_df <- main_data %>% dplyr::filter(codesource_area %in% within_areas)
      #   tmp_main_df <- main_data %>% dplyr::filter(gridtype %in% input$gridtype)
      # }
      
      flog.info("Footprint has to be refined from previous one !!!")
      this_footprint <- tmp_main_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>%
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
        st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() # %>% st_simplify()
      # flog.info("Current footprint for filters is %s: ",whole_footprint)
      current_selection_footprint_wkt(this_footprint)
      
      flog.info("Replacing filtered dataset with the new one")
      whole_filtered_df(tmp_main_df)
      
      if(wkt == all_wkt){
        main_data <- whole_filtered_df()
      }else{
        main_data <- whole_filtered_df()           
        default_df <- main_data %>% filter(!is.na(geom_wkt)) %>% 
          dplyr::filter(codesource_area %in% within_areas)
        filtered_default_df(default_df)
        main_data <- filtered_default_df()
      }
      update_current_filters(list_filters_values = list("dataset"=input$dataset,
                                                            "species"=input$species,
                                                            "year"=input$year,
                                                            "gear_type"=input$species,
                                                            "unit"=input$unit,
                                                            "source_authority"=input$species,
                                                            "gridtype"=input$gridtype,
                                                            "fishing_fleet"=input$fishing_fleet))
      
      main_df <- main_data 
    }else{
      flog.info("--------------------------------------------")
      flog.info("USE CASE 3: Non spatial filters updated => different filters not all included in previous filters => filtering the whole dataset from scratch")
      flog.info("USE CASE 3: might take time")
      flog.info("--------------------------------------------")
      flog.info("Loading all (grouped) data")
      main_data <- whole_dataset()
      
          flog.info("Non spatial filters are different => applying these new filters to the whole dataset !!")
          flog.info("input$dataset : %s", all(input$dataset == current_dataset()))
          if(!all(input$dataset == current_dataset())){current_dataset(input$dataset)}

          flog.info("input$species : %s", all(input$species == current_species()))
          if(!all(input$species == current_species())){current_species(input$species)}

          flog.info("input$source_authority : %s", all(input$source_authority == current_source_authority()))
          if(!all(input$source_authority == current_source_authority())){current_source_authority(input$source_authority)}
          flog.info("input$gear_type : %s", all(input$gear_type == current_gear_type()))
          if(!all(input$gear_type == current_gear_type())){current_gear_type(input$gear_type)}
          flog.info("input$year : %s", all(input$year == current_year()))
          if(!all(input$year == current_year())){current_year(input$year)}
          flog.info("input$fishing_fleet : %s", all(input$fishing_fleet == current_fishing_fleet()))
          if(!all(input$fishing_fleet == current_fishing_fleet())){current_fishing_fleet(input$fishing_fleet)}
          flog.info("input$unit : %s", all(input$unit == current_unit()))
          if(!all(input$unit == current_unit())){current_unit(input$unit)}
          flog.info("input$gridtype : %s", all(input$gridtype == current_gridtype()))
          if(!all(input$gridtype == current_gridtype())){current_gridtype(input$gridtype)}
      
      
      if(all(input$dataset == target_dataset) && 
         all(input$species == target_species) && 
         all(input$source_authority == target_source_authority) && 
         all(input$gear_type == target_gear_type) && 
         all(input$year == target_year) && 
         all(input$fishing_fleet == target_fishing_fleet) && 
         all(input$unit == target_measurement_unit) && 
         all(input$gridtype == target_gridtype)
         ){
           flog.info("--------------------------------------------")
           flog.info("USE CASE 3: All non spatial filters have just been reset !!")
           flog.info("--------------------------------------------")
           
           update_current_filters(list_filters_values = list("dataset"=list_values_dimensions$dataset,
                                                                 "species"=list_values_dimensions$species,
                                                                 "year"=list_values_dimensions$year,
                                                                 "gear_type"=list_values_dimensions$gear_type,
                                                                 "unit"=list_values_dimensions$measurement_unit,
                                                                 "source_authority"=list_values_dimensions$source_authority,
                                                                 "gridtype"=list_values_dimensions$gridtype,
                                                                 "fishing_fleet"=list_values_dimensions$fishing_fleet))
           
           
           current_selection_footprint_wkt(all_polygons_footprint)
           whole_filtered_df(main_data)
           
           if(wkt != all_wkt){
             default_df <- main_data %>% filter(!is.na(geom_wkt)) %>% 
               dplyr::filter(codesource_area %in% within_areas)
             filtered_default_df(default_df)
             main_data <- default_df
           }
           main_df <- main_data
         }
         else{
           flog.info("--------------------------------------------")
           flog.info("USE CASE 3: Not a full reset / just few additional filters not present in the previous filters => filtering the whole dataset but are a subset of previous ones")
           flog.info("--------------------------------------------")
           
           update_current_filters(list_filters_values = list("dataset"=input$dataset,
                                                                 "species"=input$species,
                                                                 "year"=input$year,
                                                                 "gear_type"=input$gear_type,
                                                                 "unit"=input$unit,
                                                                 "source_authority"=input$source_authority,
                                                                 "gridtype"=input$gridtype,
                                                                 "fishing_fleet"=input$fishing_fleet))

           flog.info("Filtering all grouped data")
           main_data <- whole_dataset()
           
           tmp_main_df <- main_data  %>% filter(!is.na(geom_wkt)) %>%
             dplyr::filter(
               dataset %in% input$dataset,
               species %in% input$species,
               source_authority %in% input$source_authority,
               gear_type %in% input$gear_type,
               year %in% input$year,
               fishing_fleet %in% input$fishing_fleet,
               measurement_unit %in% input$unit,
               gridtype %in% input$gridtype
             )
           # %>%
           #   dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, year, measurement_unit) %>%
           #   # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
           #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()

           flog.info("Footprint of all grouped filtered data")
           this_footprint <- tmp_main_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>%
             dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
             st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text() #%>% st_simplify() 
           current_selection_footprint_wkt(this_footprint)

           whole_filtered_df(tmp_main_df)
           
           if(wkt == all_wkt){
             main_data <- whole_filtered_df()
             
           }else{
             main_data <- whole_filtered_df()           
             default_df <- main_data %>% filter(!is.na(geom_wkt)) %>% 
               dplyr::filter(codesource_area %in% within_areas)
             filtered_default_df(default_df)
             main_data <- filtered_default_df()
           }
           
           
           update_current_filters(list_filters_values = list("dataset"=input$dataset,
                                                                 "species"=input$species,
                                                                 "year"=input$year,
                                                                 "gear_type"=input$gear_type,
                                                                 "unit"=input$unit,
                                                                 "source_authority"=input$source_authority,
                                                                 "gridtype"=input$gridtype,
                                                                 "fishing_fleet"=input$fishing_fleet))
           
           
           main_df <- main_data
           
         }
      

      }
    
    
    
    
    
    
    
    
    
    
    
    # 
    # flog.info("Check if some filters have been selected")
    # if(all(input$dataset == target_dataset) && 
    #    all(input$species == target_species) && 
    #    all(input$source_authority == target_source_authority) && 
    #    all(input$gear_type == target_gear_type) && 
    #    all(input$year == target_year) && 
    #    all(input$fishing_fleet == target_fishing_fleet) && 
    #    all(input$gridtype == target_gridtype) && 
    #    all(input$unit == target_measurement_unit)
    # ){
    #   flog.info("No non spatial filters have been selected => full dataset !!!")
    #   
    #   flog.info("input$dataset : %s", all(input$dataset == target_dataset))
    #   flog.info("input$species : %s", all(input$species == target_species))
    #   flog.info("input$source_authority : %s", all(input$source_authority == target_source_authority))
    #   flog.info("input$gear_type : %s", all(input$gear_type == target_gear_type))
    #   flog.info("input$year : %s", all(input$year == target_year))
    #   flog.info("input$fishing_fleet : %s", all(input$fishing_fleet == target_fishing_fleet))
    #   flog.info("input$unit : %s", all(input$unit == target_measurement_unit))
    #   flog.info("Tests TRUE !!!")
    #   
    #   main_data <- whole_dataset()
    #   
    #   flog.info("test if spatial filter TRUE !!!")
    #   
    #   if(wkt==all_wkt){
    #     flog.info("No spatial filter either !!")
    #     current_selection_footprint_wkt(all_wkt)
    #     main_df<- main_data
    #   }else{
    #     flog.info("Listing remaining areas within the new WKT: %s", wkt)
    #     # current_wkt(wkt)
    #     current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
    #     current_df_distinct_geom <- df_distinct_geom %>% dplyr::filter(gridtype %in% input$gridtype)
    #     list_areas <- process_list_areas(current_df_distinct_geom, current_selection)
    #     flog.info("Remaining number of different areas within this WKT: %s", nrow(list_areas))
    #     within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
    #       rename_at(1,~"codesource_area") %>% dplyr::select(codesource_area) %>% pull()
    #     current_selection_footprint_wkt(all_wkt)
    #     flog.info("Laoding all grouped data")
    #     main_df<- main_data %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
    #   }
    # }else{
    #   flog.info("Some filters have been selected : checking if new selection is refining the previous one")
    #   # reset_all_filters <- FALSE
    #   if(wkt != target_wkt){
    #     flog.info("Listing remaining areas within this new WKT: %s", wkt)
    #     # current_wkt(wkt)
    #     current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
    #     current_df_distinct_geom <- df_distinct_geom %>% dplyr::filter(gridtype %in% input$gridtype)
    #     list_areas <- process_list_areas(current_df_distinct_geom, current_selection)
    #     flog.info("Remaining number of different areas within this WKT: %s", nrow(list_areas))
    #     within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
    #       rename_at(1,~"codesource_area") %>% dplyr::select(codesource_area) %>% pull()
    #   }
    #   flog.info("Testing if non spatial filters have been updated !")
    #   if(all(input$dataset == current_dataset()) && 
    #      all(input$species == current_species()) && 
    #      all(input$source_authority == current_source_authority()) && 
    #      all(input$gear_type == current_gear_type()) && 
    #      all(input$year == current_year()) && 
    #      all(input$fishing_fleet == current_fishing_fleet()) && 
    #      all(input$unit == current_unit())
    #   ){
    #     
    #     flog.info("Non spatial filters are the same => Loading pre-filtered dataset !!")
    #     flog.info("main_df  rows: %s", nrow(filtered_default_df()))
    #     if(wkt == target_wkt){
    #       flog.info("Default WKT nothing has changed!!!")
    #       main_df <- filtered_default_df()
    # 
    #     }else{
    #       flog.info("Same filters but new WKT => filtering remaining areas")
    #       main_df <- whole_filtered_df() %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
    #       
    #     }
    #   }else if(length(setdiff(input$dataset,current_dataset())) == 0 && 
    #            length(setdiff(input$species,current_species())) == 0 && 
    #            length(setdiff(input$source_authority,current_source_authority())) == 0 && 
    #            length(setdiff(input$gear_type,current_gear_type())) == 0 && 
    #            length(setdiff(input$year,current_year())) == 0 && 
    #            length(setdiff(input$fishing_fleet,current_fishing_fleet())) == 0 && 
    #            length(setdiff(input$unit,current_unit())) == 0  
    #            ){ 
    #     flog.info("New filters are just a subset of previous ones : refining  !!")
    #     flog.info("Loading new values")
    #     
    #     current_dataset(input$dataset)
    #     current_species(input$species)
    #     current_source_authority(input$source_authority)
    #     current_gear_type(input$gear_type)
    #     current_year(input$year)
    #     current_fishing_fleet(input$fishing_fleet)
    #     current_unit(input$unit)        
    #     
    #     flog.info("Subsetting / refining previous dataset with new values")
    #     whole_filtered_df <- whole_filtered_df()
    #     tmp_main_df <- whole_filtered_df  %>% filter(!is.na(geom_wkt)) %>%  
    #       dplyr::filter(
    #         dataset %in% input$dataset,
    #         species %in% input$species,
    #         source_authority %in% input$source_authority,
    #         gear_type %in% input$gear_type,
    #         year %in% input$year,
    #         fishing_fleet %in% input$fishing_fleet,
    #         measurement_unit %in% input$unit
    #       )
    #     
    #     # %>% 
    #     #   dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, year, measurement_unit) %>%
    #     #   # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
    #     #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    #     
    #     flog.info("Footprint of all grouped filtered data")
    #     this_footprint <- tmp_main_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
    #       dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
    #       st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine() %>% st_simplify() %>% st_as_text()
    #     # flog.info("Current footprint for filters is %s: ",whole_footprint)
    #     current_selection_footprint_wkt(this_footprint)
    #     
    #     whole_filtered_df(tmp_main_df)
    #     default_df <- tmp_main_df  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
    #     filtered_default_df(default_df)
    #     main_df <- default_df
    #     
    #     }else{
    #     flog.info("Non spatial filters are different => applying these new filters to the whole dataset !!")
    #     flog.info("input$dataset : %s", all(input$dataset == current_dataset()))
    #     if(!all(input$dataset == current_dataset())){current_dataset(input$dataset)}
    #     
    #     flog.info("input$species : %s", all(input$species == current_species()))
    #     flog.info("input$species : %s", print(input$species))
    #     if(!all(input$species == current_species())){current_species(input$species)}
    #     
    #     flog.info("input$source_authority : %s", all(input$source_authority == current_source_authority()))
    #     if(!all(input$source_authority == current_source_authority())){current_source_authority(input$source_authority)}
    #     flog.info("input$gear_type : %s", all(input$gear_type == current_gear_type()))
    #     if(!all(input$gear_type == current_gear_type())){current_gear_type(input$gear_type)}
    #     flog.info("input$year : %s", all(input$year == current_year()))
    #     if(!all(input$year == current_year())){current_year(input$year)}
    #     flog.info("input$fishing_fleet : %s", all(input$fishing_fleet == current_fishing_fleet()))
    #     if(!all(input$fishing_fleet == current_fishing_fleet())){current_fishing_fleet(input$fishing_fleet)}
    #     flog.info("input$unit : %s", all(input$unit == current_unit()))
    #     if(!all(input$unit == current_unit())){current_unit(input$unit)}
    #     
    #     
    #     flog.info("Laoding all grouped data")
    #     main_data <- whole_dataset()
    #     
    #     flog.info("Filtering all grouped data")
    #     
    #     tmp_main_df <- main_data  %>% filter(!is.na(geom_wkt)) %>%  
    #       dplyr::filter(
    #         dataset %in% input$dataset,
    #         species %in% input$species,
    #         source_authority %in% input$source_authority,
    #         gear_type %in% input$gear_type,
    #         year %in% input$year,
    #         fishing_fleet %in% input$fishing_fleet,
    #         measurement_unit %in% input$unit
    #       )
    #     # %>% 
    #     #   dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, year, measurement_unit) %>%
    #     #   # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
    #     #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    #     
    #     flog.info("Footprint of all grouped filtered data")
    #     this_footprint <- tmp_main_df  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
    #       dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
    #       st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine() %>% st_simplify() %>% st_as_text()
    #     # flog.info("Current footprint for filters is %s: ",whole_footprint)
    #     current_selection_footprint_wkt(this_footprint)
    #     
    #     whole_filtered_df(tmp_main_df)
    #     default_df <- tmp_main_df  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
    #     filtered_default_df(default_df)
    #     
    #     main_df <- default_df
    #   }
    # }
    flog.info("Check number of rows of main df")
    if(nrow(main_df)==0)
      showModal(modalDialog(
        title = "Warning",
        "No data left with current filters, back to default filters!",
        easyClose = TRUE,
        footer = NULL
      ))else{
        main_df
      }
    
    
  },
  ignoreInit = FALSE,ignoreNULL = FALSE)
  # ignoreInit = TRUE, once = TRUE)
  
  map_df <- reactive({
    
    # map_df <- whole_map_df
    req(main_df())
    main_df <- main_df()
    flog.info("###############################################################################################")
    flog.info("Applying new filters to MAP data")
    flog.info("###############################################################################################")
    # map_wkt <- current_wkt()
    #
    #   flog.info("###############################################################################################")
    #   flog.info("Applying new filters to main data 2 ")
    #   flog.info("###############################################################################################")
    #
    #   current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
    #
    #   flog.info("Spatial filter : keep only data whose areas are within the current WKT : %s", wkt)
    #
    #   if(wkt!=all_wkt){
    #   list_areas <- process_list_areas(df_distinct_geom, current_selection)
    #
    #   flog.info("Remaining number of different areas within this WKT: %s", length(list_areas))
    #   within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
    #     rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
    #
    #   map_df <- main_df %>% filter(!is.na(geom)) %>%  dplyr::filter(codesource_area %in% within_areas)
    # }else{
    #   map_df <- main_df
    # }
    # map_df <- main_df  %>% dplyr::left_join(dplyr::as_tibble(df_distinct_geom), by=c('codesource_area')) %>%
    #   dplyr::mutate(geom=st_as_text(st_sfc(geom_wkt),EWKT = TRUE))
    
    map_df <- main_df %>% 
      dplyr::group_by(geom_wkt, dataset, measurement_unit) %>%
      # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(wkt="geom_wkt",crs=4326)
    # flog.info("Number of rows of map data : %s", nrow(map_df))
    
    # flog.info("Main data number rows: %s", nrow(map_df))
    # # https://shiny.posit.co/r/reference/shiny/latest/modaldialog
    # if(nrow(map_df)==0)
    #   showModal(modalDialog(
    #     title = "Warning",
    #     "No data left with current filters !",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))else{
    #     map_df
    #   }
    
    
  })
  
  
  
  
  plot_df <- reactive({
    flog.info("###############################################################################################")
    flog.info("Applying new filters to PLOT data")
    flog.info("###############################################################################################")
    
    # plot_df <- whole_plot_df
    req(main_df())
    main_df <- main_df()
    plot_df <- main_df  %>% 
      dplyr::group_by(dataset, year, gridtype, measurement_unit) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    # flog.info("Number of rows of plot data : %s", nrow(plot_df))
  })
  
  
  flog.info("##########################################################")
  flog.info("Outputs: text & Data tables")
  flog.info("##########################################################")
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", current_wkt())
  })
  
  
  # output$updatedWKT <- renderText({input$yourWKT})
  
  output$verbatimWKT <- renderText({
    current_wkt()
  })
  
  output$current_filters <- renderText({ 
    species_list <- input$species     
    # within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%  rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
    year_list <- input$year
    fishing_fleet_list <- input$fishing_fleet
    paste("You have selected the following filters:\n", class(species_list))
  })
  
  output$DT_main_dataset <- renderDT({
    main_df() %>% top_n(10)
  })
  
  flog.info("##########################################################")
  flog.info(" Modules forOutputs: maps / plots / charts")
  flog.info("##########################################################")
  
  
  flog.info("Starting leaflet in the global map module")
  # callModule(module = map_leaflet, id = "id_1")
  map_leafletServer(id = "map_global",map_df,map_wkt)
  
  flog.info("Starting time series module")
  timeSeriesServer(id = "time_series",plot_df)
  
  flog.info("Starting pie and bar Charts module")
  pieBarChartsServer(id= "pie_bar_charts",plot_df)
  
  flog.info("Extra module to detail what gears are the most important in the time series of catches")
  # timeSeriesGearServer(id= "time_series_gear",plot_df)
  
  # nav_bar_menu_rmd <- c(
  #   "rmd/Authors.Rmd", 
  #   "rmd/Fundings.Rmd", 
  #   "rmd/sidebar_explenations.Rmd", 
  #   "rmd/General_disclaimer.Rmd", 
  #   "rmd/Running_the_app.Rmd"
  # )
  # render_rmd_files <- function(rmd_files, output_dir = "www") {
  #   # Render all Rmd files to HTML if necessary
  #   html_files <- lapply(rmd_files, render_rmd_to_html, output_dir = output_dir)
  #   return(unlist(html_files))
  # }
  # nav_bar_menu_html <- render_rmd_files(nav_bar_menu_rmd)
  # aboutServer("about", rmd_paths=nav_bar_menu_html)
  
  onStop(function() {
    # dbDisconnect(con)
  })
  
}