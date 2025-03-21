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
    
    # current_dataset(list_values_dimensions$dataset)
    shinyWidgets::updatePickerInput(session,"dataset",selected=list_values_dimensions$dataset)
    
    # current_species(list_values_dimensions$species)
    shinyWidgets::updatePickerInput(session,"species",selected=list_values_dimensions$species,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,
                                       yes = "color: rgba(119, 119, 119, 0.5);",
                                       no = "")
                      ))
    
    # current_gear_type(list_values_dimensions$gear_type)
    shinyWidgets::updatePickerInput(session,"gear_type",selected=list_values_dimensions$gear_type)
    
    # current_year(list_values_dimensions$year)
    shinyWidgets::updatePickerInput(session,"year",selected=list_values_dimensions$year)
    
    # current_fishing_fleet(list_values_dimensions$fishing_fleet)
    shinyWidgets::updatePickerInput(session,"fishing_fleet",selected=list_values_dimensions$fishing_fleet)
    
    # current_unit(list_values_dimensions$measurement_unit)
    shinyWidgets::updatePickerInput(session,"unit",selected=list_values_dimensions$measurement_unit)
    
    # current_source_authority(list_values_dimensions$source_authority)
    shinyWidgets::updatePickerInput(session,"source_authority",selected=list_values_dimensions$source_authority)
    
    # current_gridtype(list_values_dimensions$gridtype)
    shinyWidgets::updatePickerInput(session,"gridtype",selected=list_values_dimensions$gridtype)
    
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
    wkt <- current_wkt()
    map_wkt(wkt)
    # flog.info("Define the rules to update the spatial filtering of list of areas within the WKT !!")
    if (wkt == list_default_filters$wkt){
      flog.info("************ Default  within_areas Nothing to do ***************************** !!")
      within_areas <- list_default_filters$within_areas
    } else if( (wkt != last_wkt() && !(wkt == all_wkt && all(input$gridtype == current_gridtype()))) ||
        (wkt==last_wkt() && !all(input$gridtype == current_gridtype())) || 
        !all(input$gridtype == current_gridtype()) ){
      flog.info("************ YES UPDATE within_areas ***************************** !!")
      within_areas <- process_list_areas(df_distinct_geom, wkt=current_wkt(), list_gridtype=input$gridtype) 
    }else{
      flog.info("************ NO UPDATE within_areas ***************************** !!")
      within_areas <- NULL
    } 
    
    list_filters <- list("dataset"=input$dataset,"species"=input$species,"source_authority"=input$source_authority,
                         "gear_type"=input$gear_type,"year"=input$year,"fishing_fleet"=input$fishing_fleet,
                         "gridtype"=input$gridtype,"unit"=input$unit,"wkt"= wkt,"within_areas"=within_areas)
    
    flog.info("Check if filters are the same or if they have been updated")
    if(all(input$dataset == current_dataset()) && 
       all(input$species == current_species()) && 
       all(input$source_authority == current_source_authority()) && 
       all(input$gear_type == current_gear_type()) && 
       all(input$year == current_year()) && 
       all(input$fishing_fleet == current_fishing_fleet()) && 
       all(input$unit == current_unit()) && 
       all(input$gridtype == current_gridtype())
    ){
      flog.info("--------------------------------------------")
      flog.info("USE CASE 1: Non spatial filters => same / not updated")
      flog.info("USE CASE 1: Check if the Spatial filters / WKT has been updated")
      
      if(wkt == last_wkt()){
        flog.info("USE CASE 1: Spatial filters => same WKT as previous one => Nothing to do !! Exactly the same dataset !!")
        flog.info("--------------------------------------------")
        main_df <- filtered_default_df()
      }else{
        main_data <- whole_filtered_df()
        if(wkt==all_wkt) {
          flog.info("USE CASE 1: Spatial filters => none / loading whole dataset !!")
          flog.info("--------------------------------------------")
          filtered_default_df(main_data)
          main_df <- main_data
        }else{
          flog.info("USE CASE 1: Spatial filters => new WKT applied to filter the whole dataset !!")
          flog.info("--------------------------------------------")
          flog.info("Listing remaining areas within the new WKT: %s", wkt)
          new_data <- main_data %>% filter(!is.na(codesource_area)) %>% 
            dplyr::filter(codesource_area %in% within_areas)
          filtered_default_df(new_data)
          # wkt(new_df_footprint)
          main_df <- new_data
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
      flog.info("USE CASE 2: Non spatial filters updated =>  subset of previous ones without additionnal filters")
      flog.info("USE CASE 2: Non spatial filters updated =>  we will just refine current data")
      flog.info("--------------------------------------------")
     
      flog.info("Starting from previous dataset to refine / subset it with new values")
      main_data <- whole_filtered_df()
      updates <- apply_filters(df=main_data, list_filters=list_filters)
      filtered_default_df(updates$filtered_default_df)
      current_selection_footprint_wkt(updates$current_selection_footprint_wkt)
      update_current_filters(list_filters_values = list_filters)
      whole_filtered_df(updates$whole_filtered_df)
      main_df <- updates$filtered_default_df
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

      if(all(input$dataset == list_values_dimensions$dataset) && 
         all(input$species == list_values_dimensions$species) && 
         all(input$source_authority == list_values_dimensions$source_authority) && 
         all(input$gear_type == list_values_dimensions$gear_type) && 
         all(input$year == list_values_dimensions$year) && 
         all(input$fishing_fleet == list_values_dimensions$fishing_fleet) && 
         all(input$unit == list_values_dimensions$measurement_unit) && 
         all(input$gridtype == list_values_dimensions$gridtype)
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
             default_df <- main_data %>% filter(!is.na(codesource_area)) %>% 
               dplyr::filter(codesource_area %in% within_areas)
             filtered_default_df(default_df)
             main_data <- default_df
           }else{
             filtered_default_df(main_data)
           }
           main_df <- main_data
         }
         else{
           flog.info("--------------------------------------------")
           flog.info("USE CASE 3: Not a full reset / just few additional filters not present in the previous filters")
           flog.info("USE CASE 3: => filtering the whole dataset / all grouped data")
           flog.info("--------------------------------------------")
           updates <- apply_filters(df=main_data, list_filters=list_filters)
           filtered_default_df(updates$filtered_default_df)
           current_selection_footprint_wkt(updates$current_selection_footprint_wkt)
           update_current_filters(list_filters_values = list_filters)
           whole_filtered_df(updates$whole_filtered_df)
           
           # if(updates$whole_filtered_df==updates$filtered_default_df){
           #   map_wkt(updates$current_selection_footprint_wkt)
           # }
           main_df <- updates$filtered_default_df
         }
      }
    
    # flog.info("Check number of rows of main df")
    # if(nrow(main_df)==0){
    #   showModal(modalDialog(
    #     title = "Warning",
    #     "No data left with current filters, back to default filters!",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # main_df <- filtered_default_df()
    # }else{
    #     main_df
    #   }
    
  },
  ignoreInit = FALSE,ignoreNULL = FALSE)
  # ignoreInit = TRUE, once = TRUE)
  
  map_df <- reactive({
    
    # map_df <- whole_map_df
    req(main_df())
    shiny::validate(
      need(nrow(main_df())>0, 'Sorry no data left with current filters !'),
      errorClass = "myClass"
    )
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
    # if(!exists("df_distinct_geom_light")){
    # df_distinct_geom_light <- qs::qread("data/df_distinct_geom_light.qs")
    # }
    
    map_df <- main_df %>%
      dplyr::group_by(codesource_area, dataset, measurement_unit) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::left_join(df_distinct_geom, by=c('codesource_area'))  %>% st_sf()
    
    # map_df <- main_df %>%
    #   dplyr::group_by(codesource_area, dataset, measurement_unit) %>%
    #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
    #   ungroup()
    # map_df <- df_distinct_geom %>% dplyr::right_join(map_df, by=c('codesource_area')) %>% st_sf()
    # 
    # map_df <- map_df %>%
    #   dplyr::mutate(geom_wkt = as.character(geom_wkt)) 
    # 
    # map_df <- map_df %>%
    #   dplyr::mutate(geometry = purrr::map(geom_wkt, ~ {
    #     tryCatch(st_as_sfc(.x, crs=4326), error = function(e) NA)
    #   })) %>%
    #   dplyr::filter(!is.na(geometry))  %>%
    #   st_as_sf(wkt="geom_wkt", crs=4326) #hotfix for now
    
    # map_df <- main_df %>%
    #   dplyr::left_join((df_distinct_geom_light ), by=c('codesource_area'))%>% 
    #   dplyr::group_by(geom_wkt, dataset, measurement_unit) %>%
    #   # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>%
    #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(wkt="geom_wkt",crs=4326)
    # flog.info("Number of rows of map data : %s", nrow(map_df))
    # rm(df_distinct_geom_light)
    # gc()
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
    map_df
    
  }) 
  
  
  plot_df <- reactive({
    flog.info("###############################################################################################")
    flog.info("Applying new filters to PLOT data")
    flog.info("###############################################################################################")
    # plot_df <- whole_plot_df
    req(main_df())
    shiny::validate(
      need(nrow(main_df())>0, 'Sorry no data left with current filters !'),
      errorClass = "myClass"
    )
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
  
  output$DT_DOIs <- renderDT({#escape = FALSE,
    DOIs_metadata <- readr::read_csv(here::here("data/DOIs_enriched.csv")) %>% 
      dplyr::select(c("identifier","title","DOI","Filename")) %>% 
      dplyr::mutate(url= '<a href = \"https://doi.org/10.5281/zenodo.11410529\">https://doi.org/10.5281/zenodo.11410529</a>')
  })
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('doc/table.Rmd', quiet = TRUE)))
  })
  
  # fmt_markdown(columns =  ) |> 
    
  DOIs_metadata <- readr::read_csv(here::here("data/DOIs_enriched.csv")) %>% 
    # dplyr::select(c("identifier","title","DOI","Filename")) %>% dplyr::rename("Identifier"=identifier,"Title"=title) %>% 
    dplyr::select(c("Rmd_badge","URL","Filename","title","Citation")) %>% 
    # dplyr::mutate(Citation= RefManageR::Cite(RefManageR::GetBibEntryWithDOI(URL), .opts = list(style = "text", bib.style = "authoryear")))  %>% 
    dplyr::rename("Title"=title,Url= URL) |>
    gt() |> 
    tab_header(
      title = "Undelrying datasets",
      subtitle = "Zenodo DOIs"
    )  |> 
    fmt_markdown(columns = Rmd_badge) |> 
    fmt_markdown(columns = Citation) |> 
    fmt_url(columns = Url)  |> 
    tab_style(
      style = cell_borders(
        # sides = c("t", "l"),
        color = "black",
        weight = px(3)
      ),
      locations = cells_body()
    ) 
  
  output$tableDOIs <- render_gt(expr = DOIs_metadata, width = "100%",height="auto")
  
  
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