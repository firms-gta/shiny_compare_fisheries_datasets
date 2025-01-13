server <- function(input, output, session) {
  
  ########################################################## Function for spatial filters ########################################################## 
  
  
  process_list_areas <- function(df_distinct_geom, current_selection) {
    
    # Vérifier si qgisprocess est installé
    if (requireNamespace("qgisprocess", quietly = TRUE)) {
      
      # Essayer de configurer qgisprocess pour voir s'il fonctionne
      qgis_path <- try(qgisprocess::qgis_configure(), silent = TRUE)
      flog.info("QGIS is used to select remaing data within the default or newly dran polygon !")
      
      if (!inherits(qgis_path, "try-error") && !is.null(qgis_path)) {
        # Utiliser qgisprocess si disponible et configuré
        message("Utilisation de qgisprocess pour traiter les données.")
        list_areas <- df_distinct_geom %>%
          dplyr::filter(!is.na(gridtype)) %>%
          qgisprocess::qgis_run_algorithm(
            "native:extractbylocation",
            INPUT = .,
            PREDICATE = "are within",
            INTERSECT = st_sf(current_selection)
          ) %>% 
          sf::st_as_sf()
        
        return(list_areas)
      }
    }
    
    # Si qgisprocess n'est pas disponible ou configuré, utiliser sf
    message("qgisprocess non disponible ou non configuré. Utilisation de sf pour traiter les données.")
    flog.info("QGIS instead")
    list_areas <- df_distinct_geom %>%
      dplyr::filter(!is.na(gridtype)) %>%
      dplyr::filter(sf::st_within(., st_sf(current_selection), sparse = FALSE)) %>%
      sf::st_as_sf()
    
    return(list_areas)
  }
  

  ########################################################## Dynamic filters ########################################################## 
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear_type),collapse="|"),"|",fixed=TRUE))
  })

  observeEvent(input$yourWKT,{
    updateSelectInput(session = session,
                      inputId = "yourWKT",
                      selected = main_wkt())
  })
  
  # observeEvent(updated_main_wkt$updated_main_wkt(), {
  #   req(main_wkt())
  #   if(updated_main_wkt$updated_main_wkt() != main_wkt()){
  #     main_wkt(updated_main_wkt$updated_main_wkt())
  #     submitTrigger(TRUE)
  #   }
  # })
  
  observeEvent(input$resetWkt, {
    main_wkt(default_wkt)
    output$verbatimWKT <- renderText({
      default_wkt
    })
    # updateTextInput(session,"yourWKT", value = main_wkt())
    #   updateTextInput(session,ns("yourWKT"), value = main_wkt())
  })
  
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"gear_type",choices = unique(temp$gear_type),selected=unique(temp$gear_type))
  }
  )
  
  
  # observeEvent(input$gear_type,{
  #   if(!'All' %in% input$gear_type) {
  #     updateSelectInput(session,"gear_type",choices = unique(temp$gear_type),selected=unique(temp$gear_type))
  #   }
  # }
  # )
  # if All is not in selection, filter to selected continents

  
  observeEvent(input$switched, {
    if(switch_unit()){switch_unit(FALSE)}else{switch_unit(TRUE)}
  })
  
  # observeEvent(input$year,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])
  #   updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  # }
  # )
  flog.info("##########################################################")
  flog.info("set the main parameterized query (options for geom might be st_collect(geom) or  ST_ConvexHull(st_collect(geom)) as convexhull )")
  
  flog.info("Apply current filters to the main datasets when click on submit")
  sql_query_all <- eventReactive(input$submit, {
    
    # sql_query <- reactive({
    main_data <- initial_data()
    
    req(main_wkt())
    wkt <- main_wkt()
    current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
    
    current_df_distinct_geom <- df_distinct_geom %>% dplyr::filter(gridtype %in% input$gridtype)
    list_areas <- process_list_areas(current_df_distinct_geom, current_selection)
    flog.info("Remaining number of different areas within this WKT: %s", length(list_areas))
    within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
      rename_at(1,~"codesource_area") %>% dplyr::select(codesource_area) %>% pull()

    sql_query_all <- main_data  %>% filter(!is.na(geom_wkt)) %>%  
      dplyr::filter(
        dataset %in% input$dataset,
        species %in% input$species,
        source_authority %in% input$source_authority,
        gear_type %in% input$gear_type,
        year %in% input$year,
        fishing_fleet %in% input$fishing_fleet,
        measurement_unit %in% input$unit
      ) %>% 
      dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, year, measurement_unit) %>% 
      # dplyr::group_by(codesource_area, gridtype, geom_wkt, dataset, source_authority, species, gear_type, year, measurement_unit) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup()
    
     whole_footprint <- sql_query_all  %>% dplyr::group_by(codesource_area, geom_wkt) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%  
      st_as_sf(wkt="geom_wkt",crs=4326) %>% st_combine()  %>% st_as_text()
    flog.info("Current footprint for filters is %s: ",whole_footprint)
    current_selection_footprint_wkt(whole_footprint)
    
    sql_query_all <- sql_query_all  %>% filter(!is.na(geom_wkt)) %>% dplyr::filter(codesource_area %in% within_areas)
    
    },
  ignoreNULL = FALSE)
  

  sql_query <- reactive({
    req(sql_query_all())
    sql_query_all <- sql_query_all()
    flog.info("###############################################################################################") 
    flog.info("Applying new filters to main data") 
    flog.info("###############################################################################################") 
  #   
  #   req(main_wkt())
  #   wkt <- main_wkt()    
  #   flog.info("Spatial filter: main WKT : %s", wkt)
  #   
  #   flog.info("###############################################################################################") 
  #   flog.info("Applying new filters to main data 2 ") 
  #   flog.info("###############################################################################################") 
  #   
  #   current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
  #   
  #   flog.info("Spatial filter : keep only data whose areas are within the current WKT : %s", wkt)
  #   
  #   if(wkt!=default_wkt){
  #   list_areas <- process_list_areas(df_distinct_geom, current_selection)
  #   
  #   flog.info("Remaining number of different areas within this WKT: %s", length(list_areas))
  #   within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>% 
  #     rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
  #   
  #   sql_query <- sql_query_all %>% filter(!is.na(geom)) %>%  dplyr::filter(codesource_area %in% within_areas) 
  # }else{
  #   sql_query <- sql_query_all
  # }
    # sql_query <- sql_query_all  %>% dplyr::left_join(dplyr::as_tibble(df_distinct_geom), by=c('codesource_area')) %>% 
    #   dplyr::mutate(geom=st_as_text(st_sfc(geom_wkt),EWKT = TRUE))
    
    sql_query <- sql_query_all 
    
  flog.info("Main data number rows: %s", nrow(sql_query))
  # https://shiny.posit.co/r/reference/shiny/latest/modaldialog
  if(nrow(sql_query)==0)
    showModal(modalDialog(
      title = "Warning",
      "No data left with current filters !",
      easyClose = TRUE,
      footer = NULL
    ))else{
      sql_query 
    }
  })
  
  
  
  flog.info("##########################################################")
  flog.info("Outputs: text & Data tables")
  flog.info("##########################################################")
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", main_wkt())
  })
  
  
  # output$updatedWKT <- renderText({input$yourWKT})
  
  output$verbatimWKT <- renderText({
    main_wkt()
  })
  
  output$current_filters <- renderText({ 
    # paste("Your SQL Query is : \n", sql_query())
    species_list <- input$species     
    # within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%  rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
    year_list <- input$year
    fishing_fleet_list <- input$fishing_fleet
    paste("You have selected the following filters:\n", class(species_list))
  })
  
  output$DT_main_dataset <- renderDT({
    sql_query() %>% top_n(10)
  })
  
  flog.info("##########################################################")
  flog.info(" Modules forOutputs: maps / plots / charts")
  flog.info("##########################################################")
  
  
  flog.info("Starting leaflet in the global map module")
  # callModule(module = map_leaflet, id = "id_1")
  map_leafletServer(id = "map_global",sql_query)
  
  flog.info("Starting time series module")
  timeSeriesServer(id = "time_series",sql_query)
    
  flog.info("Starting pie and bar Charts module")
  pieBarChartsServer(id= "pie_bar_charts",sql_query)
  
  flog.info("Extra module to detail what gears are the most important in the time series of catches")
  # timeSeriesGearServer(id= "time_series_gear",sql_query)
  
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
