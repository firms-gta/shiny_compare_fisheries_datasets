server <- function(input, output, session) {
  
  ########################################################## Dynamic filters ########################################################## 
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear_type),collapse="|"),"|",fixed=TRUE))
  })
  
  # observeEvent(input$yourWKT,{
  #   updateSelectInput(session = session,
  #                     inputId = "yourWKT",
  #                     selected = wkt())
  # })
  
  # observeEvent(input$resetWkt, {
  #   wkt(default_wkt)
  #   updateTextInput(session,ns("yourWKT"), value = wkt())
  # })
  
  observeEvent(input$resetWkt, {
    wkt(default_wkt)
    # updateTextInput(session,"yourWKT", value = wkt())
  })
  
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"gear_type",choices = unique(temp$gear_type),selected=unique(temp$gear_type))
  }
  )
  
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
  
  # list_areas <-  eventReactive(input$mymap_draw_new_feature, {
  #   # feature <- input$mymap_draw_new_feature
  #   # geoJson <- geojsonio::as.json(feature)
  #   # geom <- st_read(geoJson) # wkt(st_as_text(st_geometry(geom[1,])))
  #   current_selection <- st_as_sfc(wkt, crs = 4326)
  #   df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(wkt()), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
  # },
  # ignoreNULL = FALSE)
  
  flog.info("Apply current filters to the main datasets when click on submit")
  sql_query_all <- eventReactive(input$submit, {
    
    # sql_query <- reactive({
    main_data <- initial_data()

    sql_query_all <- main_data  %>%  dplyr::filter(
      # codesource_area %in% within_areas,
      dataset %in% input$dataset,
      species %in% input$species,
      gear_type %in% input$gear_type,
      year %in% input$year,
      fishing_fleet %in% input$fishing_fleet,
      measurement_unit %in% input$unit,
      gridtype %in% input$gridtype) %>%
      dplyr::group_by(codesource_area,geom, dataset, species,gear_type, year, measurement_unit, gridtype) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>%
      filter(!is.na(geom))
    
  },
  ignoreNULL = FALSE)

  sql_query_footprint <- reactive({
    # flog.info("Create spatial footprints for current filters")
    # req(sql_query_all())
    # sql_query_all <- sql_query_all()
    flog.info("###############################################################################################") 
    flog.info("spatial footprints for current filters number of row is: %s", nrow(sql_query_footprint)) 
    flog.info("###############################################################################################") 
    
    sql_query_footprint <-  sql_query_all() %>% dplyr::group_by(codesource_area,geom) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>% dplyr::filter(!is.na(geom)) %>% 
      st_as_sf(wkt="geom", crs = 4326) #%>% st_combine()
    
  })
  
  sql_query <- reactive({
    req(sql_query_all())
    sql_query_all <- sql_query_all()
    flog.info("###############################################################################################") 
    flog.info("Applying new filters to main data") 
    flog.info("###############################################################################################") 
    
    req(wkt())
    wkt <- wkt()    
    
    flog.info("###############################################################################################") 
    flog.info("Applying new filters to main data 2 ") 
    flog.info("###############################################################################################") 
    
    current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
    
    flog.info("Spatial filter : keep only data whose areas are within the current WKT : %s", wkt)
    list_areas <- df_distinct_geom %>% dplyr::filter(!is.na(gridtype)) %>% 
      qgisprocess::qgis_run_algorithm("native:extractbylocation",INPUT = ., PREDICATE = "are within", INTERSECT = st_sf(current_selection)) %>% sf::st_as_sf()
    flog.info("Remaining number of different areas within this WKT: %s", length(list_areas))
    within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%  rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
    
    sql_query <- sql_query_all  %>%  dplyr::filter(codesource_area %in% within_areas)
  
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
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", wkt())
  })
  
  
  # output$updatedWKT <- renderText({input$yourWKT})
  
  output$verbatimWKT <- renderText({
    wkt()
  })
  
  output$sql_query <- renderText({ 
    # paste("Your SQL Query is : \n", sql_query())
    paste("You have selected the following filters:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", wkt())
  })
  
  
  output$query_all_datasets <- renderText({ 
    # paste("Your SQL Query is : \n", query_all_datasets())
    paste("\n \n \n \n \n", "Here is the list of id areas within the current WKT : \n", list_areas())
    # list_areas()
    
  })
  
  
  flog.info("##########################################################")
  flog.info(" Modules forOutputs: maps / plots / charts")
  flog.info("##########################################################")
  
  
  flog.info("Starting leaflet in the global map module")
  # callModule(module = map_leaflet, id = "id_1")
  map_leafletServer(id = "map_global",sql_query,sql_query_footprint)
  
  flog.info("Starting time series module")
  timeSeriesServer(id = "time_series",sql_query)
    
  flog.info("Starting pie and bar Charts module")
  pieBarChartsServer(id= "pie_bar_charts",sql_query)
  
  flog.info("Extra module to detail what gears are the most important in the time series of catches")
  timeSeriesGearServer(id= "time_series_gear",sql_query)
  
  
  onStop(function() {
    # dbDisconnect(con)
  })
  
  
}