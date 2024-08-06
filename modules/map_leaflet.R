map_leafletUI <- function(id) {
  ns <- NS(id)
  tagList(
  leafletOutput(ns("map"),width="100%", height="100%"),
  map_proxy_UI(ns("other"))
  )
}

map_leafletServer <- function(id,sql_query) {
  flog.info("Starting global map module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_map <- reactive({
      # req(sql_query())
      this_df <- sql_query()
      flog.info("Main data number of rows before leaflet map pre-procesing : %s", nrow(this_df))
      # wkt <- "POLYGON ((-159.6094 -58.81374, -159.6094 43.06889, 112.5 43.06889, 112.5 -58.81374, -159.6094 -58.81374))"
      
      data_map <- this_df %>% filter(!is.na(gridtype)) %>% dplyr::group_by(dataset,codesource_area,geom) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE))  %>%  ungroup() %>% st_as_sf(wkt="geom",crs=4326)
        # left_join(df_distinct_geom,by="codesource_area")
      
      # data_map <- df_distinct_geom %>% left_join(list_cwp)
      # data_map <- df_distinct_geom  %>% merge(list_cwp) %>% filter(!st_is_empty(.))
      # list_cwp  %>% left_join(df_distinct_geom)  %>% filter(!st_is_empty(.))
      flog.info("Data map head : %s", head(data_map))
      flog.info("Data map number of rows data nrow: %s", nrow(data_map))
      data_map
    }) 
    
    output$map <- renderLeaflet({
      flog.info("Testing truthiness of the dataframe with req()")
      req(data_map())
      req(wkt())
      wkt <- wkt()
      current_selection <- st_as_sfc(wkt, crs = 4326)
      # flog.info("Check current value of WKT  : %s", wkt)
      spatial_footprint_1 <- df_distinct_geom  %>% dplyr::filter(gridtype == '1deg_x_1deg') %>% dplyr::filter(st_contains(current_selection, sparse = FALSE))  %>% st_combine() #%>% st_convex_hull()
      spatial_footprint_5 <- df_distinct_geom  %>% dplyr::filter(gridtype == '5deg_x_5deg') %>% dplyr::filter(st_contains(current_selection, sparse = FALSE))  %>% st_combine() #%>% st_convex_hull()
      remaining_polygons <- df_distinct_geom  %>%  dplyr::filter(st_contains(current_selection, sparse = FALSE))
      all_polygons <- df_distinct_geom %>% st_combine() 
      
      # df_distinct_geom  %>% dplyr::filter(gridtype == '5deg_x_5deg')  %>% st_within(.,st_as_sfc(bbox, crs = 4326), sparse = FALSE)
      
      # flog.info("Filtering with new WKT  : %s", wkt)
      # bbx <- st_as_sfc(wkt(), crs = 4326)
      
      # req(data_map())
      df <- data_map()
      flog.info("Data map head again: %s", head(df))
      
      flog.info("Class of map data object : %s", class(df))
      flog.info("Column names of map data: %s", as.character(colnames(df)))
      flog.info("Number of rows of map data : %s", nrow(df))
      
      
      convex_hull <- st_convex_hull(df)
      # bbox <- st_bbox(convex_hull) %>% as.vector()  
      # req(wkt())
      # wkt <- wkt()    
      # bbox <-  st_bbox(st_as_sfc(wkt() , crs = 4326)) %>% as.vector()
      centroid <-  convex_hull %>% st_centroid()
      lat_centroid <- st_coordinates(centroid, crs = 4326)[2]
      lon_centroid <- st_coordinates(centroid, crs = 4326)[1]
      flog.info("lat_centroid : %s",lat_centroid)
      datasets <- unique(df$dataset)
      flog.info("listing different datasets : %s", datasets)
      
      flog.info("Setting the palette")
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = df$measurement_value
      )
      # brewer.pal(7, "OrRd")
      pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
      
      qpal <- colorQuantile(rev(viridis::viridis(10)),
                            df$measurement_value, n=10)
      
      # https://r-spatial.github.io/sf/articles/sf5.html
      map <- leaflet(
        options=leafletOptions(doubleClickZoom = T, dragging=T,scrollWheelZoom=T
                               # zoom to limits? https://rdrr.io/cran/leaflet/man/mapOptions.html
        )
      ) %>% 
        clearShapes() %>%
        setView(lng = lon_centroid, lat =lat_centroid, zoom = 3) %>%
        # setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4], zoom = 3) %>%  
        # fitBounds(lat1=bbox[1], lng1=bbox[2], lat2=bbox[3], lng2=bbox[4]) %>%  
        # clearBounds() %>%
        # Base groups
         addProviderTiles("CartoDB")  %>% addMouseCoordinates() %>%
        # addProviderTiles("Esri.OceanBasemap")   %>% 
        addPolygons(data = current_selection,fillColor = "transparent", group="current_selection") %>%
        addPolygons(data = spatial_footprint_1,fillColor = "transparent", group="footprint1") %>%
        addPolygons(data = spatial_footprint_5,fillColor = "transparent", group="footprint5") %>%
        addPolygons(data = remaining_polygons,fillColor = "transparent", group="remaining")  %>%
        addPolygons(data = all_polygons,fillColor = "transparent", group="all")
      
      # Overlay groups
      for(d in datasets){
        flog.info("Adding one layer for each selected dataset : %s",d)
        this_layer <- df %>% filter(dataset %in% d)
        map <- map  %>% 
          addPolygons(data = this_layer,
                      label = ~measurement_value,
                      popup = ~paste0("Captures pour cette espece: ", round(measurement_value), " tonnes(t) et des brouettes"),
                      # fillColor = ~pal_fun(value),
                      fillColor = ~qpal(measurement_value),
                      fill = TRUE,
                      fillOpacity = 0.8,
                      smoothFactor = 0.5,
                      group=eval(d)
                      # color = ~pal(value)
          )  

        
        
      }
      # Layers control
      map <- map   %>% #  addPolygons(data = bbx) # %>%  addPolygons(data = convex_hull) %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        )     %>% 
        addLayersControl(
          position = "topleft", 
          baseGroups = c("draw"),
          overlayGroups = c(datasets,"footprint1","footprint5","remaining","current_selection","all"),
          options = layersControlOptions(collapsed = FALSE)
        )  %>% 
        leaflet::addLegend("bottomleft", pal = qpal, values = df$measurement_value,
                           title = "Total catch per cell for selected criteria",
                           labFormat = labelFormat(prefix = "MT "),
                           opacity = 1
        ) 
      # %>% addMeasure(
      #     position = "bottomleft",
      #     primaryLengthUnit = "meters",
      #     primaryAreaUnit = "sqmeters",
      #     activeColor = "#10bae0",
      #     completedColor = "#241ad9"
      #   ) 
      map
      # return(map)
  })
 
  # flog.info("View data of the global map")
  # output$this_wkt <- renderText({
  #   wkt()
  # })
  
  # output$DT_query_data_map <- renderDT({
  #   data_map()
  # })
    flog.info("Calling Proxy module !!!!!!!!!!!!!!!!!!!!!!")
    observe({
      flog.info("Listen draw !!!!!!!!!!!!!!!!!!!!!!")
      req(input$map_draw_new_feature)
      flog.info("Listen stop module !!!!!!!!!!!!!!!!!!!!!!")
      req(input$map_draw_stop)
      flog.info("Call module !!!!!!!!!!!!!!!!!!!!!!")
      # feature <- input$map_draw_new_feature
      feature <- input$map_draw_new_feature
      # print(feature)
      flog.info("New wkt : %s", feature)
      
      polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      
      map_proxy_server(
        id="other",
        map_id = "map", 
        feature=feature,
        parent_session = session
      )  
    
  })
    })
  flog.info("End of global map module")
}


map_proxy_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("yourWKT"),label ="Draw or paste a new WKT")
  )
}

map_proxy_server <- function(id, map_id,feature, parent_session){
  moduleServer(id, function(input, output, session) {
    # ns <- parent_session$ns
    ns <- NS(id)
    map <- map_id
    flog.info("Within Proxy module !!!!!!!!!!!!!!!!!!!!!!")
    
    #https://cobalt-casco.github.io/r-shiny-geospatial/07-spatial-input-selection.html
    # observe({
      #use the draw_stop event to detect when users finished drawing
      # req(input$map_draw_new_feature)
      # req(input$map_draw_stop)
      # feature <- input$map_draw_new_feature
      # # print(feature)
      # flog.info("New wkt : %s", feature)
      # 
      # polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      # see  https://rstudio.github.io/leaflet/shiny.html
      # bb <- input$mymap_bounds
      # flog.info("bb mymap_boupolygon_coordinatesnds : %s",bb)
      
      # geom_polygon <- input$mymap_draw_new_feature$geometry
      geoJson <- geojsonio::as.json(feature)
      geom <- st_read(geoJson)
      coord <- st_as_text(st_geometry(geom[1,]))
      new_wkt = wkt(coord)
      updateTextInput(session=parent_session, "yourWKT", value = new_wkt)
      # flog.info("Filter areas id that are within the current wkt")
      # list_areas(df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(coord, crs = 4326), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>%pull())
      # flog.info("First ten values of the matching areas : %s", list_areas())
      
      #East
      lng1 <- st_bbox(geom)$xmin
      #South
      lat1 <- st_bbox(geom)$ymin
      #West
      lng2 <- st_bbox(geom)$xmax
      #North
      lat2 <- st_bbox(geom)$ymax
      
      my_wkt = paste0("POLYGON ((",lng1," ",lat2, ",", lng2, " ",lat2,",", lng2," ", lat1,",", lng1," ", lat1,",", lng1," ", lat2,"))" )
      flog.info("My WKT %s",my_wkt)
  
      flog.info("SF Pop up WKT %s",paste("North ", lat2, "South ", lat1, "West ", lng2, "East ", lng1))
      
      # mymap_proxy = leafletProxy("mymap") %>% clearPopups()
      flog.info("Map proxy %s",paste(lat1, lng1, lat2, lng2,sep="|"))
      mymap_proxy = leafletProxy(
        # Taking the mapId from the parent module
        mapId = map_id,
        # Evaluating the mapId inside the parent module
        # instead of inside itself
        session = parent_session
      ) %>% 
        # clearShapes() %>%
        addPopups(lng2,lat1,coord) %>% 
        addRectangles(lng1=lng1,lat1=lat1,lng2=lng2,lat2=lat2,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group = "draw")  %>% 
        setMaxBounds(lat1, lng1, lat2, lng2)
      
      
      # textOutput("wkt")
  })
  flog.info("End of proxy map module")
}