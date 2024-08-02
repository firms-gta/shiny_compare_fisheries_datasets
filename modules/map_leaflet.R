map_leafletUI <- function(id) {
  ns <- NS(id)
  # navbarMenu("toto map",
             # tabPanel("df mymap",
                      leafletOutput(ns("mymap"),width="100%", height="100%")
                      # )
  # )
  # tagList(
  # fluidRow(
  #     leafletOutput(ns("mymap"), width="100%", height="100%")
  #   )
}

map_leafletServer <- function(id,sql_query) {
  flog.info("Starting global map module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_map <- reactive({
      flog.info("Filtering with new WKT  : %s", wkt())
      
      req(sql_query())
      this_df <- sql_query()
      # wkt <- "POLYGON ((-159.6094 -58.81374, -159.6094 43.06889, 112.5 43.06889, 112.5 -58.81374, -159.6094 -58.81374))"
      # wkt <- wkt()
      
      data_map <- this_df %>% filter(!is.na(gridtype)) %>% dplyr::group_by(dataset,codesource_area,geom) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE))  %>%  ungroup() %>% st_as_sf(wkt="geom",crs=4326)
        # left_join(df_distinct_geom,by="codesource_area")
      
      # data_map <- df_distinct_geom %>% left_join(list_cwp)
      # data_map <- df_distinct_geom  %>% merge(list_cwp) %>% filter(!st_is_empty(.))
      # list_cwp  %>% left_join(df_distinct_geom)  %>% filter(!st_is_empty(.))
      flog.info("Main data: %s", head(data_map))
      flog.info("Main data nrow: %s", nrow(data_map))
      data_map
    }) 
    
    output$mymap <- renderLeaflet({
      # flog.info("Testing truthiness of the dataframe with req()")
      # req(data_map(),TRUE)

      df <- data_map()
      flog.info("Main data check: %s", head(df))
      
      flog.info("Class of data object : %s", class(df))
      flog.info("Column names : %s", colnames(df))
      flog.info("Number of rows : %s", nrow(df))
      
      # bbox <- st_bbox(df) %>% as.vector()  
      # req(wkt())
      # wkt <- wkt()    
      # bbox <-  st_bbox(st_as_sfc(wkt() , crs = 4326)) %>% as.vector()
      centroid <-  st_convex_hull(df) %>% st_centroid()
      lat_centroid <- st_coordinates(centroid)[2]
      lon_centroid <- st_coordinates(centroid)[1]
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
      map_leaflet <- leaflet(
        options=leafletOptions(doubleClickZoom = T, dragging=T,scrollWheelZoom=T
                               # zoom to limits? https://rdrr.io/cran/leaflet/man/mapOptions.html
        )
      ) %>%
        setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
        ) %>%
        # setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4], zoom = 3) %>%  
        # fitBounds(lat1=bbox[1], lng1=bbox[2], lat2=bbox[3], lng2=bbox[4]) %>%  
        # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])  %>%
        # clearBounds() %>%
        # Base groups
        addProviderTiles("Esri.OceanBasemap") 
      # map_leaflet <- map_leaflet  %>% addRectangles(lat1=bbox[1], lng1=bbox[2], lat2=bbox[3], lng2=bbox[4],fillColor = "transparent")
      # Overlay groups
      for(d in datasets){
        flog.info("Adding one layer for each selected dataset : %s",d)
        this_layer <- df %>% filter(dataset %in% d)
        map_leaflet <- map_leaflet  %>% 
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
      map_leaflet <- map_leaflet   %>% 
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        )     %>% 
        addLayersControl(
          position = "topleft", 
          baseGroups = c("draw"),
          overlayGroups = datasets,
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
      map_leaflet
      # return(map_leaflet)
  })
  
  #https://cobalt-casco.github.io/r-shiny-geospatial/07-spatial-input-selection.html
  observe({
    #use the draw_stop event to detect when users finished drawing
    feature <- input$mymap_draw_new_feature
    req(input$mymap_draw_stop)
    # print(feature)
    flog.info("New wkt : %s", feature)
    
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    # bb <- input$mymap_bounds
    # flog.info("bb mymap_boupolygon_coordinatesnds : %s",bb)
    
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    geom <- st_read(geoJson)
    coord <- st_as_text(st_geometry(geom[1,]))
    wkt(coord)
    flog.info("Filter areas id that are within the current wkt")
    # list_areas(df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(coord, crs = 4326), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>%pull())
    # flog.info("First ten values of the matching areas : %s", list_areas())
    
    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]
    
    #South
    lng1 <- st_bbox(geom)$xmin
    #East
    lat1 <- st_bbox(geom)$ymin
    #West
    lng2 <- st_bbox(geom)$xmax
    #North
    lat2 <- st_bbox(geom)$ymax
    
    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", south, "West ", west, "East ", east)
    flog.info("Pop up WKT %s",text)
    text<-paste("North ", lat1, "South ", lat2, "West ", lng2, "East ", lng1)
    flog.info("SF Pop up WKT %s",text)
    flog.info("Pop up WKT %s",wkt())
    
    # mymap_proxy = leafletProxy("mymap") %>% clearPopups()  
    mymap_proxy = leafletProxy("mymap")  %>% addPopups(south,west,coord) %>% 
      addRectangles(lng1=lng1,lat1=lat1,lng2=lng2,lat2=lat2,fillColor = "transparent")  %>% 
    fitBounds(lat1, lng1, lat2, lng2)
    
    textOutput("wkt")

  })
  
  # flog.info("View data of the global map")
  # output$this_wkt <- renderText({
  #   wkt()
  # })
  
  # output$DT_query_data_map <- renderDT({
  #   data_map()
  # })
  
  })
  flog.info("End of global map module")
}
