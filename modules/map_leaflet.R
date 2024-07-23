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


map_leafletServer <- function(id,metadata) {
  flog.info("Starting global map module")
  moduleServer(id, function(input, output, session) {
    # ns <- session$ns
    output$mymap <- renderLeaflet({
      
      df <- metadata()
      flog.info(class(df))
      
      bbox <- st_bbox(df) %>% as.vector()  
      centroid <-  st_convex_hull(df) %>% st_centroid()
      lat_centroid <- st_coordinates(centroid)[2]
      lon_centroid <- st_coordinates(centroid)[1]
      
      flog.info("lat_centroid")
      flog.info(lat_centroid)
      datasets <- unique(df$dataset)
      flog.info("listing different datasets")
      datasets <- unique(df$dataset)
      flog.info(datasets)
      
      
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = df$measurement_value
      )
      # brewer.pal(7, "OrRd")
      pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
      
      qpal <- colorQuantile(rev(viridis::viridis(10)),
                            df$measurement_value, n=10)
      
      # https://r-spatial.github.io/sf/articles/sf5.html
      map_leaflet <- leaflet() %>%
        setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
        ) %>%  
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])  %>%
        clearBounds() %>%
        # Base groups
        addProviderTiles("Esri.OceanBasemap") 
      # addPolygons(data = df,
      #             label = ~value,
      #             popup = ~paste0("Captures pour cette espece: ", round(value), " tonnes(t) et des brouettes"),
      #             # fillColor = ~pal_fun(value),
      #             fillColor = ~qpal(value),
      #             fill = TRUE,
      #             fillOpacity = 0.8,
      #             smoothFactor = 0.5
      #             # color = ~pal(value)
      # ) %>%
      
      # Overlay groups
      for(d in datasets){
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
      
      map_leaflet
      # return(map_leaflet)
  })
  
  
  observe({
    #use the draw_stop event to detect when users finished drawing
    feature <- input$mymap_draw_new_feature
    req(input$mymap_draw_stop)
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    wkt(st_as_text(st_geometry(geom[1,])))
    coord <- st_as_text(st_geometry(geom[1,]))
    
    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]
    
    
    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)
    
    mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
    textOutput("wkt")
    
  })
  
  
  
  })
  flog.info("End of global map module")
}
