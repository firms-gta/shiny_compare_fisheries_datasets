map_leafletUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"),width="100%", height="100%"),
    textInput(ns("yourWKT"),"Draw paste a spatial WKT",width="98%"),
    verbatimTextOutput("verbatimWKT", placeholder = TRUE),

    tags$br(),
    tags$br(),
    # map_proxy_UI(ns("other")),
    DT::DTOutput(ns("DT_query_data_map")),
    DT::DTOutput(ns("DT_data_footprint"))
  )
}



map_leafletServer <- function(id,map_df,map_wkt) {
  flog.info("Starting global map module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("Data Table of the map")
    output$DT_query_data_map <- renderDT({
      # sql_query()
    })
    output$DT_data_footprint <- renderDT({
      # sql_query_footprint()
    })
    
    # observeEvent(input$resetWkt, {
    #   current_wkt(all_wkt)
    #   output$verbatimWKT <- renderText({ "all_wkt" })
    #   #   updateSelectInput(session = session,
    #   #                     inputId = ns("yourWKT"),
    #   #                     selected = current_wkt())
    #   updateTextInput(session,ns("yourWKT"), value = current_wkt())
    #   # updateTextInput(session,"yourWKT", value = current_wkt())
    # })

    
    flog.info("Set map" )
    output$map <- renderLeaflet({
      flog.info("Testing truthiness of the dataframe with req()")
      shiny::validate(
        need(nrow(map_df())>0, 'Sorry no data with current filters !'),
        errorClass = "myClass"
      )
      # req(map_df())
      
      this_wkt <- map_wkt()
      current_selection <- st_sf(st_as_sfc(this_wkt, crs = 4326))
      flog.info("New this_wkt OK %s",this_wkt)
      
      data_map <- map_df() 
      
      remaining_polygons <- list_areas %>% st_combine() #%>% st_convex_hull(
      flog.info("Updating spatial_footprint_1 to fit the new WKT  : %s", this_wkt)
      # spatial_footprint_1 <- list_areas  %>% dplyr::filter(gridtype == '1deg_x_1deg') %>% st_combine() #%>% st_convex_hull(
      flog.info("Updating spatial_footprint_5 to fit the new WKT  : %s", this_wkt)
      # spatial_footprint_5 <- list_areas  %>% dplyr::filter(gridtype == '5deg_x_5deg') %>% st_combine() #%>% st_convex_hull(
      all_polygons <- df_distinct_geom %>% st_combine() 
      whole_footprint <- st_sf(st_as_sfc(current_selection_footprint_wkt(), crs = 4326)) %>% st_combine()  # %>% st_union() st_convex_hull
      
      convex_hull <- st_convex_hull(whole_footprint)
      bbx <- st_bbox(remaining_polygons) %>% as.numeric()
      centroid <-  convex_hull %>% st_centroid()
      lat_centroid <- st_coordinates(centroid, crs = 4326)[2]
      lon_centroid <- st_coordinates(centroid, crs = 4326)[1]
      flog.info("lat_centroid : %s",lat_centroid)
      datasets <- unique(data_map$dataset)
      flog.info("listing different datasets : %s", print(datasets))
      
      flog.info("Setting the palette")
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = data_map$measurement_value
      )
      # brewer.pal(7, "OrRd")
      pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
      
      qpal <- colorQuantile(rev(viridis::viridis(10)),
                            data_map$measurement_value, n=10)
      
      # https://r-spatial.github.io/sf/articles/sf5.html
      flog.info("Creating base map with data overview")
      # zoom to limits? https://rdrr.io/cran/leaflet/man/mapOptions.html
      map <- leaflet(
        options=leafletOptions(doubleClickZoom = T, dragging=T,scrollWheelZoom=T, minZoom = 3, maxZoom = 10)
      ) %>% 
        # clearGroup("draw")%>%
        # clearShapes() %>%
        setView(lng = lon_centroid, lat =lat_centroid, zoom = 4) %>%
        # setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4], zoom = 3) %>%  
        # fitBounds(lat1=bbx[1], lng1=bbx[2], lat2=bbx[3], lng2=bbx[4]) %>%
        # clearBounds() %>%
        # Base groups
        # addMouseCoordinates() %>%
        addProviderTiles("CartoDB", "background")   %>%
        addProviderTiles("Esri.OceanBasemap", "background")   %>%
        addPolygons(data = current_selection,color="red",fillColor = "transparent", group="current_selection") %>%
        # addPolygons(data = spatial_footprint_1,color="blue",fillColor = "transparent", group="footprint1") %>%
        # addPolygons(data = spatial_footprint_5,color="green",fillColor = "transparent", group="footprint5") %>%
        # addPolygons(data = current_fooprint,color="yellow",fillColor = "transparent", group="data_for_filters")  %>%
        # addPolygons(data = remaining_polygons,color="red",fillColor = "transparent", group="remaining")  %>%
        addPolygons(data = all_polygons,color="cyan",fillColor = "transparent", group="all")  %>%
        addPolygons(data = whole_footprint,color="yellow",fillColor = "transparent", group="data_for_filters")
      
      flog.info("Adding new layers for each dataset in the selected WKT")
      # Overlay groups
      for(d in datasets){
        flog.info("Adding one layer for each selected dataset : %s", print(datasets))
        flog.info("Adding one layer for each selected dataset : %s",d)
        this_layer <- data_map %>% filter(dataset %in% d)
        flog.info("Number of lines : %s", nrow(this_layer))
        
        map <- map  %>% 
          addPolygons(data = this_layer,
                      label = ~measurement_value,
                      popup = ~paste0("Captures pour cette espece: ", round(measurement_value), " tonnes(t)"),
                      # fillColor = ~pal_fun(value),
                      fillColor = ~qpal(measurement_value),
                      stroke = FALSE,
                      fill = TRUE,
                      fillOpacity = 0.8,
                      smoothFactor = 0.5,
                      group=eval(d)
                      # color = ~pal(value)
          )  
        
      }
      # Layers control
      flog.info("Adding layers control")
      map <- map   %>% #  addPolygons(data = bbx) # %>%  addPolygons(data = convex_hull) %>%
        addDrawToolbar(
          targetGroup = "draw",
          singleFeature = TRUE,
          # clearFeatures = TRUE,
          drag = TRUE,
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()),
          rectangleOptions = filterNULL(list(shapeOptions = drawShapeOptions()))
        ) %>% addWMSTiles(
          "https://geo.vliz.be/geoserver/MarineRegions/wms?SERVICE=WMS&VERSION=1.3.0",
          layers = "eez",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          group ="EEZ",
          attribution = "Marine Regions WMS"
        )    %>% 
        addLayersControl(
          position = "topleft", 
          baseGroups = c("background","EEZ"),
          overlayGroups = c(datasets,"draw","footprint1","footprint5","data_for_filters","current_selection","all"),
          options = layersControlOptions(collapsed = TRUE)
        )  %>% 
        leaflet::addLegend("bottomleft", pal = qpal, values = data_map$measurement_value,
                           title = "Total catch per cell for selected criteria",
                           labFormat = labelFormat(prefix = "MT "),
                           opacity = 1
        )  %>%  
        addMiniMap(zoomLevelFixed = 1) %>%
        addScaleBar(
          position = "topright",
          options = scaleBarOptions(
            maxWidth = 10,
            metric = TRUE,
            imperial = FALSE
          )
        )
      
      
      shapeOptions = drawShapeOptions(color = "green",
                                      fillOpacity = 1,
                                      fillColor = "yellow",
                                      weight = 20)
      
      flog.info("########################## MAP UPDATED !!! ########################## ")
      
      # observeEvent(current_wkt(), function() {
      #   wkt <- current_wkt()
      #   new_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
      #                          
      #                          flog.info("leafletProxy !!")
      #                          textPopup <- paste0("<b>Click Submit button if you want to extract data in this polygon</b>:", new_wkt)
      #                          
      #                          leafletProxy(
      #                            "map",session, data=
      #                          ) %>%
      #                            setView(lng = lon_centroid, lat =lat_centroid, zoom = 3) %>%
      #                            addPopups(lng = st_bbox(new_selection)$xmax,lat = st_bbox(new_selection)$ymin, popup =  textPopup) %>%
      #                            addPolygons(data = new_selection,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group="draw")
      #             })
      #   
      #   
      
      
      
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
    # observe({
    observeEvent(input$map_draw_stop,{
      # observe({
      #use the draw_stop event to detect when users finished drawing
      # req(input$mymap_draw_new_feature)
      flog.info("Check if the user is drawing a new feature !")
      # req(input$map_draw_new_feature)
      flog.info("Check if the user is done drawing a new feature !")
      req(input$map_draw_stop)
      feature <- input$map_draw_new_feature
      flog.info("User has drawn a new feature, wkt : %s", feature)
      #https://cobalt-casco.github.io/r-shiny-geospatial/07-spatial-input-selection.html
      # output$yourWKT <- renderText({
      #use the draw_stop event to detect when users finished drawing
      polygon_coordinates <- feature$geometry$coordinates[[1]]
      # see  https://rstudio.github.io/leaflet/shiny.html
      # bb <- input$mymap_bounds
      # flog.info("bb mymap_boupolygon_coordinatesnds : %s",bb)
      
      # geom_polygon <- input$mymap_draw_new_feature$geometry
      # geoJson <- geojsonio::as.json(feature)
      # geom <- st_read(geoJson)
      # coord <- st_as_text(st_geometry(geom[1,]))
      # flog.info("Filter areas id that are within the current wkt")
      # list_areas(df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(coord, crs = 4326), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>%pull())
      flog.info("polygon_coordinates : %s", print(polygon_coordinates))
      
      # [1] "POLYGON ((-34.67285 36.985, -34.67285 42.58544, -26.23535 42.58544, -26.23535 36.985, -34.67285 36.985))"      #East
      # lng1 <- st_bbox(geom)$xmin
      lng1 <- polygon_coordinates[[1]][[1]]
      #South
      # lat1 <- st_bbox(geom)$ymin
      lat1 <- polygon_coordinates[[1]][[2]]
      #West
      # lng2 <- st_bbox(geom)$xmax
      lng2 <- polygon_coordinates[[3]][[1]]
      #North
      # lat2 <- st_bbox(geom)$ymax
      lat2 <- polygon_coordinates[[2]][[2]]
      
      new_wkt = paste0("POLYGON ((",lng1," ",lat2, ",", lng2, " ",lat2,",", lng2," ", lat1,",", lng1," ", lat1,",", lng1," ", lat2,"))" )
      new_selection <- st_sf(st_as_sfc(new_wkt, crs = 4326))
      centroid <-  new_selection %>% st_centroid() 
      lat_centroid <- st_coordinates(centroid, crs = 4326)[2]
      lon_centroid <- st_coordinates(centroid, crs = 4326)[1]
      # polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      
      textPopup <- paste0("<b>Click Submit button if you want to extract data in this polygon</b>:", new_wkt)
      
      output$verbatimWKT <- renderText({ input$yourWKT })
      
      # updateTextInput(session,ns("yourmoduleWKT"), value = new_wkt)
      # output$moduleverbatimWKT <- renderText({
      #   wkt()
      
      # observe({
      #   
      #   leafletProxy("map",session) %>%
      #     removeShape(c("theWKT")) %>% 
      #     setView(lng = lon_centroid, lat =lat_centroid, zoom = 3) %>%
      #     addPopups(lng = st_bbox(new_selection)$xmax,lat = st_bbox(new_selection)$ymin, popup =  textPopup) %>%
      #     addRectangles(layerId="theWKT",lng1=lng1,lat1=lat1,lng2=lng2,lat2=lat2,
      # fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group = "draw")
      #     # addPolygons(data = new_selection,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group="draw")
      #   
      # })
      
      whole_footprint <- st_sf(st_as_sfc(current_selection_footprint_wkt(), crs = 4326)) 
      
      
      if (requireNamespace("qgisprocess", quietly = TRUE) && spatial_processing_mode=="QGIS" ) {
        
        # Essayer de configurer qgisprocess pour voir s'il fonctionne
        qgis_path <- try(qgisprocess::qgis_configure(), silent = TRUE)
        flog.info("QGIS is used to select remaing data within the default or newly drawn polygon !")
        
        if (!inherits(qgis_path, "try-error") && !is.null(qgis_path)) {
          flog.info("QGIS checking if remaining data in the new polygon just drawn !!")
          disjoint_WKT <- qgisprocess::qgis_run_algorithm("native:extractbylocation",
                                                          INPUT = whole_footprint,
                                                          PREDICATE = "disjoint",
                                                          INTERSECT = new_selection)
        }
      }else{
        flog.info("SF checking if remaining data in the new polygon just drawn !!")
        # disjoint_WKT <- new_selection %>% dplyr::filter(sf::st_crosses(., whole_footprint, sparse = FALSE))
        disjoint_WKT <- new_selection %>% dplyr::filter(sf::st_disjoint(., whole_footprint, sparse = FALSE))
      }
      
      disjoint <- sf::st_as_sf(disjoint_WKT)
      
      # 
      #       process_disjoint_WKT <- function(current_footprint, new_selection) {
      # 
      #         # Vérifier si qgisprocess est installé
      #         if (requireNamespace("qgisprocess", quietly = TRUE)) {
      # 
      #           # Essayer de configurer qgisprocess pour voir s'il fonctionne
      #           qgis_path <- try(qgisprocess::qgis_configure(), silent = TRUE)
      # 
      #           if (!inherits(qgis_path, "try-error") && !is.null(qgis_path)) {
      #             # Utiliser qgisprocess si disponible et configuré
      #             flog.info("QGIS checking AGAIN !!!! if remaining data in the new polygon ust drawn !!")
      #             message("Utilisation de qgisprocess pour traiter les données.")
      #             disjoint_WKT <- qgisprocess::qgis_run_algorithm(
      #               "native:extractbylocation",
      #               INPUT = st_sf(current_footprint),
      #               PREDICATE = "disjoint",
      #               INTERSECT = new_selection
      #             ) %>%
      #               sf::st_as_sf()
      # 
      #             return(disjoint_WKT)
      #           }
      #         }
      #         
      #         # Si qgisprocess n'est pas disponible ou configuré, utiliser sf
      #         message("qgisprocess non disponible ou non configuré. Utilisation de sf pour traiter les données.")
      #         flog.info("SF checking if remaining data in the new polygon ust drawn !!")
      #         current_footprint_sf <- st_sf(current_footprint)
      #         disjoint_WKT <- current_footprint_sf %>%
      #           dplyr::filter(sf::st_disjoint(., new_selection, sparse = FALSE)) %>%
      #           sf::st_as_sf()
      # 
      #         return(disjoint_WKT)
      #       }
      #       
      #       disjoint_WKT <- process_disjoint_WKT(current_fooprint, new_selection)
      # 
      #       
      # # st_disjoint(current_selection,current_fooprint)
      
      flog.info("Test if new wkt is disjoint from current polygon")
      if(nrow(disjoint)==1){
        flog.info("New wkt not OK: disjoint")
        flog.info("Reset WKT: no spatial filter !")
        flog.info("Apply filters with no spatial filter !")
        # shiny::validate(
        #   need(nrow(disjoint)!=1, 'Sorry no data with current filters !'),
        #   errorClass = "myClass"
        # )
        # current_wkt(target_wkt)
        
        # shinyjs::click(id = "resetWkt")
        # showModal(modalDialog(
        #   title = "Warning",
        #   "No data left in this area with current filters, plase draw another polygon !",
        #   easyClose = TRUE,
        #   footer = NULL
        # ))
      }else{
        flog.info("New wkt OK: not disjoint")
        flog.info("Update WKT; replace with new valid one")
        current_wkt(new_wkt)
        # flog.info("Apply filters with new spatial filter !")
        # shinyjs::click(id = "submit")
        
        # flog.info("Calling Proxy module !!!!!!!!!!!!!!!!!!!!!!")
        # map_proxy_server(
        #   id="other",
        #   map_id = "map",
        #   new_wkt=new_wkt,
        #   parent_session = session
        # )
      }
      
      # if(nrow(disjoint)==0)
      #   showModal(modalDialog(
      #     title = "Warning",
      #     "No data left with current spatial filter !",
      #     easyClose = TRUE,
      #     footer = NULL
      #   ))else{
      #     new_selection <- st_sf(st_as_sfc(this_wkt, crs = 4326))
      #     map_proxy_server(
      #       id="other",
      #       map_id = "map", 
      #       new_wkt=new_wkt,
      #       parent_session = session
      #     )
      #   }
      
      
      
      # updateTextInput(session,ns("yourWKT"), value = new_wkt)
    },ignoreInit = TRUE,ignoreNULL = TRUE)
  
    # end observe)
    
  })
  flog.info("End of global map module")
}


map_proxy_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("yourmoduleWKT"),label ="Module Draw or paste a new WKT", value=new_wkt, width="98%"),
    verbatimTextOutput("moduleverbatimWKT", placeholder = TRUE)
  )
}

map_proxy_server <- function(id, map_id,new_wkt, parent_session){
  moduleServer(id, function(input, output, session) {
    # ns <- parent_session$ns
    ns <- NS(id)
    map <- map_id
    flog.info("Within Proxy module !!!!!!!!!!!!!!!!!!!!!!")
    flog.info("Calculate centroid to set the map view !!!!!!!!!!!!!!!!!!!!!!")
    wkt_sf <- st_sf(st_as_sfc(new_wkt, crs = 4326))
    centroid <-  wkt_sf %>% st_centroid() 
    lat_centroid <- st_coordinates(centroid, crs = 4326)[2]
    lon_centroid <- st_coordinates(centroid, crs = 4326)[1]
    flog.info("My WKT %s",new_wkt)
    textPopup <- paste0("<b>Click Submit button if you want to extract data in this polygon</b>:", new_wkt)
    # observe({
    leafletProxy(
      # Taking the mapId from the parent module
      mapId = map_id,
      # Evaluating the mapId inside the parent module
      # instead of inside itself
      session = parent_session
    ) %>% 
      # clearShapes() %>%
      setView(lng = lon_centroid, lat =lat_centroid, zoom = 3) %>%
      # fitBounds(lat1, lng1, lat2, lng2) %>%
      # setMaxBounds(lat1, lng1, lat2, lng2)
      # clearShapes() %>%
      # addPopups(lng = st_bbox(wkt_sf)$xmax,lat = st_bbox(wkt_sf)$ymin, popup =  textPopup
      #           # ,
      #           # popupOptions = popupOptions(  minWidth = 300
      #           #                             # # noHide = TRUE, 
      #           #                             # # direction = "bottom",
      #           #                             # textsize = "24px",
      #           #                             # style = list(
      #           #                             #   "color" = "red",
    #           #                             #   "font-family" = "serif",
    #           #                             #   "font-style" = "italic",
    #           #                             #   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    #           #                             #   "font-size" = "12px",
    #           #                             #   "border-color" = "rgba(0,0,0,0.5)"
    #           #                             # )
    #           #   )
    #             ) %>% 
    addPolygons(data = wkt_sf,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group="draw")
    # addRectangles(lng1=lng1,lat1=lat1,lng2=lng2,lat2=lat2,fillColor = "grey",fillOpacity = 0.1, stroke = TRUE, color = "red", opacity = 1, group = "draw")
    
    
    current_wkt(new_wkt)
    # })
    output$moduleverbatimWKT <- renderText({ input$yourmoduleWKT })
    # updateTextInput(session,ns("yourmoduleWKT"), value = wkt())
    # output$moduleverbatimWKT <- renderText({
    #   wkt()
    # })
    
    # })
    
  })
  flog.info("End of proxy map module")
}