server <- function(input, output, session) {
  
  ########################################################## Dynamic filters ########################################################## 
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear_type),collapse="|"),"|",fixed=TRUE))
  })
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
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
  
  ########################################################## DATA & SQL queries ########################################################## 
  #set the main parameterized query (options for geom might be st_collect(geom) or  ST_ConvexHull(st_collect(geom)) as convexhull )
  sql_query <- eventReactive(input$submit, {
  query <- df_sf %>% 
    dplyr::filter(st_within(df_sf, wkt(), sparse = FALSE), dataset %in% input$dataset, gear_type_name %in% input$gear_type, year_name %in% year_name, measurement_unit_name %in% input$unit, gridtype_name %in% input$gridtype) %>% 
    dplyr::select(dataset, measurement_unit,  gear_type, year, species, measurement_value, gridtype, fishing_fleet)
  },
  ignoreNULL = FALSE)
  
  # 
  # sql_query <- eventReactive(input$submit, {
  #   if(is.null(input$year)){year_name=target_year}else{year_name=input$year}
  #   # if(is.null(input$dataset)){dataset_name=target_dataset$dataset}else{year_name=input$dataset}
  #   query <- glue::glue_sql(
  #     "SELECT dataset, measurement_unit,  gear_type, year, species, measurement_value, gridtype, fishing_fleet, geom FROM shinycatch 
  #     WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326)) 
  #     AND  dataset IN ({dataset_name*}) 
  #     AND  species IN ({species_name*}) 
  #     AND gear_type IN ({gear_type_name*}) 
  #     AND fishing_fleet IN ({fishing_fleet_name*}) 
  #     AND year IN ({year_name*}) 
  #     AND gridtype IN ({gridtype_name*}) 
  #     AND measurement_unit IN ({measurement_unit_name*}) ",
  #     wkt = wkt(),
  #     dataset_name = input$dataset,
  #     species_name = input$species,
  #     gear_type_name = input$gear_type,
  #     fishing_fleet_name = input$fishing_fleet,
  #     year_name = year_name,
  #     measurement_unit_name = input$unit,
  #     gridtype_name = input$gridtype,
  #     .con = con)
  # },
  # ignoreNULL = FALSE)
  # 
  metadata <- reactive({
    query_metadata(paste0("SELECT dataset, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY dataset,geom"))
    st_read(con, query = query_metadata()) 
    
  })  
  
  # measurement_unit,  gear_type, year, species ;")
  data_all_datasets <- eventReactive(input$submit, {
    query_all_datasets(paste0("SELECT dataset, year, sum(measurement_value) as measurement_value, measurement_unit FROM (",sql_query(),") AS foo GROUP BY dataset, year, measurement_unit"))
    dbGetQuery(con,query_all_datasets())
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_pie_all_datasets <- eventReactive(input$submit, {
    dbGetQuery(con,paste0("SELECT dataset, sum(measurement_value) as measurement_value FROM (",sql_query(),") AS foo GROUP BY  dataset"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  data_pie_gridtype_catch <- eventReactive(input$submit, {
    dbGetQuery(con,paste0("SELECT dataset, gridtype, measurement_unit, count(*), SUM(measurement_value)  as measurement_value FROM (",sql_query(),") AS foo GROUP BY dataset, gridtype, measurement_unit ORDER BY dataset"))
  },
  ignoreNULL = FALSE)
  
  data_barplot_all_datasets <- eventReactive(input$submit, {
    dbGetQuery(con,paste0("SELECT  dataset, measurement_unit, count(*) AS count, SUM(measurement_value) AS measurement_value  FROM (",sql_query(),") AS foo GROUP BY dataset, measurement_unit ORDER BY dataset"))
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i1 <- eventReactive(input$submit, {
    dbGetQuery(con,paste0("SELECT dataset, measurement_unit, year, species, sum(measurement_value) as measurement_value FROM (",sql_query(),") AS foo GROUP BY  dataset, measurement_unit,  year, species"))
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i2 <- eventReactive(input$submit, {
    dbGetQuery(con,paste0("SELECT measurement_unit, gear_type AS gear_type, year, species, sum(measurement_value) as measurement_value FROM (",sql_query(),") AS foo GROUP BY measurement_unit, gear_type, year, species")) 
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  ########################################################## Outputs: text & Data tables ########################################################## 
  output$value <- renderText({ 
    wkt()
    # output$measurement_value <- renderText({ input$caption })
  })
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  output$query_metadata <- renderText({ 
    paste("Your SQL Query is : \n", query_metadata())
  })
  
  output$query_all_datasets <- renderText({ 
    paste("Your SQL Query is : \n", query_all_datasets())
  })
  
  
  # output$DT_data <- renderDT({
  #   data()
  # })
  
  output$DT_query_metadata <- renderDT({
    metadata() %>% st_drop_geometry()
  })
  
  output$DT_data_all_datasets <- renderDT({
    data_all_datasets()
  })
  
  output$DT_data_barplot_all_datasets <- renderDT({
    # data_barplot_all_datasets()   %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))  %>% pivot_wider(names_from = measurement_unit, measurement_values_from = c("measurement_value", "count"), names_sep="_",measurement_values_fill = 0)
    data_barplot_all_datasets() 
  }) 
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", wkt())
  })
  
  ########################################################## Outputs: maps / plots / charts ########################################################## 
  
  
  # callModule(module = map_leaflet, id = "id_1")
  map_leafletServer(id = "id_1",metadata)
# 
# 
#   output$mymap <- renderLeaflet({
# 
#     df <- metadata()
#     bbox <- st_bbox(df) %>%
#       as.vector()
#     centroid <-  st_convex_hull(df) %>% st_centroid()
#     lat_centroid <- st_coordinates(centroid)[2]
#     lon_centroid <- st_coordinates(centroid)[1]
# 
#     datasets <- unique(df$dataset)
# 
# 
#     pal <- colorNumeric(
#       palette = "YlGnBu",
#       domain = df$measurement_value
#     )
#     # brewer.pal(7, "OrRd")
#     pal_fun <- colorQuantile(   "YlOrRd", NULL, n = 10)
# 
#     qpal <- colorQuantile(rev(viridis::viridis(10)),
#                           df$measurement_value, n=10)
# 
#     # https://r-spatial.github.io/sf/articles/sf5.html
#     map_leaflet <- leaflet() %>%
#       setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
#       ) %>%
#       fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])  %>%
#       clearBounds() %>%
#       # Base groups
#       addProviderTiles("Esri.OceanBasemap")
#     # addPolygons(data = df,
#     #             label = ~value,
#     #             popup = ~paste0("Captures pour cette espece: ", round(value), " tonnes(t) et des brouettes"),
#     #             # fillColor = ~pal_fun(value),
#     #             fillColor = ~qpal(value),
#     #             fill = TRUE,
#     #             fillOpacity = 0.8,
#     #             smoothFactor = 0.5
#     #             # color = ~pal(value)
#     # ) %>%
# 
#     # Overlay groups
#     for(d in datasets){
#       this_layer <- df %>% filter(dataset %in% d)
#       map_leaflet <- map_leaflet  %>%
#         addPolygons(data = this_layer,
#                     label = ~measurement_value,
#                     popup = ~paste0("Captures pour cette espece: ", round(measurement_value), " tonnes(t) et des brouettes"),
#                     # fillColor = ~pal_fun(value),
#                     fillColor = ~qpal(measurement_value),
#                     fill = TRUE,
#                     fillOpacity = 0.8,
#                     smoothFactor = 0.5,
#                     group=eval(d)
#                     # color = ~pal(value)
#         )
#     }
#     # Layers control
#     map_leaflet <- map_leaflet   %>%
#       addDrawToolbar(
#         targetGroup = "draw",
#         editOptions = editToolbarOptions(
#           selectedPathOptions = selectedPathOptions()
#         )
#       )     %>%
#       addLayersControl(
#         position = "topleft",
#         baseGroups = c("draw"),
#         overlayGroups = datasets,
#         options = layersControlOptions(collapsed = FALSE)
#       )  %>%
#       leaflet::addLegend("bottomleft", pal = qpal, values = df$measurement_value,
#                          title = "Total catch per cell for selected criteria",
#                          labFormat = labelFormat(prefix = "MT "),
#                          opacity = 1
#       )
# 
#     # map_leaflet
# 
# 
#   })
# 
# 
#   observe({
#     #use the draw_stop event to detect when users finished drawing
#     feature <- input$mymap_draw_new_feature
#     req(input$mymap_draw_stop)
#     print(feature)
#     polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
#     # see  https://rstudio.github.io/leaflet/shiny.html
#     bb <- input$mymap_bounds
#     geom_polygon <- input$mymap_draw_new_feature$geometry
#     # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
#     geoJson <- geojsonio::as.json(feature)
#     # spdf <- geojsonio::geojson_sp(feature)
#     geom <- st_read(geoJson)
#     wkt(st_as_text(st_geometry(geom[1,])))
#     coord <- st_as_text(st_geometry(geom[1,]))
# 
#     north <- polygon_coordinates[[1]][[1]]
#     south <- polygon_coordinates[[2]][[1]]
#     east <- polygon_coordinates[[1]][[2]]
#     west <- polygon_coordinates[[2]][[2]]
# 
# 
#     if(is.null(polygon_coordinates))
#       return()
#     text<-paste("North ", north, "South ", east)
# 
#     mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
#     textOutput("wkt")
# 
#   })
# 

  output$plot2 <- renderPlotly({ 
    
    df_i2 <- data_i2()
    
    i2 <- Atlas_i2_SpeciesByGear(as.data.frame(df_i2),
                                 yearAttributeName="year",
                                 speciesAttributeName="species",
                                 valueAttributeName="measurement_value",
                                 gearTypeAttributeName="gear_type",
                                 withSparql=FALSE)
    i2 
  })
  
  
  
  
  
  output$plot2_streamgraph<- renderStreamgraph({ 
    df_i2 =  data_i2() %>% 
      streamgraph("gear_type", "measurement_value", "year", offset="zero", interpolate="step") %>%
      sg_axis_x(1, "year", "%Y") %>%
      sg_fill_brewer("PuOr") %>%
      sg_legend(show=TRUE, label="I=RFMO - names: ")
  })
  
  
  
  output$dygraph_all_datasets <- renderDygraph({
    
    df_i1 = data_all_datasets()  %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) %>% spread(dataset, measurement_value, fill=0) 
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    tuna_catches_timeSeries <-NULL
    
    if(length(unique(df_i1$measurement_unit))>1){
      df_i1_t <- df_i1 %>% filter(measurement_unit  == 't') 
      df_i1_no <- df_i1 %>% filter(measurement_unit == 'no')  
      # if(switch_unit()){
      #   df_i1 <- df_i1_no
      # }else{
      #   df_i1 <- df_i1_t
      # }
      
      # if(length(colnames(dplyr::select(df_i1_t,-c(species,year,measurement_unit))))>1){
      for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(species,year,measurement_unit))))){
        this_dataset <-colnames(dplyr::select(df_i1_t,-c(species,year,measurement_unit)))[d]
        if(sum(dplyr::select(df_i1_t, this_dataset))>0){
          df_i1_t  <- df_i1_t  %>% dplyr::rename(!!paste0(this_dataset,'_t') := !!this_dataset)
          this_time_serie <- xts(x = dplyr::select(df_i1_t, c(paste0(this_dataset,'_t'))), order.by = df_i1_t$year)
          if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
            tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
          }
        }
      }
      # if(length(colnames(dplyr::select(df_i1_no,-c(species,year,measurement_unit))))>1){
      for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(species,year,measurement_unit))))){
        this_dataset <- colnames(dplyr::select(df_i1_no,-c(species,year,measurement_unit)))[d]
        if(sum(dplyr::select(df_i1_no,this_dataset))>0){
          df_i1_no  <- df_i1_no  %>% dplyr::rename(!!paste0(this_dataset,'_no') := !!this_dataset)
          this_time_serie <- xts(x = dplyr::select(df_i1_no, c(paste0(this_dataset,'_no'))), order.by = df_i1_no$year)
          tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
        }
      }
      
    }else{
      for(d in 1:length(colnames(dplyr::select(df_i1,-c(species,year,measurement_unit))))){
        this_dataset <- colnames(dplyr::select(df_i1,-c(species,year,measurement_unit)))[d]
        this_time_serie <- xts(x = dplyr::select(df_i1, c(this_dataset)), order.by = df_i1$year)
        if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
          tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
        }
      }
      
      
      # create the gridtype chart
      # g1 <- dygraph(tuna_catches_timeSeries) # %>% dyOptions( fillGraph=TRUE )
      # g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by ocean") %>% dyRangeSelector() %>%       dyLegend(labelsDiv = "legendDivID")
      # %>%
      #   dyStackedBarGroup(c('global_catch_5deg_1m_firms_level0', 'global_catch_1deg_1m_ps_bb_firms_level0','spatial','nominal'))
      # >%  dyOptions( fillGraph=TRUE) %>%        # create bar chart with the passed dygraph
      #   dyOptions(stackedGraph = stack())
      #     dySeries(iotc, label = "iotc") %>%
      #   dySeries(iattc, label = "iattc") %>%
      # dySeries(iccat, label = "iccat") 
      #   
    }
    
    g1 <- dygraph(tuna_catches_timeSeries, main = "Times series of measurement_values with selected measurement_units (tons or numbers) for the selected datasets") %>% dyRangeSelector() %>% dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = T)
    
    
    
  })
  
  
  
  output$plotly_time_series_all_datasets <- renderPlotly({
    
    
    df_i1 = data_all_datasets()  %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) %>% spread(dataset, measurement_value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    
    
    if(length(unique(df_i1$measurement_unit))>1){
      df_i1_t <- df_i1 %>% filter(measurement_unit  == 't') 
      df_i1_no <- df_i1 %>% filter(measurement_unit == 'no')  
      if(nrow(df_i1_t)>0){
        
        # for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(species,year,measurement_unit))))){
        for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(year,measurement_unit)))) ){
          this_dataset <-colnames(dplyr::select(df_i1_t,-c(year,measurement_unit)))[d]
          if(sum(dplyr::select(df_i1_t, this_dataset))>0){
            # data  <- df_i1_t  %>% dplyr::select(c(year,!!this_dataset))
            if(d==1){
              fig <- plot_ly(df_i1_t, x = df_i1_t$year, y =df_i1_t[,this_dataset][[1]], name = paste0(this_dataset,"_",d), type = 'scatter', mode = 'lines' )
            }else{
              fig <-  fig %>% add_trace(y = df_i1_t[,this_dataset][[1]], name = paste0(this_dataset,"_t"), mode = 'lines') 
            }
          }
        }
      }
      if(nrow(df_i1_no)>0){
        for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(year,measurement_unit))))){
          this_dataset <-colnames(dplyr::select(df_i1_no,-c(year,measurement_unit)))[d]
          if(sum(dplyr::select(df_i1_no, this_dataset))>0){
            fig <-  fig %>% add_trace(x = df_i1_no$year, y = df_i1_no[,this_dataset][[1]], name = paste0(this_dataset,"_no"), mode = 'lines') 
          }
        }
      }
      
      
    }else{
      for(d in 1:length(colnames(dplyr::select(df_i1,-c(year,measurement_unit))))){
        this_dataset <-colnames(dplyr::select(df_i1,-c(year,measurement_unit)))[d]
        if(sum(dplyr::select(df_i1, this_dataset))>0){
          # data  <- df_i1  %>% dplyr::select(c(year,!!this_dataset))
          if(d==1){
            fig <- plot_ly(df_i1, x = df_i1$year, y =df_i1[,this_dataset][[1]], name = this_dataset, type = 'scatter', mode = 'lines' )
          }else{
            fig <-  fig %>% add_trace(y = df_i1[,this_dataset][[1]], name = paste0(this_dataset,"_",d), mode = 'lines') 
          }
        }
      }
    }
    fig
  })
  
  
  
  
  
  
  output$pie_ratio_catch<- renderPlotly({ 
    # output$plot_species<- renderPlot({ 
    df_i2 = data_pie_all_datasets() # %>% spread(dataset, measurement_value, fill=0)  
    if('global_nominal_catch_firms_level0' %in% unique(df_i2$dataset)){
      total <- filter(df_i2, dataset=='global_nominal_catch_firms_level0')  
      total <- total$measurement_value
    }else{total=1}
    df_i2 =  df_i2 %>% mutate(measurement_value = measurement_value/total)  %>% subset(dataset!='global_nominal_catch_firms_level0')
    
    fig <- plot_ly(df_i2, labels = ~dataset, values = ~measurement_value, type = 'pie',
                   # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Ratio of all datasets for selected measurement_units',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
    
  })
  
  
  
  
  output$pie_gridtype_catch<- renderPlotly({ 
    
    df_i2 = data_pie_gridtype_catch() %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) # %>% filter(measurement_unit == 't') # %>% filter(dataset=='global_nominal_catch_firms_level0')
    if(length(unique(df_i2$measurement_unit))>1){
      df_i2_t <- df_i2 %>% filter(measurement_unit == 't')
      df_i2_no <- df_i2 %>% filter(measurement_unit == 'no')
      if(switch_unit()){
        df_i2 <- df_i2_no
      }else{
        df_i2 <- df_i2_t
      }
    }
    
    row=c(0,0,1,1)
    column=c(0,1,0,1)
    
    if(length(unique(df_i2$dataset))>1){
      fig <- plot_ly()
      for(d in 1:length(unique(df_i2$dataset))){
        cat(df_i2$dataset[d])
        fig <- fig %>% add_pie(data = df_i2 %>% filter(dataset == unique(df_i2$dataset)[d]), labels = ~gridtype, values = ~measurement_value,
                               name = paste0("Dataset : ",df_i2$dataset[d]), domain = list(row =row[d], column =column[d]))
      }
      fig <- fig %>% layout(title = "One pie chart showing the total catch by type of spatial objects (identifed by its gridtype) for each dataset using selected measurement_unit(s)", showlegend = T,
                            grid=list(rows=2, columns=2),
                            xaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE)) %>% add_annotations(x=row+0.5,
                                                                                                                         y=column+05,
                                                                                                                         text = unique(df_i2$dataset),
                                                                                                                         xref = "paper",
                                                                                                                         yref = "paper",
                                                                                                                         xanchor = "left",
                                                                                                                         showarrow = FALSE
                            )
    }else{
      fig <- plot_ly(df_i2, labels = ~gridtype, values = ~measurement_value, type = 'pie',
                     # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     showlegend = TRUE)
      fig <- fig %>% layout(title = 'Pie Chart showing the total catch by type of spatial objects (identifed by its gridtype) for the selected dataset and selected measurement_unit(s) (tons or numbers)',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    
    fig
    # barplot_datasets <- ggplotly(fig, tooltip="text")
    
    
  })
  
  # st_read(con, query = paste0("SELECT dataset, gridtype, measurement_unit, count(*), SUM(measurement_value)  as measurement_value FROM (",sql_query(),") AS foo GROUP BY dataset, gridtype, measurement_unit ORDER BY dataset"))
  # st_read(con, query = paste0("SELECT  dataset, measurement_unit, count(*) AS count, SUM(measurement_value) AS measurement_value  FROM (",sql_query(),") AS foo  WHERE measurement_unit in ('t','MT','no') GROUP BY dataset, measurement_unit ORDER BY dataset"))
  
  # output$barplot_datasets <- renderPlotly({
  #   
  #   df_i1 <- data_barplot_all_datasets()
  #   # df_i1 <- data_pie_gridtype_catch() 
  #   df_i1 <- df_i1 %>% filter(measurement_unit  == 't') %>% dplyr::select(c(dataset, measurement_unit,measurement_value)) # %>% spread(dataset, measurement_value, fill=0)
  #   
  #   # df_i1$dataset <- factor(df_i1$dataset) 
  #   # df_i1$measurement_unit <- factor(df_i1$measurement_unit) 
  #   
  #   p <- ggplot(df_i1) + geom_bar(aes(x = dataset, stat=measurement_value, fill = measurement_unit))
  #   # geom_bar(aes(x = dataset, fill = factor(measurement_unit)), position = position_dodge(preserve = 'single'))
  # 
  #   # p <- ggplot(data=df_i1, aes(x = dataset, stat = measurement_value, fill = measurement_unit)) + geom_bar(stat = "identity", width = 1)
  #   p <- p + ggtitle("Catch by dataset") + xlab("") + ylab("Datasets") # Adds titles
  #   # p <- p + facet_grid(facets=. ~ dataset) # Side by side bar chart
  #   # p <- p + coord_polar(theta="y") # side by side pie chart
  #   # 
  #   
  #   # Turn it interactive
  #   barplot_datasets <- ggplotly(p, tooltip="text")
  #   
  #   
  # })
  
  output$barplot_datasets <- renderPlotly({
    # https://tutorials.cpsievert.me/20190821/#13
    # https://stackoverflow.com/questions/55002248/plotly-stacked-bar-chart-add-trace-loop-issue
    df_i1 <- data_barplot_all_datasets()  %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))  %>% pivot_wider(names_from = measurement_unit, values_from = c("measurement_value", "count"), names_sep="_",values_fill = 0)
    df_i1 <- as.data.frame(df_i1)
    # df_i1 <- data_barplot_all_datasets()  %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))   %>% df_i1(id = rownames(.))  %>% pivot_wider(names_from = measurement_unit, measurement_values_from = c("measurement_value", "count"), names_sep="_",measurement_values_fill = 0, -id)  %>%  plot_ly(x = ~id, y=~measurement_value, type="bar", color=~variable) %>% layout(barmode = "stack")
    
    # mtcars %>%    df_i1(id = rownames(.)) %>% gather(key = "variable",measurement_value = "measurement_value",-id) %>%  plot_ly(x = ~id, y=~measurement_value, type="bar", color=~variable) %>%       layout(barmode = "stack")
    
    # fig <- plot_ly(data=df_i1)
    # for(c in 2:length(colnames(df_i1))){
    for(c in 1:length(colnames(dplyr::select(df_i1,-c(dataset))))){
      this_column_name <- colnames(dplyr::select(df_i1,-c(dataset)))[c]
      # df_i1$tmp <-  dplyr::select(df_i1, c(dataset,!!this_column_name)) 
      # this_df <-  df_i1   %>%  dplyr::select(c(dataset,!!this_column_name))  %>% rename(measurement_value=!!this_column_name)
      
      if(c==1){
        fig <- plot_ly(x = df_i1$dataset, y =df_i1[,this_column_name], type = 'bar', text=df_i1[,this_column_name], name = this_column_name, stroke = I("black"))
        # fig <- plot_ly(df_i1, x = ~dataset, y =~measurement_value_no, type = 'bar', name = this_column_name)
      }else{
        fig <- fig %>% add_trace(y =df_i1[,this_column_name], text=df_i1[,this_column_name],  name = this_column_name, stroke = I("black"))
        # fig <- fig %>% add_trace(y =~count_t,type = 'bar',  name = this_column_name)
        # fig <- fig %>% add_trace(y = ~dplyr::select(df_i1_t, c(paste0(this_column,'_t'))), name = this_column)
      }
    }
    fig <- fig %>%  layout(title = "Total catch for selected datasets and measurement_units (weight or number of fish)",
                           xaxis = list(title = "Datasets"),
                           yaxis = list (title = "Count lines or catches in tons"))
    
    fig
    
    # fig <- plot_ly(df_i1, x = ~dataset, y = ~count, type = 'bar', name = 'Number of lines')
    # fig <- fig %>% add_trace(y = ~measurement_value, name = 'Total catch (in tons)')
    # fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
  
  
}
