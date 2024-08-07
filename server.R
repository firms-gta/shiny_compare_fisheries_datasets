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
  
  # flog.info("Filter areas id in the current wkt")
  # list_areas(df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(wkt(), crs = 4326), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% as.vector())
  # flog.info("Fist ten values of the matching areas : %s", list_areas()[1:10])
  
  # input$applytWkt
  # list_areas <-  eventReactive(input$mymap_draw_new_feature, {
  #   # feature <- input$mymap_draw_new_feature
  #   # geoJson <- geojsonio::as.json(feature)
  #   # geom <- st_read(geoJson) # wkt(st_as_text(st_geometry(geom[1,])))
  #   current_selection <- st_as_sfc(wkt, crs = 4326)
  #   df_distinct_geom  %>%  dplyr::filter(st_within(st_as_sfc(wkt()), sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
  # },
  # ignoreNULL = FALSE)
  
  eventReactive(input$resetWkt, {
    wkt(bbox)
    },
    ignoreNULL = FALSE)
  
  flog.info("Apply current filters to the main datasets when click on submit")
  sql_query <- eventReactive(input$submit, {
    # sql_query <- reactive({
    # req(initial_data())
    main_data <- initial_data()
    req(wkt())
    wkt <- wkt()    
    current_selection <- st_as_sfc(wkt , crs = 4326)
    
    flog.info("Current WKT is : %s", wkt)
    # list_areas <- df_distinct_geom[st_within(df_distinct_geom, current_selection, sparse = FALSE), ] %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
    # list_areas <- df_distinct_geom[st_contains(current_selection, df_distinct_geom, sparse = FALSE),] %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
    list_areas <- df_distinct_geom %>% dplyr::filter(!is.na(gridtype)) %>% 
      # dplyr::filter(st_contains(current_selection, sparse = FALSE))  %>%
      dplyr::filter(st_within(df_distinct_geom,current_selection, sparse = FALSE)) # %>%    st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
    # list_areas <- df_distinct_geom %>% dplyr::filter(st_contains(current_selection, sparse = FALSE)) %>% st_drop_geometry() %>%  dplyr::select(codesource_area) %>% pull()
    # df_distinct_geom %>% mutate(area=st_area(.)) %>% arrange(desc(area))
    flog.info("Remaining number of different areas within this WKT: %s", length(list_areas))
    within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%  rename_at(1,~"codesource_area") %>%  dplyr::select(codesource_area) %>% pull()
    
    
    sql_query <- main_data  %>%  dplyr::filter(
      # codesource_area %in% within_areas,
      dataset %in% input$dataset,
      species %in% input$species,
      gear_type %in% input$gear_type,
      year %in% input$year,
      fishing_fleet %in% input$fishing_fleet,
      measurement_unit %in% input$unit,
      gridtype %in% input$gridtype) %>%
      dplyr::group_by(codesource_area,geom, dataset, species,gear_type, year, measurement_unit, gridtype) %>% 
      # dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% ungroup() %>%
      filter(!is.na(geom)) %>% st_as_sf(wkt="geom",crs=4326)  %>%
      dplyr::filter(st_within(.,current_selection, sparse = FALSE)) 
      # dplyr::filter(st_within(main_data,current_selection, sparse = FALSE))
    # browser()
    #%>% top_n(10000)
    # %>% st_as_sf(wkt="geom")  # %>% dplyr::group_by(codesource_area,dataset, species,gear_type, year, fishing_fleet, measurement_unit, gridtype) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) #%>% top_n(10000)

    
    # sql_query <- initial_data() %>% dplyr::filter(
    #   dataset %in% input$dataset,
    #   species %in% input$species,
    #   gear_type %in% input$gear_type,
    #   year %in% input$year,
    #   fishing_fleet %in% input$fishing_fleet,
    #   measurement_unit %in% input$unit,
    #   gridtype %in% input$gridtype)   %>% 
    #   dplyr::group_by(codesource_area,geom, dataset, species,gear_type, year, measurement_unit, gridtype) %>%
    #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE))  %>% 
    #   filter(!is.na(gridtype)) %>% st_as_sf(wkt="geom",crs=4326) %>% dplyr::filter(st_within(initial_data(),current_selection, sparse = FALSE)) %>%  st_drop_geometry()
    # 
                          
    flog.info("Main data number rows: %s", nrow(sql_query))
    sql_query
  },
  ignoreNULL = FALSE)
# })

  # measurement_unit,  gear_type, year, species ;")
  data_all_datasets <- reactive({
    # query_all_datasets(paste0("SELECT dataset, year, sum(measurement_value) as measurement_value, measurement_unit FROM (",sql_query(),") AS foo GROUP BY dataset, year, measurement_unit"))
    # dbGetQuery(con,query_all_datasets())
    data_all_datasets <- sql_query() %>%  st_drop_geometry() %>% dplyr::group_by(dataset, year, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
    data_all_datasets
  })
  
  
  data_pie_all_datasets <- reactive({
    # dbGetQuery(con,paste0("SELECT dataset, sum(measurement_value) as measurement_value FROM (",sql_query(),") AS foo GROUP BY  dataset"))
    data_pie_all_datasets <- sql_query()  %>%  st_drop_geometry() %>% dplyr::group_by(dataset) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
    data_pie_all_datasets
    
  })
  
  data_pie_gridtype_catch <- reactive({
    sql_query()  %>%  st_drop_geometry() %>% dplyr::group_by(dataset, gridtype, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), count=n()) 
  })
  
  data_barplot_all_datasets <- reactive({
    sql_query() %>%  st_drop_geometry() %>% dplyr::group_by(dataset, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), count=n()) 
  })
  
  
  data_i1 <- reactive({
    sql_query() %>%  st_drop_geometry() %>% dplyr::group_by(dataset, measurement_unit, year, species) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
  })
  
  data_i2 <- reactive({
    sql_query() %>%  st_drop_geometry() %>% dplyr::group_by(measurement_unit, gear_type, year, species) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
  })
  
  
  flog.info("##########################################################")
  flog.info("Outputs: text & Data tables")
  flog.info("##########################################################")
  
  
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$fishing_fleet, "and \n", wkt())
  })
  
  
  output$updatedWKT <- renderText({
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
  
  flog.info("Data Table of the map")
  output$DT_query_data_map <- renderDT({
    sql_query()
  })

  flog.info("Data Table of the time series")
  output$DT_data_all_datasets <- renderDT({
    data_all_datasets()
  })
  
  flog.info("Data Table of the bar plots")
  output$DT_data_barplot_all_datasets <- renderDT({
    # data_barplot_all_datasets()   %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))  %>% pivot_wider(names_from = measurement_unit, measurement_values_from = c("measurement_value", "count"), names_sep="_",measurement_values_fill = 0)
    data_barplot_all_datasets() 
  }) 
  

  
  flog.info("##########################################################")
  flog.info("Outputs: maps / plots / charts")
  flog.info("##########################################################")
  
  
  flog.info("Starting leaflet in the global map module")
  # callModule(module = map_leaflet, id = "id_1")
  map_leafletServer(id = "map_global",sql_query)

  flog.info("Starting plot if indicator 2")
  output$plot2 <- renderPlotly({ 
    
    df_i2 <- data_i2()
    df_i2 <- as_tibble(df_i2)
    i2 <- Atlas_i2_SpeciesByGear(as.data.frame(df_i2),
                                 yearAttributeName="year",
                                 speciesAttributeName="species",
                                 valueAttributeName="measurement_value",
                                 gearTypeAttributeName="gear_type",
                                 withSparql=FALSE)
    i2 
  })
  
  
  
  
  flog.info("Starting plot2 with streamgraph")
  output$plot2_streamgraph<- renderStreamgraph({ 
    df_i2 =  data_i2() %>% 
      streamgraph("gear_type", "measurement_value", "year", offset="zero", interpolate="step") %>%
      sg_axis_x(1, "year", "%Y") %>%
      sg_fill_brewer("PuOr") %>%
      sg_legend(show=TRUE, label="I=RFMO - names: ")
  })
  
  
  
  flog.info("Starting dygraph_all_datasets with Dygraph")
  output$dygraph_all_datasets <- renderDygraph({
    
    df_i1 <- data_all_datasets()  %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) %>% spread(dataset, measurement_value, fill=0) 
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
    
    g1 <- dygraph(tuna_catches_timeSeries, main = "Times series of measurement_values with selected measurement_units (tons or numbers) for the selected datasets") %>% 
      dyRangeSelector() %>% dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = T)
    
    
    
  })
  
  
  flog.info("Starting plotly_time_series_all_datasets")
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
  
  
  
  
  
  flog.info("Starting plotly pie_ratio_catch")
  output$pie_ratio_catch<- renderPlotly({ 
    # output$plot_species<- renderPlot({ 
    df_i2 = data_pie_all_datasets() # %>% spread(dataset, measurement_value, fill=0)  
    if('global_nominal_catch_firms_level0' %in% unique(df_i2$dataset)){
      total <- filter(df_i2, dataset=='global_nominal_catch_firms_level0')  
      total <- total$measurement_value
    }else{total=1}
    df_i2 =  df_i2 %>% mutate(measurement_value = measurement_value/total)  %>% subset(dataset!='global_nominal_catch_firms_level0')
    df_i2 <- as_tibble(df_i2)
    
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
  
  
  
  flog.info("Starting plotly pie_gridtype_catch")
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
  flog.info("Starting plotly barplot_datasets")
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
    # dbDisconnect(con)
  })
  
  
}
