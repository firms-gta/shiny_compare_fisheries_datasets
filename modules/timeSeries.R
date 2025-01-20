timeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # plotlyOutput(ns("plotly_time_series_all_datasets")),
    plotlyOutput(ns("plotly_time_series_all_datasets"))
    # dygraphOutput(ns("dygraph_all_datasets"),height="400")
  )
}

timeSeriesServer <- function(id,sql_query_all) {
  flog.info("Starting time series module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("Starting timeSeriesServer")
    
    data_all_datasets <- reactive({
      data_all_datasets <- sql_query_all()  %>% as.data.frame()  %>%  dplyr::group_by(dataset, year, measurement_unit) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
    })
    
    flog.info("Data Table of the time series")
    output$DT_data_all_datasets <- renderDT({
      data_all_datasets() 
    })
    
    flog.info("Starting plotly_time_series_all_datasets")
    output$plotly_time_series_all_datasets <- renderPlotly({
      req(data_all_datasets())
      df_i1 = data_all_datasets()  %>% 
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't')) %>% 
        spread(dataset, measurement_value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
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
        # g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by ocean") %>% dyRangeSelector() %>% dyLegend(labelsDiv = "legendDivID")
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
    
    
 
    })
  flog.info("End of time series module")
}

