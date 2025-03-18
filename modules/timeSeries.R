timeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # plotlyOutput(ns("plotly_time_series_all_datasets")),
    plotlyOutput(ns("plotly_time_series_all_datasets"), height = 'auto', width = 'auto')
    # dygraphOutput(ns("dygraph_all_datasets"), height = 'auto', width = 'auto')
  )
  # fluidRow(
  #   # column(10,plotlyOutput(ns("plotly_time_series_all_datasets"), height = 'auto', width = 'auto')),
  #   plotlyOutput(ns("plotly_time_series_all_datasets"), height = '18%', width = '50%')
  #   # column(2,textOutput("legendDivID"))
  # )
  
}

timeSeriesServer <- function(id,plot_df) {
  flog.info("Starting time series module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("Starting timeSeriesServer")
    
    data_all_datasets <- reactive({
      data_all_datasets <- plot_df()  %>% as.data.frame()  %>%  dplyr::group_by(dataset, year, measurement_unit) %>% 
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
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'),measurement_value=as.integer(measurement_value)) %>% 
        spread(dataset, measurement_value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
      df_i1 <- as_tibble(df_i1)
      all_years <- unique(df_i1$year)  %>% as_tibble() %>% dplyr::rename("year"=value)

      fig <- NULL
      if(length(unique(df_i1$measurement_unit))>0){
        for(u in 1:length(unique(df_i1$measurement_unit))){
          flog.info("Processing Unit ID and name %s:", unique(df_i1$measurement_unit)[u])
          this_unit <- unique(df_i1$measurement_unit)[u]
          df_i1_unit <- df_i1 %>% filter(measurement_unit  == this_unit) 
          column_names <- setdiff(colnames(df_i1_unit),c("year","measurement_unit"))
          for(d in 1:length(column_names) ){
            this_dataset <- column_names[d]
            flog.info("Processing now dataset name %s:", this_dataset)
            df_i1_unit_this_dataset <- df_i1_unit %>% dplyr::select(year,this_dataset)
            flog.info("Adding a time serie if total > 0")
            if(sum(df_i1_unit_this_dataset[,c(this_dataset)])>0){
              flog.info("Extending the time serie to all years (adding null values)")
              df_i1_unit_this_dataset <- all_years %>% left_join(df_i1_unit_this_dataset)
              df_i1_unit_this_dataset[,c(this_dataset)][is.na(df_i1_unit_this_dataset[,c(this_dataset)])] <- 0
              this_name = paste0(this_dataset,"_",this_unit)
              if(is.null(fig)){
                flog.info("First dataset of the time serie")
                fig <- plot_ly(df_i1_unit_this_dataset, x = df_i1_unit_this_dataset$year, y =df_i1_unit_this_dataset[,this_dataset][[1]], 
                               name = this_name, type = 'scatter', mode = 'lines' )
              }else{#class(fig)[1]=="plotly"
                flog.info("Add new layer to the time serie")
                fig <-  fig %>% add_trace(y = df_i1_unit_this_dataset[,this_dataset][[1]],
                                          name = this_name, mode = 'lines') 
              }
            }
          }
            
        }
      }
      fig
    })
    
    flog.info("Starting dygraph_all_datasets with Dygraph")
    output$dygraph_all_datasets <- renderDygraph({
      
      df_i1 <- data_all_datasets()  %>% 
        # mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't') , year = lubridate::ymd(df_i1$year, truncated = 2L)) %>% 
        mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't') , year = as.Date(as.character(df_i1$year), format = "%Y")) %>% 
        spread(dataset, measurement_value, fill=0) 
      # mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't') , year = as.Date(as.character(df_i1$year), format = "%Y")) %>% spread(dataset, measurement_value, fill=0) 
      
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
        
        # if(length(colnames(dplyr::select(df_i1_t,-c(year,measurement_unit))))>1){
        for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(year,measurement_unit))))){
          this_dataset <-colnames(dplyr::select(df_i1_t,-c(year,measurement_unit)))[d]
          if(sum(dplyr::select(df_i1_t, this_dataset))>0){
            df_i1_t  <- df_i1_t  %>% dplyr::rename(!!paste0(this_dataset,'_t') := !!this_dataset)
            this_time_serie <- xts(x = dplyr::select(df_i1_t, c(paste0(this_dataset,'_t'))), order.by = df_i1_t$year)
            if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
              tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
            }
          }
        }
        # if(length(colnames(dplyr::select(df_i1_no,-c(year,measurement_unit))))>1){
        for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(year,measurement_unit))))){
          this_dataset <- colnames(dplyr::select(df_i1_no,-c(year,measurement_unit)))[d]
          if(sum(dplyr::select(df_i1_no,this_dataset))>0){
            df_i1_no  <- df_i1_no  %>% dplyr::rename(!!paste0(this_dataset,'_no') := !!this_dataset)
            this_time_serie <- xts(x = dplyr::select(df_i1_no, c(paste0(this_dataset,'_no'))), order.by = df_i1_no$year)
            tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
          }
        }
        
      }else{
        for(d in 1:length(colnames(dplyr::select(df_i1,-c(year,measurement_unit))))){
          this_dataset <- colnames(dplyr::select(df_i1,-c(year,measurement_unit)))[d]
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

