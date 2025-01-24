timeSeriesGearUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot2"))
  )
}

timeSeriesGearServer <- function(id,plot_df) {
  flog.info("Starting time series module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("Starting timeSeriesGearServer")
    
    data_i2 <- reactive({
      plot_df()  %>%  dplyr::group_by(measurement_unit, gear_type, year, species) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
    })
    
    flog.info("Starting plot to illustrate time series and the contribution of gear types")
    output$plot2 <- renderPlotly({ 
      
      df_i2 <- data_i2()
      df_i2 <- as_tibble(df_i2)
      if(length(unique(df_i2$gear_type))>9){
        sum_df_i2_per_gear <-  df_i2  %>%  dplyr::group_by(measurement_unit, gear_type) %>% 
          dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>% arrange(measurement_unit) 
        list_gear_types <- unique(sum_df_i2_per_gear$gear_type) %>% as.data.frame() %>%  rename_at(1,~"gear_type") %>% 
          dplyr::select(gear_type) %>% top_n(n = 9) %>% pull()
      }
      df_i2 <- df_i2 %>% dplyr::filter(gear_type %in% list_gear_types)
      i2 <- Atlas_i2_SpeciesByGear(as.data.frame(df_i2),
                                   yearAttributeName="year",
                                   speciesAttributeName="species",
                                   valueAttributeName="measurement_value",
                                   gearTypeAttributeName="gear_type",
                                   withSparql=FALSE)
      i2     
      
    })
    
    
    
  })
  flog.info("End of time series module")
}

