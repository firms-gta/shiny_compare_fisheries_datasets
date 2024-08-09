pieBarChartsUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("pie_gridtype_catch"), width="100%"),
    tags$br(),
    plotlyOutput(ns("barplot_datasets"), width="100%"),
    tags$br(),
    plotlyOutput(ns("pie_ratio_catch"))
  )
}

pieBarChartsServer <- function(id,sql_query) {
  flog.info("Starting pie and bar charts module")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("Starting pieBarChartsServer")
    
    data_pie_gridtype_catch <- reactive({
      sql_query()  %>% dplyr::group_by(dataset, gridtype, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), count=n()) 
    })
    
    data_barplot_all_datasets <- reactive({
      sql_query()  %>%  dplyr::group_by(dataset, measurement_unit) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), count=n()) 
    })
    
    data_pie_all_datasets <- reactive({
      data_pie_all_datasets <- sql_query()  %>% dplyr::group_by(dataset) %>% dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) 
    })
    
    flog.info("Data Table of the bar plots")
    output$DT_data_barplot_all_datasets <- renderDT({
      # data_barplot_all_datasets()   %>% mutate(measurement_unit=replace(measurement_unit,measurement_unit=='MT', 't'))  %>% pivot_wider(names_from = measurement_unit, measurement_values_from = c("measurement_value", "count"), names_sep="_",measurement_values_fill = 0)
      data_barplot_all_datasets() 
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
    
    
  output$barplot_datasets2 <- renderPlotly({

    df_i1 <- data_barplot_all_datasets()
    # df_i1 <- data_pie_gridtype_catch()
    df_i1 <- df_i1 %>% filter(measurement_unit  == 't') %>% dplyr::select(c(dataset, measurement_unit,measurement_value)) # %>% spread(dataset, measurement_value, fill=0)

    # df_i1$dataset <- factor(df_i1$dataset)
    # df_i1$measurement_unit <- factor(df_i1$measurement_unit)

    p <- ggplot(df_i1) + geom_bar(aes(x = dataset, stat=measurement_value, fill = measurement_unit))
    # geom_bar(aes(x = dataset, fill = factor(measurement_unit)), position = position_dodge(preserve = 'single'))

    # p <- ggplot(data=df_i1, aes(x = dataset, stat = measurement_value, fill = measurement_unit)) + geom_bar(stat = "identity", width = 1)
    p <- p + ggtitle("Catch by dataset") + xlab("") + ylab("Datasets") # Adds titles
    # p <- p + facet_grid(facets=. ~ dataset) # Side by side bar chart
    # p <- p + coord_polar(theta="y") # side by side pie chart
    #

    # Turn it interactive
    barplot_datasets <- ggplotly(p, tooltip="text")


  })
    
    
    
 
    })
  flog.info("End of time series module")
}

