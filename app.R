library(shiny)
library(geojsonio)
library(plyr)
library(dplyr)
library(DBI)
library(leaflet)
library(leaflet.extras)
library(leafpm)
library(mapedit)
library(sf)
library(sp)
library(plotly)
library(DT)
library(ncdf4)
library(raster)
library(maps)
library(ggplot2)
library(XML)
library(streamgraph)
library(viridis)
library(xts)
library(dygraphs)
library(tidyr)
####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i1_SpeciesByOcean.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i2_SpeciesByGear.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
# source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
###################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
switch_unit <- reactiveVal(TRUE) 
query_metadata <- reactiveVal() 
query_all_datasets <- reactiveVal() 

# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
target_dataset <- dbGetQuery(con, "SELECT DISTINCT(dataset) FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY dataset;")
target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY year;")
target_gear <- dbGetQuery(con, "SELECT DISTINCT(gear_group) as gear FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY gear_group;")
target_ocean <- dbGetQuery(con, "SELECT DISTINCT(ocean) as ocean FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY ocean;")
target_unit <- dbGetQuery(con, "SELECT DISTINCT(unit) AS unit FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY unit;")
target_area <- dbGetQuery(con, "SELECT DISTINCT(area)::varchar AS area FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY area DESC;")
target_fishingfleet <- dbGetQuery(con, "SELECT DISTINCT(fishingfleet_label)::varchar AS fishingfleet FROM public.i1i2_spatial_all_datasets_with_flags ORDER BY fishingfleet DESC;")

default_species <- 'YFT'
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
# default_gear <- c('BB','PS')
default_gear <- unique(target_gear)
# default_dataset <- 'global_catch_5deg_1m_firms_level0'
default_dataset <- unique(target_dataset$dataset)
default_unit <-  c('MT','t','no')
# default_unit <- unique(target_unit$unit)
# default_area <- '25'
default_area <- unique(target_area$area)
default_fishingfleet  <-  c('Australia')


#check what are existing / possible combinations between dimension values
filters_combinations <- dbGetQuery(con, "SELECT species, year, gear_group  as gear ,fishingfleet_label as fishingfleet FROM public.i1i2_spatial_all_datasets_with_flags GROUP BY species, year,gear_group,fishingfleet_label;")

ui <- fluidPage(
  titlePanel("Tuna Atlas: spatial indicators (maps, charts, plots...)"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive Indicator 11",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/styles.css")),
                          # leafletOutput('map_i11', width = "60%", height = 1500),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 150,  left = "3%", width = "21%", height = "auto",
                                        selectInput(
                                          inputId = "dataset",
                                          label = "Dataset",
                                          choices = target_dataset$dataset,
                                          multiple = TRUE,
                                          selected= default_dataset,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "unit",
                                          label = "Unit",
                                          choices = target_unit$unit,
                                          multiple = TRUE,
                                          selected= default_unit,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "area",
                                          label = "Area",
                                          choices = target_area$area,
                                          multiple = TRUE,
                                          selected= default_area,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "species",
                                          label = "Species",
                                          choices = target_species$species,
                                          multiple = TRUE,
                                          selected= default_species,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year$year,
                                          multiple = TRUE,
                                          selected= default_year,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "gear",
                                          label = "Gear",
                                          choices = target_gear$gear,
                                          multiple = TRUE,
                                          selected= target_gear$gear,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "fishingfleet",
                                          label = "fishingfleet",
                                          choices = target_fishingfleet$fishingfleet,
                                          multiple = TRUE,
                                          selected= default_fishingfleet,
                                          width = "99%"
                                        ),
                                        textInput("yourWKT","Paste you WKT",value=new_wkt),
                                        verbatimTextOutput("value"),
                                        actionButton(
                                          inputId = "submit",
                                          label = "Submit"
                                        ),
                                        actionButton("resetWkt", "Reset WKT to global"),
                                        tags$br()
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 150, left = "auto", right="1%", width = "23%", height = "auto",
                                        tags$br(),
                                        actionButton(
                                          inputId = "switched",
                                          label = "Switch unit for pie chart"
                                        ),
                                        plotlyOutput("pie_area_catch", width="100%"),
                                        tags$br(),
                                        plotlyOutput("barplot_datasets", width="100%"),
                                        tags$br(),
                                        plotlyOutput("pie_ratio_catch")
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default", bottom =  "2%", left = "25%", width = "50%", fixed=TRUE, draggable = FALSE, height = "auto",
                                        plotlyOutput("plotly_time_series_all_datasets")
                                        # fluidRow(
                                        #   column(10,dygraphOutput("dygraph_all_datasets",height="400")),
                                        #   column(2,textOutput("legendDivID"))
                                        # )
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))
                          )
                      )
             ),
             tabPanel(
               title = "Plot indicator 2",
               plotlyOutput("plot2")
             ),
             tabPanel(
               title = "Streamgraph indicator 2",
               streamgraphOutput("plot2_streamgraph")
             ),
             navbarMenu("Browse Data Tables",
                        tabPanel(
                          title = "Browse query_metadata",
                          DT::dataTableOutput("DT_query_metadata")
                        ),
                        tabPanel(
                          title = "Browse data_all_datasets",
                          DT::dataTableOutput("DT_data_all_datasets")
                        ),
                        tabPanel(
                          title = "Browse data_barplot_all_datasets",
                          DT::dataTableOutput("DT_data_barplot_all_datasets")
                        )
             ),
             # tabPanel(
             #   title = "Your filters",
             #   textOutput("selected_var")
             # ),
             navbarMenu("Browse underlying SQL queries",
                        tabPanel(
                          title = "The main SQL query: sql_query: sql_query",
                          textOutput("sql_query")
                        ),
                        tabPanel(
                          title = "SQL query: query_metadata",
                          textOutput("query_metadata")
                        ),
                        tabPanel(
                          title = "SQL query: query_all_datasets",
                          textOutput("query_all_datasets")
                        )
             ),
             tabPanel("About",
                      fluidRow(
                        column(6,
                               includeMarkdown("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/README.md")
                        ),
                        column(6,
                               img(class="logo_IRD",
                                   src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                               tags$small(
                                 "Source: IRD",
                                 "Julien Barde ",
                                 "Funding : BlueCloud ",
                                 a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                   "IRD Tuna Atlas (Alain Fontenau)"),
                                 a(href="https://github.com/juldebar/IRDTunaAtlas/wiki/Indicator-I11-:-Catches-by-country",
                                   "IRD Indicator 11"),
                                 a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                   "IRD Tuna Atlas (Alain Fontenau)"),
                                 a(href="https://horizon.documentation.ird.fr/exl-doc/pleins_textes/divers11-03/010012425.pdf",
                                   "PDF")
                               )
                        )
                      )
             )
  )
)



server <- function(input, output, session) {
  
  
  ########################################################## Dynamic filters ########################################################## 
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear),collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
  })
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
    
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
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    # if(is.null(input$dataset)){dataset_name=target_dataset$dataset}else{year_name=input$dataset}
    query <- glue::glue_sql(
      "SELECT dataset, unit, ocean, gear_group, year, species, value, area, fishingfleet_label AS fishingfleet, geom  FROM public.i1i2_spatial_all_datasets_with_flags 
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND  dataset IN ({dataset_name*}) 
      AND  species IN ({species_name*}) 
      AND gear_group IN ({gear_group_name*})
      AND fishingfleet_label IN ({fishingfleet_name*})
      AND year IN ({year_name*})
      AND area::varchar IN ({area_name*})
      AND unit IN ({unit_name*}) ",
      wkt = wkt(),
      dataset_name = input$dataset,
      species_name = input$species,
      gear_group_name = input$gear,
      fishingfleet_name = input$fishingfleet,
      year_name = year_name,
      unit_name = input$unit,
      area_name = input$area,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  metadata <- reactive({
    query_metadata(paste0("SELECT geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY geom"))
    st_read(con, query = query_metadata()) 
  })  
  
  # unit, ocean, gear_group, year, species ;")
  data_all_datasets <- eventReactive(input$submit, {
    query_all_datasets(paste0("SELECT dataset, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value, unit FROM (",sql_query(),") AS foo GROUP BY dataset, year, species, unit"))
    st_read(con, query = query_all_datasets())
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_pie_all_datasets <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT dataset, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  dataset"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  data_pie_area_catch <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (",sql_query(),") AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
  },
  ignoreNULL = FALSE)
  
  data_barplot_all_datasets <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT  dataset, unit, count(*)::decimal AS count, SUM(value) AS value  FROM (",sql_query(),") AS foo GROUP BY dataset, unit ORDER BY dataset"))
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i1 <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT dataset, unit, ocean, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  dataset, unit, ocean, year, species"))
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i2 <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT unit, gear_group AS gear_type, year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY unit, gear_group, year, species")) 
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  ########################################################## Outputs: text & Data tables ########################################################## 
  output$value <- renderText({ 
    wkt()
    # output$value <- renderText({ input$caption })
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
    # data_barplot_all_datasets()   %>% mutate(unit=replace(unit,unit=='MT', 't'))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0)
    data_barplot_all_datasets() 
  }) 
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$flag, "and \n", wkt())
  })
  
  
  ########################################################## Outputs: maps / plots / charts ########################################################## 
  
  
  
  output$mymap <- renderLeaflet({
    
    df <- metadata()
    centroid <-  st_convex_hull(df) %>% st_centroid()
    lat_centroid <- st_coordinates(centroid)[2]
    lon_centroid <- st_coordinates(centroid)[1]
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = df$value
    )
    # brewer.pal(7, "OrRd")
    pal_fun <- colorQuantile(   "YlOrRd", NULL, n = 10)
    
    qpal <- colorQuantile(rev(viridis::viridis(10)),
                          df$value, n=10)
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_leaflet <- leaflet() %>%
      setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
      ) %>%  
      clearBounds() %>%
      addProviderTiles("Esri.OceanBasemap")  %>% 
      addPolygons(data = df,
                  label = ~value,
                  popup = ~paste0("Captures pour cette espece: ", round(value), " tonnes(t) et des brouettes"),
                  # fillColor = ~pal_fun(value),
                  fillColor = ~qpal(value),
                  fill = TRUE,
                  fillOpacity = 0.8,
                  smoothFactor = 0.5
                  # color = ~pal(value)
      ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      )  %>%
      addLayersControl(
        overlayGroups = c("draw"),
        options = layersControlOptions(collapsed = FALSE)
      )  %>% 
      leaflet::addLegend("bottomright", pal = qpal, values = df$value,
                         title = "Total catch per cell for selected criteria",
                         labFormat = labelFormat(prefix = "MT "),
                         opacity = 1
      )  
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
  
  

  

  output$plot2 <- renderPlotly({ 
    
    
    # df_i2 = data() %>% filter(year %in% input$year) %>% filter(gear_group %in% input$gear) %>% group_by(year,gear_group,species)   %>% summarise(value = sum(value))  %>% dplyr::rename(gear_type=gear_group)
    df_i2 <- data_i2()
    
    
    i2 <- Atlas_i2_SpeciesByGear(as.data.frame(df_i2),
                                 yearAttributeName="year",
                                 speciesAttributeName="species",
                                 valueAttributeName="value",
                                 gearTypeAttributeName="gear_type",
                                 withSparql=FALSE)
    i2 
  })
  

  
  
  
  output$plot2_streamgraph<- renderStreamgraph({ 
    df_i2 =  data_i2() %>% 
      streamgraph("gear_type", "value", "year", offset="zero", interpolate="step") %>%
      sg_axis_x(1, "year", "%Y") %>%
      sg_fill_brewer("PuOr") %>%
      sg_legend(show=TRUE, label="I=RFMO - names: ")
  })
  
  
  
  output$dygraph_all_datasets <- renderDygraph({
    
    # df_i1 = data_all_datasets()  %>% filter(unit %in% c('t','MT'))  %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    # df_i1 = data_all_datasets()  %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 = data_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't')) %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    tuna_catches_timeSeries <-NULL
    
    if(length(unique(df_i1$unit))>1){
      df_i1_t <- df_i1 %>% filter(unit  == 't') 
      df_i1_no <- df_i1 %>% filter(unit == 'no')  
      # if(switch_unit()){
      #   df_i1 <- df_i1_no
      # }else{
      #   df_i1 <- df_i1_t
      # }
      
      # if(length(colnames(dplyr::select(df_i1_t,-c(species,year,unit))))>1){
      for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(species,year,unit))))){
        this_dataset <-colnames(dplyr::select(df_i1_t,-c(species,year,unit)))[d]
        if(sum(dplyr::select(df_i1_t, this_dataset))>0){
          df_i1_t  <- df_i1_t  %>% dplyr::rename(!!paste0(this_dataset,'_t') := !!this_dataset)
          this_time_serie <- xts(x = dplyr::select(df_i1_t, c(paste0(this_dataset,'_t'))), order.by = df_i1_t$year)
          if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
            tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
          }
        }
      }
      # if(length(colnames(dplyr::select(df_i1_no,-c(species,year,unit))))>1){
      for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(species,year,unit))))){
        this_dataset <- colnames(dplyr::select(df_i1_no,-c(species,year,unit)))[d]
        if(sum(dplyr::select(df_i1_no,this_dataset))>0){
          df_i1_no  <- df_i1_no  %>% dplyr::rename(!!paste0(this_dataset,'_no') := !!this_dataset)
          this_time_serie <- xts(x = dplyr::select(df_i1_no, c(paste0(this_dataset,'_no'))), order.by = df_i1_no$year)
          tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
        }
      }
      
    }else{
      for(d in 1:length(colnames(dplyr::select(df_i1,-c(species,year,unit))))){
        this_dataset <- colnames(dplyr::select(df_i1,-c(species,year,unit)))[d]
        this_time_serie <- xts(x = dplyr::select(df_i1, c(this_dataset)), order.by = df_i1$year)
        if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
          tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
        }
      }
      
      
      # create the area chart
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
    
    g1 <- dygraph(tuna_catches_timeSeries, main = "Times series of values with selected units (tons or numbers) for the selected datasets") %>% dyRangeSelector() %>% dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = T)
    
    
    
  })
  
  
  
  output$plotly_time_series_all_datasets <- renderPlotly({
    
    
    # df_i1 =  st_read(con, query = "SELECT dataset, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value, unit FROM (SELECT dataset, unit, ocean, gear_group, year, species, value, area, geom FROM public.i1i2_spatial_all_datasets_with_flags WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND dataset IN ('global_catch_1deg_1m_ps_bb_firms_level0', 'global_catch_5deg_1m_firms_level0', 'global_catch_firms_level0', 'global_nominal_catch_firms_level0') AND species IN ('YFT') AND gear_group IN ('BB', 'LL', 'OTHER', 'PS', 'UNK') AND year IN ('1918', '1919', '1920', '1921', '1922', '1923', '1924', '1925', '1926', '1927', '1928', '1929', '1930', '1931', '1932', '1933', '1934', '1935', '1936', '1937', '1938', '1939', '1940', '1941', '1942', '1943', '1944', '1945', '1946', '1947', '1948', '1949', '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019') AND area::varchar IN ('NA', '9101.3790835111', '825', '775', '750', '6995.49529425369', '6014.62349518009', '600', '525', '50', '450', '400', '375', '325', '3125', '3021.33388249682', '2750', '2625', '2612.4315513254', '2550', '250', '25', '2375', '2325', '2275', '2100', '200', '1425', '1350', '1300', '1250', '1125', '10040.35433829', '100', '1') AND unit IN ('MT', 'no', 't') ) AS foo GROUP BY dataset, year, species, unit")  %>%
      # mutate(unit=replace(unit,unit=='MT', 't')) %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 = data_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't')) %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    
    if(length(unique(df_i1$unit))>1){
      df_i1_t <- df_i1 %>% filter(unit  == 't') 
      df_i1_no <- df_i1 %>% filter(unit == 'no')  
      for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(species,year,unit))))){
        this_dataset <-colnames(dplyr::select(df_i1_t,-c(species,year,unit)))[d]
        if(sum(dplyr::select(df_i1_t, this_dataset))>0){
          # data  <- df_i1_t  %>% dplyr::select(c(year,!!this_dataset))
          if(d==1){
            fig <- plot_ly(df_i1_t, x = df_i1_t$year, y =df_i1_t[,this_dataset][[1]], name = paste0(this_dataset,"_",d), type = 'scatter', mode = 'lines' )
            }else{
              fig <-  fig %>% add_trace(y = df_i1_t[,this_dataset][[1]], name = paste0(this_dataset,"_t"), mode = 'lines') 
          }
        }
      }
      for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(species,year,unit))))){
        this_dataset <-colnames(dplyr::select(df_i1_no,-c(species,year,unit)))[d]
        if(sum(dplyr::select(df_i1_no, this_dataset))>0){
          fig <-  fig %>% add_trace(x = df_i1_no$year, y = df_i1_no[,this_dataset][[1]], name = paste0(this_dataset,"_no"), mode = 'lines') 
        }
      }
      
    }else{
      for(d in 1:length(colnames(dplyr::select(df_i1,-c(species,year,unit))))){
        this_dataset <-colnames(dplyr::select(df_i1,-c(species,year,unit)))[d]
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
    df_i2 = data_pie_all_datasets() # %>% spread(dataset, value, fill=0)  
    if('global_nominal_catch_firms_level0' %in% unique(df_i2$dataset)){
      total <- filter(df_i2, dataset=='global_nominal_catch_firms_level0')  
      total <- total$value
    }else{total=1}
    df_i2 =  df_i2 %>% mutate(value = value/total)  %>% subset(dataset!='global_nominal_catch_firms_level0')
    
    fig <- plot_ly(df_i2, labels = ~dataset, values = ~value, type = 'pie',
                   # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Ratio of all datasets for selected units',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
    
  })
  
  
  
  
output$pie_area_catch<- renderPlotly({ 
  
  df_i2 = data_pie_area_catch()   %>% mutate(unit=replace(unit,unit=='MT', 't')) # %>% filter(unit == 't') # %>% filter(dataset=='global_nominal_catch_firms_level0')
  # df_i2 = st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (SELECT dataset, unit, ocean, gear_group, year, species, value, area, geom FROM public.i1i2_spatial_all_datasets_with_flags                                         WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND  species IN ('YFT') AND gear_group IN ('LL', 'PS', 'BB', 'OTHER', 'UNK') AND year IN ('1919', '1920', '1921', '1922', '1923', '1924', '1925', '1926', '1927', '1928', '1929', '1930', '1931', '1932', '1933', '1934', '1935', '1936', '1937', '1938', '1939', '1940', '1941', '1942', '1943', '1944', '1945', '1946', '1947', '1948', '1949', '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019') AND area::varchar IN ('NA', '9101.3790835111', '825', '775', '750', '6995.49529425369', '6014.62349518009', '600', '525', '50', '450', '400', '375', '325', '3125', '3021.33388249682', '2750', '2625', '2612.4315513254', '2550', '250', '25', '2375', '2325', '2275', '2100', '200', '1425', '1350', '1300', '1250', '1125', '10040.35433829', '100', '1') ) AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
  # df_i2 <- df_i2 %>% mutate(unit=replace(unit,unit=='MT', 't')) # %>% filter(unit == 't')
  if(length(unique(df_i2$unit))>1){
    df_i2_t <- df_i2 %>% filter(unit == 't')
    df_i2_no <- df_i2 %>% filter(unit == 'no')
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
      fig <- fig %>% add_pie(data = df_i2 %>% filter(dataset == unique(df_i2$dataset)[d]), labels = ~area, values = ~value,
                             name = paste0("Dataset : ",df_i2$dataset[d]), domain = list(row =row[d], column =column[d]))
    }
    fig <- fig %>% layout(title = "One pie chart showing the total catch by type of spatial objects (identifed by its area) for each dataset using selected unit(s)", showlegend = T,
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
    fig <- plot_ly(df_i2, labels = ~area, values = ~value, type = 'pie',
                   # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Pie Chart showing the total catch by type of spatial objects (identifed by its area) for the selected dataset and selected unit(s) (tons or numbers)',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  
  fig
  # barplot_datasets <- ggplotly(fig, tooltip="text")
  
  
})

# st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (",sql_query(),") AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
# st_read(con, query = paste0("SELECT  dataset, unit, count(*) AS count, SUM(value) AS value  FROM (",sql_query(),") AS foo  WHERE unit in ('t','MT','no') GROUP BY dataset, unit ORDER BY dataset"))

# output$barplot_datasets <- renderPlotly({
#   
#   df_i1 <- data_barplot_all_datasets()
#   # df_i1 <- data_pie_area_catch() 
#   df_i1 <- df_i1 %>% filter(unit  == 't') %>% dplyr::select(c(dataset, unit,value)) # %>% spread(dataset, value, fill=0)
#   
#   # df_i1$dataset <- factor(df_i1$dataset) 
#   # df_i1$unit <- factor(df_i1$unit) 
#   
#   p <- ggplot(df_i1) + geom_bar(aes(x = dataset, stat=value, fill = unit))
#   # geom_bar(aes(x = dataset, fill = factor(unit)), position = position_dodge(preserve = 'single'))
# 
#   # p <- ggplot(data=df_i1, aes(x = dataset, stat = value, fill = unit)) + geom_bar(stat = "identity", width = 1)
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
  df_i1 <- data_barplot_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't'))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0)
  # df_i1 <- st_read(con, query = paste0("SELECT  dataset, unit, count(*)::decimal AS count, SUM(value) AS value  FROM (SELECT dataset, unit, ocean, gear_group, year, species, value, area, geom FROM public.i1i2_spatial_all_datasets_with_flags WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND dataset IN ('global_catch_1deg_1m_ps_bb_firms_level0', 'global_catch_5deg_1m_firms_level0', 'global_catch_firms_level0', 'global_nominal_catch_firms_level0') AND species IN ('YFT') AND gear_group IN ('BB', 'LL', 'OTHER', 'PS', 'UNK') AND year IN ('1918', '1919', '1920', '1921', '1922', '1923', '1924', '1925', '1926', '1927', '1928', '1929', '1930', '1931', '1932', '1933', '1934', '1935', '1936', '1937', '1938', '1939', '1940', '1941', '1942', '1943', '1944', '1945', '1946', '1947', '1948', '1949', '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019') AND area::varchar IN ('NA', '9101.3790835111', '825', '775', '750', '6995.49529425369', '6014.62349518009', '600', '525', '50', '450', '400', '375', '325', '3125', '3021.33388249682', '2750', '2625', '2612.4315513254', '2550', '250', '25', '2375', '2325', '2275', '2100', '200', '1425', '1350', '1300', '1250', '1125', '10040.35433829', '100', '1') AND unit IN ('MT', 'no', 't')) AS foo GROUP BY dataset, unit ORDER BY dataset"))   %>% mutate(unit=replace(unit,unit=='MT', 't'))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0)
  df_i1 <- as.data.frame(df_i1)
  # df_i1 <- data_barplot_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't'))   %>% df_i1(id = rownames(.))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0, -id)  %>%  plot_ly(x = ~id, y=~value, type="bar", color=~variable) %>% layout(barmode = "stack")
  
  # mtcars %>%    df_i1(id = rownames(.)) %>% gather(key = "variable",value = "value",-id) %>%  plot_ly(x = ~id, y=~value, type="bar", color=~variable) %>%       layout(barmode = "stack")
  
  # fig <- plot_ly(data=df_i1)
  # for(c in 2:length(colnames(df_i1))){
  for(c in 1:length(colnames(dplyr::select(df_i1,-c(dataset))))){
    this_column_name <- colnames(dplyr::select(df_i1,-c(dataset)))[c]
    # df_i1$tmp <-  dplyr::select(df_i1, c(dataset,!!this_column_name)) 
    # this_df <-  df_i1   %>%  dplyr::select(c(dataset,!!this_column_name))  %>% rename(value=!!this_column_name)
    
    if(c==1){
      fig <- plot_ly(x = df_i1$dataset, y =df_i1[,this_column_name], type = 'bar', text=df_i1[,this_column_name], name = this_column_name, stroke = I("black"))
      # fig <- plot_ly(df_i1, x = ~dataset, y =~value_no, type = 'bar', name = this_column_name)
    }else{
      fig <- fig %>% add_trace(y =df_i1[,this_column_name], text=df_i1[,this_column_name],  name = this_column_name, stroke = I("black"))
      # fig <- fig %>% add_trace(y =~count_t,type = 'bar',  name = this_column_name)
      # fig <- fig %>% add_trace(y = ~dplyr::select(df_i1_t, c(paste0(this_column,'_t'))), name = this_column)
    }
  }
  fig <- fig %>%  layout(title = "Total catch for selected datasets and units (weight or number of fish)",
                         xaxis = list(title = "Datasets"),
                         yaxis = list (title = "Count lines or catches in tons"))
  
  fig
  
  # fig <- plot_ly(df_i1, x = ~dataset, y = ~count, type = 'bar', name = 'Number of lines')
  # fig <- fig %>% add_trace(y = ~value, name = 'Total catch (in tons)')
  # fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  
  
})


}


# Run the application 
shinyApp(ui = ui, server = server)




