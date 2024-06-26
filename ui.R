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
                                          choices = target_measurement_unit$measurement_unit,
                                          multiple = TRUE,
                                          selected= default_unit,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "gridtype",
                                          label = "Grditype",
                                          choices = target_gridtype$gridtype,
                                          multiple = TRUE,
                                          selected= default_gridtype,
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
                                          inputId = "gear_type",
                                          label = "Gear",
                                          choices = target_gear_type$gear_type,
                                          multiple = TRUE,
                                          selected= target_gear_type$gear_type,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "fishing_fleet",
                                          label = "fishing_fleet",
                                          choices = target_flag$fishing_fleet,
                                          multiple = TRUE,
                                          selected= default_fishing_fleet,
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
                                        plotlyOutput("pie_gridtype_catch", width="100%"),
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
