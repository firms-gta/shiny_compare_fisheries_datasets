ui <- fluidPage(
  # titlePanel("Global Tuna Atlas"),
  navbarPage(title="Compare datasets",
             position = c("fixed-top"),
             fluid = TRUE,
             collapsible = TRUE,
             # tags$head(
             #   tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
             #                padding-top:4px !important; 
             #                padding-bottom:0 !important;
             #                height: 5%;
             #                width: 80%;
             #                }
             #               .navbar {min-height:25px !important;}'))
             # ),
             # map_leafletUI("id_1"),
             tabPanel("Datasets overview",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/styles.css")),
                          map_leafletUI("map_global"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 150,  left = "3%", width = "21%", height = "auto",
                                        selectInput(
                                          inputId = "dataset",
                                          label = "Dataset",
                                          choices = target_dataset,
                                          multiple = TRUE,
                                          selected= default_dataset,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "unit",
                                          label = "Unit",
                                          choices = target_measurement_unit,
                                          multiple = TRUE,
                                          selected= default_unit,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "gridtype",
                                          label = "Gridtype",
                                          choices = target_gridtype,
                                          multiple = TRUE,
                                          selected= default_gridtype,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "species",
                                          label = "Species",
                                          choices = target_species,
                                          multiple = TRUE,
                                          selected= default_species,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year,
                                          multiple = TRUE,
                                          selected= default_year,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "gear_type",
                                          label = "Gear",
                                          choices = target_gear_type,
                                          multiple = TRUE,
                                          selected= default_gear_type,
                                          width = "99%"
                                        ),
                                        selectInput(
                                          inputId = "fishing_fleet",
                                          label = "fishing_fleet",
                                          choices = target_flag,
                                          multiple = TRUE,
                                          selected= default_fishing_fleet,
                                          width = "99%"
                                        ),
                                        # textInput(inputId ="yourWKT",label ="Draw or paste a new WKT"),
                                        # textInput("yourWKT","Paste you WKT",value=new_wkt),
                                        # textInput("yourWKT","Paste you WKT",value=textOutput("updatedWKT")),
                                        verbatimTextOutput("updatedWKT"),
                                        map_leafletUI("other"),
                                        actionButton(inputId ="resetWkt", label = "Reset WKT (no spatial filter)"),
                                        
                                        actionButton(inputId = "submit",label = "Submit"),
                                        # actionButton(inputId="applytWkt", label="Select features within this WKT"),
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
                          
                          absolutePanel(id = "logo", class = "card", bottom = 15, right = 150, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))
                          )
                      )
             ),
             tabPanel(
               title = "Plot indicator 2",
               # map_leafletUI("map_global"),
               plotlyOutput("plot2")
             ),
             tabPanel(
               title = "Streamgraph indicator 2",
               streamgraphOutput("plot2_streamgraph")
             ),
             navbarMenu("Browse Data Tables",
                        tabPanel(
                          title = "Browse map data",
                          DT::dataTableOutput("DT_query_data_map")
                        ),
                        tabPanel(
                          title = "Browse time series data",
                          DT::dataTableOutput("DT_data_all_datasets")
                        ),
                        tabPanel(
                          title = "Browse data bar plots",
                          DT::dataTableOutput("DT_data_barplot_all_datasets")
                        )
             ),
             # tabPanel(
             #   title = "Your filters",
             #   textOutput("selected_var")
             # ),
             navbarMenu("Browse underlying filters",
                        tabPanel(
                          title = "The current WKT",
                          textOutput("wkt")
                        ),
                        tabPanel(
                          title = "SQL query: query_metadata",
                          textOutput("query_metadata")
                        ),
                        tabPanel(
                          title = "Lits of areas id",
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
