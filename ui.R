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
                          tags$head(includeCSS("./styles.css")),
                          # shinycssloaders::withSpinner(map_leafletUI("map_global")),
                          map_leafletUI("map_global"),
                          absolutePanel(id = "filters", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 150,  left = "3%", width = "21%", height = "auto",
                                        pickerInput(
                                          inputId = "dataset",
                                          label = "Dataset",
                                          choices = target_dataset,
                                          multiple = TRUE,
                                          selected= default_dataset,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "unit",
                                          label = "Unit",
                                          choices = target_measurement_unit,
                                          multiple = TRUE,
                                          selected= default_unit,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        # selectInput(
                                        pickerInput(
                                          inputId = "gridtype",
                                          label = "Grid size",
                                          choices = target_gridtype,
                                          multiple = TRUE,
                                          selected= default_gridtype,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                        # selectInput(
                                          inputId = "species",
                                          label = "Species",
                                          choices = target_species,
                                          multiple = TRUE,
                                          selected= default_species,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year,
                                          multiple = TRUE,
                                          selected= default_year,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "gear_type",
                                          label = "Gear",
                                          # choices = c("All",target_gear_type),
                                          choices =target_gear_type,
                                          multiple = TRUE,
                                          selected= default_gear_type,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "fishing_fleet",
                                          label = "Fishing fleet",
                                          choices = target_flag,
                                          multiple = TRUE,
                                          selected= default_fishing_fleet,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        map_leafletUI("other"),
                                        textInput("yourWKT","Draw or paste a new WKT",width="98%"),
                                        # textInput("yourWKT","Paste you WKT",value=textOutput("updatedWKT")),
                                        verbatimTextOutput("updatedWKT", placeholder = TRUE),
                                        verbatimTextOutput("verbatimWKT"),
                                        
                                        actionButton(inputId ="resetWkt", label = "Reset WKT (no spatial filter)", icon("map"), 
                                                     style="color: #fff; background-color: #2271b1; border-color: #2e6da4;font-size: xx-large;
                                                                                                                           font-weight: bold;"),
                                        actionButton(inputId = "submit",label = "Apply filters !", icon("paper-plane"), 
                                                     style="color: #fff; background-color: #d63638; border-color: #2e6da4;font-size: xx-large;
                                                     font-weight: bold;"),
                                        # actionButton(inputId="applytWkt", label="Select features within this WKT"),
                                        tags$br()
                          ),
                          absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 150, left = "auto", right="1%", width = "23%", height = "auto",
                                        tags$br(),
                                        actionButton(
                                          inputId = "switched",
                                          label = "Switch unit for pie chart (number or tons)",
                                          # icon("move"), 
                                          style="color: #fff; background-color: #008a20; border-color: #2e6da4; font-size: xx-large;font-weight: bold;"
                                        ),
                                        pieBarChartsUI(id = "pie_bar_charts")
                          ),
                          absolutePanel(id = "plots", class = "panel panel-default", bottom =  "2%", left = "25%", width = "50%", fixed=TRUE, draggable = FALSE, height = "auto",
                                        # timeSeriesGearUI(id = "time_series_gear"),
                                        timeSeriesUI(id= "time_series")
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
             # tabPanel(
             #   title = "Time series per gear type",
             #   timeSeriesGearUI(id= "time_series_gear")
             # ),
             navbarMenu("Browse Data Tables",
                        tabPanel(
                          title = "Browse main dataset",
                          DT::dataTableOutput("DT_main_dataset")
                        )
             ),
             # tabPanel(
             #   title = "Your filters",
             #   textOutput("selected_var")
             # ),
             navbarMenu("Browse underlying filters",
                        tabPanel(
                          title = "The current WKT",
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          textOutput("current_WKT")
                        ),
                        tabPanel(
                          title = "SQL query: query_metadata",
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          textOutput("current_filters")
                        )
             ),
             # aboutUI("about"),
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
