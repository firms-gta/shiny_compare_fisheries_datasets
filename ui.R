ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  shinyjs::useShinyjs(),  # Set up shinyjs
  # titlePanel("Global Tuna Atlas"),
  navbarPage(title="Compare Global Tuna Atlas datasets",
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
                      modalDialog(
                        title = "Information",
                        includeMarkdown("doc/popup.md"),
                        size = "l",
                        easyClose = TRUE,
                        footer=modalButton("OK", icon =icon("check"))
                      ),
                      div(class="outer",
                          # theme = bs_theme(version = 5),
                          tags$head(includeCSS("./styles.css")),
                          # shinycssloaders::withSpinner(map_leafletUI("map_global")),
                          map_leafletUI("map_global"),
                          shiny::tags$a('<a href="https://github.com/you"><img decoding="async" width="149" height="149" src="https://github.blog/wp-content/uploads/2008/12/forkme_right_red_aa0000.png" class="attachment-full size-full" alt="Fork me on GitHub" loading="lazy"></a>'),
                          absolutePanel(id = "filters", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE,top = "12%",  left = "3%", width = "21%", height = "auto",
                                        shinyWidgets::pickerInput(
                                          inputId = "dataset",
                                          label = "Dataset",
                                          choices = list_values_dimensions$dataset,
                                          multiple = TRUE,
                                          selected= list_default_filters$dataset,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          inputId = "unit",
                                          label = "Unit",
                                          choices = list_values_dimensions$measurement_unit,
                                          multiple = TRUE,
                                          selected= list_default_filters$unit,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          inputId = "source_authority",
                                          label = "Source authority",
                                          choices = list_values_dimensions$source_authority,
                                          multiple = TRUE,
                                          selected= list_default_filters$source_authority,
                                          options = list(`actions-box` = TRUE),
                                          # subtext,
                                          width = "98%"
                                        ),
                                        # selectInput(
                                        shinyWidgets::pickerInput(
                                          inputId = "gridtype",
                                          label = "Grid size",
                                          choices = list_values_dimensions$gridtype,
                                          multiple = TRUE,
                                          selected= list_default_filters$gridtype,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          # selectInput(
                                          inputId = "species",
                                          label = "Species",
                                          choices = list_values_dimensions$species,
                                          multiple = TRUE,
                                          selected= list_default_filters$species,
                                          options = list(`actions-box` = TRUE),
                                          # autocomplete=TRUE,
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = list_values_dimensions$year,
                                          multiple = TRUE,
                                          selected= list_default_filters$year,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          inputId = "gear_type",
                                          label = "Gear",
                                          # choices = c("All",list_values_dimensions$gear_type),
                                          choices =list_values_dimensions$gear_type,
                                          multiple = TRUE,
                                          selected= list_default_filters$gear_type,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        shinyWidgets::pickerInput(
                                          inputId = "fishing_fleet",
                                          label = "Fishing fleet",
                                          choices = list_values_dimensions$fishing_fleet,
                                          multiple = TRUE,
                                          selected= list_default_filters$fishing_fleet,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        # textInput("yourWKT","Draw paste a spatial WKT",width="98%"),
                                        # textInput("yourWKT","Paste you WKT",value=textOutput("updatedWKT")),
                                        # verbatimTextOutput("updatedWKT", placeholder = TRUE),
                                        # verbatimTextOutput("verbatimWKT"),
                                        
                                        # actionButton(inputId ="resetWkt", label = "Remove spatial filter", icon("map"), 
                                        #              style="color: #fff; background-color: #2271b1; border-color: #2e6da4;font-size: xx-large;
                                        #                                                                                    font-weight: bold;"),
                                        tags$br(),
                                        actionButton(inputId ="resetWkt", label = "Remove spatial filter", icon("map"), 
                                                     style="color: #fff; background-color: #2271b1; border-color: #2e6da4;font-size: xx-large; font-weight: bold;"),
                                        tags$br(),
                                        tags$br(),
                                        map_leafletUI("other"),
                                        tags$br(),
                                        tags$br(),
                                        
                                        actionButton(inputId ="resetAllFilters", label = "Remove all filters", icon("map"), 
                                                     style="color: #fff; background-color: #91c9b9; border-color: #2e6da4;font-size: xx-large;
                                                                                                                           font-weight: bold;"),
                                        # includeHTML(&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;),
                                        # tags$,
                                        actionButton(inputId = "submit",label = "Apply filters !", icon("paper-plane"), 
                                                     style="color: #fff; background-color: #d63638; border-color: #2e6da4;font-size: xx-large;
                                                     font-weight: bold;"),
                                        # actionButton(inputId="applytWkt", label="Select features within this WKT"),
                                        tags$br()
                          ),
                          absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = "12%", left = "auto", right="1%", width = "23%", height = "auto",
                                        tags$br(),
                                        actionButton(
                                          inputId = "switched",
                                          label = "Switch unit for pie chart (number or tons)",
                                          # icon("move"), 
                                          style="color: #fff; background-color: #008a20; border-color: #2e6da4; font-size: xx-large;font-weight: bold;"
                                        ),
                                        pieBarChartsUI(id = "pie_bar_charts")
                          ),
                          absolutePanel(id = "plots", class = "panel panel-default",  fixed=TRUE, 
                                        draggable = FALSE, bottom =  "2%", left = "25%", width = "50%", height = "auto",
                                        # timeSeriesGearUI(id = "time_series_gear"),
                                        timeSeriesUI(id= "time_series")
                                        # fluidRow(
                                        #   column(10,dygraphOutput("dygraph_all_datasets",height="400")),
                                        #   column(2,textOutput("legendDivID"))
                                        # )
                          ),
                          
                          absolutePanel(id = "logo", class = "logo", bottom = "2%", left = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='logo_IRD.svg',height='5%'))),
                          absolutePanel(id = "logo", class = "logo", top = "1.5%", right = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://blue-cloud.d4science.org/', tags$img(src='logo_blue-cloud_2026.svg',height='5%')))
                      )
             ),
             # tabPanel(
             #   title = "Time series per gear type",
             #   timeSeriesGearUI(id= "time_series_gear")
             # ),
             navbarMenu("Browse Data Tables",
                        tabPanel(
                          title = "Browse main dataset",
                          DT::DTOutput("DT_main_dataset")
                        )
             ),
             # tabPanel(
             #   title = "Your filters",
             #   textOutput("selected_var")
             # ),
             navbarMenu("Browse underlying filters",
                        tabPanel(
                          title = "SQL query: query_metadata",
                          tags$br(),
                          textOutput("current_filters")
                        )
             ),
             navbarMenu("About",
                        tabPanel("Context",
                                 fluidRow(
                                   # includeMarkdown("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/README.md")
                                   column(width =2,
                                          markdown('
                                          [<img src="logo_VLab5.png" height="10%">](https://blue-cloud.d4science.org/group/globalfisheriesatlas)
                                          
                                          <br>
                                          
                                          [<img src="logo_IRD.svg" height="108">](https://www.ird.fr/)   
                                                   ')
                                   ),
                                   column(width =6,
                                          includeMarkdown("doc/about.md"),
                                   ),
                                   column(width =2,
                                          markdown('
                                          [<img src="BET_YFT_SKJ_ALB.svg" width="20%">](https://blue-cloud.d4science.org/group/globalfisheriesatlas)
                                                   ')
                                   )
                                 )
                        )
             )
  )
)
