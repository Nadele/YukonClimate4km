# Yukon Climate Analog Explorer
# designed by Nadele Flynn (nadele@ualberta.ca)
# built by Ashton Drew (ashton.drew@kdv-decisions.com) with assistance form Eliot Dixon (eliot.dixon@kdv-decisions.com)
# Fall 2020



# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        useShinyjs(),
        # style modals
        tags$style(
            #HTML('#test{background-color:orange}'),
            HTML('#run{background-color:cyan}'),
            HTML('#reset{background-color:orange}'),
            HTML('#export{background-color:cyan}'),
            HTML(
                ".error {
                    background-color: red;
                    color: white;
                    }
                    .success {
                    background-color: green;
                    color: white;
                    }"
            )),
        #shinyEventLogger::log_init(),
        
        # Title----     
        fluidRow(
            column(12,
                   h1("Yukon Climate Analog Explorer"),
            )
        ),
        
        # Contents----
        fluidRow(
            # Tab Panels
            tabsetPanel(
                
                # Viewer ----
                tabPanel("Viewer",
                         column(4,
                                h3("Build an analog model"),
                                # LOCATION ----
                                div(id = "allInputs",
                                    h4("1. Choose the reference location"),
                                    radioButtons("source", "How do you wish to identify a location of interest?", choiceNames=c("Click on map", "Enter coordinates"), choiceValues = c("map", "text"), selected="map", inline=TRUE),
                                    conditionalPanel(condition="input.source=='text'",
                                                     wellPanel(
                                                         textInput("latitude", "Enter latitude ", value = "", placeholder = "e.g. 60.1234"),
                                                         textInput("longitude", "Enter longitude ", value = "", placeholder = "e.g. -135.1234"),
                                                         actionButton("coordinates", "Submit Coordinates")
                                                     )
                                    ),
                                    helpText(textOutput("chosenXY")),
                                    hr(),
                                    
                                    # TIMEFRAME ----
                                    h4("2. Choose the analog timeframes"),
                                    fluidRow(
                                        column(6,
                                               radioButtons("ref", "Select a reference period:", choiceValues=analogLookup$ID, choiceNames=analogLookup$LABEL)),
                                        column(6,
                                               radioButtons("comp", "Select a comparison climate:", choiceValues=analogLookup$ID, choiceNames=analogLookup$LABEL))
                                    ),
                                    helpText(textOutput("chosenTime")),
                                    hr(),
                                    
                                    # VARIABLES ----
                                    h4("3. Choose TWO or more climate variables"),
                                    DT::dataTableOutput('checkbox_table',width="50pt"),
                                    helpText("The available variables are seasonal minimum temperature (Tmin), maximum temperature (Tmax), and precipitation (PPT)."),
                                    textOutput("chosenVars"),
                                    hr()
                                ), # close controlPanel div
                                
                                # RUN MODEL BUTTON ----
                                h4("4. Run or reset the model"),
                                actionButton("run", label="Run Model"),
                                actionButton("reset", label="Reset")
                         ),
                         column(8,
                                
                                # OUTPUTS ----
                                
                                div(id = "allOutputs",
                                    h3("Instructions"),
                                    tags$div(
                                    HTML("Adjust the settings at left to set up the model you wish to run.  When you have made your selections, hit the <strong>Run Model</strong> button and wait a few moments for the model results to appear on the map.  If you wish to run a new model, you can reset all the selections by hitting the <strong>Reset</strong> button.")
                                    ),
                                    hr(),
                                    withSpinner(leafletOutput("locationMap", height = 800)),
                                    textOutput("mapLegend"),
                                    uiOutput("ui_exportButton")
                                ) # end div modelOutputs
                         )
                ),
                
                # About ----
                tabPanel("About",
                         column(12,
                                h2("Project Background"),
                                "The climate analog code visualized with this application is a product of Nadele Flynn's dissertation research.  She adapted code, originally developed by Dr. Colin Mahoney at University of British Columbia, for application within Yukon ecosystems.",
                                h2("Technical Details of the Analog Calculations"),
                                "The calculations that calculate and map climatic novelty use climatic distance (Dmin) based on the Mahalanobis distance between the comparison climate of a location of interest and its closest analog among the observed end-of-20th-century (1971-2000) climates of an analog pool between the comparison climate and its closest reference climate analog. This Mahalanobis distance is scaled to the historical (1951-1991) interannual variability of the climate variables for the location of interest (based on the 4 closest CRU TS3.23 weather station using IDW average, as described in Mahony et al 2017.")
                ),
                
                # Help ----
                tabPanel("Help",
                         column(12,
                                h3("Helpful tips.")
                         )
                ),
                
                # Climate Analogs ----
                tabPanel("Climate Analogs",
                         column(12,
                                h3("What is a climate analog?")
                         )
                ),
                
                # Contact ----
                tabPanel("Contact",
                         column(12,
                                h3("For more information, contact Nadele Flynn.")
                         )
                )
                
            )# end of tabsetPanel
        ) #end of content fluidRow
    )) # end of fluidPage and ui functions
