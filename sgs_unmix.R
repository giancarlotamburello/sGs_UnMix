# Load necessary libraries

library(fields)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(DT)
library(mixtools)
library(gstat)
library(sp)
library(raster)
library(splancs)
library(viridis)

source("coord_trans.R")
source("unmix_function.R")


# Define UI for the application
ui <- 
  navbarPage("sGs UnMix",id = "nav",
             tags$head(
               tags$style(
                 HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
                 )
               )
             ),
             
             
             # Tabset layout
             tabsetPanel(id = "maintabpanel",
                         #########
                         # 1 PANEL 
                         tabPanel("Load Data", fluid = TRUE, icon = icon("upload"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      fluidRow(column(3,
                                                      actionButton("help.load",icon = icon("paperclip"), "Help", align = "center"
                                                                   , style = "margin-bottom: -20px;"
                                                                   , style = "margin-top: 20px;"))),
                                      br(), br(),
                                      
                                      
                                     
                                      
                                      ########
                                      # Row with separator selector and file input button
                                      
                                      fluidRow(
                                        column(2,radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ",")),
                                        column(7,
                                               fileInput("upload", 
                                                         label = "Upload a CSV file", 
                                                         accept = c(".csv", ".tsv"))
                                        )),
                                      
                                      ########
                                      # Row with column selector and EPSG input and output
                                      
                                      fluidRow(
                                        column(3, offset = 1, 
                                               selectInput("columns",
                                                           label = "Select columns",
                                                           choices = NULL, multiple = TRUE)
                                        ),
                                        column(3, offset = 1, 
                                               numericInput("epsg.in",
                                                            label = "Input EPSG",
                                                            value=NA)
                                        ),
                                        column(3, offset = 1, 
                                               numericInput("epsg.out",
                                                            label = "Output EPSG",
                                                            value=NA)
                                        )),
                                      
                                      ########
                                      # Row with polygon control buttons
                                      
                                      fluidRow(hr(), 
                                               column(4,actionButton("remove.polygon", "Remove polygon", align = "center"
                                                                     , style = "margin-bottom: -20px;"
                                                                     , style = "margin-top: 20px;")),
                                               column(4,downloadButton("downloadPoly", "Save polygon as CSV", align = "center"
                                                                       , style = "margin-bottom: -20px;"
                                                                       , style = "margin-top: 20px;")),
                                               column(4,fileInput("upload.polygon", label="",
                                                                  buttonLabel = "Upload polygon", 
                                                                  accept = c(".csv")) ))
                                      
                                    ),
                                    
                                    ########
                                    # PANEL with leaflet map
                                    
                                    mainPanel(
                                      leafletOutput("mymap",height = 800),
                                      absolutePanel(
                                        top = 250, right = 30, draggable = T, 
                                        style = "z-index: 500; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 5px; width: 100px;",
                                        selectInput(
                                          inputId = "palette0",
                                          label = "Color scale",
                                          choices = c(
                                            "Viridis" = "viridis",
                                            "Rainbow" = "rainbow",
                                            "Magma" = "magma",
                                            "Plasma" = "plasma",
                                            "Cividis" = "cividis"
                                          ),
                                          selected = "viridis"
                                        ),
                                        radioButtons("show.log", "",choices = c("log10","raw"),selected = "log10"))
                                    )
                                  )
                         ),
                         
                         #########
                         # 2 PANEL Data table 
                         
                         tabPanel("Data", fluid = TRUE, icon = icon("table"),
                                  fluidRow(
                                    # Left column with dynamic width
                                    column(
                                      width = 8, # Half the page width
                                      div(
                                        style = "width: 100%; float: left; overflow-x: auto;", # Use full available space
                                        h4("Full Data Table"),
                                        DTOutput("table")
                                      )
                                    ),
                                    
                                    # Right column with dynamic width
                                    column(
                                      width = 4, # Half the page width
                                      div(
                                        style = "width: 100%; float: right; overflow-x: auto;", # Use full available space
                                        h4("Variable Summary and Visualization"),
                                        selectInput(
                                          "variable", 
                                          "Select a Variable:",
                                          choices = NULL
                                        ),
                                        tabsetPanel(
                                          tabPanel(
                                            "Summary",
                                            DTOutput("summary_table") # Summary table
                                          ),
                                          tabPanel(
                                            "Visualization",
                                            plotOutput("variable_plot") # Visualization
                                          )
                                        )
                                      )
                                    )
                                  )
                         ),
                         
                         #########
                         # 3 PANEL with the mix model tools
                         
                         tabPanel("Mix model", fluid = TRUE, icon = icon("chart-area"), 
                                  sidebarPanel(
                                    
                                    fluidRow(column(3,
                                                    actionButton("help.mix",icon = icon("paperclip"), "Help", align = "center"
                                                                 , style = "margin-bottom: -20px;"
                                                                 , style = "margin-top: 20px;"))),
                                    br(), br(),
                                    
                                    
                                    ########
                                    # Row with mix fit options and log-likelihood
                                    fluidRow(
                                      column(3,
                                             radioButtons("do.log", 
                                                          label = h4("Data"), 
                                                          choices = c("Log-normal", "Normal"), selected = "Log-normal")
                                      ))
                                    ,
                                    fluidRow(
                                      column(3,
                                             radioButtons("fit0", 
                                                          label = h4("Fit"), 
                                                          choices = c("Automatic", "Initial Guess","Manual"), selected = "Automatic")
                                      ),
                                      column(7, offset = 2,
                                             h4("log-likelihood"),
                                             textOutput("loglik"),
                                             helpText("* The higher the value,
                                                        the better the mixture fit")
                                      )),
                                    
                                    ########
                                    # Number of populations in mix fit
                                    
                                    fluidRow(
                                      column(4,
                                             numericInput("npop",
                                                          label= "Number of populations", 
                                                          value = 2, step = 1,min = 2,max = 10)
                                      ),
                                      column(3,
                                             textInput("mean", 
                                                       label = "Mean", 
                                                       value = NULL)
                                      ),
                                      column(3,
                                             textInput("std", 
                                                       label = "St. dev.", 
                                                       value = NULL)
                                      ),
                                      
                                      br()
                                    ),
                                    
                                    ########
                                    # Calculated mean and sd of populations
                                    
                                    
                                    fluidRow(
                                      column(4,actionButton("calc.populations", "Calculate populations", align = "center"
                                                            , style = "margin-bottom: -20px;"
                                                            , style = "margin-top: 20px;")),
                                      column(4,tableOutput("mixtable"))
                                    )
                                    
                                  ),
                                  mainPanel(
                                    plotOutput("mixPlot",height = 500,width = "100%"),
                                    br(), br(), 
                                    downloadButton("download_mixPlot", "Save Plot as PDF")
                                  )
                         ),
                         
                         #########
                         # 4 PANEL Variogram
                         
                         tabPanel("Variogram", fluid = TRUE,  icon =  icon("chart-line"), # icon("bar-chart-o", lib = "glyphicon")
                                  tabsetPanel(id = "vardirtab",
                                              tabPanel("Omnidirectional",
                                                       # Sidebar layout with an input and output definitions
                                                       sidebarLayout(position = "left",
                                                                     sidebarPanel(
                                                                       
                                                                       fluidRow(column(3,
                                                                                       actionButton("help.omni",icon = icon("paperclip"), "Help", align = "center"
                                                                                                    , style = "margin-bottom: -20px;"
                                                                                                    , style = "margin-top: 20px;"))),
                                                                       br(), br(),
                                                                       
                                                                       ########
                                                                       # Selection of variogram fit mode and SSE calculation
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                radioButtons("fit", 
                                                                                             label = h4("Fit"), 
                                                                                             choices = c("Automatic", "Manual"), selected = "Automatic")
                                                                         ),
                                                                         column(7, offset = 2,
                                                                                h4("SSE"),
                                                                                textOutput("ssr"),
                                                                                helpText("* The lower the Sum of Squared Errors (SSE),
                                           the better the variogram fit")
                                                                         )
                                                                       ),
                                                                       
                                                                       ########
                                                                       # Selection of single or nested variogram
                                                                       
                                                                       fluidRow( # <---- REVIEW
                                                                         column(3,
                                                                                radioButtons("nes", 
                                                                                             label = h4("Type"), 
                                                                                             choices = c("Single", "Nested"), selected = "Single")
                                                                         )
                                                                       ), # <---- REVIEW
                                                                       
                                                                       
                                                                       ########
                                                                       # Selection of variogram fit type, lag distance, calculated auto lag distance and robust variogram flag
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                selectInput("varType",
                                                                                            label= "Variogram model",
                                                                                            choices = c("Spherical", "Exponential", "Pentaspherical", "Gaussian", 
                                                                                                        "Circular","Linear",
                                                                                                        "Bessel"), selected = "Spherical")
                                                                         ),
                                                                         column(2,
                                                                                numericInput("lag_dist", label = "Lag distance", value = NA, step = 1)),
                                                                         column(3,
                                                                                h6("Auto Lag distance"),textOutput("calc.dist")),
                                                                         column(2,
                                                                                checkboxInput(inputId = "robust", 
                                                                                              label = "Robust",
                                                                                              value = F)
                                                                         )),
                                                                       
                                                                       ########
                                                                       # Selection of nugget, partial sill and range only for manual mode
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                numericInput("nugget", 
                                                                                             label = "Nugget", 
                                                                                             value = NA, step = 0.01)
                                                                         ),
                                                                         column(3,
                                                                                numericInput("psill", 
                                                                                             label = "Partial sill", 
                                                                                             value = NA, step = 0.01)
                                                                         ),
                                                                         column(3,
                                                                                numericInput("range", 
                                                                                             label = "Range", 
                                                                                             value = NA)
                                                                         )),
                                                                       
                                                                       
                                                                       ########
                                                                       # Selection of partial sill and range for nested variogram -- only for manual mode
                                                                       
                                                                       conditionalPanel( # <---- REVIEW
                                                                         condition = "input.nes == 'Nested'",
                                                                         fluidRow(
                                                                           column(3,
                                                                                  selectInput("varType2",
                                                                                              label= "2 Variogram model", 
                                                                                              choices = c("Spherical", "Exponential", "Pentaspherical", "Gaussian", 
                                                                                                          "Circular","Linear",
                                                                                                          "Bessel"), selected = "Spherical")
                                                                           ),
                                                                           column(3,
                                                                                  numericInput("psill2", 
                                                                                               label = "2 Partial sill", 
                                                                                               value = NA, step = 0.01)
                                                                           ),
                                                                           column(3,
                                                                                  numericInput("range2", 
                                                                                               label = "2 Range", 
                                                                                               value = NA)
                                                                           )
                                                                           
                                                                         )
                                                                       ) # <---- REVIEW
                                                                     ),
                                                                     
                                                                     
                                                                     mainPanel(
                                                                       tabPanel("Plot",
                                                                                column(
                                                                                  width=8,
                                                                                  fluidRow(
                                                                                    column(width = 10, plotOutput("varPlot", height = 450))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 10, plotOutput("varCloudPlot", height = 450))
                                                                                  )
                                                                                ),
                                                                                
                                                                                column(
                                                                                  width = 4,
                                                                                  downloadButton("download_table", "Save Table as CSV"),
                                                                                  br(), br(), 
                                                                                  downloadButton("download_plot", "Save Plot as PDF"),
                                                                                  br(),br(),
                                                                                  tableOutput("varTable")
                                                                                  
                                                                                )
                                                                       )
                                                                       
                                                                     )
                                                       )
                                              ),
                                              tabPanel("Anisotropic",
                                                       # Sidebar layout with an input and output definitions
                                                       sidebarLayout(position = "left",
                                                                     sidebarPanel(
                                                                       
                                                                       fluidRow(column(3,
                                                                                       actionButton("help.anis",icon = icon("paperclip"), "Help", align = "center"
                                                                                                    , style = "margin-bottom: -20px;"
                                                                                                    , style = "margin-top: 20px;"))),
                                                                       br(), br(),
                                                                       
                                                                       ########
                                                                       # Selection of variogram fit mode and SSE calculation
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                textInput("an.dir", 
                                                                                          label = "Directions", 
                                                                                          value = "0,45,90,135")
                                                                         ),
                                                                         column(7, offset = 2,
                                                                                h4("SSE"),
                                                                                textOutput("an.ssr"),
                                                                                helpText("* The lower the Sum of Squared Errors (SSE),
                                                                      the better the variogram fit")
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(2, 
                                                                                numericInput("an.maindir", 
                                                                                             label = "Main dir", 
                                                                                             value = "45")
                                                                                
                                                                         ),
                                                                         column(2,
                                                                                numericInput("an.ratio", 
                                                                                             label = "Ratio", 
                                                                                             value = "0.4")
                                                                                
                                                                                
                                                                         )
                                                                       ),
                                                                       
                                                                       
                                                                       ########
                                                                       # Selection of variogram fit type, lag distance, calculated auto lag distance and robust variogram flag
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                selectInput("an.varType",
                                                                                            label= "Variogram model", 
                                                                                            choices = c("Spherical" = "Sph", "Exponential" = "Exp", "Pentaspherical" = "Pen", "Gaussian" = "Gau", 
                                                                                                        "Circular" = "Cir","Linear" = "Lin","Bessel" = "Bes"), selected = "Spherical")
                                                                         ),
                                                                         column(2,
                                                                                numericInput("an.lag_dist", label = "Lag distance", value = NA, step = 1)),
                                                                         column(3,
                                                                                h6("Auto Lag distance"),textOutput("an.calc.dist")),
                                                                       ),
                                                                       
                                                                       ########
                                                                       # Selection of nugget, partial sill and range only for manual mode
                                                                       
                                                                       fluidRow(
                                                                         column(3,
                                                                                numericInput("an.nugget", 
                                                                                             label = "Nugget", 
                                                                                             value = NA, step = 0.01)
                                                                         ),
                                                                         column(3,
                                                                                numericInput("an.psill", 
                                                                                             label = "Partial sill", 
                                                                                             value = NA, step = 0.01)
                                                                         ),
                                                                         column(3,
                                                                                numericInput("an.range", 
                                                                                             label = "Range", 
                                                                                             value = NA)
                                                                         )),
                                                                       
                                                                       
                                                                     ),
                                                                     
                                                                     
                                                                     mainPanel(
                                                                       tabPanel("Plot",
                                                                                column(
                                                                                  width=8,
                                                                                  fluidRow(
                                                                                    column(width = 10, plotOutput("varMapPlot", height = 450))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 10, plotOutput("varDirPlot", height = 450))
                                                                                  )
                                                                                ),
                                                                                
                                                                                column(
                                                                                  width = 4,
                                                                                  downloadButton("an.download_table", "Save Table as CSV"),
                                                                                  br(), br(), 
                                                                                  downloadButton("an.download_plot", "Save Plot as PDF"),
                                                                                  br(),br(),
                                                                                  tableOutput("an.varTable")
                                                                                )
                                                                       )
                                                                       
                                                                     )
                                                       )
                                                       
                                                       
                                              ))),
                         
                         
                         #########
                         # 5 PANEL sequential Gasussian simulations 
                         
                         tabPanel("sGs", fluid = TRUE, icon = icon("globe-americas"),
                                  sidebarLayout(position = "left",
                                                sidebarPanel(
                                                  fluidRow(column(3,
                                                                  actionButton("help.sgs",icon = icon("paperclip"), "Help", align = "center"
                                                                               , style = "margin-bottom: -20px;"
                                                                               , style = "margin-top: 20px;"))),
                                                  br(), br(),
                                                  fluidRow(
                                                    column(3, offset = 1, 
                                                           numericInput("xmin",
                                                                        label = "X min:",
                                                                        value=NA)
                                                    ),
                                                    column(3, offset = 1, 
                                                           numericInput("xmax",
                                                                        label = "X max:",
                                                                        value=NA)
                                                    ),
                                                    column(3, offset = 1, 
                                                           numericInput("dx",
                                                                        label = "Delta X:",
                                                                        value=10,min = 1)
                                                    )),
                                                  fluidRow(
                                                    column(3, offset = 1, 
                                                           numericInput("ymin",
                                                                        label = "Y min:",
                                                                        value=NA)
                                                    ),
                                                    column(3, offset = 1, 
                                                           numericInput("ymax",
                                                                        label = "Y max:",
                                                                        value=NA)
                                                    ),
                                                    column(3, offset = 1, 
                                                           numericInput("dy",
                                                                        label = "Delta Y:",
                                                                        value=10,min = 1)
                                                    )),
                                                  fluidRow(
                                                    column(3, offset = 1, 
                                                           numericInput("nsims",
                                                                        label = "Number simulations:",
                                                                        value=100,max = 1000,min = 10)
                                                    ),
                                                    column(3, offset = 1, 
                                                           numericInput("prob.thres",
                                                                        label = "Probability treshold:",
                                                                        value=NA)
                                                    ),
                                                    column(3, offset = 1, 
                                                           actionButton("runsgs", "Run sGs", align = "center"
                                                                        , style = "margin-bottom: -24px;"
                                                                        , style = "margin-top: 24px;")
                                                    )
                                                  ), 
                                                  fluidRow(
                                                    h5("Mean Map")),
                                                  fluidRow(style = "margin-top: -3em;",
                                                           hr(),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmin",
                                                                               label = "Z min:",
                                                                               value=NA)
                                                           ),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmax",
                                                                               label = "Z max:",
                                                                               value=NA)
                                                           ),
                                                           column(numericInput("raster.opacity", "Opacity",
                                                                               min = 0, max = 1,step = 0.1,
                                                                               value = 1),width = 2),
                                                           column(actionButton("refresh","", icon = icon("eye"), align = "center"
                                                                               , style = "margin-bottom: -24px;"
                                                                               , style = "margin-top: 24px;"),width=2)
                                                  ),
                                                  fluidRow(
                                                    h5("Error Map")),
                                                  fluidRow(style = "margin-top: -3em;",
                                                           hr(),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmin2",
                                                                               label = "Z min:",
                                                                               value=NA)
                                                           ),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmax2",
                                                                               label = "Z max:",
                                                                               value=NA)
                                                           ),
                                                           column(numericInput("raster.opacity2", "Opacity",
                                                                               min = 0, max = 1,step = 0.1,
                                                                               value = 1),width = 2),
                                                           column(actionButton("refresh2","", icon = icon("eye"), align = "center"
                                                                               , style = "margin-bottom: -24px;"
                                                                               , style = "margin-top: 24px;"),width=2)
                                                  ),
                                                  fluidRow(
                                                    h5("Probability Map")),
                                                  fluidRow(style = "margin-top: -3em;",
                                                           hr(),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmin1",
                                                                               label = "Z min:",
                                                                               min = 0, max = 1,step = 0.1,
                                                                               value=0)
                                                           ),
                                                           column(3, offset = 1, 
                                                                  numericInput("zmax1",
                                                                               label = "Z max:",
                                                                               min = 0, max = 1,step = 0.1,
                                                                               value=1)
                                                           ),
                                                           column(numericInput("raster.opacity1", "Opacity",
                                                                               min = 0, max = 1,step = 0.1,
                                                                               value = 1),width = 2),
                                                           column(actionButton("refresh1","", icon = icon("eye"), align = "center"
                                                                               , style = "margin-bottom: -24px;"
                                                                               , style = "margin-top: 24px;"),width=2)
                                                  ),
                                                  
                                                  # fluidRow(
                                                  #   column(4, 
                                                  #          downloadButton("downloadRaster", "Save Mean Map as raster",
                                                  #                         , style = "margin-bottom: -20px;"
                                                  #                         , style = "margin-top: 20px;"))
                                                  #   
                                                  # ),
                                                  br(),
                                                  fluidRow(
                                                    column(4, 
                                                           actionButton("calcIntegral", "Calculate 2D integral ",
                                                                        , style = "margin-bottom: -20px;"
                                                                        , style = "margin-top: 20px;")),
                                                    column(8,tableOutput("outputtable"))
                                                    
                                                  )
                                                  
                                                ),
                                                mainPanel(
                                                  leafletOutput("mymap2",height = 800),
                                                  absolutePanel(
                                                    top = 250, right = 30, draggable = T, 
                                                    style = "z-index: 500; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 5px; width: 100px;",
                                                    selectInput(
                                                      inputId = "palette1",
                                                      label = "Color scale",
                                                      choices = c(
                                                        "Viridis" = "viridis",
                                                        "Rainbow" = "rainbow",
                                                        "Magma" = "magma",
                                                        "Plasma" = "plasma",
                                                        "Cividis" = "cividis"
                                                      ),
                                                      selected = "viridis"
                                                    )),
                                                  absolutePanel(
                                                    top = 470, right = 30, draggable = F, 
                                                    style = "z-index: 500",
                                                    downloadButton("downloadRaster", "Save Mean raster",
                                                                   style = "margin-bottom: -20px; margin-top: 20px;")
                                                  ),
                                                  absolutePanel(
                                                    top = 510, right = 30, draggable = F, 
                                                    style = "z-index: 500",
                                                    downloadButton("downloadRaster1", "Save Prob. raster",
                                                                   style = "margin-bottom: -20px; margin-top: 20px;")
                                                  ),
                                                  absolutePanel(
                                                    top = 550, right = 30, draggable = F, 
                                                    style = "z-index: 500",
                                                    downloadButton("downloadRaster2", "Save Error raster",
                                                                   style = "margin-bottom: -20px; margin-top: 20px;")
                                                  )
                                                )
                                  )
                         )
                         
             )
  )

##########################################
##########################################
# SERVER
##########################################
##########################################

server <- function(input, output, session) {
  # Reactive value to store the data
  values <- reactiveValues(df.poly=NULL,
                           loaded.poly=NULL,
                           df=NULL,
                           unmix.data=NULL,
                           selected.tab=NULL,
                           raster=NULL,
                           raster.p=NULL,
                           raster.sd=NULL,
                           utm.coord=NULL,
                           data.var.an=NULL,
                           data.vm.an=NULL,
                           an.sse=NULL)
  
  #############
  # 1 PANEL Map
  #############
  
  ########
  # Leaflet map output
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
      addProviderTiles('Esri.WorldImagery',group = "Esri World Imagery") %>%  
      setView(lng = 0, lat = 20, zoom = 2) %>% # Set initial view of the map
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addLayersControl(position ="topleft",
                       baseGroups = c("Esri World Imagery", "OpenStreetMap"),
                       options = layersControlOptions(collapsed = T)) %>%
      addDrawToolbar(targetLayerId = NULL, targetGroup = "draw",
                     position = c("topleft", "topright", "bottomleft", "bottomright"),
                     polylineOptions = F,
                     polygonOptions = drawPolygonOptions(repeatMode=T),
                     circleOptions = F,
                     rectangleOptions = drawRectangleOptions(repeatMode=T),
                     markerOptions =F,
                     circleMarkerOptions = F, editOptions = F,
                     singleFeature = T)
  })
  
  
  ########
  # Load the CSV file and update the column selector
  
  observeEvent(input$upload,{
    req(input$upload)
    uploaded_data <- read.csv(input$upload$datapath,
                              header = T,
                              sep = input$sep, na.strings = c("NaN","NAN","NA","Na"))
    
    if (ncol(uploaded_data) > 2) {
      values$df <- uploaded_data
      updateSelectInput(session, "columns", choices = names(values$df ), selected = NULL)
      updateSelectInput(session, "variable", choices = names(values$df), selected = NULL)
      
    } else {
      showNotification("The CSV must contain at least three columns.", type = "error")
    }
    
  })
  
  
  output$downloadPoly <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(format(Sys.time(),"%Y%m%d%H%M%S"), "_polygon_sgs.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(values$loaded.poly, file,row.names = F)
    }
  )
  
  ########
  # Draw polygon on map
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    df.poly <-  as.data.frame(feature$geometry$coordinates)
    df.poly <-  data.frame(X=as.numeric(df.poly[,seq(1,ncol(df.poly),2)]),
                           Y=as.numeric(df.poly[,seq(2,ncol(df.poly),2)]))
    row.names(df.poly) <- 1:nrow(df.poly)
    
    lonlat.poly <- df.poly[,c("X","Y")]
    
    
    if (is.na(input$epsg.in)) {
      if(class(try(crs(paste0("epsg:",input$epsg.out)),silent = T)) == "try-error" ){
        return(NULL)
      }
      
      df.poly[,c("X","Y")] <- coord.trans(df.poly[,c("X","Y")], epsg.in = 4326, epsg.out =input$epsg.out)
    }else{
      if(class(try(crs(paste0("epsg:",input$epsg.in)),silent = T)) == "try-error" ){
        return(NULL)
      }
      
      df.poly[,c("X","Y")] <- coord.trans(df.poly[,c("X","Y")], epsg.in = 4326, epsg.out =input$epsg.in)
    }
    
    values$df.poly <- df.poly
    
    values$loaded.poly <- cbind(df.poly,lonlat.poly)
    
    updateNumericInput(session,inputId ="xmin",value=round(min(df.poly$X), 1))
    updateNumericInput(session,inputId ="xmax",value=round(max(df.poly$X), 1))
    updateNumericInput(session,inputId ="ymin",value=round(min(df.poly$Y), 1))
    updateNumericInput(session,inputId ="ymax",value=round(max(df.poly$Y), 1))
    
  })
  
  observeEvent(input$upload.polygon,{
    req(input$upload.polygon)
    values$loaded.poly <- read.csv(input$upload.polygon$datapath,
                                   header = T)
    values$df.poly=values$loaded.poly[,1:2]
    
    leafletProxy("mymap")  %>%    addPolygons(lng = values$loaded.poly[,3],lat = values$loaded.poly[,4])
    
    
    updateNumericInput(session,inputId ="xmin",value=min(values$df.poly$X))
    updateNumericInput(session,inputId ="xmax",value=max(values$df.poly$X))
    updateNumericInput(session,inputId ="ymin",value=min(values$df.poly$Y))
    updateNumericInput(session,inputId ="ymax",value=max(values$df.poly$Y))
    
  })
  
  
  
  ########
  # Remove polygon from map
  
  observeEvent(input$remove.polygon,{
    values$loaded.poly=NULL
    values$df.poly=NULL
    leafletProxy("mymap")  %>%   clearShapes()
  })
  
  ########
  # Draw points on map when enter a EPSG
  
  observeEvent(input$epsg.in 
               | input$epsg.out 
               | as.numeric(factor(input$palette0)
                            | as.numeric(factor(input$show.log))),{
                              req(values$df)
                              
                              
                              data <- values$df[,input$columns]
                              data <- na.omit(data)
                              names(data) <- c("X", "Y", "Z")
                              
                              if (!is.na(input$epsg.in)) {
                                if(class(try(crs(paste0("epsg:",input$epsg.in)),silent = T)) == "try-error" ){
                                  return(NULL)
                                }
                                values$utm.coord <- data.frame(x= data$X,y=data$Y) 
                                data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = input$epsg.in, epsg.out =4326)
                              }else{
                                if(class(try(crs(paste0("epsg:",input$epsg.out)),silent = T)) == "try-error" ){
                                  return(NULL)
                                }
                                values$utm.coord <- coord.trans(data[,c("X","Y")], epsg.in = 4326, epsg.out = input$epsg.out)
                              }
                              
                              
                              updateNumericInput(session,inputId ="xmin",value=min(values$utm.coord$x))
                              updateNumericInput(session,inputId ="xmax",value=max(values$utm.coord$x))
                              updateNumericInput(session,inputId ="ymin",value=min(values$utm.coord$y))
                              updateNumericInput(session,inputId ="ymax",value=max(values$utm.coord$y))
                              
                              # Color palette setting
                              
                              selected_palette <- switch(input$palette0,
                                                         "viridis" = viridis::viridis(10),
                                                         "rainbow" = fields::tim.colors(10),
                                                         "magma" = viridis::magma(10),
                                                         "plasma" = viridis::plasma(10),
                                                         "cividis" = viridis::cividis(10)
                              )
                              
                              if (!all(data$Z > 0)) {
                                updateRadioButtons(session,inputId = "show.log",selected ="raw")
                              }
                              
                              if (input$show.log == "log10") {
                                ZZ <- log10(data$Z)
                                color_title <- paste0("log10(", names(values$df[, input$columns])[3], ")")
                              }else{
                                ZZ <- data$Z
                                color_title <- names(values$df[, input$columns])[3]
                              }
                              
                              
                              pal <- colorNumeric(palette = selected_palette, domain = ZZ, na.color = "transparent")
                              
                              col.pal <- pal(ZZ)
                              
                              leafletProxy("mymap") %>% setView(lng = mean(data$X,na.rm=T), lat = mean(data$Y,na.rm=T), zoom = 14)  %>%
                                clearMarkers() %>%
                                addCircleMarkers( lng=data$X,
                                                  lat=data$Y,label = data$Z,
                                                  # color=color.scale2(ZZ,col = selected_palette),opacity = 1,
                                                  color=col.pal,opacity = 1,
                                                  radius=2,group = "circles") %>%
                                clearControls() %>% 
                                leaflet::addLegend(
                                  pal = pal,
                                  values = ZZ,
                                  title = color_title,
                                  opacity = 0.8
                                  
                                )
                              
                              data <- values$df[input$columns]
                              data <- na.omit(data)
                              names(data) <- c("X", "Y", "Z")
                              
                            })
  
  ########
  # Tab switch trigger
  
  observeEvent(input$maintabpanel, {
    if (input$maintabpanel == "Mix model") {
      values$selected.tab <- T
    }else if (input$maintabpanel == "sGs") {
      values$selected.tab <- T
    }
  })
  
  
  
  
  ###############
  # 2 PANEL Data
  ###############
  
  ########
  # Show the data as table
  
  output$table <- renderDT({
    req(values$df)
    datatable(values$df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  output$summary_table <- renderDT({
    summary_data <- summary(values$df[, input$variable], digits=5)
    summary_df <- data.frame(Statistic = names(summary_data), Value = as.character(summary_data))
    datatable(summary_df, options = list(scrollX = TRUE, pageLength = 6, searching = FALSE, paging = FALSE))
  })
  
  # Render the visualization
  output$variable_plot <- renderPlot({
    var_data <- values$df[, input$variable]
    
    if (is.numeric(var_data)) {
      hist(var_data, 
           main = paste("Histogram of", input$variable), 
           xlab = input$variable, 
           col = "#21908CFF", 
           border = "white")
      rug(var_data)
    } else {
      barplot(table(var_data), 
              main = paste("Bar Plot of", input$variable), 
              xlab = input$variable, 
              col = "#21908CFF", 
              border = "white")
    }
  })
  
  
  
  
  ###################
  # 3 PANEL Mix Model
  ###################
  
  ########
  # Mix calculation
  
  observeEvent(input$calc.populations | values$selected.tab,{
    
    # Save mix parameters
    
    values$selected.tab
    
    npop <- input$npop
    
    if (input$npop > 10) {
      updateNumericInput(session,"npop",value = 10)
      npop <- 10
    }
    
    
    fit0 <- input$fit0
    mix.mean <- input$mean
    mix.sd <- input$std
    mean.start <- as.numeric(unlist(strsplit(mix.mean,",")))
    sd.start <- as.numeric(unlist(strsplit(mix.sd,",")))
    
    output$mixPlot <- renderPlot({
      
      req(values$df)
      
      data <- values$df[,input$columns]
      data <- na.omit(data)
      names(data) <- c("X", "Y", "Z")
      
      lab <- input$columns[3]
      
      if(input$do.log == "Log-normal"){
        data$Z = log10(data$Z)
        lab <- paste0("log10(",lab,")")
      }
      
      # Convert coordinates to UTM and crop data within the polygon
      
      if (!is.na(input$epsg.out)) {
        data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = 4326, epsg.out = input$epsg.out)
      }
      
      if(!is.null(values$df.poly)){
        data <- data[inout(data[, 1:2], values$df.poly),] 
      }
      
      # Format graphs for mix plot
      
      par(mfrow=c(1,2), cex.axis=1.4, cex.lab=1.6, cex.main=1.4, family="Arial")
      
      par(mar = c(5.1, 5.1, 4.1, 2.1))
      
      if (fit0 == "Automatic"){
        
        unmix.data <- unmix(data$Z, n.pop=npop, breaks="fd", adj.density = 1, theor.density=FALSE, is.log10=TRUE,lab=lab)
        
      }else if(fit0 == "Initial Guess" & length(mean.start) == npop & length(sd.start) == npop ){
        
        unmix.data <- unmix(data$Z, n.pop=npop, breaks="fd", adj.density = 1, theor.density=FALSE, is.log10=TRUE,lab=lab,
                            mu=mean.start, sigma=sd.start)
        
      }else if(fit0 == "Manual" & length(mean.start) == npop & length(sd.start) == npop ){
        
        unmix.data <- unmix(data$Z, n.pop=npop, breaks="fd", adj.density = 1, theor.density=FALSE, is.log10=TRUE,lab=lab,
                            mean.constr=mean.start, sd.constr=sd.start)
      }else{
        showNotification("Number of input mean and st. dev. differ from the number of populations", type = "error",duration = NULL)
        return(NULL)
      }
      
      
      updateTextInput(session, "mean", value = as.character(round(unmix.data$data.mix$mu, 2)))
      updateTextInput(session, "std", value = as.character(round(unmix.data$data.mix$sigma, 2)))
      
      values$unmix.data <- unmix.data
      
    })
  })
  
  ########
  # Calculation log-likelihood of mix fit
  
  output$loglik <- renderText(
    if (is.null(values$unmix.data$data.mix$loglik) )
      return(NULL) else
        as.character(round(values$unmix.data$data.mix$loglik, 2))
  )
  
  
  ########
  # Print table Monte Carlo mix
  
  output$mixtable <- renderTable({
    if(is.null(values$unmix.data)){return(NULL)}
    
    if(input$do.log != "Log-normal"){return(NULL)}
    
    data.frame(Mean = round(values$unmix.data$mean, 2),
               St.Dev. = round(values$unmix.data$sd, 2),
               Lambda = round(values$unmix.data$data.mix$lambda, 2))
  })
  
  
  ########
  # Download handler for saving the mix plots as PDF
  
  output$download_mixPlot <- downloadHandler(
    filename = function() {
      "mix_plot.pdf"
    },
    content = function(file) {
      
      data <- values$df[,input$columns]
      data <- na.omit(data)
      names(data) <- c("X", "Y", "Z")
      
      lab <- input$columns[3]
      
      if(input$do.log == "Log-normal"){
        data$Z = log10(data$Z)
        lab <- paste0("log10(",lab,")")
      }
      
      
      if(!is.null(values$df.poly)){
        data <- data[inout(data[, 1:2], values$df.poly),] 
      }
      
      pdf(file, width = 13, height = 6) # Start a PDF device # Increase canvas size
      
      # Adjust margins to avoid cutting off labels
      par(mfrow=c(1,2), cex.axis=1.4, cex.lab=1.6, cex.main=1.4, mar = c(5, 5, 2, 2) + 0.1)
      
      # Density Plot
      plot(values$unmix.data$data.mix, which=2, breaks="fd", xlab2 = lab)
      lines(density(data$Z, adjust = 1), lty=1, lwd=1.5)
      rug(values$unmix.data$data.mix$x)
      
      ## Q-Q plot 
      # Sampling and plotting 100 distributions, consisting in the mixture of n.pop populations:
      n.pop <- length(values$unmix.data$mean) # the number of populations stored in values$unmix.data
      qqnorm(values$unmix.data$data.mix$x, type="n")
      for (i in 1:100) {
        mix.pop <- list(0)
        for (ii in 1:n.pop) { 
          mix.pop[[ii]] <- rnorm(values$unmix.data$data.mix$lambda[ii]*length(data$Z), values$unmix.data$data.mix$mu[ii], values$unmix.data$data.mix$sigma[ii])
        }
        mix.pop <- unlist(mix.pop); attributes(mix.pop) <- NULL
        points(qqnorm(mix.pop, plot.it = FALSE), pch=16, cex=0.5, col="lightblue")
      }
      # Plotting the qqlines of the n.pop populations
      for (i in 1:n.pop) { 
        qqline( rnorm(values$unmix.data$data.mix$lambda[i]*10000, values$unmix.data$data.mix$mu[i], values$unmix.data$data.mix$sigma[i]), col=i+1, lwd=2)
      }
      # Plotting the sample nscores
      points(qqnorm(values$unmix.data$data.mix$x, plot.it = FALSE), pch=16, cex=0.8)
      box(lwd=1.5)
      
      dev.off() # Close the PDF device
    }
  )
  
  
  
  ###################
  # 4 PANEL Variogram
  ###################
  
  ########
  # ANISOTROPIC variogram plot and calculation
  
  output$varMapPlot <- renderPlot({
    
    req(values$df)
    
    Sys.sleep(0.3)
    
    data <- values$df[input$columns]
    data <- na.omit(data)
    names(data) <- c("X", "Y", "Z")
    
    if (!is.na(input$epsg.out)) {
      data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = 4326, epsg.out = input$epsg.out)
    }
    
    if(!is.null(values$df.poly)){
      data <- data[inout(data[, 1:2], values$df.poly),] 
    }
    
    data.qq <- as.data.frame(qqnorm(data$Z, plot.it = FALSE))
    data$Z <- data.qq$x
    coordinates(data) <- c("X", "Y")
    
    data <- remove.duplicates(data)
    
    if(is.na(input$an.lag_dist)){
      data.var0 <- gstat::variogram(Z ~ 1, data, width=NULL) # cutoff=cutoff0
      
      dist.substr <- NULL
      for (i in 1:length(data.var0$dist)) { # <--
        dist.substr[i] <- data.var0$dist[i+1] -data.var0$dist[i]
        ave.dist <- round(mean(dist.substr, na.rm=T), 1)
      }
      values$an.calc.dist <- ave.dist
    }
    else{
      #updateNumericInput(session, "lag_dist", value = ave.dist)
      ave.dist <- input$an.lag_dist
    }
    
    alpha.dirs <- as.numeric(unlist(strsplit(input$an.dir,",")))
    
    # Directional
    values$data.var.an <- gstat::variogram(Z ~ 1, data, width=ave.dist, alpha=alpha.dirs, tol.hor=22.5)
    values$data.vm.an <- vgm(psill=input$an.psill, model= input$an.varType, range=input$an.range, nugget=input$an.nugget, anis = c(input$an.maindir, input$an.ratio))
    
    ii <- values$data.var.an$dir.hor == input$an.maindir
    
    # SSE calculation:
    fitted_values <- variogramLine(values$data.vm.an, dist_vector = values$data.var.an$dist[ii])
    residual <- fitted_values$gamma - values$data.var.an$gamma[ii]
    weight <- values$data.var.an$np[ii]/(values$data.var.an$dist[ii]^2)
    values$an.sse <- sum(weight*residual^2)
    
    
    vgm.map <- variogram(Z ~ 1, data, cutoff=input$an.range*3, width = ave.dist, map = TRUE)
    
    values$var.list <- list(data,data.vm = values$data.vm.an,data.qq,vgm.map)

    
    if(values$data.var.an$np[1] < 30){
      showNotification("A number of pairs > 30 is recommended (Journel and Huijbregts, 1978)",
                       type =  "message",duration =NULL,)
      
    }
    
    if(input$an.nugget > 0.5){
      showNotification("A nugget value > 0.5 might indicate poor spatial continuity at short scale. Please consider removing any outliers or using a robust variogram estimator.",
                       type =  "message",duration =NULL,)
      
    }
    
    
    plot(vgm.map, threshold = 5)
    
  })
  
  output$varDirPlot <- renderPlot({
    plot(values$data.var.an, values$data.vm.an, as.table = TRUE)
  })
  
  output$an.ssr <- renderText(
    values$an.sse
  )
  
  output$an.varTable <- renderTable({
    req(values$data.var.an)
    df <- data.frame(pair_n=round(values$data.var.an$np,0),
                     dist=round(values$data.var.an$dist,2), 
                     gamma=round(values$data.var.an$gamma,2),
                     dir=round(values$data.var.an$dir.hor,0))
    gamma <- intToUtf8(947)
    setNames(df, c("number of pairs", "distance", gamma,"direction"))
  })
  
  
  
  ########
  # ISOTROPIC variogram plot and calculation
  
  output$varPlot <- renderPlot({
    
    req(values$df)
    
    Sys.sleep(0.3)
    
    data <- values$df[input$columns]
    data <- na.omit(data)
    names(data) <- c("X", "Y", "Z")
    
    if (!is.na(input$epsg.out)) {
      data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = 4326, epsg.out = input$epsg.out)
    }
    
    if(!is.null(values$df.poly)){
      data <- data[inout(data[, 1:2], values$df.poly),] 
    }
    
    data.qq <- as.data.frame(qqnorm(data$Z, plot.it = FALSE))
    data$Z <- data.qq$x
    coordinates(data) <- c("X", "Y")
    
    data <- remove.duplicates(data)
    
    
    
    
    
    if(is.na(input$lag_dist)){
      data.var0 <- gstat::variogram(Z ~ 1, data,  cressie= input$robust, width=NULL) # cutoff=cutoff0
      
      dist.substr <- NULL
      for (i in 1:length(data.var0$dist)) { # <--
        dist.substr[i] <- data.var0$dist[i+1] -data.var0$dist[i]
        ave.dist <- round(mean(dist.substr, na.rm=T), 1)
      }
      values$calc.dist <- ave.dist
    }
    else{
      
      #updateNumericInput(session, "lag_dist", value = ave.dist)
      ave.dist <- input$lag_dist
    }
    
    data.var <- gstat::variogram(Z ~ 1, data, cressie= input$robust, width= ave.dist) # cutoff=cutoff0
    
    data.var.cloud <- gstat::variogram(Z ~ 1, data, cloud= TRUE, cressie= input$robust, width=ave.dist) # cutoff=cutoff0
    
    ifelse(input$varType == "Exponential", type <- "Exp", 
           ifelse(input$varType == "Spherical", type <- "Sph", 
                  ifelse(input$varType == "Gaussian", type <- "Gau", 
                         ifelse(input$varType == "Circular", type <- "Cir", 
                                ifelse(input$varType == "Linear", type <- "Lin", 
                                       ifelse(input$varType == "Pentaspherical", type <- "Pen", type <- "Bes"))))))
    
    ifelse(input$varType2 == "Exponential", type2 <- "Exp", # <---- REVIEW
           ifelse(input$varType2 == "Spherical", type2 <- "Sph", 
                  ifelse(input$varType2 == "Gaussian", type2 <- "Gau", 
                         ifelse(input$varType2 == "Circular", type2 <- "Cir", 
                                ifelse(input$varType2 == "Linear", type2 <- "Lin", 
                                       ifelse(input$varType2 == "Pentaspherical", type2 <- "Pen", type2 <- "Bes")))))) # <---- REVIEW
    
    
    if (input$fit == "Automatic"){
      cutoff0 <- max(spDists(coordinates(data), longlat = FALSE))/3 # diagonale/3, come in gstat
      range <- cutoff0/3
      
      
      
      if (type=="Exp") range <- range/3
      if (type=="Gau") range <- range/sqrt(3)
      if (type=="Bes") range <- range/4
      data.vm <- fit.variogram(data.var, model=vgm(1, type, range, 1))
      updateNumericInput(session, "nugget", value = round(data.vm$psill[1],2))
      updateNumericInput(session, "psill", value = round(data.vm$psill[2],2))
      updateNumericInput(session, "range", value = ifelse(input$varType == "Exponential", round(data.vm$range[2]*3, 1),
                                                          ifelse(input$varType == "Gaussian", round(data.vm$range[2]*sqrt(3), 1),
                                                                 ifelse(input$varType == "Bessel", round(data.vm$range[2]*4, 1), 
                                                                        round(data.vm$range[2], 1)) ) ) )
      
      updateNumericInput(session, "an.nugget", value = round(data.vm$psill[1],2))
      updateNumericInput(session, "an.psill", value = round(data.vm$psill[2],2))
      updateNumericInput(session, "an.range", value = ifelse(input$varType == "Exponential", round(data.vm$range[2]*3, 1),
                                                             ifelse(input$varType == "Gaussian", round(data.vm$range[2]*sqrt(3), 1),
                                                                    ifelse(input$varType == "Bessel", round(data.vm$range[2]*4, 1), 
                                                                           round(data.vm$range[2], 1)) ) ) )
      
      if (input$nes=="Nested") { # <---- REVIEW
        cutoff0 <- max(spDists(coordinates(data), longlat = FALSE))/3 # diagonale/3, come in gstat
        range <- cutoff0/3
        if (type=="Exp") range <- range/3
        if (type=="Gau") range <- range/sqrt(3)
        if (type=="Bes") range <- range/4
        
        if (type2=="Exp") range <- range/3
        if (type2=="Gau") range <- range/sqrt(3)
        if (type2=="Bes") range <- range/4
        data.var.mod1 <- vgm(1, type, range/2, 1)
        data.var.mod2 <- vgm(0.5, type2, range, add.to = data.var.mod1)
        data.vm <- fit.variogram(data.var, model=data.var.mod2); print(data.vm)
        updateNumericInput(session, "nugget", value = round(data.vm$psill[1],2))
        updateNumericInput(session, "psill", value = round(data.vm$psill[2],2))
        updateNumericInput(session, "range", value = ifelse(input$varType == "Exponential", round(data.vm$range[2]*3, 1),
                                                            ifelse(input$varType == "Gaussian", round(data.vm$range[2]*sqrt(3), 1),
                                                                   ifelse(input$varType == "Bessel", round(data.vm$range[2]*4, 1), 
                                                                          round(data.vm$range[2], 1)) ) ) )
        updateNumericInput(session, "psill2", value = round(data.vm$psill[3],2))
        updateNumericInput(session, "range2", value = ifelse(input$varType2 == "Exponential", round(data.vm$range[3]*3, 1),
                                                             ifelse(input$varType2 == "Gaussian", round(data.vm$range[3]*sqrt(3), 1),
                                                                    ifelse(input$varType2 == "Bessel", round(data.vm$range[3]*4, 1), 
                                                                           round(data.vm$range[3], 1)) ) ) )
      } # <---- REVIEW
      #updateNumericInput(session, "lag_dist", value = input$lag_dist) #ave.dist) ## <-- 
    }
    
    else {
      range <- input$range
      if (type=="Exp") range <- input$range/3
      if (type=="Gau") range <- input$range/sqrt(3)
      if (type=="Bes") range <- input$range/4
      data.vm <- fit.variogram(data.var, model=vgm(input$psill, type, range, input$nugget),
                               fit.sills = c(FALSE, FALSE), fit.ranges = c(FALSE, FALSE))
      
      if (input$nes=="Nested") { # <---- REVIEW
        range1 <- input$range
        range2 <- input$range2
        
        if (type=="Exp") range1 <- input$range/3
        if (type=="Gau") range1 <- input$range/sqrt(3)
        if (type=="Bes") range1 <- input$range/4
        
        if (type2=="Exp") range2 <- input$range2/3
        if (type2=="Gau") range2 <- input$range2/sqrt(3)
        if (type2=="Bes") range2 <- input$range2/4
        data.var.mod1 <- vgm(input$psill, type, range1, input$nugget)
        data.var.mod2 <- vgm(input$psill2, type2, range2, add.to = data.var.mod1)
        data.vm <- fit.variogram(data.var, model=data.var.mod2, fit.sills = c(FALSE, FALSE), fit.ranges = c(FALSE, FALSE)) # <---- REVIEW
      } # <---- REVIEW
      
    }
    
    
    # SSE calculation:
    fitted_values <- variogramLine(data.vm, dist_vector = data.var$dist)
    residual <- fitted_values$gamma-data.var$gamma
    weight <- data.var$np/(data.var$dist^2)
    attr(data.vm, "SSErr") <- sum(weight*residual^2)
    
    
    data.vm.line <- variogramLine(data.vm, maxdist = max(data.var$dist)+2*max(data.var$dist)/min(data.var$dist)) # 800 # maxdist = cutoff
    
    values$var <- data.var
    values$var.cloud <- data.var.cloud
    values$vm <- data.vm
    values$vm.line <- data.vm.line
    
    
    # Plot variogram and variogram cloud
    par(cex.axis=1.2, cex.lab=1.4, cex.main=1.4, family="Arial")
    
    if (data.vm$psill[1]+data.vm$psill[2] > 5) {
      # Display an error message
      showNotification("Variogram sill or range is too high.\nPlease change lag distance and/or variogram model \n
                       in automatic mode or use the manual fit.", type = "error",duration = NULL)
    } else {
      # Generate the plot if the parameter is within the threshold
      if (input$nes=="Nested"){ # <-------- REVIEW
        y_max <- data.vm$psill[1]+data.vm$psill[2]+data.vm$psill[3]+0.2
      } else {
        y_max <- data.vm$psill[1]+data.vm$psill[2]+0.2
      } # <-------- REVIEW
      
      par(mar = c(5.1, 5.1, 4.1, 2.1))
      
      plot(data.var$dist, data.var$gamma, ylim=c(0, y_max), # <-------- REVIEW
           xaxs="i", yaxs="i", cex=1.4,  xlim=c(0, max(data.var$dist)+max(data.var$dist)/min(data.var$dist)), 
           xlab= "Distance (m)", ylab=expression(paste("Semivariance  ", gamma)), main="Experimental variogram and variogram model")
      lines(data.vm.line$dist, data.vm.line$gamma, lwd=1.5)
      if (input$nes=="Nested"){ # <-------- REVIEW
        abline(h = data.vm$psill[1]+data.vm$psill[2]+data.vm$psill[3], lty=2, lwd=1.5)
      } else {
        abline(h = data.vm$psill[1]+data.vm$psill[2], lty=2, lwd=1.5)
      } # <-------- REVIEW
      
      box(lwd=2)
    }
    values$var.list <- list(data,data.vm,data.qq)
    
    if(values$var$np[1] < 30){
      showNotification("A number of pairs > 30 is recommended (Journel and Huijbregts, 1978)",
                       type =  "message",duration =NULL,)
      
    }
    
    if(input$nugget > 0.5){
      showNotification("A nugget value > 0.5 might indicate poor spatial continuity at short scale. Please consider removing any outliers or using a robust variogram estimator.",
                       type =  "message",duration =NULL,)

    }
    
  })
  
  ########
  # Print Error Sum of Squares (SSE)
  
  output$ssr <- renderText(
    attr(values$vm, "SSErr")
  )
  
  ########
  # Print calculated auto lag distance
  
  output$calc.dist <- renderText(
    if(is.na(input$lag_dist)){
      req(values$calc.dist)
      round(values$calc.dist,digits = 1)  
    }else{
      "-"
    }
    
  )
  
  output$an.calc.dist <- renderText(
    if(is.na(input$an.lag_dist)){
      req(values$an.calc.dist)
      round(values$an.calc.dist,digits = 1)  
    }else{
      "-"
    }
    
  )
  
  
  ########
  # Plot variogram cloud
  
  output$varCloudPlot <- renderPlot({
    req(values$var)
    par(cex.axis=1.2, cex.lab=1.4, cex.main=1.4, family="Arial")
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    plot(x=0, y=0, type="n", ylim=c(0, max(values$var.cloud$gamma)),
         xaxs="i", yaxs="i",  xlim=c(0, max(values$var$dist)+max(values$var$dist)/min(values$var$dist)), 
         xlab= "Distance (m)", ylab=expression(paste("Semivariance  ", gamma)), main="Variogram cloud")
    abline(v=values$var$dist, col="#21908CFF", lty=2)
    points(values$var.cloud$dist, values$var.cloud$gamma, cex=1)
    box(lwd=2)
  })
  
  ########
  # Print variogram table 
  
  output$varTable <- renderTable({
    req(values$var)
    df <- data.frame(pair_n=round(values$var$np,0), dist=round(values$var$dist,2), gamma=round(values$var$gamma,2))
    gamma <- intToUtf8(947)
    setNames(df, c("number of pairs", "distance", gamma))
  })
  
  ########
  # Download handler for saving the variogram table as CSV
  
  output$download_table <- downloadHandler(
    filename = function() {
      "Variogram_table.csv" # Default file name
    },
    content = function(file) {
      write.csv(values$var[,1:3], file, row.names = F) # Save with row names
    }
  )
  
  output$an.download_table <- downloadHandler(
    filename = function() {
      "Variogram_table.csv" # Default file name
    },
    content = function(file) {
      df <- data.frame(pair_n=round(values$data.var.an$np,0),
                       dist=round(values$data.var.an$dist,2), 
                       gamma=round(values$data.var.an$gamma,2),
                       dir=round(values$data.var.an$dir.hor,0))
      gamma <- intToUtf8(947)
      setNames(df, c("number of pairs", "distance", gamma,"direction"))
      write.csv(df, file, row.names = F) # Save with row names
    }
  )
  
  ########
  # Download handler for saving the variogram plots as PDF
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "Variogram_plot.pdf"
    },
    content = function(file) {
      par(cex.axis=1.4, cex.lab=1.6, family="Arial")
      pdf(file, width = 7, height = 6) # Start a PDF device # Increase canvas size
      
      # Adjust margins to avoid cutting off labels
      par(mar = c(5, 5, 2, 2) + 0.1) # Bottom, Left, Top, Right
      
      # Plot
      plot(values$var$dist, values$var$gamma, ylim=c(0, values$vm$psill[1]+values$vm$psill[2]+0.2), 
           xaxs="i", yaxs="i", cex=2,  xlim=c(0, max(values$var$dist)+max(values$var$dist)/min(values$var$dist)), 
           xlab= "Distance (m)", ylab=expression(paste("Semivariance  ", gamma)),
           cex.axis=1.2, cex.lab=1.2)
      lines(values$vm.line$dist, values$vm.line$gamma, lwd=1.5)
      abline(h = values$vm$psill[1]+values$vm$psill[2], lty=2, lwd=1.5)
      box(lwd=2)
      
      dev.off() # Close the PDF device
    }
  )
  
  output$an.download_plot <- downloadHandler(
    filename = function() {
      "Variogram_plot.pdf"
    },
    content = function(file) {
      pdf(file, width = 7, height = 6) # Start a PDF device # Increase canvas size
      
      # Adjust margins to avoid cutting off labels
      par(mfrow = c(2, 1))
      par(mar = c(5, 5, 2, 2) + 0.1) # Bottom, Left, Top, Right
      print(plot(values$var.list[[4]], threshold = 5))
      
      print(plot(values$data.var.an, values$data.vm.an))
      
      dev.off() # Close the PDF device
    }
  )
  
  
  
  #############
  # 5 PANEL sGs
  #############
  
  
  ########
  # Run sGS
  
  observeEvent(input$runsgs,{
    
    
    sel.vartab <- input$vardirtab
    
    showNotification("Calculating Sequential Gaussian Simulations... the results will be ready shortly",
                     type =  "message",duration =NULL,)
    
    df.grid <- expand.grid(x=seq(input$xmin-input$dx, input$xmax+input$dx, input$dx),
                           y=seq(input$ymin-input$dy, input$ymax+input$dy, input$dy))
    
    if(!is.null(values$df.poly)){
      df.grid <- df.grid[inout(df.grid, values$df.poly),]
    }
    
    
    # Trasformo le coordinate della variabile df2.grid in griglia
    gridded(df.grid) <- c("x","y")
    
    
    # Calc SGS
    
    start <- Sys.time()
    
    nsims <- input$nsims
    
    if(input$nsims > 1000){
      updateNumericInput(session,"nsims",value = 1000 )
      nsims <- 1000
    }
    
    if (TRUE) {
      df.sims <- krige(formula =  Z~1,values$var.list[[1]],df.grid,values$var.list[[2]],nsim=nsims,nmax=40,debug.level=-1)
      
      df.sims <- as.data.frame(df.sims)
    }else{
      
      numCores <- detectCores() - 1
      cl <- makeCluster(numCores)
      registerDoParallel(cl)
      
      sgs.vars <- list(var.list1 = values$var.list[[1]],
                       grid = df.grid,
                       var.list2 = values$var.list[[2]],
                       nsim = round(nsims/numCores))
      
      # Parallelized for loop
      df.sims <- foreach(i = 1:numCores) %dopar% {
        library(gstat)
        df.sims <- krige(formula =  Z~1,sgs.vars$var.list1,sgs.vars$grid,sgs.vars$var.list2,nsim=sgs.vars$nsim,nmax=40,debug.level=-1)
      }
      
      # Stop the cluster after the computations
      stopCluster(cl)
      
      df.sims <- as.data.frame(df.sims)[,-c(grep("x.",x =names(as.data.frame(df.sims)) ),
                                            grep("y.",x =names(as.data.frame(df.sims)) ))]
    }
    
    
    end <- Sys.time()
    message(end - start," elapsed")
    
    for (ii in 3:ncol(df.sims)) {
      df.sims[,ii] <- approx(values$var.list[[3]]$x,values$var.list[[3]]$y,df.sims[,ii])$y
    }
    
    # Calcolo la media del flusso di Z di tutte le simulazioni
    df.mean <- data.frame(x=df.sims$x,
                          y=df.sims$y,
                          z=rowMeans(df.sims[,-1:-2],na.rm = T))
    
    
    
    if(!is.na(input$prob.thres)){
      df.mean <- cbind(df.mean,data.frame(p=1-apply(df.sims, 1, 
                                                    function(x){x.ecdf=ecdf(as.numeric(x));x.ecdf(input$prob.thres)})))
    }
    
    
    df.sd <- apply(df.sims[,-1:-2],MARGIN = 1,FUN = sd,na.rm = T)
    
    # Output and standard deviation
    gas.out.tot <- sum(df.mean$z*(input$dx*input$dy))/1e+06
    gas.out.each <- apply(df.sims[, 3:ncol(df.sims)], 2, sum, na.rm=TRUE)
    gas.out.sd <- sd(gas.out.each*(input$dx*input$dy))/1e+06
    area.out <- input$dx*input$dy*nrow(df.mean)
    values$CO2out <- c(gas.out.tot, gas.out.sd,area.out)
    
    if (is.na(input$epsg.in)) {
      #epsg=paste0("+init=epsg:",input$epsg.out)
      epsg=paste0("epsg:",input$epsg.out)
    }else{
      #epsg=paste0("+init=epsg:",input$epsg.in)
      epsg=paste0("epsg:",input$epsg.in)
    }
    
    #values$raster <- rasterFromXYZ(df.mean[,1:3],crs=CRS(epsg))
    values$raster <- rast(df.mean[,1:3],type="xyz",crs=epsg)
    values$raster.sd <- rast(cbind(df.mean[,1:2],df.sd),type="xyz",crs=epsg)
    
    if (is.na(input$prob.thres)) {
      df.mean[,4] <- NA
      #values$raster.p <- rasterFromXYZ(df.mean[,c(1:2,4)],crs=CRS(epsg))
      values$raster.p <- rast(df.mean[,c(1:2,4)],type="xyz",crs=epsg)
    }else{
      #values$raster.p <- rasterFromXYZ(df.mean[,c(1:2,4)],crs=CRS(epsg))
      values$raster.p <- rast(df.mean[,c(1:2,4)],type="xyz",crs=epsg)
    }
  })
  
  
  ########
  # Calculate 2d numerical integration
  
  observeEvent(input$calcIntegral,{
    
    if (is.null(values$CO2out)){return(NULL)}
    
    paste(as.character(round(values$CO2out[1], 2)), "±", as.character(round(values$CO2out[2], 2)), "t/day")
    
    output$outputtable <- renderTable({
      
      data.frame(Mean = paste0(round(values$CO2out[1], 2),"·10⁶"),
                 St.Dev. = paste0(round(values$CO2out[2], 2),"·10⁶"),
                 Area = paste0(round(values$CO2out[3])," m²"))
    })
    
  })
  
  ########
  # Display sGs map
  
  output$mymap2 <- renderLeaflet({
    
    
    #req(values$df.poly)
    req(values$raster)
    
    data <- values$df[input$columns]
    data <- na.omit(data)
    names(data) <- c("X", "Y", "Z")
    
    
    if (!is.na(input$epsg.in)) {
      data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = input$epsg.in, epsg.out =4326)
    }
    
    
    updateNumericInput(session,"zmin",value =round(min(as.matrix(values$raster),na.rm=T), 1) )
    updateNumericInput(session,"zmax",value =round(max(as.matrix(values$raster),na.rm=T), 1) )
    
    updateNumericInput(session,"zmin2",value =round(min(as.matrix(values$raster.sd),na.rm=T), 1) )
    updateNumericInput(session,"zmax2",value =round(max(as.matrix(values$raster.sd),na.rm=T), 1) )
    
    pal <-  colorNumeric( viridis::viridis(10), (as.matrix(values$raster)),na.color = "transparent")
    pal.sd <-  colorNumeric( viridis::viridis(10), (as.matrix(values$raster.sd)),na.color = "transparent")
    # pal.p <-  colorNumeric( viridis::viridis(10), (as.matrix(values$raster.p)),na.color = "transparent")
    
    if (is.na(input$prob.thres)) {
      pal.p <-  colorNumeric( viridis::viridis(10), NULL, na.color = "transparent")
    }else{
      pal.p <-  colorNumeric( viridis::viridis(10), (as.matrix(values$raster.p)),na.color = "transparent")
    }
    
    
    leaflet() %>% 
      addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
      addProviderTiles('Esri.WorldImagery',group = 'Esri World Imagery') %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addLayersControl(position ="topleft",
                       baseGroups = c("Esri World Imagery", "OpenStreetMap"),
                       overlayGroups = c("Points"),
                       options = layersControlOptions(collapsed = T)) %>%
      addCircleMarkers( lng=data$X,
                        lat=data$Y,label = data$Z,
                        color="black",radius=1,group = "Points") %>%
      addRasterImage(values$raster.p, colors = pal.p,project=FALSE) %>%
      addRasterImage(values$raster.sd, colors = pal.sd,project=FALSE) %>%
      addRasterImage(values$raster, colors = pal,project=FALSE) %>%
      leaflet::addLegend(pal = pal, values = as.matrix(values$raster) ,labels = as.matrix(values$raster),opacity = 0.8,title = names(values$df[, input$columns])[3]) %>%
      hideGroup("Probability map")
  }
  )
  
  #myLeafletProxy <- leafletProxy(mapId = "mymap2", session)
  
  
  ########
  # Refresh sGs map
  
  
  observeEvent(input$refresh 
               | values$selected.tab
               | as.numeric(factor(input$palette1)),{
                 
                 
                 if (is.null(values$raster)){
                   return(NULL)
                 }
                 
                 data <- values$df[input$columns]
                 data <- na.omit(data)
                 names(data) <- c("X", "Y", "Z")
                 
                 
                 if (!is.na(input$epsg.in)) {
                   data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = input$epsg.in, epsg.out =4326)
                 }
                 
                 
                 
                 raster <- values$raster
                 
                 raster[raster<input$zmin] <- input$zmin
                 raster[raster>input$zmax] <- input$zmax
                 
                 # Dynamic palette selection
                 selected_palette <- switch(input$palette1,
                                            "viridis" = viridis::viridis(10),
                                            "rainbow" = fields::tim.colors(10),
                                            "magma" = viridis::magma(10),
                                            "plasma" = viridis::plasma(10),
                                            "cividis" = viridis::cividis(10)
                 )
                 
                 
                 pal <-  colorNumeric(selected_palette, (as.matrix(raster)),alpha = T,na.color = "transparent")
                 pal.e <-  colorNumeric(selected_palette , (as.matrix(values$raster.sd)),na.color = "transparent")
                 
                 #clearMarkers(map = myLeafletProxy) %>% 
                 leafletProxy("mymap2", session) %>%
                   clearControls() %>%
                   addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
                   addProviderTiles('Esri.WorldImagery',group = 'Esri World Imagery') %>%
                   addCircleMarkers( lng=data$X,
                                     lat=data$Y,label = data$Z,
                                     color="black",radius=1,group = "Points") %>%
                   removeImage(layerId = "r_mean") %>%
                   removeImage(layerId = "r_sd") %>%
                   removeImage(layerId = "r_prob") %>%
                   addRasterImage(raster, colors = pal,layerId = "r_mean", opacity = input$raster.opacity,project=FALSE) %>%
                   leaflet::addLegend(pal = pal, values = as.matrix(raster) ,labels = as.matrix(raster),opacity = 0.8,title = names(values$df[, input$columns])[3])
                 
               }
  )
  
  # Refresh Probability Map
  observeEvent(input$refresh1
               | values$selected.tab
               | as.numeric(factor(input$palette1)),{
                 
                 
                 if (is.null(values$raster.p)){
                   return(NULL)
                 }
                 
                 data <- values$df[input$columns]
                 data <- na.omit(data)
                 names(data) <- c("X", "Y", "Z")
                 
                 
                 if (!is.na(input$epsg.in)) {
                   data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = input$epsg.in, epsg.out =4326)
                 }
                 
                 
                 raster.p <- values$raster.p
                 
                 raster.p[raster.p<input$zmin1] <- input$zmin1
                 raster.p[raster.p>input$zmax1] <- input$zmax1
                 
                 # Dynamic palette selection
                 selected_palette <- switch(input$palette1,
                                            "viridis" = viridis::viridis(10),
                                            "rainbow" = fields::tim.colors(10),
                                            "magma" = viridis::magma(10),
                                            "plasma" = viridis::plasma(10),
                                            "cividis" = viridis::cividis(10)
                 )
                 
                 if (is.na(input$prob.thres)) {
                   pal.p <-  colorNumeric(selected_palette, NULL, na.color = "transparent")
                 }else{
                   pal.p <-  colorNumeric(selected_palette, (as.matrix(raster.p)),na.color = "transparent")
                 }
                 
                 #clearMarkers(map = myLeafletProxy) %>% 
                 leafletProxy("mymap2", session) %>%
                   clearControls() %>%
                   addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
                   addProviderTiles('Esri.WorldImagery',group = 'Esri World Imagery') %>%
                   addCircleMarkers( lng=data$X,
                                     lat=data$Y,label = data$Z,
                                     color="black",radius=1,group = "Points") %>%
                   removeImage(layerId = "r_mean") %>%
                   removeImage(layerId = "r_sd") %>%
                   removeImage(layerId = "r_prob") %>%
                   addRasterImage(raster.p, colors = pal.p,layerId = "r_prob",opacity = input$raster.opacity1,project=FALSE) %>%
                   leaflet::addLegend(pal = pal.p, values = as.matrix(raster.p) ,labels = as.matrix(raster.p),opacity = 0.8,title = paste0("Pr ",names(values$df[, input$columns])[3]," > ", input$prob.thres))
                 
               }
  )
  
  
  # Refresh Error Map
  observeEvent(input$refresh2
               | values$selected.tab
               | as.numeric(factor(input$palette1)),{
                 
                 
                 if (is.null(values$raster.sd)){
                   return(NULL)
                 }
                 
                 data <- values$df[input$columns]
                 data <- na.omit(data)
                 names(data) <- c("X", "Y", "Z")
                 
                 
                 if (!is.na(input$epsg.in)) {
                   data[,c("X","Y")] <- coord.trans(data[,c("X","Y")], epsg.in = input$epsg.in, epsg.out =4326)
                 }
                 
                 
                 raster.sd <- values$raster.sd
                 

                 raster.sd[raster.sd<input$zmin2] <- input$zmin2
                 raster.sd[raster.sd>input$zmax2] <- input$zmax2
                 
                 # Dynamic palette selection
                 selected_palette <- switch(input$palette1,
                                            "viridis" = viridis::viridis(10),
                                            "rainbow" = fields::tim.colors(10),
                                            "magma" = viridis::magma(10),
                                            "plasma" = viridis::plasma(10),
                                            "cividis" = viridis::cividis(10)
                 )
                 
                 pal.sd <-  colorNumeric(selected_palette , (as.matrix(raster.sd)),na.color = "transparent")
                 
                 #clearMarkers(map = myLeafletProxy) %>% 
                 leafletProxy("mymap2", session) %>%
                   clearControls() %>%
                   addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
                   addProviderTiles('Esri.WorldImagery',group = 'Esri World Imagery') %>%
                   addCircleMarkers( lng=data$X,
                                     lat=data$Y,label = data$Z,
                                     color="black",radius=1,group = "Points") %>%
                   removeImage(layerId = "r_mean") %>%
                   removeImage(layerId = "r_sd") %>%
                   removeImage(layerId = "r_prob") %>%
                   addRasterImage(raster.sd, colors = pal.sd,layerId = "r_sd",opacity = input$raster.opacity2,project=FALSE) %>%
                   leaflet::addLegend(pal = pal.sd, values = as.matrix(raster.sd) ,labels = as.matrix(raster.sd),opacity = 0.8,title = paste0("Sd ",names(values$df[, input$columns])[3]))
                 
               }
  )
  
  
  ## Download Mean map as raster
  output$downloadRaster <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(format(Sys.time(),"%Y%m%d%H%M%S"), "_mean_raster.tif")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      #write.csv(values$raster, file,row.names = F)
      raster::writeRaster(values$raster, filename = file, overwrite=T)
    }
  )
  
  ## Download Probability map as raster
  output$downloadRaster1 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(format(Sys.time(),"%Y%m%d%H%M%S"), "_prob_raster.tif")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      #write.csv(values$raster, file,row.names = F)
      raster::writeRaster(values$raster.p, filename = file, overwrite=T)
    }
  )
  
  ## Download Error map as raster
  output$downloadRaster2 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(format(Sys.time(),"%Y%m%d%H%M%S"), "_error_raster.tif")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      #write.csv(values$raster, file,row.names = F)
      raster::writeRaster(values$raster.sd, filename = file, overwrite=T)
    }
  )
  
  ####
  # Help
  
  observeEvent(input$help.load,{
    showModal(modalDialog(size = "l",
                          title = "Help",
                          helpText("1) Select the appropriate column separator (comma by default) and upload the csv file.",br(),br(),
                                   "2) Select 3 columns in the following order: longitude, latitude, and a numerical variable (e.g., soil CO₂ flux or T)",br(),br(),
                                   "3) Enter the correct EPSG code:",br(),
                                   "  3a) If longitude and latitude are decimal WGS84, enter the output EPSG to convert to UTM 
                                               (a list of EPSG codes is available at https://epsg.io/).",br(),
                                   "  3b) If longitude and latitude are not decimal WGS84, enter the correct input EPSG code and leave the output EPSG field empty.",br(),br(),
                                   "4) Draw a rectangular or polygonal perimeter on the map (using the buttons below the layer selection) to enclose the data to be processed. Alternatively, you can upload a .csv file containing the polygon vertices.", br(),br(),
                                   "  * To ensure user privacy, all uploaded data are processed locally and are not stored on any server; data are automatically discarded when the session ends or the browser is refreshed.",
                                   br(),br()
                          ),
                          
                          easyClose = TRUE,
                          footer = NULL
    ))
    
  })
  
  observeEvent(input$help.mix,{
    showModal(modalDialog(size = "l",
                          title = "Help",
                          helpText("This panel enables us to model mixtures of populations describing the empirical distribution of the data.",br(),br(),
                                   "By default, the data distribution is transformed into lognormal space and automatically modeled using two populations, identifyied through unsupervised clustering. 
                                               The goodness of fit is measured using the log-likelihood.",br(),br(),
                                   "In all Fit modes (Automatic, Initial Guess, and Manual), you can adjust the number of populations used to fit the data distribution.",br(),
                                   "  In Initial Guess mode, you can enter initial values of Mean and St.Dev. of each population. These values are used as starting points to optimize the fit.", br(),
                                   "  In Manual mode, you can explicitly set and constrain the Mean and St.Dev. values for each population.",
                                   br(),br(),
                                   "After maky any changes, you must click the 'Calculate populations' button. The app will return the Mean, St.Dev. and proportion (Lambda) of each population, calculated through a Monte Carlo procedure.",
                                   br(),br(),
                                   
                          ),
                          
                          easyClose = TRUE,
                          footer = NULL
    ))
    
  })
  
  observeEvent(input$help.omni,{
    showModal(modalDialog(size = "l",
                          title = "Help",
                          helpText("By default, the experimental variogram is automatically fitted using a spherical variogram model and weighted least squares. 
                                   The goodness of fit is measured by the Sum of Squared Errors (SSE). 
                                   In both Automatic and Manual modes, you can modify the variogram model, lag distance, and enable the robust estimator option.", br(), br(),
                                   "By choosing the Manual mode, you are also able to
                                  adjust the nugget, partial sill, and range of the variogram model.",br(), br(),
                                   "The robust variogram estimator (Cressie, 1993) helps reduce the nugget effect in the presence of outliers.", br(), br(),
                                   "The lag distance defines the width of the intervals into which pairs of data points are grouped to estimate semivariance. 
                                   It is calculated automatically, but you can change it. As a rule of thumb, we recommend choosing a lag distance at least 5 meters 
                                   larger than the average distance between points. For example, if your grid spacing is 50 m, use a lag distance of 55 m. 
                                   In any case, the lag distance should ensure a minimum of 30 point pairs per lag class (Journel and Huijbregts, 1978).",br(),br(),
                                   "You can also model the sum of two variogram models by selecting the Nested option. 
                                   As with single variogram models, nested structures can be fitted in both automatic and manual modes."
                          ),
                          
                          easyClose = TRUE,
                          footer = NULL
    ))
    
  })
  
  observeEvent(input$help.anis,{
    showModal(modalDialog(size = "l",
                          title = "Help",
                          helpText("Anisotropy can only be modeled in manual mode. By default, the following directions are selected: N (0°), NE (45°), E (90°), and SE (135°).
                                   You can inspect the variogram map to identify the preferential directions of spatial continuity and manually update the four directions (separated by commas).
                                   The variograms for these four directions are displayed below the variogram map.", br(), br(),
                                 "Next, you need to set the main direction (i.e., the direction with the largest range) and the anisotropy ratio, 
                                 which is the ratio between the range in the main direction and the range in the direction perpendicular to it. By default, the Main Direction is set to 45° and the Anisotropy Ratio to 0.4.", br(), br(),
                                 "Finally, manually adjust the variogram parameters (nugget, partial sill, and range — defined with respect to the main direction) to minimize the Sum of Squared Errors (SSE)."
                          ),
                        
                          easyClose = TRUE,
                          footer = NULL
    ))
    
  })
  
  
  observeEvent(input$help.sgs, {
    showModal(modalDialog(
      size = "l",
      title = "Help",
      helpText(
        br(),
        "In this panel, you can simulate the variable at unsampled locations using sequential Gaussian simulation (sGs) to construct mean, error, and probability heat maps.",
        br(), br(),
        "Delta X and Delta Y define the cell spacing for the simulation grid.",
        br(), br(),
        "By default, Delta X and Delta Y are set to 10 m, and the number of simulations is set to 100. When you click Run sGs, the software performs 100 simulations and averages the values at the center of each cell.",
        "If you want to generate a probability map, you must specify a Probability Threshold before clicking Run sGs.",
        "After a few seconds, the heat map of the target variable will be displayed.",
        br(), br(),
        "You can customize the color scale and transparency of the heat map by adjusting Z min, Z max, and Opacity, then clicking the Eye button to refresh the display.",
        "By clicking the eye icon next to the Mean, Error, or Probability map, you can display the corresponding heat map.",
        br(), br(),
        "The Error map shows the standard deviation calculated for each cell across the 100 simulations.",
        "The Probability map indicates the likelihood that the target variable exceeds the specified Probability Threshold.",
        br(), br(),
        "The resulting heat maps can also be downloaded as raster files.", 
        br(), br(),
        "Finally, by clicking the Calculate 2D Integral button, the app computes the double integral of the selected variable over the area defined in the Load Data panel, along with its uncertainty. 
        These values reflect the mean and standard deviation of the outputs estimated for each n equiprobable realizations generated by sGs. 
        In the case of soil CO2 flux data, or any other input variable measured in g m–2 d–1, sGs UnMix returns the total flux and its standard deviation in g d–1"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

