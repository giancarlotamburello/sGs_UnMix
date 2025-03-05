# Load necessary libraries

options(warn=0)

#library(bslib)
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
                                      
                                      # Help text
                                      helpText("1) Select the appropriate column separator (comma by defaults) and upload the csv file.",br(),br(),
                                               "2) Select 3 columns ordered as: longitude, latitude, and a numerical variable (e.g., soil CO₂ flux or T)",br(),br(),
                                               "3) Enter the correct EPSG codes:",br(),
                                               "  3a) If longitude and latitude are decimal WGS84, enter the output EPSG for converting into UTM 
                                               (list of EPSG code are available at https://epsg.io/).",br(),
                                               "  3b) If longitude and latitude are not decimal WGS84, enter the proper input EPSG code and leave output EPSG empty.",
                                               br(),br()
                                      ),
                                      
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
                                        top = 250, right = 30, draggable = F, 
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
                                    
                                    #titlePanel(h3("Mix model")),
                                    helpText("This panel enables the unmixing of populations describing the empirical distribution of the data.",br(),br(),
                                             "By default, the distribution of the data is automatically fitted by two populations defined through unsupervised clustering. 
                                               The goodness of fit is expressed as log-likelihood.",br(),br(),
                                             "In both automatic and manual mode, you can change the number of populations used to fit the distribution of the data.",br(),
                                             "  In automatic mode, you can insert mean and std.dev values of the populations, which will be used as starting values to find the best fit.", br(),
                                             "  In manual mode, you can manually constrain the mean and std.dev values of the populations.",
                                             br(),br(),
                                             "The App returns the mean, std.dev and proportion of each populations, calculated through a Monte Carlo procedure.",
                                             br(),br(),
                                             
                                    ),
                                    
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
                                  # Sidebar layout with an input and output definitions
                                  sidebarLayout(position = "left",
                                                sidebarPanel(
                                                  #titlePanel(h3("Variogram")),
                                                  helpText("By default, the experimental variogram is automatically fitted with a spherical variogram model 
                                  by weighted least squares. The goodness of fit is expressed as RSE. You can change the variogram
                                  model, lag distance, and robust in both auto and manual mode.", br(), br(),
                                                           "By choosing the manual mode, you are also able to
                                  adjust the nugget, partial sill, and range of the variogram model.",br(), br(),
                                                           "The robust method enables us to lower the nugget effect in case of outliers.", br(), br(),
                                                           "The lag distance is the distance of subsequent intervals into which pairs of data points are grouped 
                                  for estimating the semivariance. It is automatically calculated by the software. If you want to change it,
                                  we recommend to choose a lag distance at least 5 m larger than the average
                                  distance between points. For example, if your grid has a 50 m-spacing, choose a lag distance of 55 m.",
                                                           br(),br()
                                                  ),
                                                  
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
                                                    )),
                                                  
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
                                                    ))
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
                                                             tableOutput("varTable"),
                                                             br(), 
                                                             downloadButton("download_table", "Save Table as CSV"),
                                                             br(), br(), 
                                                             downloadButton("download_plot", "Save Plot as PDF")
                                                           )
                                                  )
                                                  
                                                )
                                  )
                         ),
                         
                         
                         #########
                         # 5 PANEL sequential Gasussian simulations 
                         
                         tabPanel("sGs", fluid = TRUE, icon = icon("globe-americas"),
                                  sidebarLayout(position = "left",
                                                sidebarPanel(
                                                  helpText(br(),
                                                           "In this panel, you can simulate the variable at unsampled locations through sGs, for constructing maps",
                                                           br(),br(),
                                                           "Delta X and Y reflect the spacing of the cells where simulation will be performed.", br(), br(),
                                                           "By default, the number of simulations is set to 200. By pressing Run sGs, the software will compute
                                  200 simulations and average the value in each center of the cells. After a few seconds the map is visualized.
                                  You can adjust the color scale and the trasparency of the map by adjusting Z min, Z max, and Opacity, and pressing the refresh button.
                                  It is also possible to download the raster of the map."
                                                           
                                                  ),
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
                                                                        value=5,min = 1)
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
                                                                        value=5,min = 1)
                                                    )),
                                                  fluidRow(
                                                    column(3, offset = 1, 
                                                           numericInput("nsims",
                                                                        label = "Number simulations:",
                                                                        value=200,max = 1000,min = 10)
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
                                                           column(actionButton("refresh","", icon = icon("refresh"), align = "center"
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
                                                           column(actionButton("refresh1","", icon = icon("refresh"), align = "center"
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
                                                    top = 250, right = 30, draggable = F, 
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
                                                    top = 520, right = 30, draggable = F, 
                                                    style = "z-index: 500",
                                                    downloadButton("downloadRaster1", "Save Prob. raster",
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
                           utm.coord=NULL)
  
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
  # Variogram plot and calculation
  
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
      #updateNumericInput(session, "lag_dist", value = input$lag_dist) #ave.dist) ## <-- 
    }
    
    else {
      range <- input$range
      if (type=="Exp") range <- input$range/3
      if (type=="Gau") range <- input$range/sqrt(3)
      if (type=="Bes") range <- input$range/4
      data.vm <- fit.variogram(data.var, model=vgm(input$psill, type, range, input$nugget),
                               fit.sills = c(FALSE, FALSE), fit.ranges = c(FALSE, FALSE))
      
      # SSE calculation:
      fitted_values <- variogramLine(data.vm, dist_vector = data.var$dist)
      residual <- fitted_values$gamma-data.var$gamma
      weight <- data.var$np/(data.var$dist^2)
      attr(data.vm, "SSErr") <- sum(weight*residual^2)
      
    }
    
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
      plot(data.var$dist, data.var$gamma, ylim=c(0, data.vm$psill[1]+data.vm$psill[2]+0.2), 
           xaxs="i", yaxs="i", cex=1.4,  xlim=c(0, max(data.var$dist)+max(data.var$dist)/min(data.var$dist)), 
           xlab= "Distance (m)", ylab=expression(paste("Semivariance  ", gamma)), main="Experimental variogram and variogram model")
      lines(data.vm.line$dist, data.vm.line$gamma, lwd=1.5)
      abline(h = data.vm$psill[1]+data.vm$psill[2], lty=2, lwd=1.5)
      box(lwd=2)
    }
    
    values$var.list <- list(data,data.vm,data.qq)
    
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
  
  ########
  # Plot variogram cloud
  
  output$varCloudPlot <- renderPlot({
    req(values$var)
    par(cex.axis=1.2, cex.lab=1.4, cex.main=1.4, family="Arial")
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
  
  
  
  
  #############
  # 5 PANEL sGs
  #############
  
  ########
  # Run sGS
  
  observeEvent(input$runsgs,{
    
    
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
    
    
    # Output and standard deviation
    gas.out.tot <- sum(df.mean$z*(input$dx*input$dy))/1e+06
    gas.out.each <- apply(df.sims[, 3:ncol(df.sims)], 2, sum, na.rm=TRUE)
    gas.out.sd <- sd(gas.out.each*(input$dx*input$dy))/1e+06
    area.out <- input$dx*input$dy*nrow(df.mean)
    values$CO2out <- c(gas.out.tot, gas.out.sd,area.out)
    
    if (is.na(input$epsg.in)) {
      epsg=paste0("+init=epsg:",input$epsg.out)
    }else{
      epsg=paste0("+init=epsg:",input$epsg.in)
    }
    
    values$raster <- rasterFromXYZ(df.mean[,1:3],crs=CRS(epsg))
    
    if (is.na(input$prob.thres)) {
      df.mean[,4] <- NA
      values$raster.p <- rasterFromXYZ(df.mean[,c(1:2,4)],crs=CRS(epsg))
    }else{
      values$raster.p <- rasterFromXYZ(df.mean[,c(1:2,4)],crs=CRS(epsg))
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
    
    pal <-  colorNumeric( viridis::viridis(10), (as.matrix(values$raster)),na.color = "transparent")
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
                       overlayGroups = c("Mean map","Probability map","Points"),
                       options = layersControlOptions(collapsed = T)) %>%
      addCircleMarkers( lng=data$X,
                        lat=data$Y,label = data$Z,
                        color="black",radius=1,group = "Points") %>%
      addRasterImage(values$raster, colors = pal,group = "Mean map",project=FALSE) %>%
      addRasterImage(values$raster.p, colors = pal.p,group = "Probability map",project=FALSE) %>%
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
                 
                 
                 #clearMarkers(map = myLeafletProxy) %>% 
                 leafletProxy("mymap2", session) %>%
                   clearControls() %>%
                   addProviderTiles('OpenStreetMap.Mapnik',group = 'Open Street Map') %>%
                   addProviderTiles('Esri.WorldImagery',group = 'Esri World Imagery') %>%
                   addCircleMarkers( lng=data$X,
                                     lat=data$Y,label = data$Z,
                                     color="black",radius=1,group = "Points") %>%
                   removeImage(layerId = "r_mean") %>%
                   removeImage(layerId = "r_prob") %>%
                   addRasterImage(raster, colors = pal,group = "Mean map",layerId = "r_mean", opacity = input$raster.opacity,project=FALSE) %>%
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
                   removeImage(layerId = "r_prob") %>%
                   addRasterImage(raster.p, colors = pal.p,group = "Probability map",layerId =  "r_prob",opacity = input$raster.opacity1,project=FALSE) %>%
                   leaflet::addLegend(pal = pal.p, values = as.matrix(raster.p) ,labels = as.matrix(raster.p),opacity = 0.8,title = names(values$df[, input$columns])[3])
                 
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

