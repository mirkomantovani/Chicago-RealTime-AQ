# Mirko Mantovani - Ashwani Khemani - Abhishek Vasudevan - 02/20/2019

#####################################################  Libraries    #####################################################

library(shiny)
library(devtools)
library(ggplot2)
library(shinydashboard)
library(scales) # needed for percent function
library(shinythemes) # themes for bootstrapPage, fluidPage, navbarPage, or fixedPage
library(dashboardthemes)
library(ggthemes)
library(shinyalert)
library(leaflet)
library(rgdal)
library(geojson)
library(geojsonio)
library(colourpicker)
library(shinyWidgets)
library(viridis) # Color palette
library(cdlTools) # convert FIPS codes into names
library(htmltools) # to use htmlEscape function
library(RColorBrewer)
library(reshape2)
library(fst)
library(future)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)

# R data APIs libraries
library(ropenaq)
library(darksky)
library(RSocrata)

library(base)

library(sp)
library(raster)
library(gstat)
# Sys.setenv(DARKSKY_API_KEY = "17b13339acc2cb53e53ea50ea4142528")

#use this key if the above one does not work
Sys.setenv(DARKSKY_API_KEY = "049f7d70c4d28508cffd077381fad386")

#new key by abhishek

#Sys.setenv(DARKSKY_API_KEY = "5ba09d7b0b3669a512befe6433d35f33")

Sys.setenv(DARKSKY_API_KEY = "3a786ec08cd24ff6a6f807d957d5cfff")

########################################### PREPROCESSING and VARIABLES DEFINITION #########################################

# Constants
TIME_RANGE_CURRENT = "Current"
TIME_RANGE_24HOURS = "Last 24 hours"
TIME_RANGE_7DAYS = "Last 7 days"

UPDATE_NODES_STATUS <- FALSE

time_ranges <- c(TIME_RANGE_CURRENT,TIME_RANGE_24HOURS,TIME_RANGE_7DAYS)
tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")
openaq_tracked_measures <- c("co","bc","no2","o3","so2","pm2.5","pm10")
darksky_tracked_measures <- c("temperature", "humidity", "windSpeed", "windBearing", "cloudCover", "visibility", "pressure", "ozone", "summary")

all_measures <- paste0("AoT-",tracked_measures)
all_measures <- c(all_measures,paste0("Darksky-",darksky_tracked_measures))
all_measures <- c(all_measures,paste0("OpenAQ-",openaq_tracked_measures))

all_measures <-all_measures[-which(all_measures=="Darksky-summary")]

value_types <- c("min","max","average")

tab1_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","bc")
tab2_measures <- c("temperature", "humidity","intensity", "wind speed", "wind bearing", "cloud cover", "visibility", "pressure", "ozone", "summary")


last <- NULL

statistics <- c("Median","Max","90th percentile")

pie_chart_backgrounds <- "white" #bcdae0  1a4756
bar_chart_backgrounds <- "#bcdae0" #bcdae0
pie_chart_backgrounds_first <- "#bcdae0" #bcdae0

######################### theme definition #############################

darker_c <- "rgb(30, 34, 68)"
middle_c <- "rgb(52, 57, 104)"
lighter_c <- "rgb(76, 82, 136)"

mirko_theme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(204, 228, 249)" #"rgb(128,177,221)"
  ,bodyBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = darker_c #"rgb(49,56,107)"
    ,colorMiddle = "rgb(71,59,109)"
    ,colorEnd = "rgb(78,88,149)"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ### header
  ,logoBackColor = lighter_c
  
  ,headerButtonBackColor = lighter_c
  ,headerButtonIconColor = "rgb(62,133,179)"
  ,headerButtonBackColorHover = "rgb(49,56,107)"
  ,headerButtonIconColorHover = "rgb(255,255,255)"
  
  ,headerBackColor = lighter_c
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = lighter_c #"rgb(49,56,107)"
    ,colorMiddle = middle_c #"rgb(71,59,109)"
    ,colorEnd = darker_c #"rgb(78,88,149)"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ,sidebarShadowRadius = ""
  ,sidebarPadding = 10
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarMenuBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(48,103,157)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(47, 53, 99)"
    ,colorStartPos = 0
    ,colorMiddlePos = 40
    ,colorEndPos = 100
  )
  ,sidebarMenuPadding = 3
  ,sidebarMenuBorderRadius = 25
  
  ,sidebarUserTextColor = "rgb(128,177,221)"
  
  ,sidebarSearchBackColor = "rgb(40,70,115)"
  ,sidebarSearchIconColor = "rgb(50,115,145)"
  ,sidebarSearchBorderColor = "rgb(30,60,105)"
  
  ,sidebarTabTextColor = "rgb(128,177,221)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 60
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "30px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "30px"
  
  ### boxes
  ,boxBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(70,75,125)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,boxBorderRadius = 15
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(49,56,107)"
  ,boxPrimaryColor = "rgb(141,192,241)"
  ,boxSuccessColor = "rgb(64,186,170)"
  ,boxWarningColor = "rgb(255,217,144)"
  ,boxDangerColor = "rgb(249,144,144)"
  
  ,tabBoxTabColor = "rgb(80,95,155)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(128,177,221)"
  ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
  ,tabBoxBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(70,75,125)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,tabBoxHighlightColor = "rgb(80,95,155)"
  ,tabBoxBorderRadius = 15
  
  ### inputs
  ,buttonBackColor = "rgb(72,190,229)"
  ,buttonTextColor = "rgb(40,63,106)"
  ,buttonBorderColor = "rgb(72,190,229)"
  ,buttonBorderRadius = 20
  
  ,buttonBackColorHover = "rgb(115,210,240)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(115,210,240)"
  
  ,textboxBackColor = "rgb(40,70,115)"
  ,textboxBorderColor = "rgb(30,60,105)"
  ,textboxBorderRadius = 20
  ,textboxBackColorSelect = "rgb(40,70,115)"
  ,textboxBorderColorSelect = "rgb(30,60,105)"
  
  ### tables
  ,tableBackColor = "transparent"
  ,tableBorderColor = "rgb(80,95,155)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


############################################### UI ################################################


ui <- dashboardPage(
  dashboardHeader(
    title = "Chicago Real-Time AQ",
    titleWidth = 250
  ),
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    width = 250,
    sidebarMenu(
      useShinyalert(),
      span(h2("Main Menu", style = "margin-left: 10px; font-size: 20px;")),
      menuItem("Geospatial Visualizations", tabName = "geospatial_viz"),
      # menuItem("Tabular Visualizations", tabName = "tabular_viz"),
      
      menuItem("Options",
               materialSwitch(inputId = "switch_units", label = "Switch to Imperial units", status = "primary"),
               materialSwitch(inputId = "heat_map", label = "Visualize heat map", status = "primary"),
               materialSwitch(inputId = "nodes_table_switch", label = "Hide tabular nodes panel", status = "primary",value=FALSE)
      ),
      menuItem("Heatmap Inputs",
               div(
               selectizeInput("heatmap_measure","Select measure",all_measures,selected="AoT-temperature",multiple=FALSE,options=NULL,width = "200%"),
               selectizeInput("measure_type","Select value type",value_types,selected="average",multiple=FALSE,options=NULL,width = "200%"),
               selectizeInput("map_time_range","Select time range",time_ranges,selected=TIME_RANGE_CURRENT,multiple=FALSE,options=NULL),width = "200%"),style = "font-size: 50%;"),
      menuItem("About", tabName = "about")
      
    ),
    includeCSS("style.css")
  ),
  dashboardBody(
    mirko_theme,
    
    tags$head(
      # Include custom JS
      includeScript("sage2responsive.js")
    ),
    # content of each main tab (selectable from sidebar)
    tabItems(
      tabItem("geospatial_viz",
              div(class="outer",
                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
                  leafletOutput("map", width="100%", height="100%"),
                  
                  # Shiny versions prior to 0.11 should use class = "modal" instead.
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 90, left = "auto", right = 20, bottom = "auto",
                                width = 1300, height = "auto",
                                br(),
                                # h2("Node Data"),
                                selectizeInput(inputId = "time_range", "Select time range", time_ranges, selected = time_ranges[1],width = "100%"),
                                #selectizeInput(inputId = "time_range_ds", "Select time range", time_ranges, selected = time_ranges[2],width = "100%"),
                                tabsetPanel(
                                  tabPanel("Pollutants",
                                           tabsetPanel(
                                             tabPanel("Graphical",
                                                      plotOutput("graphical_data",height = "22vmin"),
                                                      plotOutput("graphical_data_last",height = "22vmin")
                                             ),
                                             tabPanel("Tabular",
                                                      div(DT::dataTableOutput("table_data", height = "44vmin"),style = "font-size:80%")
                                             )
                                           ),
                                           checkboxGroupButtons(
                                             inputId = "measures1",
                                             choices = tab1_measures[1:4],
                                             justified = TRUE, status = "primary", selected = tab1_measures[1:4],
                                             checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                           ),
                                           checkboxGroupButtons(
                                             inputId = "measures2",
                                             choices = tab1_measures[5:8],
                                             justified = TRUE, status = "primary", selected = tab1_measures[5:8],
                                             checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                           )
                                  ),
                                  tabPanel("Measures",
                                           tabsetPanel(
                                             tabPanel("Graphical",
                                                      plotOutput("graphical_data_ds",height = "22vmin"),
                                                      plotOutput("graphical_data_last_ds",height = "22vmin")
                                             ),
                                             tabPanel("Tabular",
                                                      div(DT::dataTableOutput("table_data_ds", height = "44vmin"),style = "font-size:80%")
                                             )
                                           ),
                                           
                                           checkboxGroupButtons(
                                             inputId = "measures1_ds",
                                             choices = tab2_measures[1:5],
                                             justified = TRUE, status = "primary", selected = tab2_measures[1:5],
                                             checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                           ),
                                           checkboxGroupButtons(
                                             inputId = "measures2_ds",
                                             choices = tab2_measures[6:10],
                                             justified = TRUE, status = "primary", selected = tab2_measures[6:10],
                                             checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                           )
                                  )
                                )
                                # ,materialSwitch(inputId = "switch_compare", label = "Compare nodes data", status = "primary")
                                # div( id="yearly_inputs",
                                #      selectizeInput(inputId = "D_month", "Select Month", H_months, selected = 'January',width = "100%"),
                                #      selectizeInput(inputId = "D_day", "Select Day", H_days, selected = '1',width = "100%")
                                # )
                  ),
                  
                  absolutePanel(id = "nodes", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 840, bottom = "auto",
                                width = 1000, height = "auto",
                                br(),
                                box(width=NULL,height=NULL,
                                    
                                    div(DT::dataTableOutput("nodes_table", height = "22vmin"),style = "font-size:80%")
                                )
                                ,
                                checkboxGroupButtons(
                                  inputId = "measures1_sites",
                                  choices = tracked_measures[1:5],
                                  justified = TRUE, status = "primary", selected = tracked_measures[1:5],
                                  checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                ),
                                checkboxGroupButtons(
                                  inputId = "measures2_sites",
                                  choices = tracked_measures[6:10],
                                  justified = TRUE, status = "primary", selected = tracked_measures[6:10],
                                  checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                )
                                
                                
                  ),
                  
                  # absolutePanel(id = "counties_panel", class = "panel panel-default", fixed = TRUE,
                  #               draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = -40,
                  #               width = 330, height = "auto",
                  #               h2("Shown counties"),
                  #               knobInput(
                  #                 inputId = "num_counties",
                  #                 label = "Select number of counties",
                  #                 value = 1000,
                  #                 min = 0,
                  #                 max = 1100,
                  #                 displayPrevious = TRUE,
                  #                 lineCap = "round",
                  #                 fgColor = "#428BCA",
                  #                 inputColor = "#428BCA"
                  #               ),
                  #               sliderInput(inputId = "Opacity",
                  #                           sep = "",
                  #                           label = "Confidence level control",
                  #                           step = 0.1,
                  #                           value = 0, min = 0, max = 1
                  #                           # ,width = "90%"
                  #               )
                  # ),
                  
                  tags$div(id="cite",
                           'Visual Analytics, University of Illinois at Chicago 2019'
                  )
              )),
      tabItem("tabular_viz",
              h1("WIP")),
      tabItem("about",
              htmlOutput("about_out")
      )
    )
  )
)


############################################# SERVER ##############################################

server <- function(input, output, session) {
  
  ########################## AoT R APIs modified ######################
  
  #' Timestamped message -- primarily used to push error output to user
  #'
  #' @param msg - The message to be logged
  #' @return None (invisible NULL) as per cat
  #' @noRd
  log_msg <- function (msg) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3 "), ": ", msg, "\n", sep="")
  }
  
  #' Sends a request to the API, ensures 200 response and returns the response
  #'
  #' Given a URL and optional filters/query params, this sends an HTTP GET request
  #' to the URL. The response"s status is checked -- if it isn"t 200 then an
  #' error message is logged and the process halts; it it"s 200 then the entire
  #' response object is returned.
  #'
  #' @param url - The URL to send the request to
  #' @param filters - A list of tuples to build filters/query params
  #' @return The entire response
  #' @importFrom httr GET
  #' @noRd
  send_request <- function (url, filters = NULL) {
    # send request; get response
    if (!is.null(filters)) {
      resp <- httr::GET(url, query=filters)
    } else {
      resp <- httr::GET(url)
    }
    
    # if not 200, log error
    if (resp$status_code != 200) {
      msg <- paste("Error in httr GET:", resp$status_code, resp$headers$statusmessage, url)
      if(!is.null(resp$headers$`content-length`) && (resp$headers$`content-length` > 0)) {
        details <- httr::content(resp)
        msg <- paste(msg, details)
      }
      log_msg(msg)
    }
    
    # stop or return
    httr::stop_for_status(resp)
    return(resp)
  }
  
  
  #' Parses a response object as JSON and returns the `data` object
  #'
  #' @param resp - The response object
  #' @return The parsed JSON body
  #' @importFrom jsonlite fromJSON
  #' @noRd
  parse_content <- function (resp) {
    content <- httr::content(resp, as="text")
    json <- jsonlite::fromJSON(content)
    data <- json$data
    return(data)
  }
  
  
  #' Sends a request and parses the result as a single map object
  #'
  #' Given a URL and optional filters, a request is sent and the response
  #' is processed as a single map object -- the response content has a
  #' `data` key that maps an object representing details for the metadata
  #' record requested.
  #'
  #' @param url - The URL to send the request to
  #' @param filters - A list of tuples to build query params
  #' @return The metadata details
  #' @noRd
  stat <- function (url, filters) {
    resp <- send_request(url, filters)
    details <- parse_content(resp)
    return(details)
  }
  
  #' Gets a data frame of `node` metadata
  #'
  #' Nodes are the physical devices deployed to collect observations.
  #' The are comprised of multiple sensors and are grouped by
  #' projects.
  #'
  #' @param filters - A list of tuples to create filters/query params
  #' @return A data frame of node metadata
  #' @export
  ls.nodes <- function (filters = NULL) {
    # build url, send request, get response
    url <- "https://api.arrayofthings.org/api/nodes"
    resp <- send_request(url, filters)
    
    # build data frame
    data <- parse_content(resp)
    df <- as.data.frame.list(data)
    attr(df, "vsn") <- data$vsn
    attr(df, "location") <- data$location.geometry #location.geometry.coordinates
    attr(df, "address") <- data$human_address
    attr(df, "description") <- data$description
    
    # return data frame
    return(df)
  }
  
  #' Gets a data frame of `obserations` data.
  #'
  #' Observation data are the environmental measurements made
  #' by the sensors. Data listed here is more or less tuned
  #' and trustworthy.
  #'
  #' @param filters - A list of tuples to create filters/query params
  #' @return A data frame of observation data
  #' @export
  ls.observations <- function (filters = NULL) {
    # build url, send request, get response
    url <- "https://api.arrayofthings.org/api/observations"
    resp <- send_request(url, filters)
    
    # build data frame
    data <- parse_content(resp)
    df <- as.data.frame.list(data)
    attr(df, "node_vsn") <- data$node_vsn
    attr(df, "sensor_path") <- data$sensor_path
    attr(df, "timestamp") <- data$timestamp # TODO modified because creates problems when no observations if as.POS.. as.POSIXlt(data$timestamp)
    attr(df, "value") <- data$value
    attr(df, "uom") <- data$uom
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  #####################################################  AoT utils    #####################################################
  
  
  get_and_preprocess_nodes <- function(){
    df <- ls.nodes()
    # filter out nodes not yet deployed
    df <- subset(df, address != "TBD")
    df$location.type <- NULL
    df$location.geometry$type <- NULL
    df$coordinates <- df$location.geometry$coordinates
    df$location.geometry$coordinates <-NULL
    df$location.geometry <- NULL
    df$description <- NULL
    temp <- do.call(rbind, df$coordinates)
    colnames(temp) <- c("longitude","latitude")
    df <- cbind(df[c("vsn", "address")], temp)
  }
  
  
  #gets the observations relative to h hours ago
  get_h_hours_observations <- function(h, vsn){
    # d <- get_last_available_date()
    timestamp <- ls.observations(filters=list(node=vsn,size=1))$timestamp
    
    t1 <- sub_hour_to_timestamp(timestamp,h-1)
    t2 <- sub_hour_to_timestamp(timestamp,h)
    df <- ls.observations(filters=list(
      node=vsn,
      timestamp=paste("ge:",t2,sep=""),
      timestamp=paste("lt:",t1,sep=""),
      size=200
      # timestamp="ge:2018-08-01T00:00:00",
      # timestamp="lt:2018-09-01T00:00:00"
    ))
    
    df <- data.frame(df)
    df$location.type <- NULL
    df$location.geometry <- NULL
    
    return(df)
  }
  
  #gets the observations relative to h hours ago for all nodes
  
  get_h_hours_observations_all_nodes <- function(h){
    # d <- get_last_available_date()
    timestamp <- ls.observations(filters=list(size=1))$timestamp
    
    t1 <- sub_hour_to_timestamp(timestamp,h-1)
    t2 <- sub_hour_to_timestamp(timestamp,h)
    df <- ls.observations(filters=list(
      timestamp=paste("ge:",t2,sep=""),
      timestamp=paste("lt:",t1,sep=""),
      size=200
      # timestamp="ge:2018-08-01T00:00:00",
      # timestamp="lt:2018-09-01T00:00:00"
    ))
    
    df <- data.frame(df)
    df$location.type <- NULL
    df$location.geometry <- NULL
    
    return(df)
  }
  
  
  #gets the observations relative to h hours ago
  get_d_days_observations <- function(d, vsn){
    # d <- get_last_available_date()
    timestamp <- ls.observations(filters=list(node=vsn,size=1))$timestamp
    
    t1 <- sub_day_to_timestamp(timestamp,d-1)
    t2 <- sub_day_to_timestamp(timestamp,d)
    df <- ls.observations(filters=list(
      node=vsn,
      timestamp=paste("ge:",t2,sep=""),
      timestamp=paste("lt:",t1,sep=""),
      size=200
      # timestamp="ge:2018-08-01T00:00:00",
      # timestamp="lt:2018-09-01T00:00:00"
    ))
    
    df <- data.frame(df)
    df$location.type <- NULL
    df$location.geometry <- NULL
    
    return(df)
  }
  
  #gets all the observation in the past 24 h, limited to 50k (max api limit should be 100k)
  get_last_24h_data <- function(vsn){
    # d <- get_last_available_date()
    timestamp <- ls.observations(filters=list(node=vsn,size=1))$timestamp
    
    t1 <- sub_day_to_timestamp(timestamp,1)
    t2 <- sub_day_to_timestamp(timestamp,0)
    df <- ls.observations(filters=list(
      node=vsn,
      timestamp=paste("ge:",t1,sep=""),
      timestamp=paste("lt:",t2,sep=""),
      size=50000
      # timestamp="ge:2018-08-01T00:00:00",
      # timestamp="lt:2018-09-01T00:00:00"
    ))
    
    df <- data.frame(df)
    df$location.type <- NULL
    df$location.geometry <- NULL
    
    return(df)
  }
  
  # function to get data for all the nodes for a day
  get_d_days_observations_all_nodes <- function(d){
    
    # d <- get_last_available_date()
    timestamp <- ls.observations(filters=list(size=1))$timestamp
    # print(timestamp)
    t1 <- ymd_hms(timestamp)-lubridate::days(d-1)
    t1 <- force_tz(t1, "America/Chicago")
    t2 <- ymd_hms(timestamp)-lubridate::days(d)
    t2 <- force_tz(t2, "America/Chicago")
    # print(t1)
    # print(t2)
    df <- ls.observations(filters=list(
      timestamp=paste("ge:",t2,sep=""),
      timestamp=paste("lt:",t1,sep=""),
      size=200
    ))
    
    df <- data.frame(df)
    df$location.type <- NULL
    df$location.geometry <- NULL
    
    return(df)
  }
  
  # function to get data for all the nodes for seven days
  
  get_and_preprocess_observations_7d_all_nodes <- function(){
    days <- c(1:7)
    dfs <- lapply(days, get_d_days_observations_all_nodes)
    df1 <- do.call(rbind, dfs)
    df <- data.frame(df1$node_vsn)
    names(df) <- c("vsn")
    df$measure <- df1$sensor_path
    df$time <- df1$timestamp
    df$value <- df1$value
    df$measure <-lapply(df$measure,extract_sensor)
    df$uom <- df1$uom
    
    df$measure <- unlist(df$measure)
    
    df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
    df <- extract_date_fields_d(df)
    
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "value")
    
    
    
    return(df)
  }
  
  get_and_preprocess_observations_7d <- function(vsn){
    days <- c(1:7)
    dfs <- lapply(days, get_d_days_observations, vsn)
    df1 <- do.call(rbind, dfs)
    
    df <- data.frame(df1$node_vsn)
    names(df) <- c("vsn")
    df$measure <- df1$sensor_path
    df$time <- df1$timestamp
    df$value <- df1$value
    df$measure <-lapply(df$measure,extract_sensor)
    df$uom <- df1$uom
    df <- filter_out_untracked_measures(df)
    
    df$measure <- unlist(df$measure)
    
    df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
    df <- extract_date_fields_d(df)
    
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "value")
    
    return(df)
  }
  
  get_and_preprocess_observations_24h <- function(vsn){
    # Every 5210 observations it's 1 hour
    
    # OLD METHOD, 24 requests
    hours <- c(1:24)
    dfs <- lapply(hours, get_h_hours_observations, vsn)
    df1 <- do.call(rbind, dfs)
    df <- data.frame(df1$node_vsn)
    
    # All the observations in 1 request strategy
    # df1 <- get_last_24h_data(vsn)
    # df <- data.frame(df1$node_vsn)
    
    names(df) <- c("vsn")
    df$measure <- df1$sensor_path
    df$time <- df1$timestamp
    df$value <- df1$value
    df$measure <-lapply(df$measure,extract_sensor)
    df$uom <- df1$uom
    df <- filter_out_untracked_measures(df)
    df$measure <- unlist(df$measure)
    df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
    df <- extract_date_fields_h(df)
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$h, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","h","year","month","day", "hms", "value")
    
    return(df)
  }
  
  # get data for all nodes for last 24 horus
  get_and_preprocess_observations_24h_all_nodes <- function(){
    # Every 5210 observations it's 1 hour
    
    # OLD METHOD, 24 requests
    hours <- c(1:24)
    dfs <- lapply(hours, get_h_hours_observations_all_nodes)
    df1 <- do.call(rbind, dfs)
    df <- data.frame(df1$node_vsn)
    
    # All the observations in 1 request strategy
    # df1 <- get_last_24h_data(vsn)
    # df <- data.frame(df1$node_vsn)
    
    names(df) <- c("vsn")
    df$measure <- df1$sensor_path
    df$time <- df1$timestamp
    df$value <- df1$value
    df$measure <-lapply(df$measure,extract_sensor)
    df$uom <- df1$uom
    df <- filter_out_untracked_measures(df)
    df$measure <- unlist(df$measure)
    df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
    df <- extract_date_fields_h(df)
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$h, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","h","year","month","day", "hms", "value")
    
    return(df)
  }
  
  get_and_preprocess_observations <- function(vsn){
    df1 <- ls.observations(filters=list(node=vsn))
    # filter out nodes not yet deployed
    df <- data.frame(df1$node_vsn)
    if(nrow(df)>0){
      names(df) <- c("vsn")
      df$measure <- df1$sensor_path
      df$time <- df1$timestamp
      df$value <- df1$value
      df$measure <-lapply(df$measure,extract_sensor)
      df$uom <- df1$uom
      df <- filter_out_untracked_measures(df)
      
      df$measure <- unlist(df$measure)
      df <-aggregate(df$value, by=list(df$vsn,df$measure,df$time,df$uom),
                     FUN=mean)
      names(df) <- c("vsn","measure","time","uom","value")
      df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
      df <- extract_date_fields(df)
    }
    
    return(df)
  }
  
  # get current data for all the AOT nodes
  
  get_and_preprocess_observations_all_nodes <- function(){
    df1 <- ls.observations(filters=list())
    # filter out nodes not yet deployed
    df <- data.frame(df1$node_vsn)
    if(nrow(df)>0){
      names(df) <- c("vsn")
      df$measure <- df1$sensor_path
      df$time <- df1$timestamp
      df$value <- df1$value
      df$measure <-lapply(df$measure,extract_sensor)
      df$uom <- df1$uom
      df <- filter_out_untracked_measures(df)
      
      df$measure <- unlist(df$measure)
      df <-aggregate(df$value, by=list(df$vsn,df$measure,df$time,df$uom),
                     FUN=mean)
      names(df) <- c("vsn","measure","time","uom","value")
      df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
      df <- extract_date_fields(df)
    }
    
    return(df)
  }
  
  
  # get all nodes data for open AQ last 7 days
  
  ls.observations_openaq_7d_all_nodes <- function (time) {
    # build url, send request, get response
    
    data <- aq_measurements(city = "Chicago-Naperville-Joliet",date_from = as.character(Sys.Date()-7), date_to = as.character(Sys.Date()))
    df <- as.data.frame.list(data)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  # get all nodes data for open AQ last 7 days
  
  get_and_preprocess_observations_7d_all_nodes_openaq <- function(){
    
    df1 <- ls.observations_openaq_7d_all_nodes("curr")
    
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$dateLocal
    df$value <- df1$value
    
    df$uom <- df1$unit
    df$longitude <- df1$longitude
    df$latitude <- df1$latitude
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste(df$month, "/", df$day,"/",substr(df$year,3,4), sep="")
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms,df$longitude,df$latitude),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "longitude","latitude","value")
    return(df)
    
  }
  
  # get data for all nodes for open aq last 24 hours
  
  ls.observations_openaq_24h_all_nodes <- function (time) {
    # build url, send request, get response
    # build data frame
    data <- aq_measurements(city = "Chicago-Naperville-Joliet",date_from = as.character(Sys.Date()), date_to = as.character(Sys.Date()+1))
    df <- as.data.frame.list(data)
    attr(df, "timestamp") <- data$dateLocal # TODO modified because creates problems when no observations if as.POS.. as.POSIXlt(data$timestamp)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  get_and_preprocess_observations_24h_openaq_all_nodes <- function(){
    df1 <- ls.observations_openaq_24h_all_nodes("curr")
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$dateLocal
    df$value <- df1$value
    df$longitude <- df1$longitude
    df$latitude <- df1$latitude
    
    df$uom <- df1$unit
    
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$h <- format(df$time, format = "%H")
    df$hms <- paste("d:",df$day, "h:", df$h)
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms,df$longitude,df$latitude),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms","longitude","latitude","value")
    return(df)
  }
  
  #get all nodes data for openAQ for current time
  
  ls.observations_openaq_all_nodes <- function (time) {
    # build url, send request, get response
    # build data frame
    #TODO A: Retrieving latest stats here as some locations do not have updates in the past hour
    #data <- aq_measurements(city = "Chicago-Naperville-Joliet",location=vsn,date_from = as.character(Sys.Date()-7), date_to = as.character(Sys.Date()))
    data <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet")
    df <- as.data.frame.list(data)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  get_and_preprocess_observations_openaq_all_nodes <- function(){
    df1 <- ls.observations_openaq_all_nodes("curr")
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$lastUpdated
    df$value <- df1$value
    df$longitude <- df1$longitude
    df$latitude <- df1$latitude
    
    df$uom <- df1$unit
    
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste(df$month, "/", df$day,"/",substr(df$year,3,4), sep="")
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms,df$longitude,df$latitude),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms","longitude","latitude","value")
    #print(df)
    return(df)
  }
  
  sub_hour_to_timestamp <- function(timestamp, h){
    pb.txt <- strptime(timestamp,"%Y-%m-%dT%H:%M:%S", tz="GMT")
    pb.date <- as.POSIXct(pb.txt, tz="Europe/London")
    t <- pb.date - 60*60*h+180
    return(paste(strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1],
                 "T",
                 strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2],sep=""))
    
  }
  
  sub_day_to_timestamp <- function(timestamp, d){
    pb.txt <- strptime(timestamp,"%Y-%m-%dT%H:%M:%S", tz="GMT")
    pb.date <- as.POSIXct(pb.txt, tz="Europe/London")
    t <- pb.date - 60*60*24*d+180
    return(paste(strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1],
                 "T",
                 strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2],sep=""))
    
  }
  
  convert_timestamp_to_chicago_timezone <- function(timestamp){
    pb.txt <- strptime(timestamp,"%Y-%m-%dT%H:%M:%S", tz="GMT")
    pb.date <- as.POSIXct(pb.txt, tz="Europe/London")
    return(format(pb.date, tz="America/Chicago",usetz=TRUE))
  }
  
  # Before calling this convert_timestamp_to_chicago_timezone should be applied to the time column
  extract_date_fields <- function(df){
    df$hms <- lapply(df$time, function(t) strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2])
    df$time <- lapply(df$time, function(t) strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1])
    df$time <- as.Date(unlist(df$time))
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%B")
    df$day <- format(df$time, format = "%d")
    df$time <- NULL
    return(df)
  }
  
  extract_date_fields_h <- function(df){
    df$hsm <- lapply(df$time, function(t) strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2])
    df$h <- lapply(df$hsm, function(t) strsplit(as.character(t),":", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1])
    df$hsm <- NULL
    df$time <- lapply(df$time, function(t) strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1])
    df$time <- as.Date(unlist(df$time))
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%B")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste("d:",df$day, "h:", df$h)
    df$h <- as.numeric(unlist(df$h))
    df$time <- NULL
    return(df)
  }
  
  extract_date_fields_d <- function(df){
    df$time <- lapply(df$time, function(t) strsplit(as.character(t)," ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1])
    df$time <- as.Date(unlist(df$time))
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste(df$month, "/", df$day,"/",substr(df$year,3,4), sep="")
    df$time <- NULL
    return(df)
  }
  
  get_last_available_date <- function(){
    timestamp <- ls.observations(filters=list(size=1))$timestamp
    
    year <- strsplit(as.character(timestamp),"-", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
    month <- strsplit(as.character(timestamp),"-", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
    rest <- strsplit(as.character(timestamp),"-", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
    day <- strsplit(as.character(rest),"T", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
    rest <- strsplit(as.character(rest),"T", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
    hour <- strsplit(as.character(rest),":", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
    
    date <- list("year" = year, "month" = month, "day" = day, "hour" = hour)
    
    return(date)
  }
  
  
  autoInvalidate45 <- reactiveTimer(45000, session)
  autoInvalidate50 <- reactiveTimer(50000, session)
  
  
  #get the measures required for the plots
  filter_out_untracked_measures <- function(df){
    subset(df, measure %in% tracked_measures)
  }
  
  #####################################################  OpenAQ utils    #####################################################
  ls.observations_openaq <- function (vsn, time) {
    print(vsn)
    if(vsn=="Kingery")
    {
      vsn <- "Kingery Near-road+#1"
    }
    if(vsn=="Portage-Hwy")
    {
      vsn <- "Portage-Hwy+12"
    }
    if(vsn=="Hammond-141st")
    {
      vsn <- "Hammond-141st+St"
    }
    if(vsn=="East")
    {
      vsn <-"East+Chicago+Post+Of"
    }
    # build url, send request, get response
    # build data frame
    #TODO A: Retrieving latest stats here as some locations do not have updates in the past hour
    #data <- aq_measurements(city = "Chicago-Naperville-Joliet",location=vsn,date_from = as.character(Sys.Date()-7), date_to = as.character(Sys.Date()))
    data <- aq_latest(country = "US", city = "Chicago-Naperville-Joliet", location=vsn)
    df <- as.data.frame.list(data)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  ls.observations_openaq_7d <- function (vsn, time) {
    # build url, send request, get response
    
    if(vsn=="Kingery")
    {
      vsn <- "Kingery Near-road+#1"
    }
    if(vsn=="Portage-Hwy")
    {
      vsn <- "Portage-Hwy+12"
    }
    if(vsn=="Hammond-141st")
    {
      vsn <- "Hammond-141st+St"
    }
    if(vsn=="East")
    {
      vsn <-"East+Chicago+Post+Of"
    }
    data <- aq_measurements(city = "Chicago-Naperville-Joliet",location=vsn,date_from = as.character(Sys.Date()-7), date_to = as.character(Sys.Date()))
    df <- as.data.frame.list(data)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  ls.observations_openaq_24h <- function (vsn, time) {
    # build url, send request, get response
    if(vsn=="Kingery")
    {
      vsn <- "Kingery Near-road+#1"
    }
    if(vsn=="Portage-Hwy")
    {
      vsn <- "Portage-Hwy+12"
    }
    if(vsn=="Hammond-141st")
    {
      vsn <- "Hammond-141st+St"
    }
    if(vsn=="East")
    {
      vsn <-"East+Chicago+Post+Of"
    }
    # build data frame
    data <- aq_measurements(city = "Chicago-Naperville-Joliet",location=vsn,date_from = as.character(Sys.Date()), date_to = as.character(Sys.Date()+1))
    df <- as.data.frame.list(data)
    attr(df, "timestamp") <- data$dateLocal # TODO modified because creates problems when no observations if as.POS.. as.POSIXlt(data$timestamp)
    attr(df, "value") <- data$value
    attr(df, "location") <- data$location
    
    # return data frame
    return(df)
  }
  
  
  #PREPROCESS OPENAQ 7 DAYS
  get_and_preprocess_observations_7d_openaq <- function(vsn){
    df1 <- ls.observations_openaq_7d(vsn,"curr")
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$dateLocal
    df$value <- df1$value
    
    df$uom <- df1$unit
    
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste(df$month, "/", df$day,"/",substr(df$year,3,4), sep="")
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "value")
    return(df)
  }
  
  #PREPROCESS OPENAQ 24hours
  get_and_preprocess_observations_24h_openaq <- function(vsn){
    df1 <- ls.observations_openaq_24h(vsn,"curr")
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$dateLocal
    df$value <- df1$value
    
    df$uom <- df1$unit
    
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$h <- format(df$time, format = "%H")
    df$hms <- paste("d:",df$day, "h:", df$h)
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "value")
    return(df)
  }
  
  #PREPROCESS OPENAQ current
  get_and_preprocess_observations_openaq <- function(vsn){
    df1 <- ls.observations_openaq(vsn,"curr")
    df <- data.frame(df1$location)
    names(df) <- c("vsn")
    df$measure <- df1$parameter
    levels(df$measure)[levels(df$measure)=="pm25"] <- "pm2.5"
    df$time <- df1$lastUpdated
    df$value <- df1$value
    
    df$uom <- df1$unit
    
    df$year <- format(df$time, format = "%Y")
    df$month <- format(df$time, format = "%m")
    df$day <- format(df$time, format = "%d")
    df$hms <- paste(df$month, "/", df$day,"/",substr(df$year,3,4), sep="")
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$uom, df$year, df$month, df$day, df$hms),
                   FUN=mean)
    names(df) <- c("vsn","measure","uom","year","month","day","hms", "value")
    #print(df)
    return(df)
  }
  
  ############################################### Extract sensors info ################################################
  
  
  save_df_as_fst <- function(df,path){
    write.fst(df, path)
  }
  
  #function to extract the sensor information using the sensor path in observation data
  extract_sensor <- function(elem){
    elem <- as.character(elem)
    l <- strsplit(elem, ".", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
    if(l[3] == "concentration"){
      return(l[2])
    } else {
      if(l[3] == "pm10" || l[3] == "pm10_atm"){
        return("pm10")
      } else if(l[3] == "pm2_5" || l[3] == "pm25_atm"){
        return("pm2.5")
      }
      return(tail(l, n=1))
    }
  }
  # IMPORTANT CODE
  
  update_nodes_status <- function(){
    nodes <- get_and_preprocess_nodes()
    
    for(m in tracked_measures){
      nodes[m] <- FALSE
    }
    
    for(n in unique(nodes$vsn)){
      df <- ls.observations(filters=list(node=n))
      df$sensor_path <-lapply(df$sensor_path,extract_sensor)
      u <- unique(df$sensor_path)
      v <- unlist(u)
      measures <- intersect(v,tracked_measures)
      for(m in measures){
        nodes[which(nodes$vsn == n), m] = TRUE
      }
    }
    #save to file
    save_df_as_fst(nodes,"fst/nodes.fst")
  }
  
  ########### reading data ###########
  
  if(UPDATE_NODES_STATUS){
    update_nodes_status()
  }
  
  nodes <- read_fst("fst/nodes.fst")
  nodes_oaq <- read_fst("fst/openaq.fst")
  
  # preprocessing nodes_oaq
  nodes_oaq$vsn <- nodes_oaq$location
  nodes_oaq$location <- NULL
  
  congestion_df <- read.socrata(
    "https://data.cityofchicago.org/resource/n4j6-wkkf.json",
    app_token = "wrXJQ8XKYqlE8TxQoHCSSYvwV",
    email     = "mirkomantovani23@gmail.com",
    password  = "iBdN3u5BPbhmiMW"
  )
  
  
  # customizing values for responsitivity in normal display and SAGE display
  v <- reactiveValues(axis_title_size = 14,
                      axis_text_size = 12,
                      margin_y = 30,
                      margin_x = 0,
                      legend_text_size = 5,
                      legend_title_size = 5,
                      legend_key_size = 1,
                      pie_text_size = 5,
                      slant_text_angle = 45,
                      point_size = 15,
                      zoom_level = 5,
                      tooltip_width = 100,
                      tooltip_hieght = 60,
                      tooltip_text_size = 14,
                      line_size = 1,
                      tbl_pagelength = 20,
                      annotate_text_size = 4,
                      marker_text_size = '12px',
                      select_input_width = '100%',
                      lastvsn = NULL,
                      lastvsn_dark = NULL,
                      map_inputs = list(),
                      table_inputs = list(),
                      vsn=NULL
  )
  
  
  
  observeEvent(input$dimension, {
    if(input$dimension[1] >= 2000){
      v$axis_title_size <<- 20
      v$axis_text_size <<- 20
      v$margin_y <<- 40
      v$margin_x <<- 122
      v$legend_title_size <<- 20
      v$legend_text_size <<- 15
      v$legend_key_size <<- 3
      v$pie_text_size <<- 15
      v$slant_text_angle <<- 0
      v$point_size <<- 30
      v$zoom_level <<- 12
      v$tooltip_width <<- 180
      v$tooltip_height <<- 80
      v$tooltip_text_size <<- 28
      v$line_size <<- 2
      v$tbl_pagelength <<- 20
      v$annotate_text_size <<- 8
      v$marker_text_size <<- '60px'
      v$select_input_width <<- '200%'
      v$width_daily = "100%"
      v$height_daily = 1200
      v$daily_axis_stroke = 4
      v$daily_axis_labels = 30
      v$daily_axis_title = 50
      v$daily_legend_font = 30
      v$daily_legend_size = 100
      v$daily_point_size <- 700 #not used
    } else {
      v$axis_title_size = 14
      v$axis_text_size = 12
      v$margin_y = 45
      v$margin_x = 0
      v$legend_text_size = 10
      v$legend_title_size = 10
      v$legend_key_size = 2
      v$pie_text_size = 5
      v$slant_text_angle = 45
      v$point_size = 15
      v$zoom_level = 11
      v$tooltip_width = 100
      v$tooltip_hieght = 60
      v$tooltip_text_size = 14
      v$line_size = 1
      v$tbl_pagelength = 20
      v$annotate_text_size = 4
      v$marker_text_size = '12px'
      v$select_input_width = '100%'
      v$width_daily = "100%"
      v$height_daily = 700
      v$daily_axis_stroke = 3
      v$daily_axis_labels = 10
      v$daily_axis_title = 20
      v$daily_legend_font = 10
      v$daily_point_size <- 5 #not used
      v$daily_legend_size = 30
    }
  })
  
  axis_title_size <- reactive({v$axis_title_size})
  axis_text_size <- reactive({v$axis_text_size})
  margin_x <- reactive({v$margin_x})
  margin_y <- reactive({v$margin_y})
  legend_text_size <- reactive({v$legend_text_size})
  legend_key_size <- reactive({v$legend_key_size})
  legend_title_size <- reactive({v$legend_title_size})
  pie_text_size <- reactive({v$pie_text_size})
  slant_text_angle <- reactive({v$slant_text_angle})
  point_size <- reactive({v$point_size})
  zoom_level <- reactive({v$zoom_level})
  tooltip_width <- reactive({v$tooltip_width})
  tooltip_height <- reactive({v$tooltip_height})
  tooltip_text_size <- reactive({v$tooltip_text_size})
  line_size <- reactive({v$line_size})
  tbl_pagelength <- reactive({v$tbl_pagelength})
  annotate_text_size <- reactive({v$annotate_text_size})
  
  marker_text_size <- reactive({v$marker_text_size})
  select_input_width <- reactive({v$select_input_width})
  
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[1]/input$dimension[2])
  })
  
  ############################################### Other utils ################################################
  
  translate_to_column_name <- function(pollutant) {
    if(pollutant == "CO"){
      return("Days.CO")
    } else if (pollutant == "NO2"){
      return("Days.NO2")
    } else if (pollutant == "Ozone"){
      return("Days.Ozone")
    } else if (pollutant == "SO2"){
      return("Days.SO2")
    } else if (pollutant == "PM2.5"){
      return("Days.PM2.5")
    } else if (pollutant == "PM10"){
      return("Days.PM10")
    } else if (pollutant == "AQI"){
      return("Median.AQI")
    }
    
    return("Days.CO")
  }
  
  convert_to_imperial <- function(values){
    return(values*1000000000000* 0.000000035274/35315)
  }
  
  # Mirko
  # !Important
  # The round county numbers updates continously invalidating the input for as many times as the there are
  # in the difference between the starting point and the final point, recalculating so many times and
  # make the app unusable. I will try to use a reactive value to compute the county number input based on
  # the round number picker and use debounce (or throttle) to delay the execution for a number of ms needed
  # for the user to choose the right number
  
  delayes_num_counties <- reactive({
    input$num_counties
  })
  
  delayes_num_counties_debounced <- delayes_num_counties %>% debounce(300)
  
  nodes_table <- read_fst("fst/nodes.fst")
  nodes_table$status <- "Active"
  nodes_table[nodes_table[[tracked_measures[1]]] == FALSE &
                nodes_table[[tracked_measures[2]]] == FALSE &
                nodes_table[[tracked_measures[3]]] == FALSE &
                nodes_table[[tracked_measures[4]]] == FALSE &
                nodes_table[[tracked_measures[5]]] == FALSE &
                nodes_table[[tracked_measures[6]]] == FALSE &
                nodes_table[[tracked_measures[7]]] == FALSE &
                nodes_table[[tracked_measures[8]]] == FALSE &
                nodes_table[[tracked_measures[9]]] == FALSE &
                nodes_table[[tracked_measures[10]]] == FALSE, ][, "status"] <- "Inactive"
  
  nodes_table[nodes_table[[tracked_measures[1]]] == FALSE, ][, tracked_measures[1]] <- "-"
  nodes_table[nodes_table[[tracked_measures[2]]] == FALSE, ][, tracked_measures[2]] <- "-"
  nodes_table[nodes_table[[tracked_measures[3]]] == FALSE, ][, tracked_measures[3]] <- "-"
  nodes_table[nodes_table[[tracked_measures[4]]] == FALSE, ][, tracked_measures[4]] <- "-"
  nodes_table[nodes_table[[tracked_measures[5]]] == FALSE, ][, tracked_measures[5]] <- "-"
  nodes_table[nodes_table[[tracked_measures[6]]] == FALSE, ][, tracked_measures[6]] <- "-"
  nodes_table[nodes_table[[tracked_measures[7]]] == FALSE, ][, tracked_measures[7]] <- "-"
  nodes_table[nodes_table[[tracked_measures[8]]] == FALSE, ][, tracked_measures[8]] <- "-"
  nodes_table[nodes_table[[tracked_measures[9]]] == FALSE, ][, tracked_measures[9]] <- "-"
  nodes_table[nodes_table[[tracked_measures[10]]] == FALSE, ][, tracked_measures[10]] <- "-"
  
  
  ################################# MAP #################################
  output$map <- renderLeaflet({
    
    initial_lat <- 41.900613
    initial_lng <- -87.678211
    
    nodes_by_sensor <- lapply(tracked_measures, function(measure) subset(nodes, nodes[[measure]] == TRUE))
    nodes_by_sensor_oaq <- lapply(openaq_tracked_measures, function(measure) subset(nodes_oaq, nodes_oaq[[measure]] == TRUE))
    
    nodes_with_no_data <- subset(nodes, nodes[[tracked_measures[1]]] == FALSE &
                                   nodes[[tracked_measures[2]]] == FALSE &
                                   nodes[[tracked_measures[3]]] == FALSE &
                                   nodes[[tracked_measures[4]]] == FALSE &
                                   nodes[[tracked_measures[5]]] == FALSE &
                                   nodes[[tracked_measures[6]]] == FALSE &
                                   nodes[[tracked_measures[7]]] == FALSE &
                                   nodes[[tracked_measures[8]]] == FALSE &
                                   nodes[[tracked_measures[9]]] == FALSE &
                                   nodes[[tracked_measures[10]]] == FALSE )
    
    html_legend <- "
    <b>Nodes</b><br>
    <div class='circle' id='aotactive'></div><a href='https://arrayofthings.github.io/' target='_blank'>AoT</a> active
    <div class='circle' id='aotinactive'></div><a href='https://arrayofthings.github.io/' target='_blank'>AoT</a> inactive
    <div class='circle' id='oaq'></div><a href='https://openaq.org/' target='_blank'>OpenAQ</a>
    "
    
    # Function to compute traffic color based on speed
    trafficColor <- function(speed){
      if(speed == -1){
        return("#cccccc")
      } else if(speed > -1 && speed < 10){
        return("#e03e3e");
      } else if(speed > 9 && speed < 20){
        return("#e0843e");
      } else if(speed > 19 && speed < 30){
        return("#e0d53e");
      } else {
        return("#3e50e0");
      }
    }
    
    # highlightOptions = highlightOptions(weight = 9, bringToFront = F, opacity = 1)
    
    
    
    opacity <- 0.1
    oaqOpacity <- 0.3
    inactiveColor <- "red"
    inactiveOpacity <- 0.5
    normalColor <- "navy"
    openAQColor <- "green"
    
    map = leaflet(nodes) %>%
      
      # AoT nodes
      addCircleMarkers(nodes_by_sensor[[1]]$longitude, nodes_by_sensor[[1]]$latitude, group = tracked_measures[1], layerId=paste(nodes_by_sensor[[1]]$vsn,tracked_measures[1]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[1]]$vsn,"</b>"),nodes_by_sensor[[1]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>% #, layerId=~vsn
      addCircleMarkers(nodes_by_sensor[[2]]$longitude, nodes_by_sensor[[2]]$latitude, group = tracked_measures[2], layerId=paste(nodes_by_sensor[[2]]$vsn,tracked_measures[2]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[2]]$vsn,"</b>"),nodes_by_sensor[[2]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[3]]$longitude, nodes_by_sensor[[3]]$latitude, group = tracked_measures[3], layerId=paste(nodes_by_sensor[[3]]$vsn,tracked_measures[3]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[3]]$vsn,"</b>"),nodes_by_sensor[[3]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[4]]$longitude, nodes_by_sensor[[4]]$latitude, group = tracked_measures[4], layerId=paste(nodes_by_sensor[[4]]$vsn,tracked_measures[4]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[4]]$vsn,"</b>"),nodes_by_sensor[[4]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[5]]$longitude, nodes_by_sensor[[5]]$latitude, group = tracked_measures[5], layerId=paste(nodes_by_sensor[[5]]$vsn,tracked_measures[5]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[5]]$vsn,"</b>"),nodes_by_sensor[[5]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[6]]$longitude, nodes_by_sensor[[6]]$latitude, group = tracked_measures[6], layerId=paste(nodes_by_sensor[[6]]$vsn,tracked_measures[6]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[6]]$vsn,"</b>"),nodes_by_sensor[[6]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[7]]$longitude, nodes_by_sensor[[7]]$latitude, group = tracked_measures[7], layerId=paste(nodes_by_sensor[[7]]$vsn,tracked_measures[7]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[7]]$vsn,"</b>"),nodes_by_sensor[[7]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[8]]$longitude, nodes_by_sensor[[8]]$latitude, group = tracked_measures[8], layerId=paste(nodes_by_sensor[[8]]$vsn,tracked_measures[8]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[8]]$vsn,"</b>"),nodes_by_sensor[[8]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[9]]$longitude, nodes_by_sensor[[9]]$latitude, group = tracked_measures[9], layerId=paste(nodes_by_sensor[[9]]$vsn,tracked_measures[9]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[9]]$vsn,"</b>"),nodes_by_sensor[[9]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[10]]$longitude, nodes_by_sensor[[10]]$latitude, group = tracked_measures[10], layerId=paste(nodes_by_sensor[[10]]$vsn,tracked_measures[10]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor[[10]]$vsn,"</b>"),nodes_by_sensor[[10]]$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, radius = point_size(), fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_with_no_data$longitude, nodes_with_no_data$latitude, group = "Inactive", layerId=~paste(vsn,"Inactive"), popup = paste(sep = "<br/>",paste("<b>",nodes_with_no_data$vsn,", No AoT observations</b>"),nodes_with_no_data$address, "<a href='https://arrayofthings.github.io/' target='_blank'>Array of Things</a>"), stroke = FALSE, fillOpacity = inactiveOpacity, radius = point_size(), color = inactiveColor) %>%
      # OpenAQ nodes
      addCircleMarkers(nodes_by_sensor_oaq[[1]]$longitude, nodes_by_sensor_oaq[[1]]$latitude, group = openaq_tracked_measures[1], layerId=paste(nodes_by_sensor_oaq[[1]]$vsn,openaq_tracked_measures[1]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[1]]$vsn,"</b>"),nodes_by_sensor_oaq[[1]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[2]]$longitude, nodes_by_sensor_oaq[[2]]$latitude, group = openaq_tracked_measures[2], layerId=paste(nodes_by_sensor_oaq[[2]]$vsn,openaq_tracked_measures[2]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[2]]$vsn,"</b>"),nodes_by_sensor_oaq[[2]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[3]]$longitude, nodes_by_sensor_oaq[[3]]$latitude, group = openaq_tracked_measures[3], layerId=paste(nodes_by_sensor_oaq[[3]]$vsn,openaq_tracked_measures[3]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[3]]$vsn,"</b>"),nodes_by_sensor_oaq[[3]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[4]]$longitude, nodes_by_sensor_oaq[[4]]$latitude, group = openaq_tracked_measures[4], layerId=paste(nodes_by_sensor_oaq[[4]]$vsn,openaq_tracked_measures[4]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[4]]$vsn,"</b>"),nodes_by_sensor_oaq[[4]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[5]]$longitude, nodes_by_sensor_oaq[[5]]$latitude, group = openaq_tracked_measures[5], layerId=paste(nodes_by_sensor_oaq[[5]]$vsn,openaq_tracked_measures[5]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[5]]$vsn,"</b>"),nodes_by_sensor_oaq[[5]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[6]]$longitude, nodes_by_sensor_oaq[[6]]$latitude, group = openaq_tracked_measures[6], layerId=paste(nodes_by_sensor_oaq[[6]]$vsn,openaq_tracked_measures[6]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[6]]$vsn,"</b>"),nodes_by_sensor_oaq[[6]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addCircleMarkers(nodes_by_sensor_oaq[[7]]$longitude, nodes_by_sensor_oaq[[7]]$latitude, group = openaq_tracked_measures[7], layerId=paste(nodes_by_sensor_oaq[[7]]$vsn,openaq_tracked_measures[7]), popup = paste(sep = "<br/>",paste("<b>",nodes_by_sensor_oaq[[7]]$vsn,"</b>"),nodes_by_sensor_oaq[[7]]$vsn, "<a href='https://openaq.org/' target='_blank'>Open Air Quality</a>"), stroke = FALSE, radius = point_size(), fillOpacity = oaqOpacity, color= openAQColor) %>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Matter") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addLayersControl(
        baseGroups = c("Default", "Dark Matter", "Satellite", "Terrain"),
        overlayGroups = c(tracked_measures, "bc", "Inactive", "Traffic"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addControl(html = html_legend, position = "bottomright") %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = zoom_level())
    
      for(i in 1:nrow(congestion_df)){
        map <- addPolylines(map, lat = as.numeric(congestion_df[i, c(5, 6)]),
                           lng = as.numeric(congestion_df[i, c(12, 7)]),
                           color = trafficColor(as.numeric(congestion_df[i,"X_traffic"])),
                           opacity = 0.8,
                           fillOpacity = 0.5,
                           group = "Traffic",
                           popup = paste(sep = "<br/>",paste("<b>",congestion_df[i,"street"], "&",congestion_df[i,"X_fromst"],"-",congestion_df[i,"street"], "&",congestion_df[i,"X_tost"],"</b>"),paste("Current speed:",congestion_df[i,"X_traffic"],"mph"),paste("Last updated:",congestion_df[i,"X_last_updt"]), "<a href='https://dev.socrata.com/foundry/data.cityofchicago.org/n4j6-wkkf' target='_blank'>Chicago Traffic Tracker</a>"),
                           highlightOptions = highlightOptions(
                             # color = "white",
                             weight = 9, bringToFront = F, opacity = 1)

                           )

        }
    
    map
  })
  
  # observe({
  #   a <- input$map_marker_click
  #
  # })
  
  # DYNAMIC RENDERING of things in the map
  # observe({
  #   opacity <- 0.1
  #   inactiveColor <- "red"
  #   inactiveOpacity <- 0.5
  #   normalColor <- "navy"
  #   hello <- input$switch_units
  #   leafletProxy("map", data = nodes) %>%
  #     # clearShapes() %>%
  #     addCircleMarkers(nodes_by_sensor[[1]]$longitude, nodes_by_sensor[[1]]$latitude, group = tracked_measures[1], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>% #, layerId=~vsn
  #     addCircleMarkers(nodes_by_sensor[[2]]$longitude, nodes_by_sensor[[2]]$latitude, group = tracked_measures[2], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[3]]$longitude, nodes_by_sensor[[3]]$latitude, group = tracked_measures[3], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[4]]$longitude, nodes_by_sensor[[4]]$latitude, group = tracked_measures[4], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[5]]$longitude, nodes_by_sensor[[5]]$latitude, group = tracked_measures[5], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[6]]$longitude, nodes_by_sensor[[6]]$latitude, group = tracked_measures[6], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[7]]$longitude, nodes_by_sensor[[7]]$latitude, group = tracked_measures[7], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_by_sensor[[8]]$longitude, nodes_by_sensor[[8]]$latitude, group = tracked_measures[8], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
  #     addCircleMarkers(nodes_with_no_data$longitude, nodes_with_no_data$latitude, group = "Inactive", popup = nodes_with_no_data$address, stroke = FALSE, fillOpacity = inactiveOpacity, color = inactiveColor) %>%
  #     # addMarkers(initial_lng, initial_lat, group = "group2", popup = "myhouse") %>%
  #     addLayersControl(
  #       # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  #       overlayGroups = c(tracked_measures, "Inactive"),
  #       options = layersControlOptions(collapsed = TRUE)
  #     )
  # })
  
  
  #observe map input and add type identifier
  
  observeEvent(input$map_marker_click,
               {
                 v$vsn <- paste0("map ",input$map_marker_click)
                 v$map_inputs <- append(v$map_inputs,list(v$vsn))
               }
  )
  #observe table input and add type indentifier
  
  observeEvent(input$nodes_table_rows_selected,
               {
                 v$vsn <- paste0("table ",input$nodes_table_rows_selected)
                 v$table_inputs <- append(v$table_inputs,list(v$vsn))
               }
  )
  
  pt2grid <- function(ptframe,n) {
    bb <- bbox(ptframe)
    ptcrs <- proj4string(ptframe)
    xrange <- abs(bb[1,1] - bb[1,2])
    yrange <- abs(bb[2,1] - bb[2,2])
    cs <- c(xrange/n,yrange/n)
    cc <- bb[,1] + (cs/2)
    dc <- c(n,n)
    x1 <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=dc)
    x2 <- SpatialGrid(grid=x1,proj4string=CRS(ptcrs))
    return(x2)
  }
  
  #function to get the raster image for interpolated measure map for the selected measure
  
  get_interpolated_map <- function(sel_measure,time_range,sel_value_type){
    
    selected <- strsplit(sel_measure, "-", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
    source <- strsplit(sel_measure, "-", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
    
    if(source=="AoT"){
      if(time_range == TIME_RANGE_CURRENT){
        df <- get_and_preprocess_observations_all_nodes()
      } else if(time_range == TIME_RANGE_24HOURS){
        df <- get_and_preprocess_observations_24h_all_nodes()
      } else if(time_range == TIME_RANGE_7DAYS){
        df <- get_and_preprocess_observations_7d_all_nodes()
      }
    }
    else if(source=="Darksky"){
      if(time_range == TIME_RANGE_CURRENT){
        if(file.exists("fst/all_nodes_current.fst"))
          df <- read_fst("fst/all_nodes_current.fst")
        else
          df <- get_and_preprocess_observations_all_nodes_ds()
      } else if(time_range == TIME_RANGE_24HOURS){
        if(file.exists("fst/all_nodes_24hours.fst"))
          df <- read_fst("fst/all_nodes_24hours.fst")
        else
          df <- get_and_preprocess_observations_24h_all_nodes_ds()
      } else if(time_range == TIME_RANGE_7DAYS){
        if(file.exists("fst/all_nodes_7days.fst"))
          df <- read_fst("fst/all_nodes_7days.fst")
        else
          df <- get_and_preprocess_observations_7d_all_nodes_ds()
      }
    }
    else if (source=="OpenAQ"){
      if(time_range == TIME_RANGE_CURRENT){
        df <- get_and_preprocess_observations_openaq_all_nodes()
      } else if(time_range == TIME_RANGE_24HOURS){
        df <- get_and_preprocess_observations_24h_openaq_all_nodes()
      } else if(time_range == TIME_RANGE_7DAYS){
        df <- get_and_preprocess_observations_7d_all_nodes_openaq()
      }
    }
    
    # print(nrow(df))
    
    # Show alert if no current available for the measure
    if(nrow(df)==0)
      shinyalert(paste0("No data for ", source, " for this measure"), type = "error",closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    
    #get the specific value aggregation
    if(source=="AoT" || source=="OpenAQ"){
      
      if(sel_value_type=="average"){
        df[df$measure==selected,]%>%
          group_by(vsn) %>%
          summarize(req_measure = mean(value))%>%
          {. ->> results}
      }
      else if (sel_value_type=="min"){
        df[df$measure==selected,]%>%
          group_by(vsn) %>%
          summarize(req_measure = min(value))%>%
          {. ->> results}
      }
      else{
        df[df$measure==selected,]%>%
          group_by(vsn) %>%
          summarize(req_measure = max(value))%>%
          {. ->> results}
      }
    }
    else if(source=="Darksky"){
      if(time_range!=TIME_RANGE_CURRENT){
        
        if(sel_value_type=="average"){
          df1 <- data.frame(df$vsn)
          names(df1) <- c("vsn")
          df1$measure <- df[[selected]]
          
          df1 %>%
            group_by(vsn) %>%
            summarize(req_measure = mean(measure))%>%
            {. ->> results}
        }
        else if (sel_value_type=="min"){
          df1 <- data.frame(df$vsn)
          names(df1) <- c("vsn")
          df1$measure <- df[[selected]]
          
          df1 %>%
            group_by(vsn) %>%
            summarize(req_measure = min(measure))%>%
            {. ->> results}
        }
        else{
          df1 <- data.frame(df$vsn)
          names(df1) <- c("vsn")
          df1$measure <- df[[selected]]
          
          df1 %>%
            group_by(vsn) %>%
            summarize(req_measure = max(measure))%>%
            {. ->> results}
        }
      }
      else{
        #current has different format
        
        #TODO current has one records
        #fix this by querying multiple records for current time for darksky
        
        if(sel_value_type=="average"){
          df[df$measures==selected,]%>%
            group_by(vsn) %>%
            summarize(req_measure = mean(value))%>%
            {. ->> results}
        }
        else if (sel_value_type=="min"){
          df[df$measures==selected,]%>%
            group_by(vsn) %>%
            summarize(req_measure = min(value))%>%
            {. ->> results}
        }
        else{
          df[df$measures==selected,]%>%
            group_by(vsn) %>%
            summarize(req_measure = max(value))%>%
            {. ->> results}
        }
        
      }
    }
    
    # print(nrow(results))
    
    # Show alert if no current available for the measure
    if(nrow(results)==0){
      
      shinyalert(paste0("No data of this measure for ", source), type = "error",closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      return (NULL);
    }
    else{
      chiCA <- shapefile("data/ChiComArea.shp")
      
      if(source=="AoT" || source=="Darksky"){
        active_nodes <- nodes_table[nodes_table$status=="Active",]
        data <- merge(results, active_nodes, by = c("vsn"))
        if(nrow(data)==0){
          shinyalert(paste0("Not enough data for ", source, " of this measure"), type = "error",closeOnEsc = TRUE,closeOnClickOutside = TRUE)
          return (NULL);
        }
        else
          coordinates(data) <- data[,c("longitude", "latitude")]
      }
      else{
        active_nodes <- nodes_oaq
        data <- merge(results, active_nodes, by = c("vsn"))
        #if there are no active nodes reporting the measure
        if(nrow(data)==0){
          shinyalert(paste0("Not enough data for ", source, " of this measure"), type = "error",closeOnEsc = TRUE,closeOnClickOutside = TRUE)
          return (NULL);
        }
        else
          coordinates(data) <- data[,c("longitude", "latitude")]
        
      }
      
      
      #if there are no active nodes reporting the measure
      if(nrow(data)>1){
        proj4string(data) <- CRS("+init=epsg:4326")
        mes <- data$req_measure
        tmp.vgm <- variogram(data$req_measure ~ 1, data)
      }
      else
        tmp.vgm <- NULL
      
      
      if(is.null(tmp.vgm)){
        shinyalert(paste0("Not enough data for ", source, " of this measure"), type = "error",closeOnEsc = TRUE,closeOnClickOutside = TRUE)
        return (NULL);
      }
      
      
fit.sph <- fit.variogram(tmp.vgm,vgm(c("Exp", "Mat", "Ste","Sph"),fit.ranges = TRUE))
      
      
      chi.grid <- pt2grid((chiCA),30)
      
      chi.grid <- pt2grid((chiCA),100)
      
      projection(chi.grid) <- CRS("+init=epsg:4326")
      projection(data) <-  CRS("+init=epsg:4326")
      projection(chiCA) <- CRS("+init=epsg:4326")
      
      kriged <- krige(data$req_measure ~ 1, data, chi.grid, model = fit.sph)
      
      chi.kriged <- kriged[chiCA,]
      
        response <- list("map"=chi.kriged,"avg"=results$req_measure)
        return (response)
      
    }
    
  }
  
  # add the heatmap when the heatmap switch is enabled or based on any change in heatmap inputs
  observeEvent({input$heat_map
    input$heatmap_measure
    input$map_time_range
    input$measure_type}, {
      proxy <- leafletProxy("map")
      #interpolation map
      if(input$heat_map){
          response <- get_interpolated_map(input$heatmap_measure,input$map_time_range,input$measure_type)
          if(!is.null(response$map)){
            mypal <- colorNumeric(palette = "viridis", reverse = TRUE, domain = response$avg
                                  ,na.color = "#ffffff11"
            )
            html_legend <- "
      <b>Nodes</b><br>
            <div class='circle' id='aotactive'></div><a href='https://arrayofthings.github.io/' target='_blank'>AoT</a> active
            <div class='circle' id='aotinactive'></div><a href='https://arrayofthings.github.io/' target='_blank'>AoT</a> inactive
            <div class='circle' id='oaq'></div><a href='https://openaq.org/' target='_blank'>OpenAQ</a>
            "
            
            proxy %>% clearControls() %>% clearImages() %>%
            addControl(html = html_legend, position = "bottomright") %>%
              
            addRasterImage(raster(response$map), colors= mypal,opacity = 0.8)%>% addLegend("bottomright", pal = mypal, response$avg,title = input$heatmap_measure,opacity = 1)        
          }
        }
    })
  
  
  
  #####################################################  GRAPHICAL DATA    #####################################################
  output$graphical_data <- renderPlot({
    autoInvalidate45()
    vsn_ <- v$vsn
    if(!is.null(vsn_)){
      #get input type either map or table
      
      type <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
      
      # if map input, get the vsn and the active status
      if(type=="map"){
        vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$map_inputs)<2){
          prev_input <-NULL
        }
        else{
          last_two <- tail(v$map_inputs,2)
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
      # if table input, get the vsn and the active status
      else{
        row_id <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        selected_row <- nodes_table[row_id,]
        active <- selected_row$status
        vsn <- selected_row$vsn
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$table_inputs)<2)
          prev_input <-NULL
        else{
          last_two <- tail(v$table_inputs,2)
          
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
    }
    else
      vsn <-NULL
    
    if(is.null(vsn)){
      plot_title <- "No node selected"
      
      gl <- ggplot() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
          axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
          plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#0d2025"),
          legend.background = element_rect(color = NA, fill = "#0d2025"),
          legend.key = element_rect(color = NA, fill = "#0d2025"),
          panel.background = element_rect(fill = "#0d2025", color  =  NA),
          panel.grid.major = element_line(color = "#FFFFFF"),
          panel.grid.minor = element_line(color = "#FFFFFF"),
          legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
          legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
        )+labs(title=plot_title,x = "Time", y = "Measurement")
      
      gl
    }
    else {
      #some node is selected
      time_range <- input$time_range
      
      if(!(active == "Inactive")){#TODO A: If node is active, but has values only for temperature/intensity/humidity, graph should show no observations
        # TODO check if AoT node or openAQ and get corresponding dataset
        # Suggestion: AoT nodes vsn start with "0" except one that starts with "8", OpenAQ vsn never start with a number
        
        
        #set the previous click
        if(!is.null(prev_input)){
          #get input type either map or table
          
          type <- strsplit(prev_input, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
          
          # if map input, get the vsn and the active status
          if(type=="map"){
            v$lastvsn <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          }
          # if table input, get the vsn and the active status
          else{
            prev_row_id <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
            prev_row <- nodes_table[prev_row_id,]
            
            #if the previous table node was inactive, set vsn as inactive
            if(prev_row$status=="Active")
              v$lastvsn <- prev_row$vsn
            else
              v$lastvsn <- "Inactive"
            
          }
        }
        else{
          v$lastvsn <-NULL
        }
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations_openaq(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h_openaq(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d_openaq(vsn)
          }
        }
        else
        {
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d(vsn)
          }
        }
        if(time_range == TIME_RANGE_CURRENT){
          plot_title <- paste("Current (or most recent) data for node:",df$vsn[1])
        } else if(time_range == TIME_RANGE_24HOURS){
          plot_title <- paste("Last 24 hours data for node:",df$vsn[1])
        } else {
          plot_title <- paste("Last 7 days data for node:",df$vsn[1])
        }
        
        df <- as.data.frame(lapply(df, unlist))
        
        
        # gl <- ggplot(data = df, aes(x = df$hms)) +
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        labs <-c()
        vals <-c()
        # This doesnt work because R sucks
        # for(m in tracked_measures){
        #   g <- 1
        #   if (m %in% c(input$measures1,input$measures2)){
        #     suffx_co = "(ppm)" #TODO get uom from measure function
        #     labs <-c(labs,m = paste(m,suffx_co, sep=" "))
        #     vals <-c(vals,c("#c6c60f"))  #TODO get color from measure
        #     gl <- gl + geom_line(aes(y = subset(df, measure == m)$value, x = subset(df, measure == m)$hms, color = m), size = line_size(), group = g) +
        #       geom_point(aes(y = subset(df, measure == m)$value, x = subset(df, measure == m)$hms , color = m), size = line_size()*3)
        #   }
        #   g <- g+1
        # }
        # tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")
        # test <- subset(df, measure == "co")
        
        retrieved_measures <- unique(df$measure)
        
        
        if ("co" %in% c(input$measures1,input$measures2) && "co" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "co")$uom)
          labs <-c(labs,"co" = paste("co",suffx_co, sep=" "))
          vals <-c(vals,"co" = "#c6c60f")
          gl <- gl + geom_line(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms, color = "co"), size = line_size(), group = 1) +
            geom_point(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms , color = "co"), size = line_size()*3)
        }
        if ("no2" %in% c(input$measures1,input$measures2) && "no2" %in% retrieved_measures){
          suffx_no2 = unique(subset(df, measure == "no2")$uom)
          labs <-c(labs,"no2" = paste("no2",suffx_no2, sep=" "))
          vals <-c(vals,"no2" = "#13c649")
          gl <- gl + geom_line(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms, color = "no2"), size = line_size(), group = 2) +
            geom_point(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms , color = "no2"), size = line_size()*3)
        }
        if ("bc" %in% c(input$measures1,input$measures2) && "bc" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "bc")$uom)
          labs <-c(labs,"bc" = paste("bc",suffx_co, sep=" "))
          vals <-c(vals,"bc" = "#c6c60f")
          gl <- gl + geom_line(aes(y = subset(df, measure == "bc")$value, x = subset(df, measure == "bc")$hms, color = "bc"), size = line_size(), group = 1) +
            geom_point(aes(y = subset(df, measure == "bc")$value, x = subset(df, measure == "bc")$hms , color = "bc"), size = line_size()*3)
        }
        
        
        # TODO A: MOVE THIS TO PANEL 2
        # if ("humidity" %in% c(input$measures1,input$measures2) && "humidity" %in% retrieved_measures){
        #   suffx_humidity = unique(subset(df, measure == "humidity")$uom)
        #   # y = subset(df, measure == "humidity")$value
        #     gl <- gl + geom_line(aes(y = subset(df, measure == "humidity")$value, x = subset(df, measure == "humidity")$hms, color = "humidity"), size = line_size(), group = 3) +
        #       geom_point(aes(y = subset(df, measure == "humidity")$value, x = subset(df, measure == "humidity")$hms , color = "humidity"), size = line_size()*3)
        #     labs <-c(labs,"humidity" = paste("humidity",suffx_humidity, sep=" "))
        #     vals <-c(vals,"humidity" = "#194649")
        # }
        
        # TODO A: MOVE THIS TO PANEL 2
        # if ("intensity" %in% c(input$measures1,input$measures2) && "intensity" %in% retrieved_measures){
        #   suffx_intensity = unique(subset(df, measure == "intensity" & uom == "lux")$uom)
        #   gl <- gl + geom_line(aes(subset(df, measure == "intensity" & uom == "lux")$value, x = subset(df, measure == "intensity" & uom == "lux")$hms, color = "intensity"), size = line_size(), group = 4) +
        #     geom_point(aes( y = subset(df, measure == "intensity" & uom == "lux")$value, x = subset(df, measure == "intensity" & uom == "lux")$hms , color = "intensity"), size = line_size()*3)
        #   labs <-c(labs,"intensity" = paste("intensity",suffx_intensity, sep=" "))
        #   vals <-c(vals,"intensity" = "#a3d659")
        # }
        
        if ("o3" %in% c(input$measures1,input$measures2) && "o3" %in% retrieved_measures){
          suffx_o3 = unique(subset(df, measure == "o3")$uom)
          labs <-c(labs,"03" = paste("o3",suffx_o3, sep=" "))
          vals <-c(vals,"o3" = "#0fa2af")
          gl <- gl + geom_line(aes(y = subset(df, measure == "o3")$value, x = subset(df, measure == "o3")$hms, color = "o3"), size = line_size(), group = 5) +
            geom_point(aes(y = subset(df, measure == "o3")$value, x = subset(df, measure == "o3")$hms , color = "o3"), size = line_size()*3)
        }
        if ("so2" %in% c(input$measures1,input$measures2) && "so2" %in% retrieved_measures){
          suffx_so2 =  unique(subset(df, measure == "so2")$uom)
          labs <-c(labs,"so2"=paste("so2",suffx_so2, sep=" "))
          vals <-c(vals,"so2" = "#B899E7")
          gl <- gl + geom_line(aes(y = subset(df, measure == "so2")$value, x = subset(df, measure == "so2")$hms, color = "so2"), size = line_size(), group = 6) +
            geom_point(aes(y = subset(df, measure == "so2")$value, x = subset(df, measure == "so2")$hms , color = "so2"), size = line_size()*3)
        }
        if ("h2s" %in% c(input$measures1,input$measures2) && "h2s" %in% retrieved_measures){
          suffx_h2s = unique(subset(df, measure == "h2s")$uom)
          labs <-c(labs,"h2s"=paste("h2s",suffx_h2s, sep=" "))
          vals <-c(vals,"h2s" = "#A877E0")
          gl <- gl + geom_line(aes(y = subset(df, measure == "h2s")$value, x = subset(df, measure == "h2s")$hms, color = "h2s"), size = line_size(), group = 7) +
            geom_point(aes(y = subset(df, measure == "h2s")$value, x = subset(df, measure == "h2s")$hms , color = "h2s"), size = line_size()*3)
        }
        convert_to_imperial <- function(values){
          return(values*1000000000000* 0.000000035274/35315)
        }
        
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        if ("pm2.5" %in% c(input$measures1,input$measures2) && "pm2.5" %in% retrieved_measures){
          if(input$switch_units){
            # df$data_conv <-df$"pm2.5"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm2.5","conv",sep="_")
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms, color = "pm2.5"), size = line_size(), group = 8) +
              geom_point(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms , color = "pm2.5"), size = line_size()*3)
          }
          else{
            
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms, color = "pm2.5"), size = line_size(), group = 8) +
              geom_point(aes(y= subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms , color = "pm2.5"), size = line_size()*3)
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            labs <-c(labs,"pm2.5"=paste("pm2.5",suffx_pm2.5, sep=" "))
            vals <-c(vals,"pm2.5" = "#cc8112")
            
          }
        }
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        
        if ("pm10" %in% c(input$measures1,input$measures2) && "pm10" %in% retrieved_measures){
          
          if(input$switch_units){
            # df$data_conv <-df$"pm10"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm10","conv",sep="_")
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms, color = "pm10"), size = line_size(), group = 9) +
              geom_point(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms , color = "pm10"), size = line_size()*3)
          }
          else{
            
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            gl <- gl + geom_line(aes(y= subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms, color = "pm10"), size = line_size(), group = 9) +
              geom_point(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms , color = "pm10"), size = line_size()*3)
            labs <-c(labs,"pm10"= paste("pm10",suffx_pm10, sep=" "))
            vals <-c(vals,"pm10" = "#ba1010")
            
          }
        }
        convert_temp_to_metric <- function(values){
          return((values-32)/1.8)
        }
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        
        #MOVE THIS TO PANEL 2
        # if ("temperature" %in% c(input$measures1,input$measures2) && "temperature" %in% retrieved_measures){
        #   if(input$switch_units){
        #     temp_suffx = "(Degrees Fahrenheit)"
        #     gl <- gl + geom_line(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms, color = "temperature"), size = line_size(), group = 2) +
        #       geom_point(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms , color = "temperature"), size = line_size()*3)
        #   }
        #   else{
        #       temp_suffx = "(Degrees Celsius)"
        #       gl <- gl + geom_line(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms, color = "temperature"), size = line_size(), group = 2) +
        #         geom_point(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms , color = "temperature"), size = line_size()*3)
        #       labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
        #       vals <-c(vals,"temperature" = "#6B1F13")
        #   }
        # }
        gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                      values = vals)
        gl
      } else {
        plot_title <- "This node has no observations"
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        gl
      }
    }
  })
  
  
  
  
  
  
  
  
  last_vsn <- reactive({
    
    input_id <- input$map_marker_click
    if(!is.null(input_id)){
      vsn_ <- input_id$id
      vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
      active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
      if(active == "Inactive"){
        "Inactive"
      } else {
        last <- vsn
        vsn
      }
    }
    #check if table row selected
    else if(nrow(selected_row)>0){
      last <- vsn
      vsn
    }
    else {
      NULL
    }
  })
  
  #####################################################  GRAPHICAL DATA COMPARISON    #####################################################
  
  # Second plot for comparison
  output$graphical_data_last <- renderPlot({
    autoInvalidate50()
    
    time_range <- input$time_range
    irrelevant_variable <- input$map_marker_click
    
    vsn <- isolate(v$lastvsn)
    # || input$switch_compare
    if(is.null(vsn)){
      
      plot_title <- "No node selected for previous output"
      
      gl <- ggplot() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
          axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
          plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#0d2025"),
          legend.background = element_rect(color = NA, fill = "#0d2025"),
          legend.key = element_rect(color = NA, fill = "#0d2025"),
          panel.background = element_rect(fill = "#0d2025", color  =  NA),
          panel.grid.major = element_line(color = "#FFFFFF"),
          panel.grid.minor = element_line(color = "#FFFFFF"),
          legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
          legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
        )+labs(title=plot_title,x = "Time", y = "Measurement")
      
      gl
    }
    else {
      if(!(vsn == "Inactive")){
        
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations_openaq(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h_openaq(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d_openaq(vsn)
          }
        }
        else
        {
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d(vsn)
          }
        }
        
        if(time_range == TIME_RANGE_CURRENT){
          plot_title <- paste("Current (or most recent) data for node:",df$vsn[1])
        } else if(time_range == TIME_RANGE_24HOURS){
          plot_title <- paste("Last 24 hours data for node:",df$vsn[1])
        } else {
          plot_title <- paste("Last 7 days data for node:",df$vsn[1])
        }
        
        # df <- subset(df, measure == "co")
        df <- as.data.frame(lapply(df, unlist))
        
        # gl <- ggplot(data = df, aes(x = df$hms)) +
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        labs <-c()
        vals <-c()
        # This doesnt work because R sucks
        # for(m in tracked_measures){
        #   g <- 1
        #   if (m %in% c(input$measures1,input$measures2)){
        #     suffx_co = "(ppm)" #TODO get uom from measure function
        #     labs <-c(labs,m = paste(m,suffx_co, sep=" "))
        #     vals <-c(vals,c("#c6c60f"))  #TODO get color from measure
        #     gl <- gl + geom_line(aes(y = subset(df, measure == m)$value, x = subset(df, measure == m)$hms, color = m), size = line_size(), group = g) +
        #       geom_point(aes(y = subset(df, measure == m)$value, x = subset(df, measure == m)$hms , color = m), size = line_size()*3)
        #   }
        #   g <- g+1
        # }
        # tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")
        
        # Getting all the measures that this dataset contains
        retrieved_measures <- unique(df$measure)
        
        
        if ("co" %in% c(input$measures1,input$measures2) && "co" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "co")$uom)
          labs <-c(labs,"co" = paste("co",suffx_co, sep=" "))
          vals <-c(vals,"co" = "#c6c60f")
          gl <- gl + geom_line(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms, color = "co"), size = line_size(), group = 1) +
            geom_point(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms , color = "co"), size = line_size()*3)
        }
        if ("no2" %in% c(input$measures1,input$measures2) && "no2" %in% retrieved_measures){
          suffx_no2 = unique(subset(df, measure == "no2")$uom)
          labs <-c(labs,"no2" = paste("no2",suffx_no2, sep=" "))
          vals <-c(vals,"no2" = "#13c649")
          gl <- gl + geom_line(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms, color = "no2"), size = line_size(), group = 2) +
            geom_point(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms , color = "no2"), size = line_size()*3)
        }
        #TODO A: Move to second panel
        # if ("humidity" %in% c(input$measures1,input$measures2) && "humidity" %in% retrieved_measures){
        #   suffx_humidity = unique(subset(df, measure == "humidity")$uom)
        #
        #     gl <- gl + geom_line(aes(y = subset(df, measure == "humidity")$value, x = subset(df, measure == "humidity")$hms, color = "humidity"), size = line_size(), group = 3) +
        #       geom_point(aes(y = subset(df, measure == "humidity")$value, x = subset(df, measure == "humidity")$hms , color = "humidity"), size = line_size()*3)
        #     labs <-c(labs,"humidity" = paste("humidity",suffx_humidity, sep=" "))
        #     vals <-c(vals,"humidity" = "#194649")
        #
        # }
        # if ("intensity" %in% c(input$measures1,input$measures2) && "intensity" %in% retrieved_measures){
        #   suffx_intensity = unique(subset(df, measure == "intensity" & uom == "lux")$uom)
        #   gl <- gl + geom_line(aes(subset(df, measure == "intensity" & uom == "lux")$value, x = subset(df, measure == "intensity" & uom == "lux")$hms, color = "intensity"), size = line_size(), group = 4) +
        #     geom_point(aes( y = subset(df, measure == "intensity" & uom == "lux")$value, x = subset(df, measure == "intensity" & uom == "lux")$hms , color = "intensity"), size = line_size()*3)
        #   labs <-c(labs,"intensity" = paste("intensity",suffx_intensity, sep=" "))
        #   vals <-c(vals,"intensity" = "#a3d659")
        # }
        if ("o3" %in% c(input$measures1,input$measures2) && "o3" %in% retrieved_measures){
          suffx_o3 = unique(subset(df, measure == "o3")$uom)
          labs <-c(labs,"03" = paste("o3",suffx_o3, sep=" "))
          vals <-c(vals,"o3" = "#0fa2af")
          gl <- gl + geom_line(aes(y = subset(df, measure == "o3")$value, x = subset(df, measure == "o3")$hms, color = "o3"), size = line_size(), group = 5) +
            geom_point(aes(y = subset(df, measure == "o3")$value, x = subset(df, measure == "o3")$hms , color = "o3"), size = line_size()*3)
        }
        if ("so2" %in% c(input$measures1,input$measures2) && "so2" %in% retrieved_measures){
          suffx_so2 =  unique(subset(df, measure == "so2")$uom)
          labs <-c(labs,"so2"=paste("so2",suffx_so2, sep=" "))
          vals <-c(vals,"so2" = "#B899E7")
          gl <- gl + geom_line(aes(y = subset(df, measure == "so2")$value, x = subset(df, measure == "so2")$hms, color = "so2"), size = line_size(), group = 6) +
            geom_point(aes(y = subset(df, measure == "so2")$value, x = subset(df, measure == "so2")$hms , color = "so2"), size = line_size()*3)
        }
        if ("bc" %in% c(input$measures1,input$measures2) && "bc" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "bc")$uom)
          labs <-c(labs,"bc" = paste("bc",suffx_co, sep=" "))
          vals <-c(vals,"bc" = "#c6c60f")
          gl <- gl + geom_line(aes(y = subset(df, measure == "bc")$value, x = subset(df, measure == "bc")$hms, color = "bc"), size = line_size(), group = 1) +
            geom_point(aes(y = subset(df, measure == "bc")$value, x = subset(df, measure == "bc")$hms , color = "bc"), size = line_size()*3)
        }
        if ("h2s" %in% c(input$measures1,input$measures2) && "h2s" %in% retrieved_measures){
          suffx_h2s = unique(subset(df, measure == "h2s")$uom)
          labs <-c(labs,"h2s"=paste("h2s",suffx_h2s, sep=" "))
          vals <-c(vals,"h2s" = "#A877E0")
          gl <- gl + geom_line(aes(y = subset(df, measure == "h2s")$value, x = subset(df, measure == "h2s")$hms, color = "h2s"), size = line_size(), group = 7) +
            geom_point(aes(y = subset(df, measure == "h2s")$value, x = subset(df, measure == "h2s")$hms , color = "h2s"), size = line_size()*3)
        }
        convert_to_imperial <- function(values){
          return(values*1000000000000* 0.000000035274/35315)
        }
        
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        if ("pm2.5" %in% c(input$measures1,input$measures2) && "pm2.5" %in% retrieved_measures){
          if(input$switch_units){
            # df$data_conv <-df$"pm2.5"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm2.5","conv",sep="_")
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms, color = "pm2.5"), size = line_size(), group = 8) +
              geom_point(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms , color = "pm2.5"), size = line_size()*3)
          }
          else{
            
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms, color = "pm2.5"), size = line_size(), group = 8) +
              geom_point(aes(y= subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms , color = "pm2.5"), size = line_size()*3)
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            labs <-c(labs,"pm2.5"=paste("pm2.5",suffx_pm2.5, sep=" "))
            vals <-c(vals,"pm2.5" = "#cc8112")
            
          }
        }
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        
        if ("pm10" %in% c(input$measures1,input$measures2) && "pm10" %in% retrieved_measures){
          if(input$switch_units){
            # df$data_conv <-df$"pm10"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm10","conv",sep="_")
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms, color = "pm10"), size = line_size(), group = 9) +
              geom_point(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms , color = "pm10"), size = line_size()*3)
          }
          else{
            
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            gl <- gl + geom_line(aes(y= subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms, color = "pm10"), size = line_size(), group = 9) +
              geom_point(aes(y = subset(df, measure == "pm10")$value, x = subset(df, measure == "pm10")$hms , color = "pm10"), size = line_size()*3)
            labs <-c(labs,"pm10"= paste("pm10",suffx_pm10, sep=" "))
            vals <-c(vals,"pm10" = "#ba1010")
            
          }
        }
        convert_temp_to_metric <- function(values){
          return((values-32)/1.8)
        }
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        
        if ("temperature" %in% c(input$measures1,input$measures2) && "temperature" %in% retrieved_measures){
          if(input$switch_units){
            temp_suffx = "(Degrees Fahrenheit)"
            gl <- gl + geom_line(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms, color = "temperature"), size = line_size(), group = 2) +
              geom_point(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms , color = "temperature"), size = line_size()*3)
          }
          else{
            temp_suffx = "(Degrees Celsius)"
            gl <- gl + geom_line(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms, color = "temperature"), size = line_size(), group = 2) +
              geom_point(aes(y = subset(df, measure == "temperature")$value, x = subset(df, measure == "temperature")$hms , color = "temperature"), size = line_size()*3)
            labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
            vals <-c(vals,"temperature" = "#6B1F13")
          }
        }
        gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                      values = vals)
        gl
      } else {
        plot_title <- "This node has no observations"
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        gl
      }
    }
  })
  
  # Darksky table for current time
  output$table_ds <- DT::renderDataTable(
    if(is.null(input$map_marker_click$lat) && is.null(input$map_marker_click$long)){
      DT::datatable({
        empty <- data.frame()
        empty
      })
    }
    else{
      DT::datatable({
        current_forecast = get_current_forecast(input$map_marker_click$lat, input$map_marker_click$lng,exclude="minutely,hourly,daily")
        curr = data.frame(current_forecast['currently'])
        names(curr) <- substring(names(curr),11,nchar((names(curr))))
        darksky_tracked_measures <- c("temperature", "humidity", "windSpeed", "windBearing", "cloudCover", "visibility", "pressure", "ozone", "summary")
        df <- data.frame(t(subset(curr,select = darksky_tracked_measures)))
        names(df) <- c("value")
        measures <-darksky_tracked_measures
        curr_data <- data.frame(measures = darksky_tracked_measures,
                                value = df)
      },
      options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE,
      caption = 'Current Time measures from Datasky'
      
      )
    }
  )
  # get current data for darksky
  
  get_and_preprocess_observations_ds <- function(lng,lat) {
    now <-Sys.time()
    curr <-ymd_hms(now) - lubridate::minutes(10)
    curr<-force_tz(yes, "America/Chicago")
    ds <-seq(yes, now,by="min")[1:10]
    ds <-ymd_hms(ds)
    force_tz(ds, "America/Chicago")%>%
      map(~get_forecast_for(lng, lat,.x))%>%
      map_df("currently")%>%
      {. ->> response}
    
    response$time<-ymd_hms(response$time)
    response$time<-force_tz(response$time, "America/Chicago")
    
    return (response)
  }
  
  
  # get current data for all the nodes for darksky
  
  get_and_preprocess_observations_all_nodes_ds <- function(){
    #get all active nodes from AoT and their coordinates and then query them
    
    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]
    
    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)
    
    res <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_ds)) %>% unnest()%>% 
    {. ->> current_result}

    res<- extract_date_fields(res)
    
    res <- res %>% unnest()
    
    save_df_as_fst(res,"fst/all_nodes_current.fst")
    
    return(res)
  }

  
  #preprocess darksky data for last 24 hours  
  get_and_preprocess_observations_24h_ds <- function(lng,lat){

    # print("in preprocess")
    now <-Sys.time()
    yes <-ymd_hms(now) - lubridate::hours(24)
    yes<-force_tz(yes, "America/Chicago")
    ds <-seq(yes, now,by="hour")[1:25]
    ds <-ymd_hms(ds)
    force_tz(ds, "America/Chicago")%>%
      map(~get_forecast_for(lng, lat,.x))%>%
      map_df("hourly")%>%
      distinct()%>%
      {. ->> response}
    response$time<-ymd_hms(response$time)
    response$time<-force_tz(response$time, "America/Chicago")
    res <-tail(filter(response,(day(response$time)<day(now) | (day(response$time)==day(now) & hour(response$time)<=hour(now)))),24)
    res <- extract_date_fields_h(res)
    return (res)
  }

  #preprocess darksky data for last 24 hours for all nodes
  
  get_and_preprocess_observations_24h_all_nodes_ds <- function(lng,lat){
    
    #get all active nodes from AoT and their coordinates and then query them
    
    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]
    
    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)
    
    res <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_24h_ds)) %>% unnest()
    
    save_df_as_fst(res,"fst/all_nodes_24hours.fst")
    
    # print(head(res))
    
    
    return (res)
  }
  
  #preprocess darksky data for last 7 days
  get_and_preprocess_observations_7d_ds <- function(lng,lat){
    
    seq(Sys.Date()-7, Sys.Date(), "1 day") %>%
    map(~get_forecast_for(lng, lat, .x)) %>%
    map_df("daily") %>%
    {. ->> last_7 }
    last_7 <- extract_date_fields_d(last_7)
    return (last_7)
  }

  #preprocess darksky data for last 7 days for all nodes
  get_and_preprocess_observations_7d_all_nodes_ds <- function(lng,lat){
    
    #get all active nodes from AoT and their coordinates and then query them

    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]

    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)

    df <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_7d_ds)) %>% unnest()
    
    save_df_as_fst(df,"fst/all_nodes_7days.fst")
    
    return (df)
  }

    
  #Darksky graphical data
  #This takes the input from AoT table and maps markets
  
  output$graphical_data_ds <- renderPlot({
    autoInvalidate45()
    vsn_ <- v$vsn
    print(vsn_)
    if(!is.null(vsn_)){
      #get input type either map or table
      
      type <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
      
      # if map input, get the vsn and the active status
      if(type=="map"){
        vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$map_inputs)<2){
          prev_input <-NULL
        }
        else{
          last_two <- tail(v$map_inputs,2)
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
      # if table input, get the vsn and the active status
      else{
        row_id <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        selected_row <- nodes_table[row_id,]
        active <- selected_row$status
        vsn <- selected_row$vsn
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$table_inputs)<2)
          prev_input <-NULL
        else{
          last_two <- tail(v$table_inputs,2)
          
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
    }
    else
      vsn <-NULL
    
    if(is.null(vsn)){
      
      DT::datatable({
        empty <- data.frame()
        empty
      })
      
    }
    else {
      #some node is selected
      time_range <- input$time_range
      
      if(!(active == "Inactive")){#TODO A: If node is active, but has values only for temperature/intensity/humidity, graph should show no observations
        # TODO check if AoT node or openAQ and get corresponding dataset
        # Suggestion: AoT nodes vsn start with "0" except one that starts with "8", OpenAQ vsn never start with a number
        
        
        #set the previous click
        if(!is.null(prev_input)){
          #get input type either map or table
          
          type <- strsplit(prev_input, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
          
          # if map input, get the vsn and the active status
          if(type=="map"){
            v$lastvsn <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          }
          # if table input, get the vsn and the active status
          else{
            prev_row_id <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
            prev_row <- nodes_table[prev_row_id,]
            
            #if the previous table node was inactive, set vsn as inactive
            if(prev_row$status=="Active")
              v$lastvsn <- prev_row$vsn
            else
              v$lastvsn <- "Inactive"
            
          }
        }
        else{
          v$lastvsn <-NULL
        }
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations_openaq(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h_openaq(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d_openaq(vsn)
          }
        }
        else
        {
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df <- get_and_preprocess_observations_7d(vsn)
          }
        }
        
        df <- as.data.frame(lapply(df, unlist))
        retrieved_measures <- unique(df$measure)
        
        print(retrieved_measures)
        labs <- names(df)
        labs <- labs[ - which(names(labs) == c("vsn","year","month","day","humidity","intensity","temperature","uom"))]
        labs2 <- list()
        print(labs)
        #add check conditions:
        #suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
        
        if ("co" %in% c(input$measures1,input$measures2) && "co" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "co")$uom)
          labs <-c(labs,"co" = paste("co",suffx_co, sep=" "))
          labs2 = c(labs2,"co")
        }
        if ("no2" %in% c(input$measures1,input$measures2) && "no2" %in% retrieved_measures){
          suffx_no2 = unique(subset(df, measure == "no2")$uom)
          labs <-c(labs,"no2" = paste("no2",suffx_no2, sep=" "))
          labs2 = c(labs2,"no2")
        }
        
        if ("o3" %in% c(input$measures1,input$measures2) && "o3" %in% retrieved_measures){
          suffx_o3 = unique(subset(df, measure == "o3")$uom)
          labs <-c(labs,"o3" = paste("o3",suffx_o3, sep=" "))
          labs2 = c(labs2,c("o3"))
        }
        if ("so2" %in% c(input$measures1,input$measures2) && "so2" %in% retrieved_measures){
          suffx_so2 =  unique(subset(df, measure == "so2")$uom)
          labs <-c(labs,"so2"=paste("so2",suffx_so2, sep=" "))
          labs2 = c(labs2,"so2")
        }
        if ("bc" %in% c(input$measures1,input$measures2) && "bc" %in% retrieved_measures){
          suffx_co = unique(subset(df, measure == "bc")$uom)
          labs <-c(labs,"bc" = paste("bc",suffx_co, sep=" "))
          labs2 = c(labs2,"bc")
        }
        if ("h2s" %in% c(input$measures1,input$measures2) && "h2s" %in% retrieved_measures){
          suffx_h2s = unique(subset(df, measure == "h2s")$uom)
          labs <-c(labs,"h2s"=paste("h2s",suffx_h2s, sep=" "))
          labs2 = c(labs2,"h2s")
        }
        
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        if ("pm2.5" %in% c(input$measures1,input$measures2) && "pm2.5" %in% retrieved_measures){
          if(input$switch_units){
            # df$data_conv <-df$"pm2.5"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm2.5","conv",sep="_")
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            labs <-c(labs,"pm2.5"=paste("pm2.5",suffx_pm2.5, sep=" "))
            labs2 = c(labs2,c("pm2.5"))
          }
          else{
            
            gl <- gl + geom_line(aes(y = subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms, color = "pm2.5"), size = line_size(), group = 8) +
              geom_point(aes(y= subset(df, measure == "pm2.5")$value, x = subset(df, measure == "pm2.5")$hms , color = "pm2.5"), size = line_size()*3)
            suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
            labs <-c(labs,"pm2.5"=paste("pm2.5",suffx_pm2.5, sep=" "))
            labs2 = c(labs2,c("pm2.5"))
          }
        }
        # currently the same values is shown for both imperial and metric
        # to be changed accordingly based on the units in the dataset
        #
        if ("pm10" %in% c(input$measures1,input$measures2) && "pm10" %in% retrieved_measures){
          
          if(input$switch_units){
            # df$data_conv <-df$"pm10"
            # df$data_conv <- convert_to_imperial(df$data_conv)
            # names(df)[names(df)=="data_conv"] <- paste("pm10","conv",sep="_")
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            labs <-c(labs,"pm10"= paste("pm10",suffx_pm10, sep=" "))
            labs2 = c(labs2,c("pm10"))
          }
          else{
            
            suffx_pm10 = unique(subset(df, measure == "pm10")$uom)
            labs <-c(labs,"pm10"= paste("pm10",suffx_pm10, sep=" "))
            labs2 = c(labs2,"pm10")
          }
        }
        convert_temp_to_metric <- function(values){
          return((values-32)/1.8)
        }
        #
        #final dataframe which is to be shown as table is df
        
        print(df)
        drops <- c("vsn","year","month","day","uom")
        df <- df[ , !(names(df) %in% drops)]
        print(df)
        '%ni%' <- Negate('%in%')
        df <- subset(df, measure %ni% c("humidity","intensity","temperature"))
        df <- spread(df, measure, value)
        
        # print(df)
        keep <- c("hms",labs2)
        df <- df[unlist(keep)]
        if(length(keep)==1)
        {
          DT::datatable({
            empty <- data.frame()
            empty
          })
        }
        else
        {
          names(df) = unlist(c("hms",labs))
          DT::datatable({df},options = list(searching = FALSE, pageLength =10, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE,
          caption = 'Pollutant measures'
          )
        }
      }
    }
    
  })
  
  
  
  #TABLE 2:
  # TAB 2 table
  output$table_data_ds <- DT::renderDataTable({
    autoInvalidate45()
    vsn_ <- v$vsn
    
    if(!is.null(vsn_)){
      #get input type either map or table
      
      type <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
      
      # if map input, get the vsn and the active status
      if(type=="map"){
        vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #OpenAQ node
          vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          lat <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[4]][2]
          lng <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[5]][2]
          active <- "active"
        }
        else
        {
          active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
          lat <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[4]][2]
          lng <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[5]][2]
        }
        #check the size of the map_inputs if it is less than 2.
        if(length(v$map_inputs)<2){
          prev_input <-NULL
        }
        else{
          last_two <- tail(v$map_inputs,2)
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
      # if table input, get the vsn and the active status
      else{
        row_id <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        selected_row <- nodes_table[row_id,]
        active <- selected_row$status
        vsn <- selected_row$vsn
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$table_inputs)<2)
          prev_input <-NULL
        else{
          last_two <- tail(v$table_inputs,2)
          
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
    }
    else
      vsn <-NULL
    
    if(is.null(vsn)){
      
      DT::datatable({
        empty <- data.frame()
        empty
      })
      
    }
    else {
      #some node is selected
      time_range <- input$time_range
      
      if(!(active == "Inactive")){#TODO A: If node is active, but has values only for temperature/intensity/humidity, graph should show no observations
        # TODO check if AoT node or openAQ and get corresponding dataset
        # Suggestion: AoT nodes vsn start with "0" except one that starts with "8", OpenAQ vsn never start with a number
        
        
        #set the previous click
        if(!is.null(prev_input)){
          #get input type either map or table
          
          type <- strsplit(prev_input, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
          
          # if map input, get the vsn and the active status
          if(type=="map"){
            v$lastvsn <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          }
          # if table input, get the vsn and the active status
          else{
            prev_row_id <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
            prev_row <- nodes_table[prev_row_id,]
            
            #if the previous table node was inactive, set vsn as inactive
            if(prev_row$status=="Active")
              v$lastvsn <- prev_row$vsn
            else
              v$lastvsn <- "Inactive"
            
          }
        }
        else{
          v$lastvsn <-NULL
        }
        flag <- -1
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          #no data from openaq in tab 2 , so ignore
          flag <- 0
        }
        else
        {
          print("vsn:")
          print(vsn)
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df_aot <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df_aot <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df_aot <- get_and_preprocess_observations_7d(vsn)
          }
          df_aot <- as.data.frame(lapply(df_aot, unlist))
          retrieved_measures <- unique(df_aot$measure)
          print("RETRIEVED MEASURES:")
          print(retrieved_measures)
          levels(df_aot$measure)[levels(df_aot$measure)=="humidity"] <- "humidity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="intensity"] <- "intensity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="temperature"] <- "temperature(AOT)"
          '%ni%' <- Negate('%in%')
          df_aot <- subset(df_aot, uom %ni% c("dB","uW/cm^2"))
          flag <- 1
          labs <- names(df_aot)
          labs <- labs[ - which(names(labs) == c("vsn","year","month","day","uom"))]
        }
        if(time_range == TIME_RANGE_24HOURS){
          df <- get_and_preprocess_observations_24h_ds(lng,lat)
        } else if(time_range == TIME_RANGE_7DAYS){
          df <- get_and_preprocess_observations_7d_ds(lng,lat)
        }
        
        
        #print(retrieved_measures)
        
        keep <- c("hms","humidity","windSpeed","windBearing","cloudCover","pressure","ozone","temperature","visibility")
        
        df <- df[, (colnames(df) %in% keep)]
        print(df)
        
        #labs and labs2 are the required variables for AoT 
        #labs3 and labs4 are the required variables for openAQ (labs4 contains variable name and labs3 has unit also)
        labs <- list()
        labs2 <- list()
        labs4 <- list()
        labs1 <- names(df)
        labs3 <- list()
        print(labs3)
        #add check conditions:
        #suffx_pm2.5 = unique(subset(df, measure == "pm2.5")$uom)
        
        
        if(flag == 1)
        {
          if ("intensity" %in% c(input$measures1_ds,input$measures2_ds) && "intensity" %in% retrieved_measures){
            suffx_intensity = unique(subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$uom)
            labs <-c(labs,"intensity(AOT)"=paste("intensity(AOT)",suffx_intensity, sep=" "))
            labs2 = c(labs2,c("intensity(AOT)"))
          }
          
          
          if ("humidity" %in% c(input$measures1_ds,input$measures2_ds) && "humidity" %in% retrieved_measures){
            suffx_humidity = unique(subset(df_aot, measure == "humidity(AOT)")$uom)
            labs <-c(labs,"humidity(AOT)"=paste("humidity(AOT)",suffx_humidity, sep=" "))
            labs2 = c(labs2,c("humidity(AOT)"))
          }
          
          if ("temperature" %in% c(input$measures1_ds,input$measures2_ds) && "temperature" %in% retrieved_measures){
            if(input$switch_units){
              temp_suffx = "(Degrees Fahrenheit)"
              labs <-c(labs,"temperature(AOT)"= paste("temperature(AOT)",temp_suffx, sep=" "))
              labs2 = c(labs2,c("temperature(AOT)"))
            }
            else{
              temp_suffx = "(Degrees Celsius)"
              labs <-c(labs,"temperature(AOT)"= paste("temperature(AOT)",temp_suffx, sep=" "))
              labs2 = c(labs2,c("temperature(AOT)"))
            }
          }
        }
        
        if ("humidity" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_humidity = ""
          labs3 <-c(labs3,"humidity" = paste("humidity",suffx_humidity, sep=" "))
          labs4 <- c(labs4,"humidity")
        }
        if ("windSpeed" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_windSpeed = ""
          labs3 <-c(labs,"windSpeed" = paste("windSpeed",suffx_windSpeed, sep=" "))
          labs4 <- c(labs4,"windSpeed")
        }
        if ("windBearing" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_windBearing = ""
          labs3 <-c(labs3,"windBearing" = paste("windBearing",suffx_windBearing, sep=" "))
          labs4 <- c(labs4,"windBearing")
        }
        if ("cloudCover" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_cloudCover = ""
          labs3 <-c(labs3,"cloudCover" = paste("cloudCover",suffx_cloudCover, sep=" "))
          labs4 <- c(labs4,"cloudCover")
        }
        if ("visibility" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_visibility =""
          labs3 <-c(labs3,"visibility" = paste("visibility",suffx_visibility, sep=" "))
          labs4 <- c(labs4,"visibility")
        }
        if ("pressure" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_pressure = ""
          labs3 <-c(labs3,"pressure" = paste("pressure",suffx_pressure, sep=" "))
          labs4 <- c(labs4,"pressure")
        }
        if ("ozone" %in% c(input$measures1_ds,input$measures2_ds)){
          suffx_ozone  = ""
          labs3 <-c(labs3,"ozone" = paste("ozone",suffx_ozone, sep=" "))
          labs4 <- c(labs4,"ozone")
        }
        # if ("summary" %in% c(input$measures1_ds,input$measures2_ds)){
        #   suffx_summary  = ""
        #     labs3 <-c(labs3,"summary" = paste("summary",suffx_summary, sep=" "))
        #
        # }
        if ("temperature" %in% c(input$measures1_ds,input$measures2_ds)){
          if(input$switch_units){
            temp_suffx= df$""
            labs3 <-c(labs3,"temperature" = paste("temperature",suffx_summary, sep=" "))
            labs4 <- c(labs4,"temperature")
          }
        }
        else{
          temp_suffx  = ""
          labs3 <-c(labs3,"temperature"= paste("temperature",temp_suffx, sep=" "))
          labs4 <- c(labs4,"temperature")
        }
        
        convert_temp_to_metric <- function(values){
          return((values-32)/1.8)
        }
        #
        #final dataframe which is to be shown as table is df
        #keep only variables in lab3 and discard others
        
        df <- df[, (colnames(df) %in% unlist(c("hms",labs4)))]
        
        if(flag == 1)
        {
          drops <- c("vsn","year","month","day","uom")
          df_aot <- df_aot[ , !(names(df_aot) %in% drops)]
          '%ni%' <- Negate('%in%')
          df_aot <- subset(df_aot, measure %ni% c("co","h2s","no2","o3","so2","pm2.5","pm10"))
          df_aot <- spread(df_aot, measure, value)
          keep <- c("hms",labs2)
          df_aot <- df_aot[unlist(keep)]
          names(df_aot) <- unlist(c("hms",labs))
          
          df_combined <- merge(df_aot, df, by="hms", all =TRUE)
          df_combined <- df_combined %>% select(hms, everything())
        }
        else
        {
          if(length(names(df))>1)
          {
            df_combined <- df
            df_combined <- df_combined %>% select(hms, everything())
          }
          else
          {
            df_combined <- df
          }
        }
        
        labs <- unlist(labs)
        labs2 <- unlist(labs2)
        labs3 <- unlist(labs3)
        labs4 <- unlist(labs4)
        print(labs)
        print(labs2)
        print(labs3)
        print(labs4)
        #replace names of variables with variables+unit
        #darksky
        for(i in 1:length(labs3))
        {
          names(df_combined)[names(df_combined)==labs4[i]] <- labs3[i]
        }
        #aot
        for(i in 1:length(labs))
        {
          names(df_combined)[names(df_combined)==labs2[i]] <- labs[i]
        }
        if(length(names(df_combined))<=1)
        {
          DT::datatable({
            empty <- data.frame()
            empty
          })
        }
        else
        {
          
          DT::datatable({df_combined},options = list(searching = FALSE, pageLength =10, lengthChange = FALSE, order = list(1, 'desc')
          ), rownames = FALSE,
          caption = paste('Measures table for',vsn)
          )
        }
      }
    }
    
  })
  
  # Darksky table for current time
  output$table_ds <- DT::renderDataTable(
    if(is.null(input$map_marker_click$lat) && is.null(input$map_marker_click$lat)){
      DT::datatable({
        empty <- data.frame()
        empty
      })
    }
    else{
      DT::datatable({
        current_forecast = get_current_forecast(input$map_marker_click$lat, input$map_marker_click$lng,exclude="minutely,hourly,daily")
        curr = data.frame(current_forecast['currently'])
        names(curr) <- substring(names(curr),11,nchar((names(curr))))
        darksky_tracked_measures <- c("temperature", "humidity", "windSpeed", "windBearing", "cloudCover", "visibility", "pressure", "ozone", "summary")
        df <- data.frame(t(subset(curr,select = darksky_tracked_measures)))
        names(df) <- c("value")
        measures <-darksky_tracked_measures
        curr_data <- data.frame(measures = darksky_tracked_measures,
                                value = df)
      },
      options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE,
      caption = 'Current Time measures from Datasky'
      
      )
    }
  )
  
  # get current data for darksky
  
  get_and_preprocess_observations_ds <- function(lng,lat) {
    current_forecast = get_current_forecast(lat, lng,exclude="minutely,hourly,daily")
    curr = data.frame(current_forecast['currently'])
    names(curr) <- substring(names(curr),11,nchar((names(curr))))
    darksky_tracked_measures <- c("temperature", "humidity", "windSpeed", "windBearing", "cloudCover", "visibility", "pressure", "ozone", "summary")
    df <- data.frame(t(subset(curr,select = darksky_tracked_measures)))
    names(df) <- c("value")
    measures <-darksky_tracked_measures
    curr_data <- data.frame(measures = darksky_tracked_measures,
                            value = df)
  }
  
  
  # get current data for all the nodes for darksky
  
  get_and_preprocess_observations_all_nodes_ds <- function(){
    #get all active nodes from AoT and their coordinates and then query them
    
    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]
    
    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)
    
    res <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_ds)) %>% unnest()
    
    save_df_as_fst(res,"fst/all_nodes_current.fst")
    
    return(res)
  }
  
  
  #preprocess darksky data for last 24 hours
  get_and_preprocess_observations_24h_ds <- function(lng,lat){
    
    # print("in preprocess")
    now <-Sys.time()
    yes <-ymd_hms(now) - lubridate::hours(24)
    yes<-force_tz(yes, "America/Chicago")
    ds <-seq(yes, now,by="hour")[1:25]
    ds <-ymd_hms(ds)
    force_tz(ds, "America/Chicago")%>%
      map(~get_forecast_for(lng, lat,.x))%>%
      map_df("hourly")%>%
      distinct()%>%
      {. ->> response}
    response$time<-ymd_hms(response$time)
    response$time<-force_tz(response$time, "America/Chicago")
    res <-tail(filter(response,(day(response$time)<day(now) | (day(response$time)==day(now) & hour(response$time)<=hour(now)))),24)
    res <- extract_date_fields_h(res)
    return (res)
  }
  
  #preprocess darksky data for last 24 hours for all nodes
  
  get_and_preprocess_observations_24h_all_nodes_ds <- function(lng,lat){
    
    #get all active nodes from AoT and their coordinates and then query them
    
    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]
    
    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)
    
    res <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_24h_ds)) %>% unnest()
    
    save_df_as_fst(res,"fst/all_nodes_24hours.fst")
    
    # print(head(res))
    
    
    return (res)
  }
  
  #preprocess darksky data for last 7 days
  get_and_preprocess_observations_7d_ds <- function(lng,lat){
    
    seq(Sys.Date()-7, Sys.Date(), "1 day") %>%
      map(~get_forecast_for(lng, lat, .x)) %>%
      map_df("daily") %>%
      {. ->> last_7 }
    last_7 <- extract_date_fields_d(last_7)
    return (last_7)
  }
  
  #preprocess darksky data for last 7 days for all nodes
  get_and_preprocess_observations_7d_all_nodes_ds <- function(lng,lat){
    
    #get all active nodes from AoT and their coordinates and then query them
    
    active_nodes <- nodes_table[nodes_table$status=="Active",][,c("vsn","longitude","latitude")]
    
    lng <- c(active_nodes$longitude)
    lat <- c(active_nodes$latitude)
    
    df <- mutate(active_nodes,map2(lng,lat,get_and_preprocess_observations_7d_ds)) %>% unnest()
    
    save_df_as_fst(df,"fst/all_nodes_7days.fst")
    
    return (df)
  }
  
  
  #Darksky graphical data
  #This takes the input from AoT table and maps markets
  
  output$graphical_data_ds <- renderPlot({
    autoInvalidate45()
    vsn_ <- v$vsn
    
    if(!is.null(vsn_)){
      #get input type either map or table
      
      type <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
      
      # if map input, get the vsn and the active status
      if(type=="map"){
        vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        #determine if aot or openaq node was clicked
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #OpenAQ node
          vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          lat <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[4]][2]
          lng <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[5]][2]
          active <- "active"
        }
        else
        {
          active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
          lat <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[4]][2]
          lng <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[5]][2]
        }
        #check the size of the map_inputs if it is less than 2.
        if(length(v$map_inputs)<2){
          prev_input <-NULL
        }
        else{
          last_two <- tail(v$map_inputs,2)
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
        
      }
      # if table input, get the vsn and the active status
      else{
        row_id <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        selected_row <- nodes_table[row_id,]
        active <- selected_row$status
        vsn <- selected_row$vsn
        
        #get the last two clicks
        
        #check the size of the map_inputs if it is less than 2.
        if(length(v$table_inputs)<2)
          prev_input <-NULL
        else{
          last_two <- tail(v$table_inputs,2)
          
          prev <- last_two[[1]]
          prev_input <- prev[1]
        }
      }
    }
    else
      vsn <-NULL
    
    if(is.null(vsn)){
      plot_title <- "No node selected"
      
      gl <- ggplot() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
          axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
          plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#0d2025"),
          legend.background = element_rect(color = NA, fill = "#0d2025"),
          legend.key = element_rect(color = NA, fill = "#0d2025"),
          panel.background = element_rect(fill = "#0d2025", color  =  NA),
          panel.grid.major = element_line(color = "#FFFFFF"),
          panel.grid.minor = element_line(color = "#FFFFFF"),
          legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
          legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
        )+labs(title=plot_title,x = "Time", y = "Measurement")
      
      gl
    }
    else {
      
      time_range <- input$time_range #changed from time_range_ds
      
      if(!(active == "Inactive")){
        #get input type either map or table
        if(!is.null(prev_input)){
          type <- strsplit(prev_input, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
          
          # if map input, get the vsn and the active status
          if(type=="map"){
            v$lastvsn <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
          }
          # if table input, get the vsn and the active status
          else{
            prev_row_id <- strsplit(prev_input , " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
            prev_row <- nodes_table[prev_row_id,]
            
            #if the previous table node was inactive, set vsn as inactive
            if(prev_row$status=="Active")
              v$lastvsn <- prev_row$vsn
            else
              v$lastvsn <- "Inactive"
            
          }
        }
        else{
          v$lastvsn <-NULL
        }
        flag <- -1
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          #no data from openaq in tab 2 , so ignore
          flag <- 0
        }
        else
        {
          print("vsn:")
          print(vsn)
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df_aot <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df_aot <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df_aot <- get_and_preprocess_observations_7d(vsn)
          }
          df_aot <- as.data.frame(lapply(df_aot, unlist))
          retrieved_measures <- unique(df_aot$measure)
          print("RETRIEVED MEASURES:")
          print(retrieved_measures)
          levels(df_aot$measure)[levels(df_aot$measure)=="humidity"] <- "humidity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="intensity"] <- "intensity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="temperature"] <- "temperature(AOT)"
          flag <- 1
        }
        
        
        
        if(time_range == TIME_RANGE_24HOURS){
          df <- get_and_preprocess_observations_24h_ds(lng,lat)
        } else if(time_range == TIME_RANGE_7DAYS){
          df <- get_and_preprocess_observations_7d_ds(lng,lat)
        }
        retrieved_measures_darksky <- names(df)
        if(time_range == TIME_RANGE_CURRENT){
          plot_title <- paste("Current (or most recent) data for node:",as.character(vsn))
        } else if(time_range == TIME_RANGE_24HOURS){
          plot_title <- paste("Last 24 hours data for node:",as.character(vsn))
        } else {
          plot_title <- paste("Last 7 days data for node:",as.character(vsn))
        }
        
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        labs <-c()
        vals <-c()
        if ("humidity" %in% c(input$measures1_ds,input$measures2_ds) && "humidity" %in% retrieved_measures_darksky){
          suffx_humidity = ""
          gl <- gl + geom_line(aes(y = df$humidity , x= df$hms, color = "humidity"), size = line_size(), group = 1) +
            geom_point(aes(y=df$humidity, x= df$hms , color = "humidity"), size = line_size()*3)
          labs <-c(labs,"humidity" = paste("humidity",suffx_humidity, sep=" "))
          vals <-c(vals,"humidity" = "#a6cee3")
          
        }
        
        if(flag == 1)
        {
          if ("intensity" %in% c(input$measures1_ds,input$measures2_ds) && "intensity" %in% retrieved_measures){
            suffx_intensity = unique(subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$uom)
            gl <- gl + geom_line(aes(subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$value, x = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$hms, color = "intensity(AOT)"), size = line_size(), group = 4) +
              geom_point(aes( y = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$value, x = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$hms , color = "intensity(AOT)"), size = line_size()*3)
            labs <-c(labs,"intensity(AOT)" = paste("intensity(AOT)",suffx_intensity, sep=" "))
            vals <-c(vals,"intensity(AOT)" = "#a3d659")
          }
          
          
          if ("humidity" %in% c(input$measures1_ds,input$measures2_ds) && "humidity" %in% retrieved_measures){
            suffx_humidity = unique(subset(df_aot, measure == "humidity(AOT)")$uom)
            # y = subset(df, measure == "humidity")$value
            gl <- gl + geom_line(aes(y = subset(df_aot, measure == "humidity(AOT)")$value, x = subset(df_aot, measure == "humidity(AOT)")$hms, color = "humidity(AOT)"), size = line_size(), group = 3) +
              geom_point(aes(y = subset(df_aot, measure == "humidity(AOT)")$value, x = subset(df_aot, measure == "humidity(AOT)")$hms , color = "humidity(AOT)"), size = line_size()*3)
            labs <-c(labs,"humidity(AOT)" = paste("humidity(AOT)",suffx_humidity, sep=" "))
            vals <-c(vals,"humidity(AOT)" = "#194649")
          }
          
          if ("temperature" %in% c(input$measures1_ds,input$measures2_ds) && "temperature" %in% retrieved_measures){
            if(input$switch_units){
              temp_suffx = "(Degrees Fahrenheit)"
              gl <- gl + geom_line(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms, color = "temperature(AOT)"), size = line_size(), group = 2) +
                geom_point(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms , color = "temperature(AOT)"), size = line_size()*3)
              labs <-c(labs,"temperature(AOT)" = paste("temperature(AOT)",suffx_humidity, sep=" "))
              vals <-c(vals,"temperature(AOT)" = "#6B1F13")
            }
            else{
              temp_suffx = "(Degrees Celsius)"
              gl <- gl + geom_line(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms, color = "temperature(AOT)"), size = line_size(), group = 2) +
                geom_point(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms , color = "temperature(AOT)"), size = line_size()*3)
              labs <-c(labs,"temperature(AOT)"= paste("temperature(AOT)",temp_suffx, sep=" "))
              vals <-c(vals,"temperature(AOT)" = "#6B1F13")
            }
          }
        }
        
        if ("windSpeed" %in% c(input$measures1_ds,input$measures2_ds) && "windSpeed" %in% retrieved_measures_darksky){
          suffx_windSpeed = ""
          gl <- gl + geom_line(aes(y= df$windSpeed, x= df$hms, color = "windSpeed"), size = line_size(), group = 2) +
            geom_point(aes(y= df$windSpeed, x= df$hms , color = "windSpeed"), size = line_size()*3)
          labs <-c(labs,"windSpeed" = paste("windSpeed",suffx_windSpeed, sep=" "))
          vals <-c(vals,"windSpeed" = "#1f78b4")
          
        }
        if ("windBearing" %in% c(input$measures1_ds,input$measures2_ds) && "windBearing" %in% retrieved_measures_darksky){
          suffx_windBearing = ""
          gl <- gl + geom_line(aes(y= df$windBearing, x= df$hms, color = "windBearing"), size = line_size(), group = 3) +
            geom_point(aes(y= df$windBearing, x= df$hms , color = "windBearing"), size = line_size()*3)
          labs <-c(labs,"windBearing" = paste("windBearing",suffx_windBearing, sep=" "))
          vals <-c(vals,"windBearing" = "#b2df8a")
          
        }
        if ("cloudCover" %in% c(input$measures1_ds,input$measures2_ds) && "cloudCover" %in% retrieved_measures_darksky){
          suffx_cloudCover = ""
          gl <- gl + geom_line(aes(y= df$cloudCover,x=df$hms, color = "cloudCover"), size = line_size(), group = 4) +
            geom_point(aes(y= df$cloudCover, x= df$hms , color = "cloudCover"), size = line_size()*3)
          labs <-c(labs,"cloudCover" = paste("cloudCover",suffx_cloudCover, sep=" "))
          vals <-c(vals,"cloudCover" = "#33a02c")
          
        }
        if ("visibility" %in% c(input$measures1_ds,input$measures2_ds) && "visibility" %in% retrieved_measures_darksky){
          suffx_visibility =""
          gl <- gl + geom_line(aes(y= df$visibility, x= df$hms, color = "visibility"), size = line_size(), group = 5) +
            geom_point(aes(y= df$visibility, x= df$hms , color = "visibility"), size = line_size()*3)
          labs <-c(labs,"visibility" = paste("visibility",suffx_visibility, sep=" "))
          vals <-c(vals,"visibility" = "#fb9a99")
          
        }
        if ("pressure" %in% c(input$measures1_ds,input$measures2_ds) && "pressure" %in% retrieved_measures_darksky){
          suffx_pressure = ""
          gl <- gl + geom_line(aes(y= df$pressure, x= df$hms, color = "pressure"), size = line_size(), group = 6) +
            geom_point(aes(y=df$pressure, x =df$hms , color = "pressure"), size = line_size()*3)
          labs <-c(labs,"pressure" = paste("pressure",suffx_pressure, sep=" "))
          vals <-c(vals,"pressure" = "#e31a1c")
          
        }
        if ("ozone" %in% c(input$measures1_ds,input$measures2_ds) && "ozone" %in% retrieved_measures_darksky){
          suffx_ozone  = ""
          gl <- gl + geom_line(aes(y= df$ozone, x= df$hms, color = "ozone"), size = line_size(), group = 7) +
            geom_point(aes(y=df$ozone, x= df$hms , color = "ozone"), size = line_size()*3)
          labs <-c(labs,"ozone" = paste("ozone",suffx_ozone, sep=" "))
          vals <-c(vals,"ozone" = "#fdbf6f")
          
        }
        # if ("summary" %in% c(input$measures1_ds,input$measures2_ds)){
        #   suffx_summary  = ""
        #     gl <- gl + geom_line(aes(y= df$summary, x= df$hms, color = "summary"), size = line_size(), group = 8) +
        #       geom_point(aes(y= df$summary, x= df$hms , color = "summary"), size = line_size()*3)
        #     labs <-c(labs,"summary" = paste("summary",suffx_summary, sep=" "))
        #     vals <-c(vals,"summary" = "#cab2d6")
        #
        # }
        if ("temperature" %in% c(input$measures1_ds,input$measures2_ds) && "temperature" %in% retrieved_measures_darksky){
          if(input$switch_units){
            temp_suffx= df$""
            gl <- gl + geom_line(aes(y = df$temperature, x= df$hms, color = "temperature"), size = line_size(), group = 9) +
              geom_point(aes(y = df$temperature, x= df$hms , color = "temperature"), size = line_size()*3)
            labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
            vals <-c(vals,"temperature" = "#ff7f00")
          }
          else{
            # s_county$data_conv <-s_county$"Temperature"
            # s_county$data_conv <- convert_temp_to_metric(s_county$data_conv)
            # names(s_county)[names(s_county)=="data_conv"] <- paste("Temperature","conv",sep="_")
            
            temp_suffx  = ""
            gl <- gl + geom_line(aes(y= df$temperature, x= df$hms, color = "temperature"), size = line_size(), group = 9) +
              geom_point(aes(y= df$temperature, x= df$hms , color = "temperature"), size = line_size()*3)
            labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
            vals <-c(vals,"temperature" = "#ff7f00")
          }
        }
        print(vals)
        gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                      values = vals)
        gl
      }
      else {
        plot_title <- "This node has no observations"
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        gl
      }
    }
  })
  
  
  
  #####################################################  GRAPHICAL DATA DARKSKY COMPARISON    #####################################################
  
  # Darksky Second plot for comparison
  output$graphical_data_last_ds <- renderPlot({
    autoInvalidate50()
    time_range <- input$time_range
    irrelevant_variable <- input$map_marker_click
    
    vsn <- isolate(v$lastvsn)
    print("VSN for darksky previous")
    print(vsn)
    
    # vsn <- input$map_marker_click
    #COMMENTED THIS BECAUSE IT MAKES NO SENSE FOR COMPARISON GRAPH
    # if(!is.null(input$map_marker_click)){
    #   vsn <- isolate(v$lastvsn) #TODO A: Revert to v$lastvsn_dark if this causes problems
    # }
    # else if(!is.null(input$nodes_table_rows_selected)){
    #   vsn <- isolate(v$lastvsn) #TODO A: Revert to v$lastvsn_dark if this causes problems
    # }
    # else
    #   vsn <- NULL
    # || input$switch_compare
    if(is.null(vsn)){
      
      plot_title <- "No node selected for previous output"
      
      gl <- ggplot() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
          axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
          plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#0d2025"),
          legend.background = element_rect(color = NA, fill = "#0d2025"),
          legend.key = element_rect(color = NA, fill = "#0d2025"),
          panel.background = element_rect(fill = "#0d2025", color  =  NA),
          panel.grid.major = element_line(color = "#FFFFFF"),
          panel.grid.minor = element_line(color = "#FFFFFF"),
          legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
          legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
        )+labs(title=plot_title,x = "Time", y = "Measurement")
      
      gl
    }
    else {
      #determine if aot or openaq node was clicked
      if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
      {
        #OpenAQ node
        #vsn <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
        lat <- subset(nodes_oaq,vsn==vsn)$latitude[1]
        lng <- subset(nodes_oaq,vsn==vsn)$longitude[1]
        active <- "active"
      }
      else
      {
        #active <- strsplit(vsn_, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][3]
        lat <- subset(nodes,address==vsn)$latitude[1]
        lng <- subset(nodes,address==vsn)$longitude[1]
        active <- "active"
      }
      if(!(vsn == "Inactive")){
        
        flag <- -1
        if(!grepl("[^A-Za-z]", substring(vsn, 1, 1)))
        {
          #openaq
          #no data from openaq in tab 2 , so ignore
          flag <- 0
        }
        else
        {
          print("vsn:")
          print(vsn)
          # df <- get_and_preprocess_observations(vsn)
          if(time_range == TIME_RANGE_CURRENT){
            df_aot <- get_and_preprocess_observations(vsn)
          } else if(time_range == TIME_RANGE_24HOURS){
            df_aot <- get_and_preprocess_observations_24h(vsn)
          } else if(time_range == TIME_RANGE_7DAYS){
            df_aot <- get_and_preprocess_observations_7d(vsn)
          }
          df_aot <- as.data.frame(lapply(df_aot, unlist))
          retrieved_measures <- unique(df_aot$measure)
          print("RETRIEVED MEASURES:")
          print(retrieved_measures)
          levels(df_aot$measure)[levels(df_aot$measure)=="humidity"] <- "humidity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="intensity"] <- "intensity(AOT)"
          levels(df_aot$measure)[levels(df_aot$measure)=="temperature"] <- "temperature(AOT)"
          flag <- 1
        }
        
        #This is darksky preprocessing
        if(time_range == TIME_RANGE_24HOURS){
          df <- get_and_preprocess_observations_24h_ds(lng,lat)
        } else if(time_range == TIME_RANGE_7DAYS){
          df <- get_and_preprocess_observations_7d_ds(lng,lat)
        }
        retrieved_measures_darksky <- names(df)
        if(time_range == TIME_RANGE_CURRENT){
          plot_title <- paste("Current (or most recent) data for node:",as.character(vsn))
        } else if(time_range == TIME_RANGE_24HOURS){
          plot_title <- paste("Last 24 hours data for node:",as.character(vsn))
        } else {
          plot_title <- paste("Last 7 days data for node:",as.character(vsn))
        }
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        labs <-c()
        vals <-c()
        if ("humidity" %in% c(input$measures1_ds,input$measures2_ds) && "humidity" %in% retrieved_measures_darksky){
          suffx_humidity = ""
          gl <- gl + geom_line(aes(y = df$humidity , x= df$hms, color = "humidity"), size = line_size(), group = 1) +
            geom_point(aes(y=df$humidity, x= df$hms , color = "humidity"), size = line_size()*3)
          labs <-c(labs,"humidity" = paste("humidity",suffx_humidity, sep=" "))
          vals <-c(vals,"humidity" = "#a6cee3")
          
        }
        
        if(flag == 1)
        {
          if ("intensity" %in% c(input$measures1_ds,input$measures2_ds) && "intensity" %in% retrieved_measures){
            suffx_intensity = unique(subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$uom)
            gl <- gl + geom_line(aes(subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$value, x = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$hms, color = "intensity(AOT)"), size = line_size(), group = 4) +
              geom_point(aes( y = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$value, x = subset(df_aot, measure == "intensity(AOT)" & uom == "lux")$hms , color = "intensity(AOT)"), size = line_size()*3)
            labs <-c(labs,"intensity(AOT)" = paste("intensity(AOT)",suffx_intensity, sep=" "))
            vals <-c(vals,"intensity(AOT)" = "#a3d659")
          }
          
          
          if ("humidity" %in% c(input$measures1_ds,input$measures2_ds) && "humidity" %in% retrieved_measures){
            suffx_humidity = unique(subset(df_aot, measure == "humidity(AOT)")$uom)
            # y = subset(df, measure == "humidity")$value
            gl <- gl + geom_line(aes(y = subset(df_aot, measure == "humidity(AOT)")$value, x = subset(df_aot, measure == "humidity(AOT)")$hms, color = "humidity(AOT)"), size = line_size(), group = 3) +
              geom_point(aes(y = subset(df_aot, measure == "humidity(AOT)")$value, x = subset(df_aot, measure == "humidity(AOT)")$hms , color = "humidity(AOT)"), size = line_size()*3)
            labs <-c(labs,"humidity(AOT)" = paste("humidity(AOT)",suffx_humidity, sep=" "))
            vals <-c(vals,"humidity(AOT)" = "#194649")
          }
          
          if ("temperature" %in% c(input$measures1_ds,input$measures2_ds) && "temperature" %in% retrieved_measures){
            if(input$switch_units){
              temp_suffx = "(Degrees Fahrenheit)"
              gl <- gl + geom_line(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms, color = "temperature(AOT)"), size = line_size(), group = 2) +
                geom_point(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms , color = "temperature(AOT)"), size = line_size()*3)
              labs <-c(labs,"temperature(AOT)" = paste("temperature(AOT)",suffx_humidity, sep=" "))
              vals <-c(vals,"temperature(AOT)" = "#6B1F13")
            }
            else{
              temp_suffx = "(Degrees Celsius)"
              gl <- gl + geom_line(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms, color = "temperature(AOT)"), size = line_size(), group = 2) +
                geom_point(aes(y = subset(df_aot, measure == "temperature(AOT)")$value, x = subset(df_aot, measure == "temperature(AOT)")$hms , color = "temperature(AOT)"), size = line_size()*3)
              labs <-c(labs,"temperature(AOT)"= paste("temperature(AOT)",temp_suffx, sep=" "))
              vals <-c(vals,"temperature(AOT)" = "#6B1F13")
            }
          }
        }
        
        if ("windSpeed" %in% c(input$measures1_ds,input$measures2_ds) && "windSpeed" %in% retrieved_measures_darksky){
          suffx_windSpeed = ""
          gl <- gl + geom_line(aes(y= df$windSpeed, x= df$hms, color = "windSpeed"), size = line_size(), group = 2) +
            geom_point(aes(y= df$windSpeed, x= df$hms , color = "windSpeed"), size = line_size()*3)
          labs <-c(labs,"windSpeed" = paste("windSpeed",suffx_windSpeed, sep=" "))
          vals <-c(vals,"windSpeed" = "#1f78b4")
          
        }
        if ("windBearing" %in% c(input$measures1_ds,input$measures2_ds) && "windBearing" %in% retrieved_measures_darksky){
          suffx_windBearing = ""
          gl <- gl + geom_line(aes(y= df$windBearing, x= df$hms, color = "windBearing"), size = line_size(), group = 3) +
            geom_point(aes(y= df$windBearing, x= df$hms , color = "windBearing"), size = line_size()*3)
          labs <-c(labs,"windBearing" = paste("windBearing",suffx_windBearing, sep=" "))
          vals <-c(vals,"windBearing" = "#b2df8a")
          
        }
        if ("cloudCover" %in% c(input$measures1_ds,input$measures2_ds) && "cloudCover" %in% retrieved_measures_darksky){
          suffx_cloudCover = ""
          gl <- gl + geom_line(aes(y= df$cloudCover,x=df$hms, color = "cloudCover"), size = line_size(), group = 4) +
            geom_point(aes(y= df$cloudCover, x= df$hms , color = "cloudCover"), size = line_size()*3)
          labs <-c(labs,"cloudCover" = paste("cloudCover",suffx_cloudCover, sep=" "))
          vals <-c(vals,"cloudCover" = "#33a02c")
          
        }
        if ("visibility" %in% c(input$measures1_ds,input$measures2_ds) && "visibility" %in% retrieved_measures_darksky){
          suffx_visibility =""
          gl <- gl + geom_line(aes(y= df$visibility, x= df$hms, color = "visibility"), size = line_size(), group = 5) +
            geom_point(aes(y= df$visibility, x= df$hms , color = "visibility"), size = line_size()*3)
          labs <-c(labs,"visibility" = paste("visibility",suffx_visibility, sep=" "))
          vals <-c(vals,"visibility" = "#fb9a99")
          
        }
        if ("pressure" %in% c(input$measures1_ds,input$measures2_ds) && "pressure" %in% retrieved_measures_darksky){
          suffx_pressure = ""
          gl <- gl + geom_line(aes(y= df$pressure, x= df$hms, color = "pressure"), size = line_size(), group = 6) +
            geom_point(aes(y=df$pressure, x =df$hms , color = "pressure"), size = line_size()*3)
          labs <-c(labs,"pressure" = paste("pressure",suffx_pressure, sep=" "))
          vals <-c(vals,"pressure" = "#e31a1c")
          
        }
        if ("ozone" %in% c(input$measures1_ds,input$measures2_ds) && "ozone" %in% retrieved_measures_darksky){
          suffx_ozone  = ""
          gl <- gl + geom_line(aes(y= df$ozone, x= df$hms, color = "ozone"), size = line_size(), group = 7) +
            geom_point(aes(y=df$ozone, x= df$hms , color = "ozone"), size = line_size()*3)
          labs <-c(labs,"ozone" = paste("ozone",suffx_ozone, sep=" "))
          vals <-c(vals,"ozone" = "#fdbf6f")
          
        }
        # if ("summary" %in% c(input$measures1_ds,input$measures2_ds)){
        #   suffx_summary  = ""
        #     gl <- gl + geom_line(aes(y= df$summary, x= df$hms, color = "summary"), size = line_size(), group = 8) +
        #       geom_point(aes(y= df$summary, x= df$hms , color = "summary"), size = line_size()*3)
        #     labs <-c(labs,"summary" = paste("summary",suffx_summary, sep=" "))
        #     vals <-c(vals,"summary" = "#cab2d6")
        #
        # }
        if ("temperature" %in% c(input$measures1_ds,input$measures2_ds) && "temperature" %in% retrieved_measures_darksky){
          if(input$switch_units){
            temp_suffx= df$""
            gl <- gl + geom_line(aes(y = df$temperature, x= df$hms, color = "temperature"), size = line_size(), group = 9) +
              geom_point(aes(y = df$temperature, x= df$hms , color = "temperature"), size = line_size()*3)
            labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
            vals <-c(vals,"temperature" = "#ff7f00")
          }
          else{
            # s_county$data_conv <-s_county$"Temperature"
            # s_county$data_conv <- convert_temp_to_metric(s_county$data_conv)
            # names(s_county)[names(s_county)=="data_conv"] <- paste("Temperature","conv",sep="_")
            
            temp_suffx  = ""
            gl <- gl + geom_line(aes(y= df$temperature, x= df$hms, color = "temperature"), size = line_size(), group = 9) +
              geom_point(aes(y= df$temperature, x= df$hms , color = "temperature"), size = line_size()*3)
            labs <-c(labs,"temperature"= paste("temperature",temp_suffx, sep=" "))
            vals <-c(vals,"temperature" = "#ff7f00")
          }
        }
        gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                      values = vals)
        gl
      } else {
        plot_title <- "This node has no observations"
        
        gl <- ggplot() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(size = axis_title_size(),color = "#FFFFFF"),
            axis.title.x = element_text(size = axis_title_size(),color = "#FFFFFF"),
            plot.title = element_text(color = "#FFFFFF",size = axis_title_size(),hjust = 0.5),
            panel.border = element_blank(),
            plot.background = element_rect(color = NA, fill = "#0d2025"),
            legend.background = element_rect(color = NA, fill = "#0d2025"),
            legend.key = element_rect(color = NA, fill = "#0d2025"),
            panel.background = element_rect(fill = "#0d2025", color  =  NA),
            panel.grid.major = element_line(color = "#FFFFFF"),
            panel.grid.minor = element_line(color = "#FFFFFF"),
            legend.text = element_text(size = legend_text_size(), color = "#FFFFFF"),
            legend.key.size = unit(legend_key_size(), 'line'),
            axis.text = element_text(size = axis_text_size(), color = "#FFFFFF"),
            legend.title = element_text(size = legend_title_size(), color = "#FFFFFF")
          )+labs(title=plot_title,x = "Time", y = "Measurement")
        
        gl
      }
    }
  })
  
  
  # AoT sensor nodes table
  output$nodes_table <- DT::renderDataTable(
    DT::datatable({
      tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")
      
      # nodes[nodes_with_no_data,nodes$status] <-"inactive"
      
      cols <- c("vsn","address","status")
    nodes_main <-nodes_table %>% dplyr::select(cols)
      #show only the selected measures infomration
      selected = c(input$measures1_sites,input$measures2_sites)
      for(measure in selected){
        nodes_main[[measure]] <- nodes_table[[measure]]
      }
      nodes_main <- nodes_main[which(apply(nodes_main, 1, function(r) any(r %in% c("TRUE")))),]
      nodes_main
    }
    ,
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE,
      caption = 'Nodes infomration for the various sensors availability',selection = "single"
    )
  )
  
  # AoT sensor nodes table
  output$tab1_table <- DT::renderDataTable(
    DT::datatable({
      tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")
      
      # nodes[nodes_with_no_data,nodes$status] <-"inactive"
      
      cols <- c("vsn","address","status")
      nodes_main <-nodes_table %>% select(cols)
      #show only the selected measures infomration
      selected = c(input$measures1_sites,input$measures2_sites)
      for(measure in selected){
        nodes_main[[measure]] <- nodes_table[[measure]]
      }
      nodes_main <- nodes_main[which(apply(nodes_main, 1, function(r) any(r %in% c("TRUE")))),]
      nodes_main
    }
    ,
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE,
    caption = 'Nodes information for the various sensors availability',selection = "single"
    )
  )
  
  
  # About HTML
  output$about_out <- renderUI({
    author <- "<h1>Mirko Mantovani - Ashwani Khemani - Abhishek Vasudevan</h1>
    
    <a href='https://mirkomantovani.com/projects/ChicagoRealTimeAQ'>Project webpage</a>
    <br/>
    <a href='https://github.com/mirkomantovani/Chicago-RealTime-AQ'>Github repository</a><br>"
    libraries <- "<b>Used R libraries: </b> <br>
    <ul>
    <li>shiny</li>
    <li>shinydashboard, dashboardthemes, shinythemes, ggthemes</li>
    <li>ggplot2, plotly, ggvis, leaflet</li>
    <li>colourpicker, viridis, RColorBrewer</li>
    <li>geojson, geojsonio</li>
    <li>cdlTools, htmltools</li>
    <li>scales</li>
    <li>shinyalert</li>
    <li>rgdal</li>
    <li>reshape2</li>
    <li>future</li>
    <li>dplyr</li>
    <li>tidyr</li>
    <li>fst</li>
    <li>rvest</li>
    </ul>"
    data <- "<b>Dataset Source:</b></br> <a href='https://aqs.epa.gov/aqsweb/airdata/download_files.html'>United States Environmental Protection Agency</a><br>
    <a href='http://eric.clst.org/tech/usgeojson/e'>United States Counties shape in GeoJSON</a>"
    HTML(paste(author, libraries, data))
  })
  # End of server
}

shinyApp(ui = ui, server = server)
