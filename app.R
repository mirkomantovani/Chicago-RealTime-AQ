# Mirko Mantovani - Ashwani Khemani - Abhishek Vasudevan - 02/20/2019

# libraries
library(shiny)
library(devtools)
# library(ggplot2)
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
library(plotly)
library(RColorBrewer)
library(reshape2)
library(fst)
library(future)
library(data.table)
# library(ggvis)
library(dplyr)
library(tidyr)

# R data APIs libraries
library(ropenaq)
library(darksky)

library(base)
Sys.setenv(DARKSKY_API_KEY = "17b13339acc2cb53e53ea50ea4142528")

# importing datasets
# setwd("./csv/")
# temp = list.files(pattern="*.csv")
# datasets = lapply(temp, read.csv)
# dataset <- do.call(rbind, datasets)
# setwd("../")

# daily_df <- read_fst("fst/daily_all_aqi_by_county.fst")
# names(daily_df) <- c("state","county","aqi","category","pollutant","year","month","day")

# daily_all <- read_fst("fst/daily_all_pollutants_2018.fst")

# df <- read_fst("fst/hourly_all_data_2018.fst")

# needed for counties coordinates
sites <- fread(file = "sites/aqs_sites.csv", sep=",",header = TRUE)
# geojson file for counties shape

f_xy <- future({
  xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")
  # Since the xy has factored FIPS code for state instead of names, converting them in numeric and then
  # getting the names
  converted_states_names <- fips(as.numeric(levels(xy$STATE))[xy$STATE],to="name")
  xy$STATENAME<-converted_states_names
  xy
}) %plan% multiprocess


########################################### PREPROCESSING and VARIABLES DEFINITION #########################################

years<-c(1990:2018)
H_years<-c(2018) #years available for hourly data
H_years_italy<-c(2018,2019)
H_months<-c("January","February","March","April","May","June","July","August","September","October","November","December")

# H_days<-unique(df$Day)
# Constants
TIME_RANGE_CURRENT = "Current"
TIME_RANGE_24HOURS = "Last 24 hours"
TIME_RANGE_7DAYS = "Last 7 days"

UPDATE_NODES_STATUS <- FALSE


top12 <- c("Cook - Illinois","Hawaii - Hawaii","New York - New York","Los Angeles - California", "King - Washington","Harris - Texas","Miami - Dade-Florida",
           "San Juan - New Mexico","Hennepin - Minnesota","Wake - North Carolina", "San Francisco - California", "Maricopa - Arizona")
pollutants <- c("CO","NO2","Ozone","SO2","PM2.5","PM10")
pollutants_2 <- c("PM2.5","PM10","CO","NO2","Ozone","SO2")
time_ranges <- c(TIME_RANGE_CURRENT,TIME_RANGE_24HOURS,TIME_RANGE_7DAYS)
tracked_measures <- c("co","h2s","no2","o3","so2","pm2.5","pm10","temperature","humidity","intensity")

statistics <- c("Median","Max","90th percentile")

pie_chart_backgrounds <- "white" #bcdae0  1a4756
bar_chart_backgrounds <- "#bcdae0" #bcdae0
pie_chart_backgrounds_first <- "#bcdae0" #bcdae0

# All counties with state
all_counties <- c()
# for(s in states){
#   coun <- subset(dataset, State == s)
#   counti<-unique(coun$County)
#   counti <- paste(counti,"-",s)
#   all_counties <- c(all_counties,counti)
# }

############################################### UI ################################################

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
      span(h2("  Main Menu", style = "margin-left: 10px; font-size: 20px;")),
      menuItem("Geospatial Visualizations", tabName = "geospatial_viz"),
      menuItem("Tabular Visualizations", tabName = "tabular_viz"),

      menuItem("Inputs",
               materialSwitch(inputId = "switch_units", label = "Switch to Imperial units", status = "primary"),
               materialSwitch(inputId = "nodes_location", label = "Visualize sensor nodes", status = "primary"),
               materialSwitch(inputId = "heat_map", label = "Visualize heat map", status = "primary"),
               startExpanded = TRUE),
      menuItem("About", tabName = "about")

    ),
    # custom CSS
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
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 500, height = "auto",

                              h2("Node Data"),
                              selectizeInput(inputId = "time_range", "Select time range", time_ranges, selected = time_ranges[1],width = "100%"),
                              tabsetPanel(
                                tabPanel("Graphical",
                                         plotOutput("graphical_data",height = "20vmin")
                                         
                                ),
                                tabPanel("Tabular",
                                         h1("Wip2")
                                )
                              ),
                              checkboxGroupButtons(
                                inputId = "measures1",
                                choices = tracked_measures[1:5],
                                justified = TRUE, status = "primary", selected = tracked_measures[1:5],
                                checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                              ),
                              checkboxGroupButtons(
                                inputId = "measures2", 
                                choices = tracked_measures[6:10],
                                justified = TRUE, status = "primary", selected = tracked_measures[6:10],
                                checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                              ),
                              materialSwitch(inputId = "switch_compare", label = "Compare nodes data", status = "primary")                             
                              # div( id="yearly_inputs",
                              #      selectizeInput(inputId = "D_month", "Select Month", H_months, selected = 'January',width = "100%"),
                              #      selectizeInput(inputId = "D_day", "Select Day", H_days, selected = '1',width = "100%")
                              # )
                ),

                absolutePanel(id = "counties_panel", class = "panel panel-default", fixed = TRUE,
                              draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = -40,
                              width = 330, height = "auto",
                              h2("Shown counties"),
                              knobInput(
                                inputId = "num_counties",
                                label = "Select number of counties",
                                value = 1000,
                                min = 0,
                                max = 1100,
                                displayPrevious = TRUE,
                                lineCap = "round",
                                fgColor = "#428BCA",
                                inputColor = "#428BCA"
                              ),
                              sliderInput(inputId = "Opacity",
                                          sep = "",
                                          label = "Confidence level control",
                                          step = 0.1,
                                          value = 0, min = 0, max = 1
                                          # ,width = "90%"
                              )
                ),

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
  
  get_and_preprocess_observations <- function(vsn){
    df1 <- ls.observations(filters=list(node=vsn))
    
    # If only the current time is requested then filter out all unnecessary timestamp and keep only obs from the most recent
    # if(time_range == TIME_RANGE_CURRENT){
    #   df1 <- subset(df1, timestamp == df1$timestamp[1])
    # }
    # filter out nodes not yet deployed
    df <- data.frame(df1$node_vsn)
    names(df) <- c("vsn")
    df$measure <- df1$sensor_path
    df$time <- df1$timestamp
    df$value <- df1$value
    df$measure <-lapply(df$measure,extract_sensor)
    df <- filter_out_untracked_measures(df)
    
    df$measure <- unlist(df$measure)
    df <-aggregate(df$value, by=list(df$vsn,df$measure,df$time), 
                        FUN=mean)
    names(df) <- c("vsn","measure","time","value")
    return(df)
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
  
  filter_out_untracked_measures <- function(df){
    subset(df, measure %in% tracked_measures)
  }
  
  save_df_as_fst <- function(df,path){
    write.fst(df, path)
  }
  
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
  
  if(UPDATE_NODES_STATUS){
    update_nodes_status()
  }
  
  nodes <- read_fst("fst/nodes.fst")
  
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
                      point_size = 1,
                      zoom_level = 5,
                      tooltip_width = 100,
                      tooltip_hieght = 60,
                      tooltip_text_size = 14,
                      line_size = 1,
                      tbl_pagelength = 20,
                      annotate_text_size = 4,
                      marker_text_size = '12px',
                      select_input_width = '100%'
  )

  observeEvent(input$dimension, {
    if(input$dimension[1] >= 2000){
      v$axis_title_size <<- 40
      v$axis_text_size <<- 40
      v$margin_y <<- 40
      v$margin_x <<- 122
      v$legend_title_size <<- 20
      v$legend_text_size <<- 20
      v$legend_key_size <<- 5
      v$pie_text_size <<- 15
      v$slant_text_angle <<- 0
      v$point_size <<- 4
      v$zoom_level <<- 8
      v$tooltip_width <<- 180
      v$tooltip_height <<- 80
      v$tooltip_text_size <<- 28
      v$line_size <<- 5
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
      v$point_size = 1
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

  # MAP rendering
  output$map <- renderLeaflet({
    
    initial_lat <- 41.900613
    initial_lng <- -87.678211

    # pop <- if(!input$switch_daily) ~paste(sep = "<br/>",
    #                                       paste("<b><a href='https://en.wikipedia.org/wiki/",value(f_xy)$NAME,"_County,_",value(f_xy)$STATENAME,"' target='_blank'>",value(f_xy)$NAME," on Wikipedia</a></b>"),
    #                                       value(f_xy)$NAME,
    #                                       value(f_xy)$STATENAME,
    #                                       paste("Confidence level:",signif(temp$Days.with.AQI/365*100,3), "%"),
    #                                       paste("Days with data:",temp$Days.with.AQI),
    #                                       paste(pref,signif(temp$sel_feat,3),suffx)
    # )
    # else
    #   ~paste(sep = "<br/>",
    #          paste("<b><a href='https://en.wikipedia.org/wiki/",value(f_xy)$NAME,"_County,_",value(f_xy)$STATENAME,"' target='_blank'>",value(f_xy)$NAME," on Wikipedia</a></b>"),
    #          value(f_xy)$NAME,
    #          value(f_xy)$STATENAME,
    #          paste(signif(temp$sel_feat,3),suffx)
    #   )

    nodes_by_sensor <- lapply(tracked_measures, function(measure) subset(nodes, nodes[[measure]] == TRUE))
    
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
    
    opacity <- 0.1
    inactiveColor <- "red"
    inactiveOpacity <- 0.5
    normalColor <- "navy"
    
    leaflet(nodes) %>% 
      addCircleMarkers(nodes_by_sensor[[1]]$longitude, nodes_by_sensor[[1]]$latitude, group = tracked_measures[1], layerId=~paste(vsn,tracked_measures[1]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>% #, layerId=~vsn
      addCircleMarkers(nodes_by_sensor[[2]]$longitude, nodes_by_sensor[[2]]$latitude, group = tracked_measures[2], layerId=~paste(vsn,tracked_measures[2]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[3]]$longitude, nodes_by_sensor[[3]]$latitude, group = tracked_measures[3], layerId=~paste(vsn,tracked_measures[3]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[4]]$longitude, nodes_by_sensor[[4]]$latitude, group = tracked_measures[4], layerId=~paste(vsn,tracked_measures[4]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[5]]$longitude, nodes_by_sensor[[5]]$latitude, group = tracked_measures[5], layerId=~paste(vsn,tracked_measures[5]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[6]]$longitude, nodes_by_sensor[[6]]$latitude, group = tracked_measures[6], layerId=~paste(vsn,tracked_measures[6]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[7]]$longitude, nodes_by_sensor[[7]]$latitude, group = tracked_measures[7], layerId=~paste(vsn,tracked_measures[7]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[8]]$longitude, nodes_by_sensor[[8]]$latitude, group = tracked_measures[8], layerId=~paste(vsn,tracked_measures[8]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[9]]$longitude, nodes_by_sensor[[9]]$latitude, group = tracked_measures[9], layerId=~paste(vsn,tracked_measures[9]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[10]]$longitude, nodes_by_sensor[[10]]$latitude, group = tracked_measures[10], layerId=~paste(vsn,tracked_measures[10]), popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_with_no_data$longitude, nodes_with_no_data$latitude, group = "Inactive", layerId=~paste(vsn,"Inactive"), popup = nodes_with_no_data$address, stroke = FALSE, fillOpacity = inactiveOpacity, color = inactiveColor) %>%
      # addMarkers(initial_lng, initial_lat, group = "group2", popup = "myhouse") %>%
      addLayersControl(
        # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c(tracked_measures, "Inactive"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      # addPolygons(data = value(f_xy), color = ~mypal(temp$sel_feat), weight = 0.8, smoothFactor = 0.2,
      #             opacity = spread(temp$Days.with.AQI/365+0.2), fillOpacity = spread(temp$Days.with.AQI/365+0.2),
      #             label = ~htmlEscape(value(f_xy)$NAME),
      #             popup = pop,
      #             highlightOptions = highlightOptions(color = "white", weight = 3,
      #                                                 bringToFront = TRUE)) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      setView(lng = initial_lng, lat = initial_lat, zoom = zoom_level()) 
    # %>%
    #   addLegend(position = "bottomright", pal = mypal, values = temp$sel_feat,
    #             title = "Legend",
    #             labFormat = labelFormat(suffix = suffx,
    #                                     digits = 3
    #             ),
    #             opacity = 1)
  })
  
  # observe({
  #   a <- input$map_marker_click
  #   print(a)
  #   
  # })
  
  # DYNAMIC RENDERING of things in the map
  # observe({
  #   opacity <- 0.1
  #   inactiveColor <- "red"
  #   inactiveOpacity <- 0.5
  #   normalColor <- "navy"
  #   hello <- input$switch_units
  #   print("refresh")
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
  
  output$graphical_data <- renderPlot({
    vsn <- input$map_marker_click
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
    else{
    vsn <- vsn$id
    print(vsn)
    time_range <- input$time_range
    
    vsn <- strsplit(vsn, " ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][1]
    # TODO check if AoT node or openAQ and get corresponding dataset
    print(vsn)
    df <- get_and_preprocess_observations(vsn)
    # if(time_range == TIME_RANGE_CURRENT){
    #   df <-
    # }
    
    df$time <- lapply(df$time,convert_timestamp_to_chicago_timezone)
    # df <- convert_timestamp_to_chicago_timezone(df)
    df <- extract_date_fields(df)
    
    plot_title <- paste("Data for node:",df$vsn[1])
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
      
      if ("co" %in% c(input$measures1,input$measures2)){
        suffx_co = "(ppm)"
        labs <-c(labs,"co" = paste("co",suffx_co, sep=" "))
        vals <-c(vals,"co" = "#c6c60f")
        print(vals)
        gl <- gl + geom_line(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms, color = "co"), size = line_size(), group = 1) +
          geom_point(aes(y = subset(df, measure == "co")$value, x = subset(df, measure == "co")$hms , color = "co"), size = line_size()*3)
      }
      if ("no2" %in% c(input$measures1,input$measures2)){
        suffx_no2 = "(ppb)"
        labs <-c(labs,"no2" = paste("no2",suffx_no2, sep=" "))
        vals <-c(vals,"no2" = "#13c649")
        gl <- gl + geom_line(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms, color = "no2"), size = line_size(), group = 2) +
          geom_point(aes(y = subset(df, measure == "no2")$value, x = subset(df, measure == "no2")$hms , color = "no2"), size = line_size()*3)
      }
      # if ("Ozone" %in% input$c(input$measures1,input$measures2)){
      #   suffx_Ozone = "(ppm)"
      #   labs <-c(labs,"Ozone" = paste("Ozone",suffx_Ozone, sep=" "))
      #   vals <-c(vals,"Ozone" = "#0fa2af")
      #   gl <- gl+geom_line(aes(y = Ozone, color = "Ozone"), size = line_size(), group = 3) +
      #     geom_point(aes(y = Ozone, color = "Ozone"), size = line_size()*3)
      # }
      # if ("SO2" %in% input$c(input$measures1,input$measures2)){
      #   suffx_SO2 = "(ppb)"
      #   labs <-c(labs,"SO2"=paste("SO2",suffx_SO2, sep=" "))
      #   vals <-c(vals,"SO2" = "#A877E0")
      #   gl <- gl +geom_line(aes(y = SO2, color = "SO2"), size = line_size(), group = 4) +
      #     geom_point(aes(y = SO2, color = "SO2"), size = line_size()*3)
      # }
      # convert_to_imperial <- function(values){
      #   return(values*1000000000000* 0.000000035274/35315)
      # }
      # 
      # if ("PM2.5" %in% input$c(input$measures1,input$measures2)){
      #   if(input$switch_units){
      #     df$data_conv <-df$"PM2.5"
      #     df$data_conv <- convert_to_imperial(df$data_conv)
      #     names(df)[names(df)=="data_conv"] <- paste("PM2.5","conv",sep="_")
      #     suffx_PM2.5 = "(e-12 oz/ft3)"
      #     gl <- gl + geom_line(aes(y = df$PM2.5_conv, color = "PM2.5"), size = line_size(), group = 5)+
      #       geom_point(aes(y = df$PM2.5_conv, color = "PM2.5"), size = line_size()*3)
      #   }
      #   else{
      #     gl <- gl + geom_line(aes(y = df$PM2.5, color = "PM2.5"), size = line_size(), group = 5)+
      #       geom_point(aes(y = df$PM2.5, color = "PM2.5"), size = line_size()*3)
      #     suffx_PM2.5 = "(ug/m3)"
      #   }
      #   labs <-c(labs,"PM2.5"=paste("PM2.5",suffx_PM2.5, sep=" "))
      #   vals <-c(vals,"PM2.5" = "#cc8112")
      # }
      # if ("PM10" %in% input$c(input$measures1,input$measures2)){
      #   if(input$switch_units){
      #     df$data_conv <-df$"PM10"
      #     df$data_conv <- convert_to_imperial(df$data_conv)
      #     names(df)[names(df)=="data_conv"] <- paste("PM10","conv",sep="_")
      #     suffx_PM10 = "(e-12 oz/ft3)"
      #     gl <- gl + geom_line(aes(y = df$PM10_conv, color = "PM10"), size = line_size(), group = 6) +
      #       geom_point(aes(y = df$PM10_conv, color = "PM10"), size = line_size()*3)
      #   }
      #   else{
      #     suffx_PM10 = "(ug/m3)"
      #     gl <- gl + geom_line(aes(y = df$PM10, color = "PM10"), size = line_size(), group = 6) +
      #       geom_point(aes(y = df$PM10, color = "PM10"), size = line_size()*3)
      #     
      #   }
      #   labs <-c(labs,"PM10"= paste("PM10",suffx_PM10, sep=" "))
      #   vals <-c(vals,"PM10" = "#ba1010")
      # }
      # convert_temp_to_metric <- function(values){
      #   return((values-32)/1.8)
      # }
      # if ("Temperature" %in% input$c(input$measures1,input$measures2)){
      #   if(input$switch_units){
      #     temp_suffx = "(Degrees Fahrenheit)"
      #     gl <- gl + geom_line(aes(y = Temperature, color = "Temperature"), size = line_size(), group = 7) +
      #       geom_point(aes(y = Temperature, color = "Temperature"), size = line_size()*3)
      #   }
      #   else{
      #     df$data_conv <-df$"Temperature"
      #     df$data_conv <- convert_temp_to_metric(df$data_conv)
      #     names(df)[names(df)=="data_conv"] <- paste("Temperature","conv",sep="_")
      #     temp_suffx = "(Degrees Celsius)"
      #     gl <- gl + geom_line(aes(y = df$Temperature_conv, color = "Temperature"), size = line_size(), group = 7) +
      #       geom_point(aes(y = df$Temperature_conv, color = "Temperature"), size = line_size()*3)
      #   }
      #   labs <-c(labs,"Temperature"= paste("Temperature",temp_suffx, sep=" "))
      #   vals <-c(vals,"Temperature" = "#6B1F13")
      #   
      # }
      # convert_wind_to_metric <- function(values){
      #   return(values*0.51)
      # }
      # if ("Wind Speed" %in% input$c(input$measures1,input$measures2)){
      #   if(input$switch_units){
      #     wind_suffx = "(knots)"
      #     gl <- gl + geom_line(aes(y = df$`Wind Speed`, color = "Wind Speed"), size = line_size(), group = 8) +
      #       geom_point(aes(y = df$`Wind Speed`, color = "Wind Speed"), size = line_size()*3)
      #   }
      #   else{
      #     wind_suffx = "(m/s)"
      #     df$data_conv <-df$"Wind Speed"
      #     df$data_conv <- convert_wind_to_metric(df$data_conv)
      #     names(df)[names(df)=="data_conv"] <- paste("Wind","conv",sep="_")
      #     gl <- gl + geom_line(aes(y = df$Wind_conv, color = "Wind Speed"), size = line_size(), group = 8) +
      #       geom_point(aes(y = df$Wind_conv, color = "Wind Speed"), size = line_size()*3)
      #   }
      #   labs <-c(labs,"Wind Speed" = paste("Wind Speed",wind_suffx, sep=" "))
      #   vals <-c(vals,"Wind Speed" = "#E3446E")
      # }
      gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                    values = vals)
      gl
    }
  })


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
