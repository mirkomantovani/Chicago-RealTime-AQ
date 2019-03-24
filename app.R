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

# hourly_df <- read_fst("fst/hourly_all_data_2018.fst")

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


########################################### PREPROCESSING #########################################

years<-c(1990:2018)
H_years<-c(2018) #years available for hourly data
H_years_italy<-c(2018,2019)
H_months<-c("January","February","March","April","May","June","July","August","September","October","November","December")

# H_days<-unique(hourly_df$Day)


# states<-unique(dataset$State)
# t<-subset(dataset, State == 'Illinois')
# counties<-unique(t$County)
top12 <- c("Cook - Illinois","Hawaii - Hawaii","New York - New York","Los Angeles - California", "King - Washington","Harris - Texas","Miami - Dade-Florida",
           "San Juan - New Mexico","Hennepin - Minnesota","Wake - North Carolina", "San Francisco - California", "Maricopa - Arizona")
pollutants <- c("CO","NO2","Ozone","SO2","PM2.5","PM10")
pollutants_2 <- c("PM2.5","PM10","CO","NO2","Ozone","SO2")

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
                leafletOutput("map_controllers", width="100%", height="100%"),

                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",

                              h2("Time and Pollutant"),
                              selectizeInput(inputId = "pollutant_map", "Select Pollutant", c(pollutants_2,"AQI"), selected = 'PM2.5',width = "100%"),
                              materialSwitch(inputId = "switch_daily", label = "Switch to Daily Data (for 2018)", status = "primary"),
                              numericInput("year_map", "Select Year", min=1990, max=2018, value=2018)
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
  
  tracked_measures <- c("co","h2s","no2","o3","so2","temperature","humidity","intensity")

  # nodes <- get_and_preprocess_nodes()
  # 
  # extract_sensor <- function(elem){
  #   elem <- as.character(elem)
  #   l <- strsplit(elem, ".", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
  #   if(l[3] == "concentration"){
  #     return(l[2])
  #   } else {
  #     return(tail(l, n=1))
  #   }
  # }
  # 
  # 
  # for(m in tracked_measures){
  #   nodes[m] <- FALSE
  # }
  # 
  # for(n in unique(nodes$vsn)){
  #   df <- ls.observations(filters=list(node=n))
  #   df$sensor_path <-lapply(df$sensor_path,extract_sensor)
  #   u <- unique(df$sensor_path)
  #   v <- unlist(u)
  #   measures <- intersect(v,tracked_measures)
  #   for(m in measures){
  #     nodes[which(nodes$vsn == n), m] = TRUE
  #   }
  # }
  

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
      v$zoom_level = 9
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


  # computing subset of data based on user selection of year, state, county
  # current <- reactive({
  #   # print("reactive")
  #   subset(dataset, County == input$County & State == input$State & Year == input$Year)
  # 
  # })

  # observeEvent(priority = 10,input$State,{
  #   selected_state_data <- subset(dataset, State == input$State)
  #   counties_in_state <- unique(selected_state_data$County)
  # 
  #   updateSelectInput(session, inputId = "County", choices = counties_in_state)
  #   county <- input$County
  # 
  # })
  
  # observeEvent(priority = 10,input$StateD,{
  # if(!input$switch_top12_daily){
  # selected_state_data <- subset(dataset, State == input$StateD)
  # counties_in_state <- unique(selected_state_data$County)
  # 
  # updateSelectInput(session, inputId = "CountyD", choices = counties_in_state)
  # county <- input$CountyD
  # }
  # 
  # })


  ###NEEDS MODIFICATION
  # observeEvent(priority = 10,input$location_italy,{
  #   selected_state_data <- subset(dataset, State == input$State)
  #   counties_in_state <- unique(selected_state_data$County)
  # 
  #   updateSelectInput(session, inputId = "County", choices = counties_in_state)
  #   county <- input$County
  # 
  # })


  observeEvent(priority = 10,input$switch_daily,{
    if(input$switch_daily){
      updateSelectInput(session, inputId = "pollutant_map", choices = pollutants_2)
    } else {
      updateSelectInput(session, inputId = "pollutant_map", choices = c(pollutants_2,"AQI"))
    }

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
  output$map_controllers <- renderLeaflet({
    
    initial_lat <- 41.900613
    initial_lng <- -87.678211
    
    
    feature <- translate_to_column_name(input$pollutant_map)
    # value = c((current()$Days.with.AQI-current()$Days.Ozone)/current()$Days.with.AQI*100, current()$Days.Ozone/current()$Days.with.AQI*100)

    # if(!input$switch_daily){ # Yearly
    #   sub<-subset(dataset, Year == input$year_map)
    #   if(feature !="Median.AQI"){
    #     sub$sel_feat<-sub[[feature]]/sub$Days.with.AQI*100
    #     suffx = "%"
    #     pref = "Pollutant: "
    #   } else {
    #     sub$sel_feat<-sub[[feature]]
    #     suffx = ""
    #     pref = "AQI: "
    #   }
    # } else { # Daily
    #   pref = "Pollutant: "
    #   sub<-subset(daily_all, Month == input$D_month & Day == input$D_day)
    #   sub$sel_feat<-sub[[input$pollutant_map]]
    #   if(input$switch_units){
    #     if(input$pollutant_map == "PM2.5" || input$pollutant_map == "PM10"){
    #       sub$sel_feat <- convert_to_imperial(sub$sel_feat)
    #       suffx = "e-12 oz/ft3"
    #     }
    #   } else {
    #     if(input$pollutant_map == "PM2.5" || input$pollutant_map == "PM10"){
    #       suffx = " ug/m3"
    #     }
    #   }
    #   if(input$pollutant_map == "CO" || input$pollutant_map == "Ozone"){
    #     suffx = " ppm"
    #   } else if(input$pollutant_map == "SO2" || input$pollutant_map == "NO2"){
    #     suffx = " ppb"
    #   }
    # }

    # sub <- sub[order(sub$sel_feat,decreasing = TRUE),]
    # df <- head(sub,delayes_num_counties_debounced())

    # if(!input$switch_daily){ # Yearly
    #   temp <- merge(value(f_xy), df,
    #                 by.x = c("STATENAME","NAME"), by.y = c("State","County"),
    #                 all.x = TRUE)
    # } else { # Daily
    #   temp <- merge(value(f_xy), df,
    #                 by.x = c("STATENAME","NAME"), by.y = c("State Name","County Name"),
    #                 all.x = TRUE)
    # }


    # Create a color palette
    # mypal <- colorNumeric(palette = "viridis", reverse = TRUE, domain = temp$sel_feat
    #                       ,na.color = "#ffffff11"
    # )

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

    # icons <- awesomeIcons(
    #   icon = 'close-circle',
    #   iconColor = 'black',
    #   library = 'ion'
    #   # markerColor = getColor(df.20)
    # )

    spread <- function(num){
      sp <- input$Opacity
      return(if(sp < 0.4) ((num+sp)*(num+sp)-sp*sp)/(1+sp*2)-0.2+sp else ((num+sp)*(num+sp)-sp*sp)/(1+sp*2)+sp-0.2)
    }
    
    nodes_by_sensor <- lapply(tracked_measures, function(measure) subset(nodes, nodes[[measure]] == TRUE))
    
    nodes_with_no_data <- subset(nodes, nodes[[tracked_measures[1]]] == FALSE &
                                   nodes[[tracked_measures[2]]] == FALSE &
                                   nodes[[tracked_measures[3]]] == FALSE &
                                   nodes[[tracked_measures[4]]] == FALSE &
                                   nodes[[tracked_measures[5]]] == FALSE &
                                   nodes[[tracked_measures[6]]] == FALSE &
                                   nodes[[tracked_measures[7]]] == FALSE &
                                   nodes[[tracked_measures[8]]] == FALSE )
    
    opacity <- 0.1
    inactiveColor <- "red"
    inactiveOpacity <- 0.5
    normalColor <- "navy"
    
    leaflet(nodes) %>% 
      addCircleMarkers(nodes_by_sensor[[1]]$longitude, nodes_by_sensor[[1]]$latitude, group = tracked_measures[1], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>% #, layerId=~vsn
      addCircleMarkers(nodes_by_sensor[[2]]$longitude, nodes_by_sensor[[2]]$latitude, group = tracked_measures[2], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[3]]$longitude, nodes_by_sensor[[3]]$latitude, group = tracked_measures[3], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[4]]$longitude, nodes_by_sensor[[4]]$latitude, group = tracked_measures[4], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[5]]$longitude, nodes_by_sensor[[5]]$latitude, group = tracked_measures[5], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[6]]$longitude, nodes_by_sensor[[6]]$latitude, group = tracked_measures[6], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[7]]$longitude, nodes_by_sensor[[7]]$latitude, group = tracked_measures[7], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_by_sensor[[8]]$longitude, nodes_by_sensor[[8]]$latitude, group = tracked_measures[8], popup = ~address, stroke = FALSE, fillOpacity = opacity, color= normalColor) %>%
      addCircleMarkers(nodes_with_no_data$longitude, nodes_with_no_data$latitude, group = "Inactive", popup = nodes_with_no_data$address, stroke = FALSE, fillOpacity = inactiveOpacity, color = inactiveColor) %>%
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
