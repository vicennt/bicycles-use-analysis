#
# ---- Final Degree Thesis ----
# Alumni: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(stringr)
library(ggplot2)
library(jsonlite)
library(fontawesome)
library(httr)

#API
key <- readLines("api_key")
base <- "https://api.jcdecaux.com/"

#Datasets
stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")

#General variables
attr_bike1 <- list("Date" = "date", "Hour" = "hour","Houred" = "houred", "Total increment" = "totinc", "Demand" = "totdecr", "Median Bikes" = "medbikes")
attr_bike2 <- list("Mean bikes" = "meanbikes", "Last bikes" = "lastbikes", "Probability Empty" = "propempty", "Probability Full" = "propfull", "Count" = "count")
attr_weather1 <- list("Houred" = "houred", "Clouds" = "clouds_all", "Code" = "cod", "Latitude" = "coord_lat", "Longitude" = "coord_lon", "DT" = "dt") 
attr_weather2 <- list( "ID" = "id", "Humidity" = "main_humidity", "Pressure" = "main_pressure", "Temperature" = "main_temp", "Max Temp" = "main_temp_max", "Min Temp" = "main_temp_min")
attr_weather3 <- list("Sunrise" = "sys_sunrise", "Sunset" = "sys_sunset", "Weather ID" = "weather_id", "Wind Deg" = "wind_deg", "Wind Speed" = "wind_speed", "Description" = "weather_description")

bicycles_data_path <- "../datasets/bikes_agg_v2/"
weather_data_path <- "../datasets/weather_agg_v2/"

#General Functions
subset_by_date <- function(dataset, ini_date, end_date){
  subset <- cbind(dataset, date = tm1 <- as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
  subset[subset$date >= ini_date & subset$date <= end_date,]
}


# User Interface
ui <- 
  dashboardPage(skin = "green",
   dashboardHeader(
      title = "Bicycles",
      dropdownMenu(
            type = "tasks", badgeStatus = "success",
            taskItem(value = 10, color = "green","Documentation"),
            taskItem(value = 40, color = "aqua","Development"),
            taskItem(value = 5, color = "yellow", "Testing"),
            taskItem(value = 40, color = "red", "Overall project")
      ),
      #TODO: Show notifications: Server status & Station status & ??
      dropdownMenu(
        type = "notifications", 
        notificationItem(text = "The system is running well", icon("users"))
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        selectInput("cities", "Select one of these cities", c()),
        menuItem("Visualize data", tabName = "past", icon = icon("database")),
        menuItem("Real time", tabName = "present", icon = icon("th")),
        menuItem("Future predictions", tabName = "future", icon = icon("paper-plane")),
        menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/vicennt/bicycles-use-analysis")
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "past",
                fluidRow(column(6, h3("Bicycle information"))),
                fluidRow(
                     column(12,
                        infoBoxOutput("num_stations_city"),
                        infoBoxOutput("num_trips_city"),
                        infoBoxOutput("percentage_usage_city"),
                        infoBoxOutput("station_high_demand_city"),
                        infoBoxOutput("station_low_demand_city"),
                        infoBoxOutput("other")
                     )
                ),
                fluidRow(column(6, h3("Weather information"))),
                fluidRow(
                  column(12,
                         infoBoxOutput("rainy_days"),
                         infoBoxOutput("sunny_days"),
                         infoBoxOutput("snowy_days"),
                         infoBoxOutput("highest_temperature"),
                         infoBoxOutput("lowest_temperature"),
                         infoBoxOutput("average_windy")
                  )
                ),
                fluidRow(
                  box(width = 7,
                      status = "warning",
                      solidHeader = TRUE,
                      title = "Stations Map",
                      leafletOutput("map")
                  ),
                  br(),
                  br(),
                  infoBoxOutput("city_box"),
                  infoBoxOutput("stands_box"),
                  infoBoxOutput("bank_box"),
                  infoBoxOutput("bonus_box")
                ),
                
                br(),
                code(id="alert", "Select an station before continue!!!!!"),
                br(),
                br(),
                
                fluidRow(
                    box(
                      id = "bike_attr_box",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "warning",
                      title = "Bicycles dataset attributes",
                      fluidRow(
                        column(6, checkboxGroupInput("check_bike1", label = " ", choices = attr_bike1, selected = attr_bike1)),
                        column(6, checkboxGroupInput("check_bike2", label = " ", choices = attr_bike2, selected = attr_bike2))
                      )
                    ),
                    box(
                      id = "weather_attr_box",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      status = "warning",
                      title = "Weather dataset attributes",
                      fluidRow(
                        column(4, checkboxGroupInput("check_weather1", label = " ", choices = attr_weather1, selected = attr_weather1)),
                        column(4, checkboxGroupInput("check_weather2", label = " ", choices = attr_weather2, selected = attr_weather2)),
                        column(4, checkboxGroupInput("check_weather3", label = " ", choices = attr_weather3, selected = attr_weather3))
                      )
                    )
                ),
                fluidRow(
                  tabBox(
                    id = "datasets_box",
                    title = tagList(shiny::icon("database"), "Explore the data"),
                    width = 12,
                    tabPanel("Bicycles data", dataTableOutput("station_data")),
                    tabPanel("Weather data", dataTableOutput("weather_data"))
                  )
                ),
                
                br(),
                br(),

                fluidRow(
                  column(3,
                         box(
                          width = 12,
                          id = "graph_attr_box",
                          title = "Choose attributes",
                          collapsible = TRUE,
                          solidHeader = TRUE,
                          status = "warning",
                          selectInput("ycol", 'Y Variable', c()),
                          selectInput("xcol", 'X Variable', c()),
                          selectInput("plot_type", 'Type of plot', c("Line plot" = "geom_line","Barplot" = "geom_bar","Scaterplot" = "geom_point"))
                         )
                  ),
                  column(9, 
                         box(
                           id = "plot_box", 
                           width = 12, 
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           status = "warning",
                           plotOutput("user_plot")
                        )
                  )
                )
           ),
           # TODO: Get info from the API
           tabItem(tabName = "present",
                h2("TODO: Getting data from the API"),
                verbatimTextOutput("api_test")
           ),
           # TODO: Predicting the future bicycle demand
           tabItem(tabName = "future",
                h2("TODO: Try to predict the future demand of each station")
           )
      )
   )  
)

# Server function
server <- function(input, output, session) {
  
  # ------- Tab 1 "Map Information " -------------
  # Map render
  output$map <- renderLeaflet({
    leaflet(data = stations) %>% addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(), data = stations, 
                 popup = ~as.character(paste0(CITY ," ",NUM_STATION)), layerId = ~ID)
  })
  
  # Checking if a marker is clicked
  observe({
    #Getting the event click
    click <- input$map_marker_click
    # Checking if is clicked or not
    if(is.null(click)){
      # Checkboxes & Combos disabled until user has clicked on a marker
      shinyjs::disable("bike_attr_box")
      shinyjs::disable("weather_attr_box")
      shinyjs::disable("subset_date")
      shinyjs::disable("xcol")
      shinyjs::disable("ycol")
      shinyjs::disable("plot_type")
      return() 
    }else {  
      #Enabling UI widgets
      shinyjs::hide("alert")
      shinyjs::enable("bike_attr_box")
      shinyjs::enable("weather_attr_box")
      shinyjs::enable("check_weather1")
      shinyjs::enable("check_weather2")
      shinyjs::enable("check_weather3")
      shinyjs::enable("subset_date")
      shinyjs::enable("xcol")
      shinyjs::enable("ycol")
      shinyjs::enable("plot_type")
      
      city <- stations[click$id, 2]
      stands <- stations[click$id, 6]
      num_station <- stations[click$id, 3]
      bank <- stations[click$id, 7]
      bonus <- stations[click$id, 8]
      
      #Obtaining station & weather dataset
      bicycle_dataset <- read.csv(file = paste0(bicycles_data_path, city, ":", num_station,"/", city, ":", num_station, ".csv"), header=TRUE, sep=",")
      weather_dataset <- read.csv(file = paste0(weather_data_path, city ,"_agg.csv"), header=TRUE, sep=",")
      
      #Obtaining subsets by date
      bicycle_subset <- subset_by_date(bicycle_dataset, input$subset_date[1], input$subset_date[2])
      weather_subset <- subset_by_date(weather_dataset, input$subset_date[1], input$subset_date[2])
      
      
      #Rendering table with selected attributes
      output$station_data <- renderDataTable({
        atributes <-c(input$check_bike1, input$check_bike2)
        bicycle_subset[atributes]
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      #Rendering weather data
      output$weather_data <- renderDataTable({
        atributes <-c(input$check_weather1, input$check_weather2, input$check_weather3)
        weather_subset[atributes]
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      #Rendering user plot
      output$user_plot <- renderPlot({
          atributes <-c(input$check_bike1, input$check_bike1)
          plot_type <- input$plot_type
          print(plot_type)
          bicycle_subset[atributes]
          ggplot(data = bicycle_subset) + get(plot_type)(mapping = aes_string(x = input$xcol, y = input$ycol))
      })
      
      #Showing the summary information
      output$city_box <- renderInfoBox({ 
        infoBox(
          title = "City",
          icon = icon("map-marker-alt"),
          color = "red",
          value = paste0(city)
        )
      })
      output$stands_box <- renderInfoBox({ 
        infoBox(
          title = "Number of Stands",
          icon = icon("bicycle"),
          color = "olive",
          value = paste0(stands)
        )
      })
      output$bank_box <- renderInfoBox({ 
        text <- " "
        if(bank == FALSE){
          text <- paste0("There is banking")
        }else{
          text <- paste0("There is not banking")
        }
        
        infoBox(
          title = "Bank service",
          icon = icon("btc"),
          color = "yellow",
          value = text
        )
      })
      output$bonus_box <- renderInfoBox({ 
        text <- " "
        if(bonus == FALSE){
          text <- paste0("There is not bonus")
        }else{
          text <- paste0("There is bonus")
        }
        infoBox(
          title = "Bonus service",
          icon = icon("award"),
          color = "aqua",
          value = text
        )
      })
    }
  })
  
  #Contolling wich attributes are selected
  observe({
    atributes <-c(input$check_bike1, input$check_bike1)
    updateSelectInput(session, "xcol",
                    label = "X Variable",
                    choices = atributes,
                    selected = NULL)
    updateSelectInput(session, "ycol",
                      label = "Y Variable",
                      choices = atributes,
                      selected = NULL)
  })
  
  
  # Rendering infoboxes
  output$city_box <- renderInfoBox({
    infoBox(
      title = "City",
      icon = icon("map-marker-alt"),
      color = "red",
      value = "Station not selected"
    )
  })
  output$stands_box <- renderInfoBox({ 
    infoBox(
      title = "Number of Stands",
      icon = icon("bicycle"),
      color = "olive",
      value = "Station not selected"
    )
  })
  output$bank_box <- renderInfoBox({ 
    infoBox(
      title = "Bank service",
      icon = icon("btc"),
      color = "yellow",
      value = "Station not selected"
    )
  })
  output$bonus_box <- renderInfoBox({ 
    infoBox(
      title = "Bonus service",
      icon = icon("award"),
      color = "aqua",
      value = "Station not selected"
    )
  })
  
  #Rendering information from bicycle usage
  output$num_stations_city <- renderInfoBox({
    #TODO: Obtain total num of stations
    infoBox(
      title = "Number of total stations",
      icon = icon("thumbtack"),
      color = "purple",
      value = "Station not selected"
    )
  })
  output$num_trips_city <- renderInfoBox({
    #TODO: Obtain total num of trips
    infoBox(
        title = "Number of trips during this period",
      icon = icon("bicycle"),
      color = "maroon",
      value = "Station not selected"
    )
  })
 output$percentage_usage_city <- renderInfoBox({
   #TODO: Obtain percentage of bicycle usage
   infoBox(
     title = "Percentage of bicycle usage",
     icon = icon("percentage"),
     color = "yellow",
     value = "Station not selected"
   )
  })
  output$station_high_demand_city <- renderInfoBox({
    #TODO: Obtain city with highest demand
    infoBox(
      title = "Station with highest demand",
      icon = icon("arrow-circle-up"),
      color = "red",
      value = "Station not selected"
    )
  })
  output$station_low_demand_city <- renderInfoBox({
    #TODO: Obtain city with lowest demand
    infoBox(
      title = "Station with lowest demand",
      icon = icon("arrow-alt-circle-down"),
      color = "green",
      value = "Station not selected"
    )
  })
  output$other <- renderInfoBox({
    #TODO: Obtain city with lowest demand
    infoBox(
      title = "Bonus service",
      icon = icon("award"),
      color = "teal",
      value = "Station not selected"
    )
  })
  
  #Rendering weather info
  
  #Rendering weather info
  output$rainy_days <- renderInfoBox({
    #TODO: Obtain the number of rainny days
    infoBox(
      title = "Number of rainny days",
      icon =  icon("cloud-rain"),
      color = "light-blue",
      value = "Station not selected"
    )
  })
  output$sunny_days <- renderInfoBox({
    #TODO: Obtain the number of sunny days
    infoBox(
      title = "Number of sunny days",
      icon =  icon("sun"),
      color = "yellow",
      value = "Station not selected"
    )
  })
  #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
  output$snowy_days <- renderInfoBox({
    #TODO: Obtain the number of snowy days
    infoBox(
      title = "Number of snowy days",
      icon = icon("snowflake"),
      color = "black",
      value = "Station not selected"
    )
  })
  output$highest_temperature <- renderInfoBox({
    #TODO: Obtain highest temperature
    infoBox(
      title = "Highest temperature",
      icon = icon("temperature-high"),
      color = "red",
      value = "Station not selected"
    )
  })
  output$lowest_temperature <- renderInfoBox({
    #TODO: Obtain lowest temperature
    infoBox(
      title = "Lowest temperature",
      icon = icon("temperature-low"),
      color = "blue",
      value = "Station not selected"
    )
  })
  output$average_windy <- renderInfoBox({
    #TODO: Obtain average wind velocity
    infoBox(
      title = "Average wind velocity in KM",
      icon = icon("wind"),
      color = "aqua",
      value = "Station not selected"
    )
  })
  
 
  
  
  # ------- Tab 2 "Weekly demand " ---------------

  output$api_test <- renderText({
    contract_name <- "Valence"
    station_num <- "1"
    url <- paste0("https://api.jcdecaux.com/vls/v1/stations/",station_num,"/?contract=",contract_name,"&apiKey=",key)
    df_api = jsonlite::fromJSON(url)
    paste0("Number: ", df_api$number,"\nName: ", df_api$name, "\nAddress: ", df_api$address)
  })
  
  
  
  
  # ------- Tab 3 "Bicycles & Weather" -----------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

