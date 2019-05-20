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
    dashboardHeader(title = "Bicycles"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Visualize data", tabName = "past", icon = icon("database")),
        menuItem("Current status", tabName = "present", icon = icon("th")),
        menuItem("Demand prediction", tabName = "future", icon = icon("paper-plane"))
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "past",
                fluidRow(
                  box(width = 7,
                      title = "Stations Map",
                      leafletOutput("map")
                  ),
                  br(),
                  br(),
                  infoBox(
                    title = "City",
                    icon = icon("map-marker-alt"),
                    color = "red",
                    value = verbatimTextOutput("city")
                  ),
                  infoBox(
                    title = "Number of Stands",
                    icon = icon("bicycle"),
                    color = "olive",
                    value = verbatimTextOutput("stands")
                  ),
                  infoBox(
                    title = "Bank service",
                    icon = icon("btc"),
                    color = "yellow",
                    value = verbatimTextOutput("bank")
                  ),
                  infoBox(
                    title = "Bonus service",
                    icon = icon("award"),
                    color = "aqua",
                    value = verbatimTextOutput("bonus")
                  )
                ),
                
                br(),
                br(),
                
                fluidRow(
                    box(
                      id = "bike_attr_box",
                      title = "Bicycles dataset attributes",
                      fluidRow(
                        column(6, checkboxGroupInput("check_bike1", label = " ", choices = attr_bike1, selected = attr_bike1)),
                        column(6, checkboxGroupInput("check_bike2", label = " ", choices = attr_bike2, selected = attr_bike2))
                      )
                    ),
                    box(
                      id = "weather_attr_box",
                      title = "Weather dataset attributes",
                      fluidRow(
                        column(4, checkboxGroupInput("check_weather1", label = " ", choices = attr_weather1, selected = attr_weather1)),
                        column(4, checkboxGroupInput("check_weather2", label = " ", choices = attr_weather2, selected = attr_weather2)),
                        column(4, checkboxGroupInput("check_weather3", label = " ", choices = attr_weather3, selected = attr_weather3))
                      )
                    )
                ),
                
                fluidRow(
                  box( 
                    id = "data_range_box",
                    width = 12,
                    title = "Choose the data range",
                    column(12, 
                           dateRangeInput('subset_date',
                                          label = " ",
                                          start = "2014-09-29", "2015-06-31",
                                          min = "2014-09-29", max = "2015-07-01",
                                          startview = 'month', weekstart = 1),
                    column(12, offset = 4, helpText("The data will be reduced, a new dataset will be generated"))
                    )
                  )
                ),
                
                fluidRow(
                  box(
                    id = "bike_data_box",
                    width = 12,
                    dataTableOutput("station_data")
                  )
                ),
                
                fluidRow(
                  box(
                    id = "weather_data_box",
                    width = 12,
                    dataTableOutput("weather_data")
                  )
                ),
                
                br(),
                br(),

                fluidRow(
                  column(3,
                         box(
                          id = "graph_attr_box",
                          selectInput("xcol", 'X Variable', c()),
                          selectInput("ycol", 'Y Variable', c()),
                          selectInput("plot_type", 'Type of plot', c("Barplot" = "geom_bar","Scaterplot" = "geom_point","Line plot" = "geom_line"))
                         )
                  ),
                  column(9, 
                         box(id = "plot_box", plotOutput("user_plot"))
                  )
                )
           ),
           # TODO: Get info from the API
           tabItem(tabName = "present",
                h2("TODO: Getting data from the API")
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
      shinyjs::hide("bike_attr_box")
      shinyjs::hide("weather_attr_box")
      shinyjs::disable("subset_date")
      shinyjs::disable("xcol")
      shinyjs::disable("ycol")
      shinyjs::disable("plot_type")
      return() 
    }else {  
      #Enabling UI widgets
      shinyjs::show("bike_attr_box")
      shinyjs::show("weather_attr_box")
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
      output$city <- renderText({ 
        paste0(city)
      })
      output$stands <- renderText({ 
        paste0(stands)
      })
      output$bank <- renderText({ 
        if(bank == FALSE){
          paste0("There is banking")
        }else{
          paste0("There is not banking")
        }
      })
      output$bonus <- renderText({ 
        if(bonus == FALSE){
          paste0("There is not bonus")
        }else{
          paste0("There is bonus")
        }
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
  
  
  # Defaul information texts (in the case that no station is selected)
  output$city <- renderText({ 
    paste0("Station not selected")
  })
  output$stands <- renderText({ 
    paste0("Station not selected")
  })
  output$bank <- renderText({ 
    paste0("Station not selected")
  })
  output$bonus <- renderText({ 
    paste0("Station not selected")
  })

  
  
  # ------- Tab 2 "Weekly demand " ---------------

  # Check if a city is selected
  observe({
    city <- input$cities_combo
    if (is.null(city)){
      stations <- c()
    }else
      stations <- stations[stations$CITY == city, 3]
    # Can also set the label and select items
    updateSelectInput(session, "stations_combo",
                      label = "Choose a station",
                      choices = stations,
                      selected = NULL
    )
  })
  
  # Rendering the weekly demand plot with the selected city & station & dates
  output$weekly_demand_plot <- renderPlot({
    weekly_subset <- subset_by_date(input$cities_combo, 
                             input$stations_combo,
                             as.Date(input$date_picker), 
                             as.Date(input$date_picker) + 6)
    ggplot(data = weekly_subset) +
      geom_line(mapping = aes(x = c(1:168), y = totdecr)) +
      scale_x_continuous(breaks = seq(0, 168, by = 24))
  })
  
  # Debugging
  output$date_text <- renderText({
    ini_date <- as.Date(input$date_picker)
    end_date <- ini_date + 6
    paste0("Initial day: ", ini_date,
           "\nLast day: ", end_date)
  })
  
  
  
  
  # ------- Tab 3 "Bicycles & Weather" -----------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

