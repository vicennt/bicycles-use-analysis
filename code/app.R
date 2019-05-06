#
# ---- Final Degree Thesis ----
# Alumno: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(leaflet)
library(stringr)
library(ggplot2)

stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")


#Functions

subset_by_date <- function(dataset,x,y){dataset[dataset$date >= x & dataset$date <= y,]}



# User Interface
ui <- fluidPage(
   # Title
   headerPanel("Hello Shiny!"),
   navbarPage("",
        tabPanel("Map information",
          # Layout (two colums: map & info about station)
          sidebarLayout(position = "right",
            sidebarPanel(
                tags$h4("Information about the station"),
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    tags$img(src="city.png", height='50px',width='50px')),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    verbatimTextOutput("city")),
                br(),
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    tags$img(src="stands.png", height='50px',width='50px')),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    verbatimTextOutput("stands")),
                br(),
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    tags$img(src="bank.png", height='50px',width='50px')),
                div(style="display: inline-block;vertical-align:top; width: 200px;",
                    verbatimTextOutput("bank"))
              ),                                   
          mainPanel(
              #Output Map: Bicycle stations
              leafletOutput("map")
            )
          )
        )
        ,
        tabPanel("Weekly demand",
          #TODO: Statistics page
          sidebarLayout(position = "right",
            sidebarPanel(
              helpText("Here you can choose a city and visualizate the dataset"),
              selectInput("cities_combo", "Choose a citie", unique(stations$CITY), selected = NULL, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              selectInput("stations_combo", "Choose the station", c(), selected = NULL, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              dateInput("date_picker", label = "or select a Monday", value = "2014-09-29",
                        min = "2014-09-29", max="2015-06-31", startview = "month", weekstart = 1)
            )
          ,
          mainPanel(
            verbatimTextOutput("date_text"),
            verbatimTextOutput("week_text"),
            plotOutput("weekly_demand_plot", width = "100%", height = "400px", click = NULL)
          )
        )
        )
        ,
        tabPanel("Prediction"
          #TODO: Prediction page
        )
    )
  )



# Server function
server <- function(input, output, session) {
  
  #Info
  output$date_text <- renderText({
    ini_date <- as.Date(input$date_picker)
    end_date <- ini_date + 6
    paste0("Initial day: ", ini_date,
           "\nLast day: ", end_date)
  })
  
  # Rendering plot when a date is choosed
  output$weekly_demand_plot <- renderPlot({
    dataset <- read.csv(file = paste0("../datasets/bikes_agg_v2/", input$cities_combo, ":",                                      
                                      input$stations_combo,"/", input$cities_combo, ":", input$stations_combo, ".csv"), header=TRUE, sep=",")
    #TODO: Try to improve this, computational time should improve, adding date into dataset could be a good option
    dataset_date <- cbind(dataset, date = as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
    ini_date <- as.Date(input$date_picker)
    end_date <- ini_date + 6
    weeklydataset <- subset_by_date(dataset_date, ini_date, end_date)
    ggplot(data = weeklydataset, aes(x = c(1:168), y = totdecr)) + geom_line(na.rm=TRUE) + scale_x_continuous(breaks=c(0,24,48,72,96,120,144,168))
  })
  

   # Map render
   output$map <- renderLeaflet({
     leaflet(data = stations) %>% addTiles() %>%
       addMarkers(clusterOptions = markerClusterOptions(), data = stations, 
                  popup = ~as.character(paste0("City: ", CITY ,
                                               "\nNum station: ", NUM_STATION ,
                                               "\nNum stands: ", STANDS)), layerId = ~ID)
  })
  
  # Check if a marker is clicked
  observe({
     click <- input$map_marker_click
     if(is.null(click))
       return()
     else {  
       city <- stations[click$id, 2]
       stands <- stations[click$id, 6]
       num_station <- stations[click$id, 3]
       bank <- stations[click$id, 7]
       bonus <- stations[click$id, 8]
       
       #Summary information
       output$city <- renderText({ 
         paste0(city)
       })
       output$stands <- renderText({ 
         paste0(stands)
       })
       output$bank <- renderText({ 
         if(bank == FALSE){
           paste0("There are not banking")
         }else{
           paste0("There are banking")
         }
       })
     }
  })
   
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
}

# Run the application 
shinyApp(ui = ui, server = server)

