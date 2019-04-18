#
# ---- Final Degree Thesis ----
# Alumno: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(leaflet)

stations <- read.csv(file="../testdata/stations.csv", header=TRUE, sep=",")

# User Interface
ui <- fluidPage(
   # Title
   headerPanel("Hello Shiny!"),
   navbarPage("",
        tabPanel("Bicycles",
          # Layout (two colums: map & info about station)
          sidebarLayout(position = "right",
            sidebarPanel(
              #TODO: Exclusive info about the selected station
              helpText("Information about the station"),
              fluidRow(verbatimTextOutput("info"))
            ),
            mainPanel(
              #Output Map: Bicycle stations
              leafletOutput("map")
            )
          )
        ),
        tabPanel("Data Analysis"
          #TODO: Statistics page
        ),
        tabPanel("Prediction"
          #TODO: Prediction page
        )
   )
)

# Server function
server <- function(input, output) {
   #TODO: Handle inputs and outputs
   # Map render
   output$map <- renderLeaflet({
     leaflet(data = stations) %>% addTiles() %>%
       addMarkers(clusterOptions = markerClusterOptions(), data = stations, 
                  popup = ~as.character(paste0("City: ", CITY ,
                                               "\nNum station: ", NUM_STATION ,
                                               "\nNum stands: ", STANDS)), layerId = ~ID)
  })
  
   observe({
     click <- input$map_marker_click
     if(is.null(click))
       return()
     output$info <- renderText({ 
       city <- stations[click$id, 2]
       stands <- stations[click$id, 6]
       num_station <- stations[click$id, 3]
       paste0("Station number ",  num_station , " in ", city , "\nThe number of stands is ", stands)
     })
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

