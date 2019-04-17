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
              leafletOutput("stations_map")
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
   output$stations_map <- renderLeaflet({
     leaflet(data = stations) %>% addTiles() %>%
       addMarkers(clusterOptions = markerClusterOptions(),
                  popup = ~as.character(CITY))
  })
   
   observeEvent(input$stations_map_click, { 
     p <- input$stations_map_click 
     print(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

