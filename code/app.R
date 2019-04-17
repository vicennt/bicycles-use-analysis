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
   titlePanel("Final Degree Thesis"),
   
   # Layout (two colums: map & info about station)
   sidebarLayout(position = "right",
      sidebarPanel(
        #TODO: Exclusive info about the selected station
      ),
      mainPanel(
        #Output Map: Bicycle stations
        leafletOutput("stations_map")
         
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
}

# Run the application 
shinyApp(ui = ui, server = server)

