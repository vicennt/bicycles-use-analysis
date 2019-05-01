#
# ---- Final Degree Thesis ----
# Alumno: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(leaflet)

stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")

# User Interface
ui <- fluidPage(
   # Title
   headerPanel("Hello Shiny!"),
   navbarPage("",
        tabPanel("Bicycles",
          # Layout (two colums: map & info about station)
          sidebarLayout(position = "right",
            sidebarPanel(
              tabsetPanel(
                tabPanel("Choose a City", 
                         helpText("Here you can choose a city and visualizate the dataset"),
                         selectInput("cities_combo", "Choose a citie", unique(stations$CITY), selected = NULL, multiple = FALSE,
                                     selectize = TRUE, width = NULL, size = NULL),
                         selectInput("stations_combo", "Choose the station", c(), selected = NULL, multiple = FALSE,
                                     selectize = TRUE, width = NULL, size = NULL)
                         ),
                tabPanel("Summary", 
                        helpText("Information about the station"),
                        verbatimTextOutput("info"))
              )
            ),
            mainPanel(
              #Output Map: Bicycle stations
              leafletOutput("map"),
              dataTableOutput("table")
            )
          )
        ),
        tabPanel("Data Analysis",
          #TODO: Statistics page
          sidebarLayout(position = "right",
            sidebarPanel(
              helpText("Choose a Monday")
            )
          ,
          mainPanel(
            dateInput("date", label = h3("Date input"), value = "2014-09-29",
                      min = "2014-09-29", max="2015-06-31", startview = "month", weekstart = 1),
            plotOutput("weekplot", width = "100%", height = "400px", click = NULL)
          )
        )
        ),
        tabPanel("Prediction"
          #TODO: Prediction page
        )
        )
   )



# Server function
server <- function(input, output, session) {
  
  #Weekly demand plot
  output$weekplot <- renderPlot({
    #TODO: Showing the weekly demand when a Monday is selected
    #ggplot(data = week1, aes(x = houred, y = totdecr)) + geom_line()
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
     }
     
     output$info <- renderText({ 
       paste0("City: ",  city , "\nNumber of stands: ", stands, "\nBanking: ", bank,
              "\nBonus: ", bonus)
     })
     
     output$table <- renderDataTable({
       read.csv(file = paste0("../datasets/bikes_agg_v2/", city, ":", num_station,
                              "/", city, ":", num_station, ".csv"), header=TRUE, sep=",")
     })
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
   
   # Selected city
   output$table <- renderDataTable({
      read.csv(file = paste0("../datasets/bikes_agg_v2/", input$cities_combo, ":", input$stations_combo,
                                   "/", input$cities_combo, ":", input$stations_combo, ".csv"), header=TRUE, sep=",")
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

