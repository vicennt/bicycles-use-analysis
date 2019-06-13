tabItem(tabName = "stations_information",
        fluidRow(
          box(width = 12,
              status = "warning",
              solidHeader = TRUE,
              title = "Stations Map",
              fluidRow(
                column(7, leafletOutput("map")), 
                column(5, plotOutput("station_plot"))
              )
          )
        )
)