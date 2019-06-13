tabItem(tabName = "stations_information",
        fluidRow(
          box(width = 12,
              status = "warning",
              solidHeader = TRUE,
              title = "Stations Map",
              column(7 ,leafletOutput("map")),
              infoBoxOutput("city_box"),
              infoBoxOutput("stands_box"),
              infoBoxOutput("bank_box"),
              infoBoxOutput("bonus_box")
          )
        ),
        fluidRow(
          column(5, plotOutput("station_plot"))
          
        )
)

