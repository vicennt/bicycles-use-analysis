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
        )
)
