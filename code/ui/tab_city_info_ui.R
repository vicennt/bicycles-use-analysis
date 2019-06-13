tabItem(tabName = "city_information",
        fluidRow(column(6, h3("Bicycle information"))),
        fluidRow( 
          column(12,
            infoBoxOutput("num_stations_city"),
            infoBoxOutput("station_high_demand_city"),
            infoBoxOutput("station_low_demand_city"),
            infoBoxOutput("city_population"),
            infoBoxOutput("num_trips_city"),
            infoBoxOutput("city_rank")
          )
        ),
        fluidRow(column(6, h3("Weather information"))),
        fluidRow(
           column(12, 
                  infoBoxOutput("sunny_days"),
                  infoBoxOutput("cloudy_days"),
                  infoBoxOutput("rainy_days"),
                  infoBoxOutput("windy_days"),
                  infoBoxOutput("foggy_days"),
                  infoBoxOutput("snowy_days"),
                  infoBoxOutput("highest_temperature"),
                  infoBoxOutput("lowest_temperature"),
                  infoBoxOutput("average_windy")

           )
        )
)
