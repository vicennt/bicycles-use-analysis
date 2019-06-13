tabItem(tabName = "analysis",
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
        ),

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
        ),
        fluidRow(column(12, h3("Comparing cities"))),
        fluidRow(
          box(plotOutput("selected_city_plot")),
          box(plotOutput("compare_city_plot"))
        ),
        fluidRow(column(12, h3("Comparing stations"))),
        fluidRow(
          box(plotOutput("selected_station_plot")),
          box(plotOutput("compare_station_plot"))
        )
)
