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
          box(
            status = "warning",
            width = 12,
            fluidRow(
              column(8 ,plotOutput("weather_days")),
              br(),
              infoBoxOutput("highest_temperature"),
              infoBoxOutput("lowest_temperature"),
              infoBoxOutput("average_windy")
              )
            )
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            status = "warning",
            solidHeader = TRUE,
            title = "Visualize the city demand",
            width = 12,
            fluidRow(
              column(2, radioButtons("city_demand_view", label = "Choose your preferences",
                           choices = list("Daily" = "daily", "Monthly" = "monthly"), 
                           selected = "daily")),
              column(2,  checkboxGroupInput("city_demand_cheks", label = " ", 
                                            choices = list("Temperature info" = "temp_info", "Rain info" = "rain_info", "Wind info" = "wind_info"),
                                            selected = 1))
            ),
            fluidRow(
              column(12, plotOutput("city_demand"))
            )
          )
        )
)
