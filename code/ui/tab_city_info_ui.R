tabItem(tabName = "city_information",
        #fluidRow(column(6, h3("Bicycle information"))),
        fluidRow(
          box(
            status = "warning",
            title = "Bicycle information",
            width = 12,
            column(5, 
              box(
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                leafletOutput("city_map")
              )
            ),
            br(),
            br(),
            column(7, 
              fluidRow(
                column(6, uiOutput("city_population")),
                column(6, uiOutput("num_stations_city"))
              ),
              fluidRow(
                column(6, uiOutput("station_high_demand_city")),
                column(6, uiOutput("station_low_demand_city"))
              ),
              fluidRow(
                column(6, uiOutput("num_trips_city")),
                column(6, uiOutput("city_rank"))
              )
            )
          )
        ),
        #fluidRow(column(6, h3("Weather information"))),
        fluidRow(
          box(
            title = "Weather information",
            status = "warning",
            width = 12,
            fluidRow(
              column(8 ,
                 tabsetPanel(type = "tabs",
                    tabPanel("Summary days", plotOutput("weather_days_plot")),
                    tabPanel("Temperature", plotOutput("city_temperature_plot")),
                    tabPanel("Rain", plotOutput("city_rain_plot")),
                    tabPanel("Wind", plotOutput("city_wind_plot"))
                 )
              ),
              br(),
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
