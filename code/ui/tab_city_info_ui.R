tabItem(tabName = "city_information",
        #fluidRow(column(6, h3("Bicycle information"))),
        fluidRow(
          box(
            status = "warning",
            title = "Bicycle information",
            width = 12,
            column(5, 
              box(
                status = "info",
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
        
        br(),
        
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
        
        fluidRow(
          box(
            status = "warning",
            solidHeader = TRUE,
            title = "Visualizing the city demand",
            width = 12,
            fluidRow(
              column(12, 
                tabsetPanel(type = "tabs",
                  tabPanel("General city demand view", 
                    br(),
                    box(  
                     status = "info",
                     solidHeader = TRUE,
                     width = 4,
                     fluidRow(
                       column(6, radioButtons("city_demand_radio", label = "Choose your preferences",
                                      choices = list("Daily" = "daily_view", "Monthly" = "monthly_view"), selected = "daily_view")),
                       column(6, checkboxGroupInput("city_demand_check", label = "Choose weather information", 
                                      choices = list("Temperature info" = "temp_info", "Rain info" = "rain_info", "Wind info" = "wind_info")))
                     )
                    ),
                    fluidRow(
                       column(12,plotOutput("city_demand_plot"))
                    )
                  ),
                  tabPanel("City profiles", 
                    br(),
                    box(  
                       status = "info",
                       solidHeader = TRUE,
                       width = 4,
                       fluidRow(
                          column(6, radioButtons("city_profiles_radio", label = "Choose your preferences",
                                    choices = list("Weekly Profile" = "weekly_profile", "Daily Profile" = "daily_profile"), selected = "weekly_profile"))
                       )
                    ), 
                    fluidRow(
                      column(12,plotOutput("city_profile_plot"))
                    )
                  )
                )
              )
            )
          )
      )
)
