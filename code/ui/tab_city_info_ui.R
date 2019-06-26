tabItem(tabName = "city_information",
        fluidRow(
          box(width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "City information",
            column(7, leafletOutput("city_map")),
            infoBoxOutput("city_population"),
            infoBoxOutput("num_stations_city"),
            infoBoxOutput("num_trips_city"),
            infoBoxOutput("city_rank")
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "Weather information",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(12,
                 tabsetPanel(type = "tabs",
                    tabPanel("Temperature", plotlyOutput("city_temperature_plot")),
                    tabPanel("Rain", plotlyOutput("city_rain_plot")),
                    tabPanel("Wind", plotOutput("city_wind_plot"))
                 )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            status = "danger",
            solidHeader = TRUE,
            title = "Visualizing bicycle information",
            width = 12,
            fluidRow(
              column(12, 
                tabsetPanel(type = "tabs",
                  tabPanel("General city usage view", 
                    br(),
                    fluidRow(
                      column(5, 
                        box(  
                          status = "info",
                          solidHeader = TRUE,
                          width = 12,
                          fluidRow(
                              column(6, radioButtons("city_usage_radio", label = "Choose your preferences",
                                      choices = list("Daily" = "daily_view", "Monthly" = "monthly_view"), selected = "daily_view"),
                                      checkboxInput("weekend_check", "Hide weekends", FALSE)),
                              column(6, checkboxGroupInput("city_usage_check", label = "Choose weather information", 
                                      choices = list("Day description" = "weather_description", "Temperature info" = "temp_info")))
                          )
                        )
                      ),
                      column(7, 
                          fluidRow(
                            infoBoxOutput("highest_temperature"),
                            infoBoxOutput("lowest_temperature")
                          )
                      )
                    ),
                    fluidRow(
                       column(12,plotOutput("city_usage_plot"))
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
                                    choices = list("Daily Profile" = "daily_profile", "Weekly Profile" = "weekly_profile"), selected = "daily_profile"))
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
