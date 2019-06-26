tabItem(tabName = "stations_information",
       fluidRow(
          box(width = 12,
              status = "warning",
              solidHeader = TRUE,
              title = "Stations information",
              column(7 ,leafletOutput("map")),
              infoBoxOutput("stands_box"),
              infoBoxOutput("service_box"),
              infoBoxOutput("station_high_demand_city"),
              infoBoxOutput("station_low_demand_city")
          )
       ),
       
       fluidRow(
         box(
           id = "station_bicycle_box",
           title = "Visualizing station bicycle information",
           status = "danger",
           solidHeader = TRUE,
           width = 12,
           fluidRow(
             column(12,
                tabsetPanel(type = "tabs",
                   tabPanel("Station demand by date",
                       br(),
                       fluidRow(
                           column(5, 
                                 box(  
                                     status = "info",
                                     solidHeader = TRUE,
                                     width = 12,
                                     fluidRow(
                                         column(6, radioButtons("station_demand_radio", label = "Choose your preferences",
                                                                choices = list("Daily" = "daily_view", "Weekly" = "weekly_view"), selected = "daily_view")),
                                         column(6, dateInput("date_picker_station", label = "Select a date", value = "2014-10-06",
                                                             min = "2014-10-06", max="2015-04-30", startview = "month", weekstart = 1))
                                    )
                                 )
                           )
                        ),
                        fluidRow(
                            column(12, plotOutput("station_demand_plot"))
                        )
                   ),
                   tabPanel("Visualizing profiles",
                        br(),
                        box(  
                           status = "info",
                           solidHeader = TRUE,
                           width = 4,
                           fluidRow(
                              column(6, radioButtons("station_profiles_radio", label = "Choose your preference",
                                                                    choices = list("Daily Profile" = "daily_profile", "Weekly Profile" = "weekly_profile"), selected = "daily_profile"))
                           )
                         ), 
                         fluidRow( column(12,plotOutput("station_profile_plot")))
                   )
                 )
              )
            )
          )
       )
)

