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
              infoBoxOutput("station_low_demand_city"),
              fluidRow(
                box(
                  status = "danger",
                  solidHeader = TRUE,
                  title = " ",
                  width = 12,
                  fluidRow(
                    column(12,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Station demand by week",
                                                fluidRow(
                                                  column(3, 
                                                         dateInput("date_picker_week", label = "Select monday", value = "2014-10-06",
                                                                   min = "2014-10-06", max="2015-04-30", startview = "month", weekstart = 1)
                                                  )
                                                ),
                                                fluidRow(
                                                  column(12, plotOutput("week_station_demand_plot"))
                                                )
                                       ),
                                       tabPanel("Station demand by day", 
                                                fluidRow(
                                                  column(3, 
                                                         dateInput("date_picker_day", label = "Select day", value = "2014-10-06",
                                                                   min = "2014-10-06", max="2015-04-30", startview = "month", weekstart = 1)
                                                  )
                                                ),
                                                fluidRow(
                                                  column(12, plotOutput("day_station_demand_plot"))
                                                )
                                       ),
                                       tabPanel("Station profiles",
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
                                             fluidRow(
                                               column(12,plotOutput("station_profile_plot"))
                                             )
                                       ),
                                       tabPanel("Compare stations profiles")
                           )
                    )
                  )
                )
              )
          )
       ),
       fluidRow(column(12, h4("Comparing diferent stations demand"))),
       fluidRow(
         box(
           status = "danger",
           solidHeader = TRUE,
           title = "Choose the first station",
           width = 6,
           fluidRow(
             plotOutput("compare_station_one")
           )
         ),
         box(
           status = "danger",
           solidHeader = TRUE,
           title = "Choose the second station",
           width = 6,
           fluidRow(
             plotOutput("compare_station_two")
           )
         )
       )
)

