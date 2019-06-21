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
            status = "danger",
            solidHeader = TRUE,
            title = "Visualizing the station demand",
            width = 12,
            fluidRow(
              column(12,
                  tabsetPanel(type = "tabs",
                     tabPanel("Station demand by day", plotOutput("station_plot")),
                     tabPanel("Station demand by week"),
                     tabPanel("Station profiles"),
                     tabPanel("Compare stations profiles")
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

