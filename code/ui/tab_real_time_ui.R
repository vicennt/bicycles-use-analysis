tabItem(tabName = "real_time",
    box(
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        title = "Real time station information",
        fluidRow(column(12, verbatimTextOutput("weather"))),
        fluidRow(column(12, leafletOutput("real_time_map"))),
        fluidRow(column(4, offset = 8, br(), verbatimTextOutput("last_update"))),
        fluidRow(column(12, verbatimTextOutput("api_info"))),
        fluidRow(
          br(),
          infoBoxOutput("bike_stands"),
          infoBoxOutput("available_bikes"),
          infoBoxOutput("available_bike_stands")
        )
    ),
    fluidRow(br())
)
