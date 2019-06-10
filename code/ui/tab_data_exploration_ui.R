tabItem(tabName = "dataset",
  fluidRow(
    tabBox(id = "datasets_box",
         title = tagList(shiny::icon("database"), "Explore the data"),
         width = 12,
         tabPanel("Bicycles data", dataTableOutput("station_data")),
         tabPanel("Weather data", dataTableOutput("weather_data"))
    )
  )
)