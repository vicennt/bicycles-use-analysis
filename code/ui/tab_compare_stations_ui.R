tabItem(tabName = "compare_stations",
        fluidRow(column(12, h3("Comparing stations"))),
        fluidRow(
          box(plotOutput("selected_station_plot")),
          box(plotOutput("compare_station_plot"))
        )
)
