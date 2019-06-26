tabItem(tabName = "profile_comparison",
    fluidRow(
      br(),
      br(),
      box(
        status = "danger",
        title = "Comparing cities profiles",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, 
             box(  
                status = "info",
                solidHeader = TRUE,
                width = 12,    
                selectInput("city_one_comparation", "Select first city", cities$NAME),
                selectInput("city_two_comparation", "Select second city", cities$NAME)
             )
          ),
          column(9, plotOutput("compare_cities_plot")))
      )
    ),
    fluidRow(
      br(),
      br(),
      box(
        status = "danger",
        title = "Comparing station profiles",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3,
            box(  
                status = "info",
                solidHeader = TRUE,
                width = 12,    
                selectInput("station_one_comparation_city", "Select first city", cities$NAME),
                selectInput("station_one_comparation_station", "Select station", cities$NAME)
            ),
            box(  
              status = "info",
              solidHeader = TRUE,
              width = 12,    
              selectInput("station_two_comparation_city", "Select second city", cities$NAME),
              selectInput("station_two_comparation_station", "Select station", cities$NAME)
            )
          ),
          column(9,
            plotOutput("compare_stations_plot")
          )
        )
      )
    )
    
)