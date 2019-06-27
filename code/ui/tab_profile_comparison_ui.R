tabItem(tabName = "profile_comparison",
    fluidRow(
      box(
        status = "danger",
        title = "Comparing cities profiles",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          br(),
          column(3, 
             box(  
                status = "info",
                solidHeader = TRUE,
                width = 12,    
                selectInput("city_one_comparation", "Select first city", cities$NAME),
                selectInput("city_two_comparation", "Select second city", cities$NAME, selected = head(cities$NAME, 2)),
                radioButtons("city_comparation_radio", label = "Choose your view",
                             choices = list("Daily" = "daily_view", "Weekly" = "weekly_view"), selected = "daily_view")
             )
          ),
          column(9, plotOutput("compare_cities_plot")))
      )
    ),
    fluidRow(
      box(
        status = "danger",
        title = "Comparing station profiles",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          br(),
          column(3,
            box(  
                status = "info",
                solidHeader = TRUE,
                width = 12,    
                selectInput("station_one_comparation_city", "Select first city", cities$NAME),
                selectInput("station_one_comparation_station", "Select station", c(""))
            ),
            box(  
              status = "info",
              solidHeader = TRUE,
              width = 12,    
              selectInput("station_two_comparation_city", "Select second city", cities$NAME, selected = head(cities$NAME, 1)),
              selectInput("station_two_comparation_station", "Select station", c(""))
            ),
            box(  
              status = "info",
              solidHeader = TRUE,
              width = 12,  
              radioButtons("station_comparation_radio", label = "Choose your view",
                         choices = list("Daily" = "daily_view", "Weekly" = "weekly_view"), selected = "daily_view")
            )
          ),
          column(9,
            plotOutput("compare_stations_plot")
          )
        )
      )
    )
    
)