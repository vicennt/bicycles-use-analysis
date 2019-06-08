tabItem(tabName = "past",
        fluidRow(column(6, h3("Bicycle information"))),
        fluidRow( 
          column(12,
            infoBoxOutput("num_stations_city"),
            infoBoxOutput("num_trips_city"),
            infoBoxOutput("percentage_usage_city"),
            infoBoxOutput("station_high_demand_city"),
            infoBoxOutput("station_low_demand_city"),
            infoBoxOutput("other")
          )
        ),
        fluidRow(column(6, h3("Weather information"))),
        fluidRow(
           column(12,
             infoBoxOutput("rainy_days"),
             infoBoxOutput("sunny_days"),
             infoBoxOutput("snowy_days"),
             infoBoxOutput("highest_temperature"),
             infoBoxOutput("lowest_temperature"),
             infoBoxOutput("average_windy")
           )
        ),
        fluidRow(
          box(width = 12,
              status = "warning",
              solidHeader = TRUE,
              title = "Stations Map",
              column(7 ,leafletOutput("map")),
              infoBoxOutput("city_box"),
              infoBoxOutput("stands_box"),
              infoBoxOutput("bank_box"),
              infoBoxOutput("bonus_box")
          )
        ),
        br(),
        code(id="alert", "Select an station before continue!!!!!"),
        br(),
        br(),
        fluidRow(
          tabBox(id = "datasets_box",
                 title = tagList(shiny::icon("database"), "Explore the data"),
                 width = 12,
                 tabPanel("Bicycles data", dataTableOutput("station_data")),
                 tabPanel("Weather data", dataTableOutput("weather_data"))
          )
        ),
        br(),
        br(),
        fluidRow(
          column(3,
            box(width = 12,
                id = "graph_attr_box",
                title = "Choose attributes",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "warning",
                selectInput("ycol", 'Y Variable', c()),
                selectInput("xcol", 'X Variable', c()),
                selectInput("plot_type", 'Type of plot', c("Line plot" = "geom_line","Barplot" = "geom_bar","Scaterplot" = "geom_point"))
                         )
            ),
          column(9, 
             box(id = "plot_box", 
                 width = 12, 
                 collapsible = TRUE,
                 solidHeader = TRUE,
                 status = "warning",
                 plotOutput("user_plot")
             )
          )
      )
)
