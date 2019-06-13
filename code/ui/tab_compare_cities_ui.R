tabItem(tabName = "compare_cities",
        fluidRow(column(12, h3("Comparing cities"))),
        fluidRow(
          box(plotOutput("selected_city_plot")),
          box(plotOutput("compare_city_plot"))
        )
)
