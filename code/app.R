#
# ---- Final Degree Thesis ----
# Alumni: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(shinyjs)
library(leaflet)
library(stringr)
library(ggplot2)

stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")

#General variables
attr1 <- list("Date" = "date", "Hour" = "hour","Houred" = "houred", "Total increment" = "totinc", "Demand" = "totdecr", "Median Bikes" = "medbikes")
attr2 <- list("Mean bikes" = "meanbikes", "Last bikes" = "lastbikes", "Probability Empty" = "propempty", "Probability Full" = "propfull", "Count" = "count")
bicycles_data_path <- "../datasets/bikes_agg_v2/"
weather_data_path <- "../datasets/weather_agg_v2/"

#General Functions
subset_by_date <- function(dataset, ini_date, end_date){
  subset <- cbind(dataset, date = tm1 <- as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
  subset[subset$date >= ini_date & subset$date <= end_date,]
}


# User Interface
ui <- fluidPage(theme = "webstyle.css",
   shinyjs::useShinyjs(),          
   headerPanel("Hello Shiny!"),
   navbarPage("",
        tabPanel("Map information",
          fluidRow(
            column(7,
                   h3("Stations Map"),
                   leafletOutput("map")),
            column(5,
                   fluidRow(
                     h3("Static information about the station"),
                     br(),
                     column(2, tags$img(src="city.png", height='50px',width='50px')),
                     column(8, verbatimTextOutput("city"))
                   ),
                   fluidRow(
                     column(2,tags$img(src="stands.png", height='50px',width='50px')),
                     column(8, verbatimTextOutput("stands"))
                   ),
                   fluidRow(
                     column(2,tags$img(src="bank.png", height='50px',width='50px')),
                     column(8, verbatimTextOutput("bank"))
                   ),
                   fluidRow(
                     column(2,tags$img(src="bonus.png", height='50px',width='50px')),
                     column(8, verbatimTextOutput("bonus"))
                   ),
                   
                   br(),
                   br(),
                   
                   fluidRow(
                     h4("Choose dataset atributes & Range of data"),
                     fluidRow(column(6, verbatimTextOutput("info_marker"))),
                     column(4, 
                          checkboxGroupInput("check_plot1", label = " ", choices = attr1, selected = attr1)
                     ),
                     column(4, 
                          checkboxGroupInput("check_plot2", label = " ", choices = attr2, selected = attr2)
                     ),
                     column(4, 
                            dateRangeInput('subset_date',
                                label = " ",
                                           start = "2014-09-29", "2015-06-31",
                                           min = "2014-09-29", max = "2015-07-01",
                                          startview = 'month', weekstart = 1),
                            helpText("The data will be reduced, a new dataset will be generated")
                     )
                   )
              )
          ),
          
          br(),
          br(),
          
          fluidRow(
            column(h3("Dataset bicycles"), dataTableOutput("station_data"), width = 12),
            column(h3("Dataset weather"), dataTableOutput("weather_data"), width = 12)
          ),
          
          br(),
          br(),
          
          fluidRow(
            column(h3("Visualize your selected data"), width = 12)
          ),
          
          fluidRow(
            column(3,
                  selectInput("xcol", 'X Variable', c()),
                  selectInput("ycol", 'Y Variable', c()),
                  selectInput("plot_type", 'Type of plot', c("Barplot","Scaterplot","Line plot"))
                  
            ),
            column(9, plotOutput("user_plot")
            )
          )
        ),
        tabPanel("Weekly demand",
          #TODO: Statistics page
          sidebarLayout(position = "right",
            sidebarPanel(
              helpText("Here you can choose a city and visualizate the dataset"),
              selectInput("cities_combo", "Choose a citie", unique(stations$CITY), selected = NULL, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              selectInput("stations_combo", "Choose the station", c(), selected = NULL, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              dateInput("date_picker", label = "or select a Monday", value = "2014-09-29",
                        min = "2014-09-29", max="2015-06-31", startview = "month", weekstart = 1)
            )
          ,
          mainPanel(
            verbatimTextOutput("date_text"),
            verbatimTextOutput("week_text"),
            plotOutput("weekly_demand_plot", width = "100%", height = "400px", click = NULL)
          )
        )
        )
        ,
        tabPanel("Bicycles & Weather"
            #TODO: Correlations between Bicycles and weather
        )
    )
  )



# Server function
server <- function(input, output, session) {
  
  # ------- Tab 1 "Map Information " -------------
  # Map render
  output$map <- renderLeaflet({
    leaflet(data = stations) %>% addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(), data = stations, 
                 popup = ~as.character(paste0("City: ", CITY ,
                                              "\nNum station: ", NUM_STATION ,
                                              "\nNum stands: ", STANDS)), layerId = ~ID)
  })
  
  # Checking if a marker is clicked
  observe({
    #Getting the event click
    click <- input$map_marker_click
    # Checking if is clicked or not
    if(is.null(click)){
      # Checkboxes & Combos disabled until user has clicked on a marker
      shinyjs::disable("check_plot1")
      shinyjs::disable("check_plot2")
      shinyjs::disable("subset_date")
      shinyjs::disable("xcol")
      shinyjs::disable("ycol")
      shinyjs::disable("plot_type")
      # Alert saying that you have to choose an station before visualize your data
      output$info_marker <- renderText({
        paste0("Please, choose an station!")
      })
      return() 
    }else {  
      #Enabling UI widgets
      shinyjs::hide("info_marker")
      shinyjs::enable("check_plot1")
      shinyjs::enable("check_plot2")
      shinyjs::enable("subset_date")
      shinyjs::enable("xcol")
      shinyjs::enable("ycol")
      shinyjs::enable("plot_type")
      
      city <- stations[click$id, 2]
      stands <- stations[click$id, 6]
      num_station <- stations[click$id, 3]
      bank <- stations[click$id, 7]
      bonus <- stations[click$id, 8]
      
      #Obtaining station & weather dataset
      bicycle_dataset <- read.csv(file = paste0(bicycles_data_path, city, ":", num_station,"/", city, ":", num_station, ".csv"), header=TRUE, sep=",")
      weather_dataset <- read.csv(file = paste0(weather_data_path, city ,"_agg.csv"), header=TRUE, sep=",")
      
      #Obtaining subsets by date
      bicycle_subset <- subset_by_date(bicycle_dataset, input$subset_date[1], input$subset_date[2])
      weather_subset <- subset_by_date(weather_dataset, input$subset_date[1], input$subset_date[2])
      
      
      #Rendering table with selected attributes
      output$station_data <- renderDataTable({
        atributes <-c(input$check_plot1, input$check_plot2)
        bicycle_subset[atributes]
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      output$weather_data <- renderDataTable({
        weather_subset
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      #Rendering user plot
      output$user_plot <- renderPlot({
          atributes <-c(input$check_plot1, input$check_plot2)
          bicycle_subset[atributes]
          ggplot(data = bicycle_subset) + geom_line(mapping = aes_string(x = input$xcol, y = input$ycol))
      })
      
      #Showing the summary information
      output$city <- renderText({ 
        paste0(city)
      })
      output$stands <- renderText({ 
        paste0(stands)
      })
      output$bank <- renderText({ 
        if(bank == FALSE){
          paste0("There is not banking")
        }else{
          paste0("There is banking")
        }
      })
      output$bonus <- renderText({ 
        if(bonus == FALSE){
          paste0("There is not bonus")
        }else{
          paste0("There is bonus")
        }
      })
    }
  })
  
  #Contolling wich attributes are selected
  observe({
    atributes <-c(input$check_plot1, input$check_plot2)
    updateSelectInput(session, "xcol",
                    label = "X Variable",
                    choices = atributes,
                    selected = NULL)
    updateSelectInput(session, "ycol",
                      label = "Y Variable",
                      choices = atributes,
                      selected = NULL)
  })
  
  
  # Defaul information texts (in the case that no station is selected)
  output$city <- renderText({ 
    paste0("Station not selected")
  })
  output$stands <- renderText({ 
    paste0("0 stands")
  })
  output$bank <- renderText({ 
    paste0("Station not selected")
  })
  output$bonus <- renderText({ 
    paste0("Station not selected")
  })

  
  
  # ------- Tab 2 "Weekly demand " ---------------

  # Check if a city is selected
  observe({
    city <- input$cities_combo
    if (is.null(city)){
      stations <- c()
    }else
      stations <- stations[stations$CITY == city, 3]
    # Can also set the label and select items
    updateSelectInput(session, "stations_combo",
                      label = "Choose a station",
                      choices = stations,
                      selected = NULL
    )
  })
  
  # Rendering the weekly demand plot with the selected city & station & dates
  output$weekly_demand_plot <- renderPlot({
    weekly_subset <- subset_by_date(input$cities_combo, 
                             input$stations_combo,
                             as.Date(input$date_picker), 
                             as.Date(input$date_picker) + 6)
    ggplot(data = weekly_subset) +
      geom_line(mapping = aes(x = c(1:168), y = totdecr)) +
      scale_x_continuous(breaks = seq(0, 168, by = 24))
  })
  
  # Debugging
  output$date_text <- renderText({
    ini_date <- as.Date(input$date_picker)
    end_date <- ini_date + 6
    paste0("Initial day: ", ini_date,
           "\nLast day: ", end_date)
  })
  
  
  
  
  # ------- Tab 3 "Bicycles & Weather" -----------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

