  # ------- Tab 1 "Map Information " -------------
  # Map render
  output$map <- renderLeaflet({
    stations_data <- stations[stations$CITY == cities$input,]
    leaflet(data = ) %>% addTiles() %>%
      addMarkers(data = stations_data, layerId = ~ID)
  })
  
  # Checking if a marker is clicked
  observe({
    #Getting the event click
    click <- input$map_marker_click
    # Checking if is clicked or not
    if(is.null(click)){
      # Checkboxes & Combos disabled until user has clicked on a marker
      shinyjs::disable("subset_date")
      shinyjs::disable("xcol")
      shinyjs::disable("ycol")
      shinyjs::disable("plot_type")
      return() 
    }else {  
      #Enabling UI widgets
      shinyjs::hide("alert")
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
        bicycle_subset
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      #Rendering weather data
      output$weather_data <- renderDataTable({
        weather_subset
      }, options = list(scrollX = TRUE, pageLength = 5))
      
      #Rendering user plot
      output$user_plot <- renderPlot({
          plot_type <- input$plot_type
          ggplot(data = bicycle_subset) + get(plot_type)(mapping = aes_string(x = input$xcol, y = input$ycol))
      })
      
      #Showing the summary information
      output$city_box <- renderInfoBox({ 
        infoBox(
          title = "City",
          icon = icon("map-marker-alt"),
          color = "red",
          value = paste0(city)
        )
      })
      output$stands_box <- renderInfoBox({ 
        infoBox(
          title = "Number of Stands",
          icon = icon("bicycle"),
          color = "olive",
          value = paste0(stands)
        )
      })
      output$bank_box <- renderInfoBox({ 
        text <- " "
        if(bank == FALSE){
          text <- paste0("There is banking")
        }else{
          text <- paste0("There is not banking")
        }
        
        infoBox(
          title = "Bank service",
          icon = icon("btc"),
          color = "yellow",
          value = text
        )
      })
      output$bonus_box <- renderInfoBox({ 
        text <- " "
        if(bonus == FALSE){
          text <- paste0("There is not bonus")
        }else{
          text <- paste0("There is bonus")
        }
        infoBox(
          title = "Bonus service",
          icon = icon("award"),
          color = "aqua",
          value = text
        )
      })
    }
  })
  
  #Contolling wich attributes are selected
  observe({
    updateSelectInput(session, "xcol",
                    label = "X Variable",
                    choices = c(),
                    selected = NULL)
    updateSelectInput(session, "ycol",
                      label = "Y Variable",
                      choices = c(),
                      selected = NULL)
  })
  
  
  # Rendering infoboxes
  output$city_box <- renderInfoBox({
    infoBox(
      title = "City",
      icon = icon("map-marker-alt"),
      color = "red",
      value = "Station not selected"
    )
  })
  output$stands_box <- renderInfoBox({ 
    infoBox(
      title = "Number of Stands",
      icon = icon("bicycle"),
      color = "olive",
      value = "Station not selected"
    )
  })
  output$bank_box <- renderInfoBox({ 
    infoBox(
      title = "Bank service",
      icon = icon("btc"),
      color = "yellow",
      value = "Station not selected"
    )
  })
  output$bonus_box <- renderInfoBox({ 
    infoBox(
      title = "Bonus service",
      icon = icon("award"),
      color = "aqua",
      value = "Station not selected"
    )
  })
  
  
  
  observe({
    #Selected city
    city <- input$selected_city
    output$num_stations_city <- renderInfoBox({
      num_stations <- nrow(stations[stations$CITY == city,])
      infoBox(
        title = "Number of total stations",
        icon = icon("thumbtack"),
        color = "purple",
        value = paste0("Number of stations ", num_stations)
      )
    })
    output$num_trips_city <- renderInfoBox({
      infoBox(
        title = "Number of trips during this period",
        icon = icon("bicycle"),
        color = "maroon",
        value = "XX"
      )
    })
    output$percentage_usage_city <- renderInfoBox({
      #TODO: Obtain percentage of bicycle usage
      infoBox(
        title = "Percentage of bicycle usage",
        icon = icon("percentage"),
        color = "yellow",
        value = "Station not selected"
      )
    })
    output$station_high_demand_city <- renderInfoBox({
      #TODO: Obtain city with highest demand
      infoBox(
        title = "Station with highest demand",
        icon = icon("arrow-circle-up"),
        color = "red",
        value = "Station not selected"
      )
    })
    output$station_low_demand_city <- renderInfoBox({
      #TODO: Obtain city with lowest demand
      infoBox(
        title = "Station with lowest demand",
        icon = icon("arrow-alt-circle-down"),
        color = "green",
        value = "Station not selected"
      )
    })
    output$other <- renderInfoBox({
      #TODO: Obtain city with lowest demand
      infoBox(
        title = "Bonus service",
        icon = icon("award"),
        color = "teal",
        value = "Station not selected"
      )
    })
    
    #Rendering weather info
    
    #Rendering weather info
    output$rainy_days <- renderInfoBox({
      #TODO: Obtain the number of rainny days
      infoBox(
        title = "Number of rainny days",
        icon =  icon("cloud-rain"),
        color = "light-blue",
        value = "Station not selected"
      )
    })
    output$sunny_days <- renderInfoBox({
      #TODO: Obtain the number of sunny days
      infoBox(
        title = "Number of sunny days",
        icon =  icon("sun"),
        color = "yellow",
        value = "Station not selected"
      )
    })
    output$snowy_days <- renderInfoBox({
      #TODO: Obtain the number of snowy days
      infoBox(
        title = "Number of snowy days",
        icon = icon("snowflake"),
        color = "black",
        value = "Station not selected"
      )
    })
    output$highest_temperature <- renderInfoBox({
      #TODO: Obtain highest temperature
      infoBox(
        title = "Highest temperature",
        icon = icon("temperature-high"),
        color = "red",
        value = "Station not selected"
      )
    })
    output$lowest_temperature <- renderInfoBox({
      #TODO: Obtain lowest temperature
      infoBox(
        title = "Lowest temperature",
        icon = icon("temperature-low"),
        color = "blue",
        value = "Station not selected"
      )
    })
    output$average_windy <- renderInfoBox({
      #TODO: Obtain average wind velocity
      infoBox(
        title = "Average wind velocity in KM",
        icon = icon("wind"),
        color = "aqua",
        value = "Station not selected"
      )
    })  
  })
