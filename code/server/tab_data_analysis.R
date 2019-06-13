# ------- Tab 1 "Map Information " -------------


#---------- INFORMATION SECTION ----------------

observe({
  #Selected city
  city <- input$selected_city
  selected_city <- input$selected_city
  city_stations_demand_info <- filter(usage_station, city == selected_city)
  city_stations_demand_ranking <- city_stations_demand_info[order(city_stations_demand_info$average_demand, decreasing = TRUE),]
 
   output$num_stations_city <- renderInfoBox({
    num_stations <- count(select(filter(stations, CITY == city), NUM_STATION))
    infoBox(
      title = "Number of total stations",
      icon = icon("thumbtack"),
      color = "purple",
      value = paste0(num_stations, " stations")
    )
  })
   
   output$station_high_demand_city <- renderInfoBox({
     infoBox(
       title = "Station with highest demand",
       icon = icon("arrow-circle-up"),
       color = "red",
       value = paste0("Station number ", head(city_stations_demand_ranking, 1)$station)
     )
   })
   output$station_low_demand_city <- renderInfoBox({
     infoBox(
       title = "Station with lowest demand",
       icon = icon("arrow-alt-circle-down"),
       color = "green",
       value = paste0("Station number ", tail(city_stations_demand_ranking, 1)$station)
     )
   })
   
   output$city_population <- renderInfoBox({
     
     infoBox(
       title = "City Population",
       icon = icon("users"),
       color = "teal",
       value = paste0(filter(cities, NAME == city)$POPULATION, " people")
     )
   })
  output$num_trips_city <- renderInfoBox({
    data <- bicycles_dict[[input$selected_city]]
    num_trips <- sum(data$totinc)/nrow(stations[stations$CITY == city,])
    infoBox(
      title = "Number of trips during this period",
      icon = icon("bicycle"),
      color = "maroon",
      value = paste0(format(round(num_trips, 1), nsmall = 1), " rides")
    )
  })
  output$city_rank <- renderInfoBox({
    # Demand usage ranking 
    usage_city_ranking <- usage_city[order(usage_city$average_demand, decreasing = TRUE),]
    print(usage_city_ranking)
    rank_pos <- which(usage_city_ranking$city == city)
    infoBox(
      title = "City ranking position",
      icon = icon("chart-line"),
      color = "yellow",
      value = paste0("Position ", rank_pos, " of ", nrow(cities_names), " cities") #TODO: Change fix cities number
    )
  })
  
  #Rendering weather info
  output$rainy_days <- renderInfoBox({
    #TODO: Obtain the number of rainny days
    infoBox(
      title = "Number of rainny days",
      icon =  icon("cloud-rain"),
      color = "light-blue",
      value = ""
    )
  })
  output$sunny_days <- renderInfoBox({
    #TODO: Obtain the number of sunny days
    infoBox(
      title = "Number of sunny days",
      icon =  icon("sun"),
      color = "yellow",
      value = ""
    )
  })
  output$snowy_days <- renderInfoBox({
    #TODO: Obtain the number of snowy days
    infoBox(
      title = "Number of snowy days",
      icon = icon("snowflake"),
      color = "black",
      value = ""
    )
  })
  output$highest_temperature <- renderInfoBox({
    #TODO: Obtain highest temperature
    infoBox(
      title = "Highest temperature",
      icon = icon("temperature-high"),
      color = "red",
      value = ""
    )
  })
  output$lowest_temperature <- renderInfoBox({
    #TODO: Obtain lowest temperature
    infoBox(
      title = "Lowest temperature",
      icon = icon("temperature-low"),
      color = "blue",
      value = ""
    )
  })
  output$average_windy <- renderInfoBox({
    #TODO: Obtain average wind velocity
    infoBox(
      title = "Average wind velocity in KM",
      icon = icon("wind"),
      color = "aqua",
      value = ""
    )
  })  
})




#---------- MAP SECTION ----------------


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
    return() 
  }else {  
    city <- stations[click$id, 2]
    stands <- stations[click$id, 6]
    num_station <- stations[click$id, 3]
    bank <- stations[click$id, 7]
    bonus <- stations[click$id, 8]

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

# Station plot information
output$station_plot <- renderPlot({
  
})


# ---------------------------------
  
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


# ---------- Comparing plots -------------

# Cities
output$selected_city_plot <- renderPlot({

})

output$compare_city_plot <- renderPlot({
  
})

# Stations
output$selected_station_plot <- renderPlot({
  
})

output$compare_stations_plot <- renderPlot({
  
})

  

