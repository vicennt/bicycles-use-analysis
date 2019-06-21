#---------- MAP SECTION ----------------
output$map <- renderLeaflet({
  stations_data <- stations[stations$CITY == input$selected_city,]
  leaflet(data = stations_data) %>% addTiles() %>% addMarkers(data = stations_data,
                    popup = ~as.character(paste0("Station number: ", NUM_STATION)), layerId = ~ID)
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
    station <- stations[click$id, 3]
    stands <- stations[click$id, 6]
    num_station <- stations[click$id, 3]
    bank <- stations[click$id, 7]
    bonus <- stations[click$id, 8]
    
    
    output$stands_box <- renderInfoBox({ 
      infoBox(
        title = "Number of Stands",
        icon = icon("bicycle"),
        color = "blue",
        value = paste0(stands)
      )
    })
    
    output$service_box <- renderInfoBox({ 
      text <- " "
      if(bank == FALSE && bonus == FALSE){
        text <- paste0("There is not services")
      }else if (bank == TRUE && bonus == FALSE){
        text <- paste0("There is banking service")
      }else{
        text <- paste0("There is banking & bonus")
      }
      
      infoBox(
        title = "Station services",
        icon = icon("star"),
        color = "yellow",
        value = text
      )
    })
    
    output$week_station_demand_plot <- renderPlot({
      selected_city <- input$selected_city
      date <- input$date_picker_week
      subset <- get_weekly_demand_vector(bicycles_dict[[selected_city]], date, station)
      ggplot(data= subset, aes(x = 1:168, y = totdecr)) + 
        geom_line(group = 1, stat = "identity", colour = "#CC0000") +
        ylab('Demand') +
        xlab('Hour week')
    })
  }
})


# Station plot information
output$station_plot <- renderPlot({
  
})




# Rendering infoboxes
output$stands_box <- renderInfoBox({ 
  infoBox(
    title = "Number of Stands",
    icon = icon("bicycle"),
    color = "blue",
    value = "Station not selected"
  )
})

output$service_box <- renderInfoBox({ 
  infoBox(
    title = "Station services",
    icon = icon("star"),
    color = "yellow",
    value = "Station not selected"
  )
})

observe({
  #Selected city
  selected_city <- input$selected_city
  # Usage information (demand)
  city_stations_demand_info <- filter(info_usage_station, city == selected_city)
  # Station usage ranking (selected city)
  city_stations_demand_ranking <- city_stations_demand_info[order(city_stations_demand_info$average_demand, decreasing = TRUE),]
  
  output$station_high_demand_city <- renderInfoBox({
    infoBox(
      title = "Station with highest demand",
      icon = icon("arrow-circle-up"),
      color = "red",
      width = 12,
      value = paste0("Station number ", head(city_stations_demand_ranking, 1)$station)
    )
  })
  
  output$station_low_demand_city <- renderInfoBox({
    infoBox(
      title = "Station with lowest demand",
      icon = icon("arrow-circle-down"),
      color = "green",
      width = 12,
      value = paste0("Station number ", tail(city_stations_demand_ranking, 1)$station)
    )
  })
  
})


