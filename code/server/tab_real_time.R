# ---------- Comparing stations -------------

# TODO: API connections 

output$weather <- renderText({
  # city <- input$selected_city
  # url <- paste0("https://api.openweathermap.org/data/2.5/weather?q=", city)
  # weather_info = jsonlite::fromJSON(url)
  # print(weather_info)
})

output$real_time_map <- renderLeaflet({
  stations_data <- stations[stations$CITY == input$selected_city,]
  leaflet(data = stations_data) %>% addTiles() %>% addMarkers(data = stations_data,
                                                              popup = ~as.character(paste0("Station number: ", NUM_STATION)), layerId = ~ID)
})

observe({
  #Getting the event click
  click <- input$real_time_map_marker_click
  # Checking if is clicked or not
  if(is.null(click)){
    return() 
  }else {
    city <- input$selected_city
    station_num <- stations[click$id, 3]
    url <- paste0("https://api.jcdecaux.com/vls/v1/stations/", station_num,"/?contract=", city,"&apiKey=",key)
    station_info = jsonlite::fromJSON(url)
    
    output$bike_stands <- renderInfoBox({ 
      infoBox(
        title = "Number of stands",
        icon = icon("star"),
        color = "light-blue",
        value = paste0(station_info$bike_stands, " stands")
      )
    })
    
    output$available_bikes <- renderInfoBox({ 
      infoBox(
        title = "Available bikes",
        icon = icon("star"),
        color = "yellow",
        value = paste0(station_info$available_bikes, " bikes")
      )
    })
    
    output$available_bike_stands <- renderInfoBox({ 
      infoBox(
        title = "Free docks",
        icon = icon("star"),
        color = "red",
        value = paste0(station_info$available_bike_stands, " docks")
      )
    })
  }
})


output$bike_stands <- renderInfoBox({ 
  infoBox(
    title = "Number of stands",
    icon = icon("star"),
    color = "light-blue",
    value = "Station not selected"
  )
})

  output$available_bikes <- renderInfoBox({ 
  infoBox(
    title = "Available bikes",
    icon = icon("star"),
    color = "yellow",
    value = "Station not selected"
  )
})

output$available_bike_stands <- renderInfoBox({ 
  infoBox(
    title = "Available stands",
    icon = icon("star"),
    color = "red",
    value = "Station not selected"
  )
})

