# ---------- Comparing stations -------------

# TODO: API connections 

api_call <- reactive({
  invalidateLater(100000)
  city <- input$selected_city
  url <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=", city,"&apiKey=", key)
  df <- jsonlite::fromJSON(url)
  df
})


output$real_time_map <- renderLeaflet({
  city_station_info <<- api_call()
  getColor <- function(city_station_info) {
    sapply(city_station_info$available_bikes, function(available_bikes) {
      if(available_bikes >= 15) {
        "green"
      } else if(available_bikes >= 8) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(city_station_info)
  )
  
  leaflet(data = city_station_info) %>% addTiles() %>% addAwesomeMarkers(lng = city_station_info$position$lng, lat = city_station_info$position$lat, data = city_station_info,
                                                                  icon = icons, popup = ~as.character(paste0("Available bikes: ", available_bikes,"\n Available stands: ", available_bike_stands)), layerId = ~number)
})

observe({
  click <- input$real_time_map_marker_click
  if(is.null(click)){
    return() 
  }else {
    num_station <- click$id
    output$bike_stands <- renderInfoBox({ 
      infoBox(
        title = "Number of stands",
        icon = icon("star"),
        color = "light-blue",
        value = paste0(select(city_station_info, number == num_station)$bike_stands, " stands")
      )
    })
    
    output$available_bikes <- renderInfoBox({ 
      infoBox(
        title = "Available bikes",
        icon = icon("star"),
        color = "yellow",
        value = paste0(select(city_station_info, number == num_station)$available_bikes, " bikes")
      )
    })
    
    output$available_bike_stands <- renderInfoBox({ 
      infoBox(
        title = "Free docks",
        icon = icon("star"),
        color = "red",
        value = paste0(select(city_station_info, number == num_station)$available_bike_stands, " docks")
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

