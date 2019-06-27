# ---------- Comparing stations -------------

# TODO: API connections 

api_call <- reactive({
  invalidateLater(100000)
  city <- input$selected_city
  url <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=", city,"&apiKey=", key)
  df <- jsonlite::fromJSON(url)
  df
})

observeEvent(api_call, {
  aux <- match("TRUE", city_station_info$available_bikes == 0) 
  if(!is.na(aux)){
    str <- paste0("Station ", city_station_info[aux,]$number, " is full!!")
    showNotification(str, type = danger, duration = 1)
    
  }
})

output$last_update <- renderText({
  city_station_info <<- api_call()
  t <- city_station_info$last_update[1] / 1000
  date <- as.POSIXct(t, origin="1970-01-01")
  paste0("Last update: ", date)
})

output$real_time_map <- renderLeaflet({
  city_station_info <<- api_call()
  getColor <- function(city_station_info) {
    sapply(city_station_info$available_bikes, function(available_bikes) {
      if(available_bikes >= 10) {
        "green"
      } else if(available_bikes >= 6) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'bike',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(city_station_info)
  )
  
  leaflet(data = city_station_info) %>% addTiles() %>% addAwesomeMarkers(lng = city_station_info$position$lng, lat = city_station_info$position$lat, data = city_station_info,
                                                                  icon = icons, popup = ~as.character(paste0("Station number ", number)), layerId = ~number)
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
        color = "red",
        value = paste0(filter(city_station_info, number == num_station)$bike_stands, " bike stands")
      )
    })
    
    output$available_bikes <- renderInfoBox({ 
      infoBox(
        title = "Available bikes",
        icon = icon("bicycle"),
        color = "green",
        value = paste0(filter(city_station_info, number == num_station)$available_bikes, "  bikes")
      )
    })
    
    output$available_bike_stands <- renderInfoBox({ 
      infoBox(
        title = "Free docks",
        icon = icon("parking"),
        color = "light-blue",
        value = paste0(filter(city_station_info, number == num_station)$available_bike_stands, " bike stands")
      )
    })
  }
})


output$bike_stands <- renderInfoBox({ 
  infoBox(
    title = "Number of stands",
    icon = icon("star"),
    color = "red",
    value = "Station not selected"
  )
})

output$available_bikes <- renderInfoBox({ 
  infoBox(
    title = "Available bikes",
    icon = icon("bicycle"),
    color = "green",
    value = "Station not selected"
  )
})

output$available_bike_stands <- renderInfoBox({ 
  infoBox(
    title = "Available stands",
    icon = icon("parking"),
    color = "light-blue",
    value = "Station not selected"
  )
})

