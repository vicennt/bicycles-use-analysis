#---------- MAP SECTION ----------------
output$map <- renderLeaflet({
  stations_data <- stations[stations$CITY == cities$input,]
  leaflet(data = ) %>% addTiles() %>%
    addMarkers(data = stations_data, layerId = ~ID)
})

# Station plot information
output$station_plot <- renderPlot({
  
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
