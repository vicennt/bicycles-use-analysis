#---------- City information----------------



observe({
  #Selected city
  selected_city <- input$selected_city
  city_info <- filter(cities, NAME == selected_city)
  # Usage information (demand)
  city_stations_demand_info <- filter(info_usage_station, city == selected_city)
  # Station usage ranking (selected city)
  city_stations_demand_ranking <- city_stations_demand_info[order(city_stations_demand_info$average_demand, decreasing = TRUE),]
  # Cities usage ranking (all cities)
  usage_city_ranking <- info_usage_city[order(info_usage_city$average_demand, decreasing = TRUE),]
  # Weather information daily aggregated
  df_weather_daily <- weather_dict_daily[[selected_city]]
  
  output$city_map <- renderLeaflet({
    leaflet(data = city_info) %>% addTiles() %>% addMarkers(data = city_info)
  })
  
  output$num_stations_city <- renderUI({
    num_stations <- count(select(filter(stations, CITY == selected_city), NUM_STATION))
    infoBox(
      title = "Number of total stations",
      icon = icon("thumbtack"),
      color = "purple",
      width = 12,
      value = paste0(num_stations, " stations")
    )
  })
   
   output$station_high_demand_city <- renderUI({
     infoBox(
       title = "Station with highest demand",
       icon = icon("arrow-circle-up"),
       color = "red",
       width = 12,
       value = paste0("Station number ", head(city_stations_demand_ranking, 1)$station)
     )
   })
   
   output$station_low_demand_city <- renderUI({
     infoBox(
       title = "Station with lowest demand",
       icon = icon("arrow-alt-circle-down"),
       color = "green",
       width = 12,
       value = paste0("Station number ", tail(city_stations_demand_ranking, 1)$station)
     )
   })
   
   output$city_population <- renderUI({
     infoBox(
       title = "City Population",
       icon = icon("users"),
       color = "aqua",
       width = 12,
       value = paste0(filter(cities, NAME == selected_city)$POPULATION, " people")
     )
   })
   
  output$num_trips_city <- renderUI({
    num_trips <- sum(bicycles_dict_daily[[selected_city]]$totinc)
    infoBox(
      title = "Number of trips during this period",
      icon = icon("bicycle"),
      color = "navy",
      width = 12,
      value = paste0(format(round(num_trips, 1), nsmall = 1), " rides")
    )
  })
  
  output$city_rank <- renderUI({
    rank_pos <- which(usage_city_ranking$city == selected_city)
    infoBox(
      title = "City ranking position",
      icon = icon("chart-line"),
      color = "yellow",
      width = 12,
      value = paste0("Position ", rank_pos, " of 27 cities") #TODO: Change fix cities number
    )
  })
  
#  ------------- Rendering weather info -----------------
  output$highest_temperature <- renderInfoBox({
    highest_temp <- max(df_weather_daily$main_temp_max)
    infoBox(
      title = "Highest temperature",
      icon = icon("temperature-high"),
      color = "red",
      value = paste0(format(round(highest_temp, 1), nsmall = 1), " ºC")
    )
  })
  
  output$lowest_temperature <- renderInfoBox({
    lowest_temp <- min(df_weather_daily$main_temp_max)
    infoBox(
      title = "Lowest temperature",
      icon = icon("temperature-low"),
      color = "blue",
      value = paste0(format(round(lowest_temp, 1), nsmall = 1), " ºC")
    )
  })
  
  output$average_windy <- renderInfoBox({
    average_wind <- mean(df_weather_daily$wind_speed)
    infoBox(
      title = "Average wind velocity",
      icon = icon("fan"),
      color = "aqua",
      value = paste0(format(round(average_wind, 1), nsmall = 1), " KM/h")
    )
  })  
  
  #------ Rendering demand plot ------
  
  output$city_demand <- renderPlot({
    selected_city <- input$selected_city
    data_view <- input$city_demand_view
    data_options <- input$city_demand_cheks
    plot <- NULL
    if(data_view == "daily"){
      df_bikes <- select(bicycles_dict_daily[[selected_city]], date, totinc, totdecr)
      df_weather <- select(weather_dict_monthly[[selected_city]], main_temp, wind_speed, rain_3h, month, year)
      plot <- ggplot(data = df_bikes, aes(x = date, y = totinc)) + 
        geom_bar(stat="identity") + 
        theme_minimal()
    } else if (data_view == "monthly"){
      df_weather <- select(weather_dict_monthly[[selected_city]], main_temp, wind_speed, rain_3h, month, year)
      plot <- ggplot(data = df_weather, aes(x = month, y = main_temp)) + 
        scale_y_continuous(limits = c(-5, 40), breaks = seq(-5, 40, 5)) +
        scale_x_discrete(limits = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr")) +
        geom_line(group = 1) + 
        geom_point()
    }
    plot
  })
  
  output$weather_days_plot <- renderPlot({
    names <- c("Sunny days", "Rainy days", "Snowy days", "Windy days", "Cloudy days", "Foggy days")
    # Convert data frame row to a numeric vector
    data <- as.numeric(select(filter(info_weather_by_days, city == selected_city), num_sunny_days, 
                              num_rainy_days, num_snowy_days, num_windy_days, num_cloudy_days, num_foggy_days))
    barplot(data, names.arg = names,
            xlab = "Day description", ylab = "Number of days", 
            col = c("orange","#72C2F7","#296C98","#616161","#D6D8DA","#6B6B6B"),
            border = "black")
  })
  
  output$city_temperature_plot <- renderPlot({
    ggplot(df_weather_daily, aes(x = date, y = main_temp)) + geom_line(group = 1) + geom_point()
  })
  
  output$city_rain_plot <- renderPlot({
    
  })
  
  output$city_wind_plot <- renderPlot({
    
  })
})

