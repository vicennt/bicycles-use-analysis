#---------- City information----------------



observe({
  #Selected city
  selected_city <- input$selected_city
  city_info <- filter(cities, NAME == selected_city)
  # Cities usage ranking (all cities)
  usage_city_ranking <- info_usage_city[order(info_usage_city$average_demand, decreasing = TRUE),]
  # Weather information daily aggregated
  df_weather_daily <- weather_dict_daily[[selected_city]]
  #Colour Palette
  pal <- c(
    "Clear" = "#f3c600",
    "Rain" = "#0a56aa", 
    "Snow" = "#d5a0ff", 
    "Clouds" = "#808080",
    "Fog" = "#292929",
    "Mist" = "#89858c" ,
    "Drizzle" = "#81bdff",
    "Thunderstorm" = "#3e444a",
    "Haze" = "#b3606a"
  )
  output$city_map <- renderLeaflet({
    leaflet(data = city_info) %>% addTiles(options=tileOptions(minZoom=4, maxZoom=4)) %>% addMarkers(data = city_info)
  })
  
  output$city_population <- renderInfoBox({
    infoBox(
      title = "City Population",
      icon = icon("users"),
      color = "red",
      width = 12,
      value = paste0(filter(cities, NAME == selected_city)$POPULATION, " people")
    )
  })
  
  output$num_stations_city <- renderInfoBox({
    num_stations <- count(select(filter(stations, CITY == selected_city), NUM_STATION))
    infoBox(
      title = "Number of total stations",
      icon = icon("thumbtack"),
      color = "green",
      width = 12,
      value = paste0(num_stations, " stations")
    )
  })


  output$num_trips_city <- renderInfoBox({
    num_trips <- sum(bicycles_dict_daily[[selected_city]]$totinc)
    infoBox(
      title = "Number of trips during this period",
      icon = icon("bicycle"),
      color = "light-blue",
      width = 12,
      value = paste0(format(round(num_trips, 1), nsmall = 1), " rides")
    )
  })
  
  output$city_rank <- renderInfoBox({
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

  output$weather_days_plot <- renderPlot({
    names <- c("Sunny days", "Rainy days", "Snowy days", "Cloudy days", "Foggy days", "Windy days")
    # Convert data frame row to a numeric vector
    data <- as.numeric(select(filter(info_weather_by_days, city == selected_city), num_sunny_days, 
                              num_rainy_days, num_snowy_days, num_windy_days, num_cloudy_days, num_foggy_days))
    barplot(data, names.arg = names,
            xlab = "Day description", ylab = "Number of days", 
            col = pal,
            border = "black")
  })
  
  output$city_temperature_plot <- renderPlot({
    ggplot(df_weather_daily, aes(x = date, y = main_temp)) + 
      geom_line(group = 1) + geom_point()
 
  })
  
  output$city_rain_plot <- renderPlot({
    ggplot(df_weather_daily, aes(x= date,y = rain_3h)) +
      geom_point(aes(colour = rain_3h)) +
      geom_smooth(colour = "blue", size = 1) +
      scale_colour_gradient2(low = "green", mid = "orange", high = "red", midpoint = 40) +
      scale_y_continuous(breaks = seq(0,200,10)) +
      xlab("Date") +
      ylab("Rain (mm)") +
      ggtitle("Daily rain amount")
  })
  
  output$city_wind_plot <- renderPlot({

  })
  
  #------ Rendering demand plot ------
  output$city_demand_plot <- renderPlot({
    weather_dict_daily[[selected_city]]
    selected_city <- input$selected_city
    data_view <- input$city_demand_radio
    data_options <- input$city_demand_check
    weekend_check <- input$weekend_check

    plot <- NULL
    if(data_view == "daily_view"){
      shinyjs::show("weekend_check")
      shinyjs::show("city_demand_check")
      #Get daily data
      df_bikes <- select(daily_city_demand_info[[selected_city]], date, totdecr, weekend)
      df_weather <- weather_dict_daily[[c]]
      print(nrow(df_bikes))
      print(nrow(df_weather))
      # We have both dataset aggregated by day, so we can mix both and get the desired information
      dfmixed <- cbind(df_weather, weekend = df_bikes$weekend, totdecr = df_bikes$totdecr)
      # Basic demand plot
      plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr), width=.8) +
                geom_bar(stat="identity") + 
                labs(title="Average city demand by day", y="Day", x="Demand")
      
      if(!('temp_info' %in% data_options)){
        shinyjs::hide("highest_temperature")
        shinyjs::hide("lowest_temperature")
      }else{
        shinyjs::show("highest_temperature")
        shinyjs::show("lowest_temperature")
      }
      
      if(weekend_check && 'weather_description' %in% data_options){
        # If the user check to hide weekends & want to see the weather descriptions
        plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr, alpha = weekend, fill = weather_main), width=.8) +
          geom_bar(stat="identity") +
          scale_fill_manual(values = pal)+
          labs(title="Average city demand by day", y="Day", x="Demand") +
          labs(alpha="Day of the week", fill="Day description") + guides(alpha=FALSE)
        # If user only want to see weather descriptions
      }else if ('weather_description' %in% data_options){
        plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr, fill = weather_main), width=.8) +
          geom_bar(stat="identity") +
          scale_fill_manual(values = pal) +
          labs(title="Average city demand by day", y="Day", x="Demand") +
          labs(fill="Day description")
        # If user only want to hide weekends
      }else if(weekend_check){
        plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr, alpha = weekend), width=.8) +
          geom_bar(stat="identity") + labs(title="Average city demand by day", y="Day", x="Demand") +
          labs(alpha="Day of the week") + guides(alpha=FALSE)
      }
      
       # Adding linear plot with temperature 
      if('temp_info' %in% data_options){
        plot <- plot + geom_line(data= df_weather, aes(x = date, y = main_temp), 
                                 group = 1, stat = "identity", inherit.aes = FALSE, colour = "#CC0000") 
      }
      
    } else if (data_view == "monthly_view"){
      #Setting UI
      shinyjs::hide("weekend_check")
      shinyjs::hide("city_demand_check")
      plot <- ggplot(data = monthly_city_demand_info[[c]], aes(x = month, y = totdecr)) +
        geom_line(group = 1) 
    }
    plot
  })
})

