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
    num_trips <- sum(bicycles_dict_daily[[selected_city]]$totdecr)
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

  output$city_temperature_plot <- renderPlotly({
    ggplot(df_weather_daily, aes(x = date,y = main_temp)) +
      geom_point(colour = "blue") +
      geom_smooth(colour = "red", size = 1) +
      scale_y_continuous(limits = c(-5,30), breaks = seq(-5,30,5)) +
      scale_x_date(breaks = date_breaks("months"), date_labels = "%b/%Y") +
      ggtitle ("Daily average temperature") +
      xlab("Date") +  ylab ("Average Temperature ( ºC )") %>%  config(displayModeBar = F)
 
  })
  
  output$city_rain_plot <- renderPlotly({
    ggplot(df_weather_daily, aes(x= date,y = rain_3h)) +
      geom_point(aes(colour = rain_3h)) +
      geom_smooth(colour = "blue", size = 1) +
      scale_colour_gradient2(low = "green", mid = "orange", high = "red", midpoint = 40) +
      scale_y_continuous(breaks = seq(0,200,10)) +
      xlab("Date") + ylab("Rain (mm)") +
      scale_x_date(breaks = date_breaks("months"), date_labels = "%b/%Y") +
      ggtitle("Daily rain amount") %>%  config(displayModeBar = F) + 
      labs(fill="Intensity")
  })
  
  output$city_wind_plot <- renderPlot({
    city <- input$selected_city
    df <- weather_dict_daily[[city]]
    windRose(df, ws = "wind_speed", wd = "wind_deg", ws2 = NA, wd2 = NA,
             ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
             = "default", grid.line = NULL, width = 1, seg = NULL, auto.text
             = TRUE, breaks = 4, offset = 10, normalise = FALSE, max.freq =
               NULL, paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
             key.position = "bottom", key = TRUE, dig.lab = 5, statistic =
               "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
               315, border = NA)
  })
  
  output$city_weather_correlations <- renderPlot({
    variable <- input$correlation_combo
    city <- input$selected_city
    df_bikes <- select(daily_city_usage_info[[city]], totdecr)
    plot <- NULL
    if(variable == "Temperature"){
      df_weather <- select(weather_dict_daily[[city]], main_temp)
      dfmixed <- cbind(df_bikes, main_temp = df_weather$main_temp)
      plot <- ggplot(dfmixed, aes(x=main_temp, y=totdecr)) + geom_point() + geom_smooth() 
    }else if(variable == "Rain"){
      df_weather <- select(weather_dict_daily[[city]], rain_3h)
      dfmixed <- cbind(df_bikes, rain_3h = df_weather$rain_3h)
      plot <- ggplot(dfmixed, aes(x=rain_3h, y=totdecr)) + geom_point() + geom_smooth() 
    }else if(variable == "Wind"){
      df_weather <- select(weather_dict_daily[[city]], wind_speed)
      dfmixed <- cbind(df_bikes, wind_speed = df_weather$wind_speed)
      plot <- ggplot(dfmixed, aes(x=wind_speed, y=totdecr)) + geom_point() + geom_smooth() 
    }
    plot
  })

  
  #------ Rendering usage plot ------
  output$city_usage_plot <- renderPlot({
    #Shiny widgets relations
    selected_city <- input$selected_city
    data_view <- input$city_usage_radio
    data_options <- input$city_usage_check
    weekend_check <- input$weekend_check

    plot <- NULL
    if(data_view == "daily_view"){
      shinyjs::show("weekend_check")
      shinyjs::show("city_usage_check")
      #Get daily data
      df_bikes <- select(daily_city_usage_info[[selected_city]], date, totdecr, weekend)
      df_weather <- weather_dict_daily[[selected_city]]
      # We have both dataset aggregated by day, so we can mix them and get the desired information
      dfmixed <- cbind(df_weather, weekend = df_bikes$weekend, totdecr = df_bikes$totdecr)
      # Basic usage plot
      plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr)) +
                geom_bar(stat="identity", width=.4, color = "#414141") + 
                labs(title="Average city trips by day", y = "Number of trips", x = "Day") +
                scale_x_date(breaks = date_breaks("months"), date_labels = "%b/%Y")
      
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
          geom_bar(stat="identity", width=.4) +
          scale_fill_manual(values = pal)+
          labs(title="Average city trips by day", y = "Number of trips", x = "Day") +
          labs(alpha="Day of the week", fill="Day description") + guides(alpha=FALSE) 
        # If user only want to see weather descriptions
      }else if ('weather_description' %in% data_options){
        plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr, fill = weather_main), width=.8) +
          geom_bar(stat="identity", width=.6) +
          scale_fill_manual(values = pal) +
          labs(title="Average city trips by day", y = "Number of trips", x = "Day") +
          labs(fill="Day description") 
        # If user only want to hide weekends
      }else if(weekend_check){
        plot <- ggplot(data = dfmixed, aes(x = date, y = totdecr, alpha = weekend), width=.8) +
          geom_bar(stat="identity", width=.4) + labs(title="Average city trips by day", y = "Number of trips", x = "Day") +
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
      shinyjs::hide("city_usage_check")
      data <- monthly_city_usage_info[[selected_city]]
      data <- mutate(data, test = paste0(year,"/",month))
      df <- data[order(data$test),]
      plot <- ggplot() + geom_bar(data = df, aes(x = test, y = totdecr), stat="identity", alpha = 0.7, width=.4, colour = "#e0ab00", fill = "lightblue") + 
        labs(title="Average city trips by month", y = "Number of trips", x = "Year/Month") 
    }
    plot
  })
  
  # ------------ Rendering city profiles ---------------- #
  
  output$city_profile_plot <- renderPlot({
    profile_view <- input$city_profiles_radio
    plot <- NULL
    if(profile_view == 'daily_profile'){
      subset <- filter(bicycles_dict[[selected_city]])
      subset <- select(subset, hour, totdecr)
      station_daily_profile <- subset %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
      plot <- ggplot(data= station_daily_profile, aes(x = 0:23, y = totdecr)) + 
        geom_bar(alpha = .7, group = 1, stat = "identity", colour = "#e0ab00", fill = "lightblue") +
        ylab('Demand') + xlab('Hour') + scale_x_continuous(breaks = c(0:23))
    }else if(profile_view == 'weekly_profile'){
      subset <- filter(bicycles_dict[[selected_city]])
      subset <- select(subset, hour, totdecr, weekday)
      station_weekly_profile <- subset %>% group_by(hour, weekday) %>% summarise(totdecr = mean(totdecr))
      station_weekly_profile <- station_weekly_profile[order(station_weekly_profile$weekday),]
      plot <- ggplot(data= station_weekly_profile, aes(x = 0:167, y = totdecr)) + 
        geom_bar(alpha = .7, group = 1, stat = "identity", colour = "#e0ab00", fill = "lightblue") +
        ylab('Demand') + xlab('Hour') + scale_x_continuous(breaks = seq(0, 167, 24))
    }
    plot
  })
})

