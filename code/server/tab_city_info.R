#---------- City information----------------



observe({
  #Selected city
  selected_city <- input$selected_city
  city_stations_demand_info <- filter(usage_station, city == selected_city)
  # Station usage ranking (selected city)
  city_stations_demand_ranking <- city_stations_demand_info[order(city_stations_demand_info$average_demand, decreasing = TRUE),]
  # Cities usage ranking (all cities)
  usage_city_ranking <- usage_city[order(usage_city$average_demand, decreasing = TRUE),]
  
  output$num_stations_city <- renderInfoBox({
    num_stations <- count(select(filter(stations, CITY == selected_city), NUM_STATION))
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
       color = "aqua",
       value = paste0(filter(cities, NAME == selected_city)$POPULATION, " people")
     )
   })
   
  output$num_trips_city <- renderInfoBox({
    num_trips <- sum(bicycles_dict_daily[[selected_city]]$totinc)
    infoBox(
      title = "Number of trips during this period",
      icon = icon("bicycle"),
      color = "navy",
      value = paste0(format(round(num_trips, 1), nsmall = 1), " rides")
    )
  })
  
  output$city_rank <- renderInfoBox({
    rank_pos <- which(usage_city_ranking$city == selected_city)
    infoBox(
      title = "City ranking position",
      icon = icon("chart-line"),
      color = "yellow",
      value = paste0("Position ", rank_pos, " of 27 cities") #TODO: Change fix cities number
    )
  })
  
#  ------------- Rendering weather info -----------------
  df <- weather_dict_daily[[selected_city]]
  output$sunny_days <- renderInfoBox({
    num_sunny_days <- nrow(filter(df, (df$weather_main == "Clear" |
                                         df$weather_main == "Haze" )))
    infoBox(
      title = "Number of sunny days",
      icon =  icon("sun"),
      color = "yellow",
      value = paste0(num_sunny_days, " days")
    )
  })
  
  output$cloudy_days <- renderInfoBox({
    num_cloudy_days <- nrow(filter(df, (df$weather_main == "Cloud" |
                                        df$weather_main == "Mist" |
                                        df$weather_main == "Fog" )))
    infoBox(
      title = "Number of cloudy days",
      icon =  icon("cloud"),
      color = "light-blue",
      width = 3,
      value = paste0(num_cloudy_days, " days")
    )
  })
  
  output$rainy_days <- renderInfoBox({
    num_rainy_days <- nrow(filter(df, (df$weather_main == "Rain" |
                                       df$weather_main == "Thunderstorm" |
                                       df$weather_main == "Drizzle")))
    infoBox(
      title = "Number of rainny days",
      icon =  icon("cloud-rain"),
      color = "blue",
      width = 3,
      value = paste0(num_rainy_days, " days")
    )
  })
  
  output$windy_days <- renderInfoBox({
    num_windy_days <- nrow(filter(df, df$wind_speed > 10))
    infoBox(
      title = "Number of windy days",
      icon = icon("wind"),
      color = "navy",
      width = 2,
      value = paste0(num_windy_days, " days")
    )
  })  
  
  output$foggy_days <- renderInfoBox({
    num_foggy_days <- nrow(filter(df, (df$weather_main == "Mist" |
                                         df$weather_main == "Fog" )))
    infoBox(
      title = "Number of foggy days",
      icon = icon("smog"),
      color = "teal",
      width = 2,
      value = paste0(num_foggy_days, " days")
    )
  }) 

  output$snowy_days <- renderInfoBox({
    num_snowy_days <- nrow(filter(df, df$weather_main == "Snow"))
    infoBox(
      title = "Number of snowy days",
      icon = icon("snowflake"),
      color = "black",
      width = 3,
      value = paste0(num_snowy_days, " days")
    )
  })

  output$highest_temperature <- renderInfoBox({
    highest_temp <- max(df$main_temp_max)
    infoBox(
      title = "Highest temperature",
      icon = icon("temperature-high"),
      color = "red",
      width = 3,
      value = paste0(format(round(highest_temp, 1), nsmall = 1), " ºC")
    )
  })
  
  output$lowest_temperature <- renderInfoBox({
    lowest_temp <- min(df$main_temp_max)
    infoBox(
      title = "Lowest temperature",
      icon = icon("temperature-low"),
      color = "blue",
      width = 3,
      value = paste0(format(round(lowest_temp, 1), nsmall = 1), " ºC")
    )
  })
  
  output$average_windy <- renderInfoBox({
    average_wind <- mean(df$wind_speed)
    infoBox(
      title = "Average wind velocity",
      icon = icon("fan"),
      color = "aqua",
      width = 2,
      value = paste0(format(round(average_wind, 1), nsmall = 1), " KM/h")
    )
  })  
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