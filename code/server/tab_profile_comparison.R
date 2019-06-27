# Profile comparison

observe({
  city_one <- input$station_one_comparation_city
  city_two <- input$station_two_comparation_city
  stations_one <- sort(select(filter(stations, CITY == city_one), NUM_STATION)$NUM_STATION, decreasing = FALSE)
  stations_two <- sort(select(filter(stations, CITY == city_two), NUM_STATION)$NUM_STATION, decreasing = FALSE)
  
  # Can also set the label and select items
  updateSelectInput(session, "station_one_comparation_station",
                    label = paste("Select station"),
                    choices = stations_one,
                    selected = head(stations_one, 1))
  updateSelectInput(session, "station_two_comparation_station",
                    label = paste("Select station"),
                    choices = stations_two,
                    selected = head(stations_two, 2))
})


output$compare_cities_plot <- renderPlot({
  view <- input$city_comparation_radio
  city_one <- input$city_one_comparation
  city_two <- input$city_two_comparation
  if(view == 'daily_view'){
    # First city
    subset_first_city <- filter(bicycles_dict[[city_one]])
    subset_first_city <- select(subset_first_city, hour, totdecr)
    subset_first_city <- subset_first_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
    # Second city
    subset_second_city <- filter(bicycles_dict[[city_two]])
    subset_second_city <- select(subset_second_city, hour, totdecr)
    subset_second_city <- subset_second_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
    df <- data.frame(c(0:23), subset_first_city$totdecr, subset_second_city$totdecr)
  }else if (view == 'weekly_view'){
    # First city
    subset_first_city <- filter(bicycles_dict[[city_one]])
    subset_first_city <- select(subset_first_city, hour, totdecr, weekday)
    subset_first_city <- subset_first_city %>% group_by(hour, weekday) %>% summarise(totdecr = mean(totdecr))
    subset_first_city <- subset_first_city[order(subset_first_city$weekday),]
    # Second city
    subset_second_city <- filter(bicycles_dict[[city_two]])
    subset_second_city <- select(subset_second_city, hour, totdecr, weekday)
    subset_second_city <- subset_second_city %>% group_by(hour, weekday) %>% summarise(totdecr = mean(totdecr))
    subset_second_city <- subset_second_city[order(subset_second_city$weekday),]
    df <- data.frame(c(0:167), subset_first_city$totdecr, subset_second_city$totdecr)
  }

  colnames(df) <- c("x","First city","Second city")
  df2 <- melt(data = df, id.vars = "x")
  plot <- ggplot(data = df2, aes(x = x, y = value, color = variable)) +
    labs(y = "Demand", x = "Hour", color = "Cities") +
    scale_color_manual(values=c("#accc5a", "#67cce6")) 
    
  if(view == 'daily_view'){
    plot <- plot + geom_line(size = 2, inherit.aes = TRUE) + 
      scale_x_continuous(breaks = c(0:23)) + scale_y_continuous(seq(0,5,0.5))
  }else if(view == 'weekly_view'){
    plot <- plot + geom_line(size = 1, inherit.aes = TRUE) + 
      scale_x_continuous(breaks = seq(0, 167, 24)) + scale_y_continuous(seq(0,5,0.5))
  }
  plot
})


output$compare_stations_plot <- renderPlot({
  view <- input$station_comparation_radio
  city_one <- input$station_one_comparation_city
  city_two <- input$station_two_comparation_city
  station_one <- input$station_one_comparation_station
  station_two <- input$station_two_comparation_station
  
  if(view == 'daily_view'){
    # First city
    subset_first_city <- filter(bicycles_dict[[city_one]], station == station_one)
    subset_first_city <- select(subset_first_city, hour, totdecr)
    subset_first_city <- subset_first_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
    # Second city
    subset_second_city <- filter(bicycles_dict[[city_two]], station == station_two)
    subset_second_city <- select(subset_second_city, hour, totdecr)
    subset_second_city <- subset_second_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
    
    df <- data.frame(c(0:23), subset_first_city$totdecr, subset_second_city$totdecr)
  } else if(view == 'weekly_view'){
    # First city
    subset_first_city <- filter(bicycles_dict[[city_two]], station == station_one)
    subset_first_city <- select(subset_first_city, hour, totdecr, weekday)
    subset_first_city <- subset_first_city %>% group_by(hour, weekday) %>% summarise(totdecr = mean(totdecr))
    subset_first_city <- subset_first_city[order(subset_first_city$weekday),]
    # Second city
    subset_second_city <- filter(bicycles_dict[[city_two]], station == station_two)
    subset_second_city <- select(subset_second_city, hour, totdecr, weekday)
    subset_second_city <- subset_second_city %>% group_by(hour, weekday) %>% summarise(totdecr = mean(totdecr))
    subset_second_city <- subset_second_city[order(subset_second_city$weekday),]
    df <- data.frame(c(0:167), subset_first_city$totdecr, subset_second_city$totdecr) 
  }
  
  colnames(df) <- c("x","First station","Second station")
  df2 <- melt(data = df, id.vars = "x")
  plot <- ggplot(data = df2, aes(x = x, y = value, color = variable)) +
    labs(y = "Demand", x = "Hour", color = "Stations") +
    scale_color_manual(values=c("#accc5a", "#67cce6")) 
  
  if(view == 'daily_view'){
    plot <- plot + geom_line(size = 2, inherit.aes = TRUE) + 
      scale_x_continuous(breaks = c(0:23)) + scale_y_continuous(seq(0,5,0.5))
  }else if(view == 'weekly_view'){
    plot <- plot + geom_line(size = 1, inherit.aes = TRUE) + 
      scale_x_continuous(breaks = seq(0, 167, 24)) + scale_y_continuous(seq(0,5,0.5))
  }
  plot
  
})
