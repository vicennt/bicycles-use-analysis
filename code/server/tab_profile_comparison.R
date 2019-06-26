# Profile comparison

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
