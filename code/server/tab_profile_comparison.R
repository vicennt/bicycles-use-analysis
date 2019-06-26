# TODO: Profile comparision

output$compare_cities_plot <- renderPlot({
  # First city
  city_one <- input$city_one_comparation
  subset_first_city <- filter(bicycles_dict[[city_one]])
  subset_first_city <- select(subset_first_city, hour, totdecr)
  subset_first_city <- subset_first_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
  
  # Second city
  city_two <- input$city_two_comparation
  subset_second_city <- filter(bicycles_dict[[city_two]])
  subset_second_city <- select(subset_second_city, hour, totdecr)
  subset_second_city <- subset_second_city %>% group_by(hour) %>% summarise(totdecr = mean(totdecr))
  
  df <- data.frame(c(0:23), subset_first_city$totdecr, subset_second_city$totdecr)
  colnames(df) <- c("x","First city","Second city")
  df2 <- melt(data = df, id.vars = "x")
  plot <- ggplot(data = df2, aes(x = x, y = value, color = variable)) + geom_line(size = 2) +
    ylab('Demand') + xlab('Hour') + scale_x_continuous(breaks = c(0:23)) + 
    scale_y_continuous(seq(0,5,0.5)) + scale_color_manual(values=c("#accc5a", "#67cce6")) + 
    ylim(0,5) + labs(color = "Cities")
  
  plot
})

output$compare_cities <- renderPlot({
  
})