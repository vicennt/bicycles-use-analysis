#General Functions
subset_by_date <- function(dataset, ini_date, end_date){
  subset <- cbind(dataset, date = tm1 <- as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
  subset[subset$date >= ini_date & subset$date <= end_date,]
}

transform_data <- function(){
  stations <<- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")
  cities <<- read.csv(file="../datasets/cities.csv", header=TRUE, sep=",")
  cities_names <<- cities$NAME
  for(c in cities_names){
    #Bicycle data
    bicycles_dict[[c]] <<- filter(read.csv(file=paste0("../datasets/data_merged/cities/", c,"/", c,".csv"), header=TRUE, sep=","), month != 5, month != 6, month != 7, month != 9) 
    bicycles_dict_daily[[c]] <<- aggregate(.~station+date, select(mutate(bicycles_dict[[c]], date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y")), 
                                                                  station, totinc, totdecr, medbikes, meanbikes, lastbikes, propempty, propfull, count, date), sum)
    bicycles_dict_monthly[[c]] <<- aggregate(.~month+year, select(bicycles_dict[[c]], month, year, totinc, totdecr), sum)
    #bicycles_dict_monthly[[c]]$month = factor(bicycles_dict_monthly[[c]]$month, labels = c("Jan","Feb","Mar","Apr","Oct","Nov","Dec"))
    
    #Weather data
    weather_dict[[c]] <<- select(filter(read.csv(file=paste0("../datasets/weather_agg_v2/", c, "_agg.csv"), header=TRUE, sep=","), month != 5, month != 6, month != 7, month != 9), 
                                 main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, day, month, year, hour, weather_main)
    # Changing temperature to Celsius
    weather_dict[[c]]$main_temp <- weather_dict[[c]]$main_temp - 273.15
    weather_dict[[c]]$main_temp_max <- weather_dict[[c]]$main_temp_max - 273.15
    weather_dict[[c]]$main_temp_min <- weather_dict[[c]]$main_temp_min - 273.15
    weather_dict_daily[[c]] <<- agg_weather_data_by_day(weather_dict[[c]])
    weather_dict_monthly[[c]] <<- aggregate(.~month+year, select(weather_dict[[c]],  main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, month, year), mean)
    weather_dict_monthly[[c]]$month = factor(weather_dict_monthly[[c]]$month, labels = c("Jan","Feb","Mar","Apr","Oct","Nov","Dec"))
   }
}

agg_weather_data_by_day <- function(df_weather_city){
  #df_wetaher_city is a df with hourly information
  #Firstly column date is created
  subset <- select(mutate(df_weather_city, date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y")),
                   main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, weather_main, date)
  # Diferent dates 
  dataset_dates <- unique(subset$date)
  count <- 1
  #Auxiliar df with city info by day
  df_aux <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_aux) <- c("main_temp_max", "main_temp_min","wind_speed", "rain_3h", "snow_3h", "weather_main", "date")
  while(count <= length(dataset_dates)){
    #Getting only day data
    subframe <- filter(subset, date == dataset_dates[count])
    #Calculating and saving day info
    aux <- data.frame(main_temp_max = mean(subframe$main_temp_max), main_temp_min = mean(subframe$main_temp_min),
                      wind_speed = mean(subframe$wind_speed), rain_3h = mean(subframe$rain_3h),
                      snow_3h = mean(subframe$snow_3h), date = dataset_dates[count], weather_main = names(which.max(table(subframe$weather_main))))
    #Adding day info inside city df
    df_aux <- rbind(df_aux, aux)
    count <- count + 1
  }
  df_aux
}



add_city_demand <- function(c){
  # Adding city usage info
  aux <- data.frame(city = c, average_demand = sum(bicycles_dict[[c]]$totdecr)/nrow(stations[stations$CITY == c,]))
  info_usage_city <<- rbind(info_usage_city, aux)
}

add_station_demand <- function(c, s){
  # Adding station usage info
  sum_station_demand <- sum(filter(bicycles_dict[[c]], station == s)$totdecr)
  stands_station <- filter(stations, CITY == c, NUM_STATION == s)$STANDS
  aux <- data.frame(city = c, station = s, average_demand = sum_station_demand / stands_station)
  info_usage_station <<- rbind(info_usage_station, aux)
}

get_weather_info_by_day <- function(){
  for(c in cities_names){
    df <- weather_dict_daily[[c]]
    nsunny <- nrow(filter(df, (df$weather_main == "Clear" | df$weather_main == "Haze" )))
    nrainy <- nrow(filter(df, (df$weather_main == "Rain" | df$weather_main == "Thunderstorm" | df$weather_main == "Drizzle")))
    nsnowy <- nrow(filter(df, df$weather_main == "Snow"))
    nwindy <-  nrow(filter(df, df$wind_speed > 10))
    ncloudy <- nrow(filter(df, (df$weather_main == "Cloud" | df$weather_main == "Mist" | df$weather_main == "Fog" )))
    nfoggy <- nrow(filter(df, (df$weather_main == "Mist" | df$weather_main == "Fog" )))
    aux <- data.frame(city = c, num_sunny_days = nsunny, num_rainy_days = nrainy, num_snowy_days = nsnowy
                        ,num_windy_days = nwindy, num_cloudy_days = ncloudy, num_foggy_days = nfoggy)
    info_weather_by_days <<- rbind(info_weather_by_days, aux)
  }
}

