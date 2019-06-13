#General Functions
subset_by_date <- function(dataset, ini_date, end_date){
  subset <- cbind(dataset, date = tm1 <- as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
  subset[subset$date >= ini_date & subset$date <= end_date,]
}

load_data <- function(){
  stations <<- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")
  cities <<- read.csv(file="../datasets/cities.csv", header=TRUE, sep=",")
  cities_names <<- cities$NAME
  for(c in cities_names){
    bicycles_dict[[c]] <<- read.csv(file=paste0("../datasets/data_merged/cities/",c,"/",c,".csv"), header=TRUE, sep=",")
    weather_dict[[c]] <<- select(read.csv(file=paste0("../datasets/weather_agg_v2/",c,"_agg.csv"), header=TRUE, sep=","), 
                                 main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, day, month, year, hour, weather_main)
    weather_dict_agg[[c]] <<- agg_weather_data_by_day(weather_dict[[c]])
  }
  #Creating new dataframe with station demand info
  usage_station <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(usage_station) <- c("city", "station", "average_demand")
  #Creating new dataframe with city demand info
  usage_city <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(usage_city) <- c("city","average_demand")
}

agg_weather_data_by_day <- function(df_weather_city){
  #df_wetaher_city is a df with hourly information
  #Firstly column date is created
  subset <- select(mutate(df_weather_city, date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y")),
                   main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, weather_main, date)
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
                      snow_3h = mean(subframe$snow_3h), date = dataset_dates[count], weather = names(which.max(table(subframe$weather_main))))
    #Adding day info inside city df
    df_aux <- rbind(df_aux, aux)
    count <- count + 1
  }
  df_aux
}

add_city_usage <- function(c){
  # Adding city usage info
  aux <- data.frame(city = c, average_demand = sum(bicycles_dict[[c]]$totinc)/nrow(stations[stations$CITY == c,]))
  usage_city <<- rbind(usage_city, aux)
}

add_station_usage <- function(c, s){
  # Adding station usage info
  sum_station_demand <- sum(filter(bicycles_dict[[c]], station == s)$totinc)
  stands_station <- filter(stations, CITY == c, NUM_STATION == s)$STANDS
  aux <- data.frame(city = c, station = s, average_demand = sum_station_demand / stands_station)
  usage_station <<- rbind(usage_station, aux)
}

