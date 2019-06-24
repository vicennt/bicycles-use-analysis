#General Functions
subset_by_date <- function(dataset, ini, end){
  df <- dataset[dataset$date >= ini & dataset$date <= end,]
}

transform_data <- function(ini_date, end_date){
  for(c in cities_names){
    # Aggregated data by hour
    bfile <- read.csv(file=paste0("../datasets/data_merged/cities/", c,"/", c,".csv"), header=TRUE, sep=",")
    bfile_mutated <- mutate(bfile, date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y"), datetime = ISOdatetime(year, month, day, hour, 0, 0))
    bicycles_dict[[c]] <<- subset_by_date(bfile_mutated, ini_date, end_date) 
    
    wfile <- read.csv(file=paste0("../datasets/weather_agg_v2/", c, "_agg.csv"), header=TRUE, sep=",")
    wfile_mutated <- mutate(wfile, date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y"), datetime = ISOdatetime(year, month, day, hour, 0, 0))
    wfile_selected <- select(wfile_mutated, main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, day, month, year, hour, weather_main, date, datetime)
    weather_dict[[c]] <<- subset_by_date(wfile_selected, ini_date, end_date)
                                
    weather_dict[[c]]$main_temp <- weather_dict[[c]]$main_temp - 273.15
    weather_dict[[c]]$main_temp_max <- weather_dict[[c]]$main_temp_max - 273.15
    weather_dict[[c]]$main_temp_min <- weather_dict[[c]]$main_temp_min - 273.15
    
    # Aggregated data by day
    bicycles_dict_daily[[c]] <<- agg_bicycle_data_by_day(bicycles_dict[[c]])
    weather_dict_daily[[c]] <<- agg_weather_data_by_day(weather_dict[[c]])
    
    # Aggregated data by month
    bicycles_dict_monthly[[c]] <<- agg_bicycle_data_by_month(bicycles_dict_daily[[c]])
    weather_dict_monthly[[c]] <<- agg_weather_data_by_month(weather_dict_daily[[c]])
    print(paste0(c, " done!"))
    
    # Daily & Monthly city demand information
    daily_city_demand_info[[c]] <<- daily_city_demand(c, bicycles_dict_daily[[c]])
    monthly_city_demand_info[[c]] <<- monthly_city_demand(c, bicycles_dict_monthly[[c]])}
    hourly_city_profile[[c]] <<- preparing_profiles(bicycles_dict[[c]])
}

get_weekly_demand_vector <- function(dataset, monday, st){
  sunday <- monday + 6 # Get sunday
  subset <- filter(dataset, station == st)
  subset <- subset_by_date(subset, monday, sunday)
  subset <- select(subset, hour, totdecr)
  subset
}

daily_city_demand <- function(city, df_bicycle_city_day){
  subset <- select(mutate(df_bicycle_city_day, weekend = !is.weekend(date)), totinc, totdecr, medbikes, meanbikes, lastbikes, weekend, date)
  num_stations <- nrow(stations[stations$CITY == city,])
  data <- subset %>% group_by(date) %>% 
    summarise(totinc = sum(totinc), totdecr = sum(totdecr)/num_stations, medbikes = mean(medbikes), 
              meanbikes = mean(meanbikes), lastbikes = mean(lastbikes), weekend = max(weekend))
  data
}

monthly_city_demand <- function(city, df_bicycle_city_month){
  subset <- select(df_bicycle_city_month, totdecr, medbikes, meanbikes, lastbikes, month, year)
  num_stations <- nrow(stations[stations$CITY == city,])
  data <- subset %>% group_by(month) %>% 
    summarise(totdecr = sum(totdecr)/num_stations, medbikes = mean(medbikes), 
              meanbikes = mean(meanbikes), lastbikes = mean(lastbikes), year = mean(year))
  data
}

preparing_profiles <- function(df_bicycle_city){
  subset <- select(df_bicycle_city, station, totdecr, hour, date)
  data <- subset %>% group_by(station, date, hour) %>% 
    summarise(totdecr = sum(totdecr))
  data
}

agg_bicycle_data_by_day <- function(df_bicycle_city){
  subset <- select(df_bicycle_city, station, totinc, totdecr, medbikes, meanbikes, lastbikes, propempty, propfull, count, month, year, date)
  data <- subset %>% group_by(station, date) %>% 
    summarise(totinc = sum(totinc), totdecr = sum(totdecr), 
               medbikes = mean(medbikes), meanbikes = mean(meanbikes), 
               lastbikes = mean(lastbikes),propempty = mean(propempty), 
               propfull = mean(propfull),count = mean(count), 
               month = mean(month), year = mean(year))
  data
}

agg_bicycle_data_by_month <- function(df_bicycle_city){
  subset <- select(df_bicycle_city, station, totinc, totdecr, medbikes, meanbikes, lastbikes, propempty, propfull, count, month, year)
  data <- subset %>% group_by(station, month) %>% 
    summarise(totinc = sum(totinc), totdecr = sum(totdecr), 
              medbikes = mean(medbikes), meanbikes = mean(meanbikes), 
              lastbikes = mean(lastbikes), propempty = mean(propempty), 
              propfull = mean(propfull),count = mean(count), year = mean(year))
  data
}

agg_weather_data_by_day <- function(df_weather_city){
  subset <- select(df_weather_city, main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, weather_main, month, year, date)
  data <- subset %>% group_by(date) %>% 
    summarise(main_temp = mean(main_temp), main_temp_max = mean(main_temp_max), 
              main_temp_min = mean(main_temp_min), wind_speed = mean(wind_speed), 
              rain_3h = sum(rain_3h), snow_3h = sum(snow_3h), month = mean(month),
              year = mean(year), weather_main = names(which.max(table(weather_main))))
  data
}

agg_weather_data_by_month <- function(df_weather_city){
  subset <- select(df_weather_city, main_temp, main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, weather_main, month, year)
  weather_count <- names(which.max(table(subset$weather_main)))
  data <- subset %>% group_by(month) %>% 
    summarise(main_temp = mean(main_temp), main_temp_max = mean(main_temp_max), 
              main_temp_min = mean(main_temp_min), wind_speed = mean(wind_speed), 
              rain_3h = sum(rain_3h), snow_3h = sum(snow_3h), year = mean(year),
              weather_main = names(which.max(table(weather_main))))
  data
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
    # Versión 1
    # nsunny <- nrow(filter(df, (df$weather_main == "Clear" | df$weather_main == "Haze" )))
    # nrainy <- nrow(filter(df, (df$weather_main == "Rain" | df$weather_main == "Thunderstorm" | df$weather_main == "Drizzle")))
    # nsnowy <- nrow(filter(df, df$snow_3h > 1))
    # nwindy <-  nrow(filter(df, df$wind_speed > 8))
    # ncloudy <- nrow(filter(df, (df$weather_main == "Cloud" | df$weather_main == "Mist" | df$weather_main == "Fog" )))
    # nfoggy <- nrow(filter(df, (df$weather_main == "Mist" | df$weather_main == "Fog" )))
    
    # Versión 2 - Basic one, following demand weather info
    nsunny <- nrow(filter(df, df$weather_main == "Clear"))
    nrainy <- nrow(filter(df, df$weather_main == "Rain"))
    nsnowy <- nrow(filter(df, df$weather_main == "Snow"))
    nwindy <-  nrow(filter(df, df$wind_speed > 8))
    ncloudy <- nrow(filter(df, df$weather_main == "Cloud"))
    nfoggy <- nrow(filter(df, df$weather_main == "Fog" ))
    aux <- data.frame(city = c, num_sunny_days = nsunny, num_rainy_days = nrainy, num_snowy_days = nsnowy
                        ,num_windy_days = nwindy, num_cloudy_days = ncloudy, num_foggy_days = nfoggy)
    info_weather_by_days <<- rbind(info_weather_by_days, aux)
  }
}

