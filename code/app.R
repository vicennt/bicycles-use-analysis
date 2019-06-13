
# ---- Final Degree Thesis ----
# Alumni: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València


library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(fontawesome)
library(httr)
library(hash)


# Global variables
base <- "https://api.jcdecaux.com/"
bicycles_data_path <- "../datasets/bikes_agg_v2/"
weather_data_path <- "../datasets/weather_agg_v2/"
key <- readLines("api_key")
stations <- NULL
cities <- NULL
cities_names <- NULL
bicycles_dict <<- hash()
weather_dict <<- hash()
weather_dict_agg <<- hash()
usage_city <- NULL
usage_station <- NULL




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
  subset <- select(mutate(df_weather_city, date = as.Date(paste0(day,"-",month,"-",year), format = "%d-%m-%Y")),
                   main_temp_max, main_temp_min, wind_speed, rain_3h, snow_3h, weather_main, date)
  dataset_dates <- unique(subset$date)
  count <- 1
  df_aux <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_aux) <- c("main_temp_max", "main_temp_min","wind_speed", "rain_3h", "snow_3h", "weather_main", "date")
  while(count <= length(dataset_dates)){
    subframe <- filter(subset, date == dataset_dates[count])
    aux <- data.frame(main_temp_max = mean(subframe$main_temp_max), main_temp_min = mean(subframe$main_temp_min),
                      wind_speed = mean(subframe$wind_speed), rain_3h = mean(subframe$rain_3h),
                      snow_3h = mean(subframe$snow_3h), date = dataset_dates[count], weather = names(which.max(table(subframe$weather_main))))
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


# ------- DATA TRANSFORMATION ---------
load_data()
for(c in cities_names){
  add_city_usage(c)
  id_stations <- filter(stations, CITY == c)$NUM_STATION
  for(s in id_stations){
    add_station_usage(c, s)
  }
}


#General Functions
source(file.path("server", "functions.R"), local = TRUE)$value

#  ---- User Interface ----
ui <- 
  dashboardPage(skin = "green",
   dashboardHeader(
      title = "Bicycles",
      dropdownMenu(
            type = "tasks", badgeStatus = "success",
            taskItem(value = 10, color = "green","Documentation"),
            taskItem(value = 40, color = "aqua","Development"),
            taskItem(value = 5, color = "yellow", "Testing"),
            taskItem(value = 40, color = "red", "Overall project")
      ),
      #TODO: Show notifications: Server status & Station status & ??
      dropdownMenu(
        type = "notifications", 
        notificationItem(text = "The system is running well", icon("users"))
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        selectInput("selected_city", "Select one of these cities", cities$NAME),
        menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-line")),
        menuItem("Real time", tabName = "real_time", icon = icon("globe-europe")),
        menuItem("Expore the dataset", tabName = "explore", icon = icon("table")),
        menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/vicennt/bicycles-use-analysis")
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        source(file.path("ui", "tab_data_analysis_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_real_time_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_data_exploration_ui.R"), local = TRUE)$value
      )
   )  
)

# ---- Server function ----
server <- function(input, output, session) {
  source(file.path("server", "tab_data_analysis.R"), local = TRUE)$value
  source(file.path("server", "tab_real_time.R"), local = TRUE)$value
  source(file.path("server", "tab_data_exploration.R"), local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)

