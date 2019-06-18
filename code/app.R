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
library(zoo)


# Global variables
base <- "https://api.jcdecaux.com/"
bicycles_data_path <- "../datasets/bikes_agg_v2/"
weather_data_path <- "../datasets/weather_agg_v2/"
key <- readLines("api_key")
stations <- NULL # Stations info
cities <- NULL # Cities info
cities_names <- NULL # Cities name
bicycles_dict <<- hash() # Hourly data
bicycles_dict_daily <<- hash() # Daily data
bicycles_dict_monthly <<- hash() # Monthly data
weather_dict <<- hash() # Hourly data
weather_dict_daily <<- hash() # Daily data
weather_dict_monthly <<- hash() # Monthly data
usage_city <<- data.frame(matrix(ncol = 2, nrow = 0))
colnames(usage_city) <- c("city","average_demand")
usage_station <<- data.frame(matrix(ncol = 3, nrow = 0))
colnames(usage_station) <- c("city", "station", "average_demand")


#General Functions
source(file.path("server", "functions.R"), local = TRUE)$value

# ------- DATA TRANSFORMATION ---------
transform_data()
for(c in cities_names){
  add_city_demand(c)
  id_stations <- filter(stations, CITY == c)$NUM_STATION
  for(s in id_stations){
    add_station_demand(c, s)
  }
}

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
        menuItem("City information", tabName = "city_information", icon = icon("university")),
        menuItem("Stations information", tabName = "stations_information", icon = icon("map-marker-alt")),
        menuItem("Comparing cities", tabName = "compare_cities", icon = icon("chart-area")),
        menuItem("Comparing stations", tabName = "compare_stations", icon = icon("chart-bar")),
        menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/vicennt/bicycles-use-analysis")
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        source(file.path("ui", "tab_city_info_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_station_info_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_compare_cities_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_compare_stations_ui.R"), local = TRUE)$value
      )
   )  
)

# ---- Server function ----
server <- function(input, output, session) {
  source(file.path("server", "tab_city_info.R"), local = TRUE)$value
  source(file.path("server", "tab_station_info.R"), local = TRUE)$value
  source(file.path("server", "tab_compare_cities.R"), local = TRUE)$value
  source(file.path("server", "tab_compare_stations.R"), local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)

