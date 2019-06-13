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


#General Functions
source(file.path("server", "functions.R"), local = TRUE)$value

# ------- DATA TRANSFORMATION ---------
load_data()

for(c in cities_names){
  add_city_usage(c)
  id_stations <- filter(stations, CITY == c)$NUM_STATION
  for(s in id_stations){
    add_station_usage(c, s)
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

