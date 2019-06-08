#
# ---- Final Degree Thesis ----
# Alumni: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(stringr)
library(ggplot2)
library(jsonlite)
library(fontawesome)
library(httr)

#API
key <- readLines("api_key")
base <- "https://api.jcdecaux.com/"

#Datasets
stations <- read.csv(file="../datasets/stations.csv", header=TRUE, sep=",")
cities <- read.csv(file="../datasets/cities.csv", header=TRUE, sep=",")
bicycles_data_path <- "../datasets/bikes_agg_v2/"
weather_data_path <- "../datasets/weather_agg_v2/"

#General Functions
subset_by_date <- function(dataset, ini_date, end_date){
  subset <- cbind(dataset, date = tm1 <- as.Date(paste0(dataset$year,"-",dataset$month,"-",dataset$day)))
  subset[subset$date >= ini_date & subset$date <= end_date,]
}

# User Interface
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
        menuItem("Data Analysis", tabName = "past", icon = icon("chart-line")),
        menuItem("Real time", tabName = "present", icon = icon("globe-europe")),
        menuItem("Future predictions", tabName = "future", icon = icon("paper-plane")),
        menuItem("Expore the dataset", tabName = "dataset", icon = icon("table")),
        menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/vicennt/bicycles-use-analysis")
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        source(file.path("ui", "tab_data_analysis_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_real_time_ui.R"), local = TRUE)$value,
        source(file.path("ui", "tab_prediction_ui.R"), local = TRUE)$value
      )
   )  
)

# Server function
server <- function(input, output, session) {
  source(file.path("server", "tab_data_analysis.R"), local = TRUE)$value
  source(file.path("server", "tab_real_time.R"), local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)

