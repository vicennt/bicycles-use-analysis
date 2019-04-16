#
# ---- Final Degree Thesis ----
# Alumno: Vicent Pérez
# Tutor: Cèsar Ferri
# Universitat Politécnica de València
#

library(shiny)

# User Interface
ui <- fluidPage(

   # Title
   titlePanel("Final Degree Thesis"),
   
   # Layout (two colums: map & info about station)
   sidebarLayout(position = "right",
      sidebarPanel(
         #TODO: Map of bicycle stations
      ),
      mainPanel(
         #TODO: Exclusive info about the selected station
      )
   )
)

# Server function
server <- function(input, output) {
   #TODO: Handle inputs and outputs
}

# Run the application 
shinyApp(ui = ui, server = server)

