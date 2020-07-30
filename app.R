#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("modules/GitHub.R")
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title="BurndownCharts"),
    dashboardSidebar(),
    dashboardBody(
        GitHubUI("GitHubUI")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    callModule(GitHub, "GitHubUI")
}

# Run the application 
shinyApp(ui = ui, server = server)
