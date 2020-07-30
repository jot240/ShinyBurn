library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)
library(lubridate)
library(config)
GitHubUI <- function(id){
  ns <- NS(id)
  box(
    fluidRow(DT::dataTableOutput(ns("tableOutput"))),
    uiOutput(ns("selectMilestone")),
    plotlyOutput(ns("plotOutput"))
  )
  
}

GitHub<- function(input, output, session){
  ns <- session$ns
  issuesResponse <- reactive({
    res <- GET("http://api.github.com/repos/jthomp13/Test/issues", query = list(state="all"))
    data <- fromJSON(rawToChar(res$content), simplifyDataFrame = TRUE, flatten=TRUE)%>%
      mutate(size = map(labels, ~get_size(.)))
    issueDetails <- rbindlist(map(data$url, ~getIssueDates(.)))
    data <- data %>% inner_join(issueDetails)
    data
  })
  
  output$tableOutput <- DT::renderDataTable({
    data <- issuesResponse()
    data <-data %>%
      select( title,number, state,created_at, updated_at, closed_at, "milestone.title", "milestone.number", "milestone.created_at", "milestone.due_on", size)
   data
  },options = list(scrollX = TRUE, pageLength = 5))
  
  output$selectMilestone <- renderUI({
    data <- issuesResponse()
    selectInput(ns("selectMilestone"), "Select A Milestone", choices = na.omit(data$`milestone.title`), selected = NULL)
  })
  
  output$plotOutput <- renderPlotly({
    milestone <- input$selectMilestone
    print(milestone)
    if(is.null(milestone)){
      print("FAIL")
      return(NULL)
    }
    data <- issuesResponse()
    milestoneIssues <- issuesResponse() %>% filter(`milestone.title` == milestone)
    #Get start date and end date for milestone. This should be the same for all issues
    msStart <- milestoneIssues$`milestone.created_at`[1]
    print(msStart)
    msEnd <- milestoneIssues$`milestone.due_on`[1]
    print(msEnd)
    p2 <- plot_ly(milestoneIssues, x= ~size, type="histogram")
    p2
  })
}



#HELPER FUNCTIONS
get_size <- function(labelFrame){
  sizes <- labelFrame %>% filter(str_detect(name,"size")) 
  #filters out issues without a size label
  if(dim(sizes)[1]==0){
    return(NA)
  }
  regexp <- "[\\d]+"
  sizes <- sizes %>% mutate(right_size = str_extract(name, regexp))
  size <- sum(as.double(sizes$right_size))
  return(size)}

follow_URL <- function(GitHubUrl){
  res <- GET(GitHubUrl, add_headers(authorization= config::get("my_access_token")))
  data <- fromJSON(rawToChar(res$content),simplifyVector= TRUE, flatten=TRUE)
  return(data)
}

getIssueDates <- function(issueUrl){
  data <- follow_URL(issueUrl)
  selections <- data[c("id", "created_at", "updated_at", "closed_at")]
  #changes Nulls to NA to keep the table format
  selections[sapply(selections, is.null)] <- NA
  return(flatten_dfc(selections))
}

