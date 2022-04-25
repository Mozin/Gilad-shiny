library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(Kendall)

source("data_app.R")
source("trends_timeseries.R")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(uiOutput("sidebarpanel")),
  dashboardBody(
    shinyjs::useShinyjs(),
    uiOutput("body")
  )
)


server <- function(input, output){
  DATAOBJS <- reactiveValues(summary_data = "")
  
  output$body <- renderUI({
    tabItems(
      tabItem(
        tabName = "Introduction"
      ),
      tabItem(
        tabName = "Data",
        data_ui("data")
      ),
      tabItem(
        tabName = "TimeSeriesTrends",
        trends_timeseries_ui("trendsTimeseries")
      )
    )
  })
  
  output$sidebarpanel <- renderMenu({
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Data", tabName = "Data"),
      menuItem("Time Series Trends", tabName = "TimeSeriesTrends"),
      menuItem("Cause and Effect", tabName = "causeEffect")
    )
  })
  
  data_server("data", DATAOBJS)
  trends_timeseries_server("trendsTimeseries", DATAOBJS)
}


shinyApp(ui = ui, server = server)