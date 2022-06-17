library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DBI)
library(Kendall)

source("data_app.R")
source("trends_timeseries.R")
source("scatter_plots.R")


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
  db_con <- dbConnect(RPostgres::Postgres(), dbname = "postgres", host="platypus-wetlands-db.ckzvcjercc61.ap-southeast-2.rds.amazonaws.com", port=5432, user="mohsin", password="RC345aLaxxuAJ")
  
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
      ),
      tabItem(
        tabName = "causeEffect",
        scatter_plots_ui("scatterPlots")
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
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  data_server("data", DATAOBJS, db_con)
  trends_timeseries_server("trendsTimeseries", DATAOBJS)
  scatter_plots_server("scatterPlots", DATAOBJS)
}


shinyApp(ui = ui, server = server)