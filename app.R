library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DBI)
library(Kendall)

source("data_app.R")
source("trends_timeseries.R")
source("scatter_plots.R")
source("assesment_app.R")
source("reporting_app.R")


ui <- dashboardPage(
  dashboardHeader(title = tags$img(src = "logo.png", height = "100%")),
  dashboardSidebar(uiOutput("sidebarpanel")),
  dashboardBody(
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Ramsar Monitoring and Assessment Tool </span>\');
      })
     ')),
    tags$head(tags$style(HTML('
                              .skin-blue .main-header .navbar {
                                background-color: #27A8AB;
                              }      
                              /* logo */
                              .skin-blue .main-header .logo {
                                background-color: #27A8AB;
                              }
                              .skin-blue .main-sidebar {
                                background-color: #37CDD1;
                              }
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #27A8AB;
                                color: #000000;
                              }
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #37CDD1;
                                color: #ffffff;
                              }                              
                        '))),
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
      ),
      tabItem(
        tabName = "assesment",
        assessment_app_ui("assesment")
      ),
      tabItem(
        tabName = "reporting",
        assessment_app_ui("reporting")
      )
    )
  })
  
  output$sidebarpanel <- renderMenu({
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Data", tabName = "Data"),
      menuItem("Exploration", tabName = "TimeSeriesTrends"),
      menuItem("Cause and Effect", tabName = "causeEffect"),
      menuItem("Assesment", tabName = "assesment"),
      menuItem("Reporting")
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
  assesment_app_server("assesment", DATAOBJS)
}


shinyApp(ui = ui, server = server)