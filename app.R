library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(Kendall)

source("data_app.R")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(uiOutput("sidebarpanel")),
  dashboardBody(
    shinyjs::useShinyjs(),
    uiOutput("body")
  )
)


server <- function(input, output){
  output$body <- renderUI({
    tabItems(
      tabItem(
        tabName = "Data",
        data_ui("data")
      )
    )
  })
  
  output$sidebarpanel <- renderMenu({
    sidebarMenu(
      menuItem("Data", tabName = "Data")
    )
  })
  
  data_server("data")
}


shinyApp(ui = ui, server = server)