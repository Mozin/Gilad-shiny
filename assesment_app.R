assessment_app_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width=3,
        box(
          width=12,
          selectInput(ns("yearVar"), "Year Variable", choices=c()),
          sliderInput(ns("yearRange"), "Year range", min =1950, max = 2022, value = 2000),
          selectInput(ns("summaryFunction"), "Summary Function", choices=c("min", "max", "mean"))
        )
      )
    )
  )
}


assesment_app_server <- function(id, DATAOBJS){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
     
    }
  )
  
  
}