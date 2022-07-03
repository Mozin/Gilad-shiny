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
      ),
      column(
        width = 9,
        box(
          width = 3,
          tags$h4("Ambient envirnment"),
          selectInput(ns("ambientVar"), "Select Indicator", choices=c()),
          background="yellow"
        ),
        box(
          width = 3,
          tags$h4("Resources"),
          selectInput(ns("resourcesVar"), "Select Indicator", choices=c()),
          background="blue"
        ),
        box(
          width = 3,
          tags$h4("Ecosystem Value"),
          selectInput(ns("ecosystemValVar"), "Select Indicator", choices=c()),
          background="green"
        ),
        box(
          width = 3,
          tags$h4("Ecosystem Threats"),
          selectInput(ns("ecosystemThreatVar"), "Select Indicator", choices=c()),
          background="orange"
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
      
      observe({
        if(!is.null(DATAOBJS$summary_data) & is.data.frame(DATAOBJS$summary_data)){
          updateSelectInput(session = session, "yearVar", choices = colnames(DATAOBJS$summary_data))
          updateSelectInput(session = session, "ambientVar", choices = colnames(select_if(DATAOBJS$summary_data, is.numeric)))
          updateSelectInput(session = session, "resourcesVar", choices = colnames(select_if(DATAOBJS$summary_data, is.numeric)))
          updateSelectInput(session = session, "ecosystemValVar", choices = colnames(select_if(DATAOBJS$summary_data, is.numeric)))
          updateSelectInput(session = session, "ecosystemThreatVar", choices = colnames(select_if(DATAOBJS$summary_data, is.numeric)))
        }
      })
      
    }
  )
  
  
}