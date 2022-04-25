data_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
                 fluidRow(
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Upload summary data",
                       fileInput(ns("summaryFile"), "Upload Summary Data")
                     )
                   ),
                   column(
                     width=12,
                     div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("dataDF"))) 
                   )
                 )
  )
}


data_server <- function(id, DATAOBJS){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      summary_data <- NULL
      
      observe({
        inFile <- input$summaryFile
        if(is.null(inFile))
          return(NULL)
        
        
        DATAOBJS$summary_data <<- read.csv(input$summaryFile$datapath) 
        output$dataDF <- DT::renderDataTable(DT::datatable(DATAOBJS$summary_data, 
                                                           options = list(paging = FALSE,
                                                                          scrollX = TRUE), 
                                                           editable = TRUE))
      })
      
      observeEvent(input$dataDF_cell_edit, {
        data_val <- input$dataDF_cell_edit$value
        if(is.numeric(DATAOBJS$summary_data[[colnames(DATAOBJS$summary_data)[input$dataDF_cell_edit$col]]])) data_val <- as.numeric(data_val)
        DATAOBJS$summary_data[input$dataDF_cell_edit$row,input$dataDF_cell_edit$col] <<- data_val
      })
      
    }
  )
}