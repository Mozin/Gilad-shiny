data_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      tabBox(
        width = 12,
        tabPanel("Plot",
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
               width=3,
               box(
                 width=12,
                 selectInput(ns("xVar"), "X Axis variable", choices=c())
               )
             ),
             column(
               width=3,
               box(
                 width=12,
                 selectInput(ns("yVar"), "Y Axis variable", choices=c())
               )
             )
             
           ),
           fluidRow(
             plotOutput(ns("dataPlot"))
           )
        ),
        tabPanel("Data", 
                 fluidRow(
                   column(
                     width=12,
                     div(style = 'overflow-x: scroll',DT::dataTableOutput(ns("dataDF"))) 
                   )
                 ))
      )
    )
  )
}


get_summarised_data_for_plotting <- function(summary_data, x_var, y_var){
  selected_df <- summary_data[,c(x_var, y_var)] %>% setNames(c("x_var", "y_var"))
  selected_df %>% group_by(x_var) %>% summarise(y_var = mean(y_var, na.rm = T))
}


plot_summarised_data <- function(summary_data, input, output){
  plot_data <- summary_data %>% get_summarised_data_for_plotting(input$xVar, input$yVar)
  # output$dataPlot <-  renderPlot(ggplot(data = plot_data, aes(x = x_var, y = y_var)) + 
  #                                  geom_line() + 
  #                                  geom_point() +
  #                                  xlab(input$xVar) +
  #                                  ylab(input$yVar))
  output$dataPlot <- renderPlot(ggplot(data = plot_data, aes(x = x_var, y = y_var)) +
                                  geom_col() +
                                  xlab(input$xVar) +
                                  ylab(input$yVar))
}


data_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      summary_data <- NULL
      
      observe({
        inFile <- input$summaryFile
        if(is.null(inFile))
          return(NULL)
        
        
        summary_data <<- read.csv(input$summaryFile$datapath) 
        updateSelectInput(session = session, "xVar", choices = colnames(summary_data))
        updateSelectInput(session = session, "yVar", choices = colnames(summary_data))
        output$dataDF <- DT::renderDataTable(DT::datatable(summary_data, options = list(paging = FALSE), editable = TRUE))
      })
      
      observeEvent(input$xVar, {
        if(!is.null(summary_data)){
          plot_summarised_data(summary_data, input, output)
        }
      })
      
      observeEvent(input$yVar, {
        if(!is.null(summary_data)){
          plot_summarised_data(summary_data, input, output)
        }
      })
      
      
      observeEvent(input$dataDF_cell_edit, {
        data_val <- input$dataDF_cell_edit$value
        if(is.numeric(summary_data[[colnames(summary_data)[input$dataDF_cell_edit$col]]])) data_val <- as.numeric(data_val)
        summary_data[input$dataDF_cell_edit$row,input$dataDF_cell_edit$col] <<- data_val
        plot_summarised_data(summary_data, input, output)
      })
      
    }
  )
}