scatter_plots_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
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
          selectInput(ns("yVar"), 
                      "Y Axis variable", 
                      choices=c(),
                      multiple = TRUE)
        )
      ),
      column(
        width=3,
        box(
          width=12,
          selectInput(ns("plotMetric"), "Metric to plot", choices=c("min", "max", "mean"))
        )
      )
    ),
    fluidRow(
      column(
        width=3,
        box(
          width=12,
          selectInput(ns("facetVar"), "facet variable", choices=c())
        )
      ),
      column(
        width=3,
        box(
          width=12,
          selectInput(ns("plotSmooth"), "Plot smooth var", choices=c("None", "Linear", "Poly x2"))
        )
      ),
    ),
    fluidRow(
      uiOutput(ns("dataPlots"))
      # plotOutput(ns("dataPlot"))
    )
  )
}


scatter_plot_summarised_data <- function(summary_data, input, output){
  if(is.null(input$yVar)) return()
  lapply(1:(length(input$yVar)), function(i){
    yVar <- input$yVar[[i]]
    plot_data <- summary_data[,c(input$xVar, yVar)] %>% setNames(c("x_var", "y_var"))
    selected_df <- summary_data[,c(input$xVar, yVar)] %>% setNames(c("x_var", "y_var"))
    summary_stats_plot_data <- get_summary_statistics_plot_data(selected_df, input$xVar, yVar)
    output[[paste0("table", i)]] <- DT::renderDataTable(DT::datatable(summary_stats_plot_data, 
                                                                      options = list(paging = FALSE,
                                                                                     searching = FALSE)))
    if(!(input$facetVar %>% is.null() || input$facetVar == "None")){
      plot_data <- summarise_facet_var_data_for_plot_data(summary_data, input$facetVar, input$xVar, yVar, input$plotMetric)
    }
    
    plot_obj <- ggplot(data = plot_data, aes(x = x_var, y = y_var)) +
      geom_point() +
      xlab(input$xVar) +
      ylab(yVar)
    if(input$plotSmooth %>% is.null() || input$plotSmooth == "None"){
      plot_obj <- plot_obj
    }else{
      if(input$plotSmooth == "Poly x2") plot_obj <- plot_obj + geom_smooth(method = "glm", formula = y ~ poly(x, 2))
      if(input$plotSmooth == "Linear") plot_obj <- plot_obj + geom_smooth(method = "glm", formula = y ~ x)
    }
    
    if(!(input$facetVar %>% is.null() || input$facetVar == "None")){    
      plot_obj <- plot_obj + facet_wrap(~facet_var)
    }
    output[[paste0("plot", i)]] <- renderPlot(plot_obj)
  })
}


scatter_plots_server <- function(id, DATAOBJS){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(input$xVar, {
        if(!is.null(DATAOBJS$summary_data)){
          scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$plotMetric, {
        if(!is.null(DATAOBJS$summary_data)){
          scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$plotSmooth, {
        if(!is.null(DATAOBJS$summary_data)){
          scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$facetVar, {
        if(!is.null(DATAOBJS$summary_data)){
          scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observe({
        if(!is.null(DATAOBJS$summary_data)){
          scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      
      observe({
        updateSelectInput(session = session, "xVar", choices = colnames(DATAOBJS$summary_data))
        updateSelectInput(session = session, "yVar", choices = colnames(DATAOBJS$summary_data))
        updateSelectInput(session = session, "facetVar", choices = c("None", colnames(DATAOBJS$summary_data)))
      })
      
      
      observeEvent(input$yVar, {
        if(!is.null(DATAOBJS$summary_data)){
          if(length(input$yVar) > 0){
            output$dataPlots <- renderUI({
              lapply(1:length(input$yVar), function(i) {
                plotname <- paste("plot", i, sep="")
                tablename <- paste("table", i, sep="")
                fluidRow(
                  column(
                    width = 8,
                    plotOutput(ns(plotname))
                  ),
                  column(
                    width = 4,
                    DT::dataTableOutput(ns(tablename))
                  )
                )
              })
            })
            scatter_plot_summarised_data(DATAOBJS$summary_data, input, output)
          }
        }
      })
    }
  )
  
  
}