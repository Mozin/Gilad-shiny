trends_timeseries_ui <- function(id){
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


get_summarised_data_for_plotting <- function(summary_data, x_var, y_var, plot_metric){
  selected_df <- summary_data[,c(x_var, y_var)] %>% setNames(c("x_var", "y_var"))
  if(plot_metric == "mean"){
    selected_df %>% group_by(x_var) %>% summarise(y_var = mean(y_var, na.rm = T)) %>% return()
  }else if(plot_metric == "min"){
    selected_df %>% group_by(x_var) %>% summarise(y_var = min(y_var, na.rm = T)) %>% return()
  }else if(plot_metric == "max"){
    selected_df %>% group_by(x_var) %>% summarise(y_var = max(y_var, na.rm = T)) %>% return()
  }
}


get_summary_statistics_plot_data <- function(plot_data, xVar, yVar){
  y_kendall <- MannKendall(plot_data$y_var)
  summary_df <- data.frame("summary_stat" = c("min", "max", "median", "mean", "25 pc", "75 pc", "Kendall stat", "Kendall p val"),
                           "yVar" = c(min(plot_data$y_var, na.rm = T),
                                      max(plot_data$y_var, na.rm = T),
                                      median(plot_data$y_var, na.rm = T),
                                      mean(plot_data$y_var, na.rm = T),
                                      quantile(plot_data$y_var, 0.25, na.rm = T),
                                      quantile(plot_data$y_var, 0.75, na.rm = T),
                                      y_kendall$tau,
                                      y_kendall$sl) %>% round(2)) %>% 
    setNames(c("Metric", yVar))
  return(summary_df)
}


summarise_facet_var_data_for_plot_data <- function(summary_data, facet_var, x_var, y_var, plot_metric){
  facet_wrap_data <- summary_data[,c(x_var, facet_var, y_var)] %>% setNames(c("x_var", "facet_var", "y_var"))
  if(plot_metric == "mean"){
    facet_wrap_data %>% group_by(x_var, facet_var) %>% summarise(y_var = mean(y_var, na.rm = T)) %>% return()
  }else if(plot_metric == "min"){
    facet_wrap_data %>% group_by(x_var, facet_var) %>% summarise(y_var = min(y_var, na.rm = T)) %>% return()
  }else if(plot_metric == "max"){
    facet_wrap_data %>% group_by(x_var, facet_var) %>% summarise(y_var = max(y_var, na.rm = T)) %>% return()
  }
  return(facet_wrap_data)
}


plot_summarised_data <- function(summary_data, input, output){
  if(is.null(input$yVar)) return()
  lapply(1:(length(input$yVar)), function(i){
    yVar <- input$yVar[[i]]
    plot_data <- summary_data %>% get_summarised_data_for_plotting(input$xVar, yVar, input$plotMetric)
    selected_df <- summary_data[,c(input$xVar, yVar)] %>% setNames(c("x_var", "y_var"))
    summary_stats_plot_data <- get_summary_statistics_plot_data(selected_df, input$xVar, yVar)
    output[[paste0("table", i)]] <- DT::renderDataTable(DT::datatable(summary_stats_plot_data, 
                                                                      options = list(paging = FALSE,
                                                                                     searching = FALSE)))
    if(!(input$facetVar %>% is.null() || input$facetVar == "None")){
      plot_data <- summarise_facet_var_data_for_plot_data(summary_data, input$facetVar, input$xVar, yVar, input$plotMetric)
    }
    
    plot_obj <- ggplot(data = plot_data, aes(x = x_var, y = y_var)) +
      geom_col() +
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


trends_timeseries_server <- function(id, DATAOBJS){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(input$xVar, {
        if(!is.null(DATAOBJS$summary_data)){
          plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$plotMetric, {
        if(!is.null(DATAOBJS$summary_data)){
          plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$plotSmooth, {
        if(!is.null(DATAOBJS$summary_data)){
          plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })
      
      observeEvent(input$facetVar, {
        if(!is.null(DATAOBJS$summary_data)){
          plot_summarised_data(DATAOBJS$summary_data, input, output)
        }
      })

      observe({
        if(!is.null(DATAOBJS$summary_data)){
          plot_summarised_data(DATAOBJS$summary_data, input, output)
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
            plot_summarised_data(DATAOBJS$summary_data, input, output)
          }
        }
      })
    }
  )

  
}