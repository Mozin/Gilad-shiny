data_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
                 fluidRow(
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Upload summary data",
                       fileInput(ns("summaryFile"), "Upload Summary Data"),
                       textInput(ns("ramasarId"), "Ramasar id"),
                       actionButton(ns("dbDownload"), "DB data"),
                       actionButton(ns("link"), "Link")
                     )
                   ),
                   column(
                     width=12,
                     div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("dataDF"))) 
                   )
                 )
  )
}


get_db_data_list <- function(db_con, input){
  ramasar_db_df <- DBI::dbGetQuery(db_con, paste0('select * from "v_ramsar_summary" where ramsarid=', "'", input$ramasarId,"'"))
  hybas_db_df <- DBI::dbGetQuery(db_con, paste0("select * from v_hybas_lvl5_summary where ramsarid='", input$ramasarId,"'"))
  wdpa_db_df <- DBI::dbGetQuery(db_con, paste0("select * from v_wdpa_summary where ramsarid='", input$ramasarId,"'"))
  
  df_list <- list(ramasar_db_df, wdpa_db_df)  
  WDPA.Ramsar.DBDat.Filter <- df_list %>% purrr::reduce(inner_join,  by=c("ramsarid", "Year"))
  
  WDPA.Ramsar.DBDat.Filter.Summary <-  WDPA.Ramsar.DBDat.Filter %>%
    dplyr::group_by(Year,ramsarid) %>%
    summarise(Maximum.Water.Area.km2 = max(`Maximum Water Area(km2).x`,`Maximum Water Area(km2).y`),
              Average.Water.Area.km2 = max(`Average Water Area(km2).x`,`Average Water Area(km2).y`),
              Moisture.Index = max(`Moisture Index.x`, `Moisture Index.y`),
              Vegetation.Index = max(`Vegetation Index.x`, `Vegetation Index.y`),
              Annual.Precipitation = max(`Annual Precipitation.x`, `Annual Precipitation.y`))
  
  ## merge Hybas and wetland data ####
  
  colnames(hybas_db_df) <- c("ramsarid","Ramsar.Name","Year","Annual.Precipitation.Hybas5","Palmer.Drought.Hybas5","Runoff.Hybas5")
  list(wdpa = WDPA.Ramsar.DBDat.Filter.Summary, hybas = hybas_db_df) %>% return()
}


data_server <- function(id, DATAOBJS, db_con){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      summary_data <- NULL
      
      observe({
        inFile <- input$summaryFile
        if(is.null(inFile))
          return(NULL)
        
        if(DATAOBJS$raw_data_path == inFile$datapath)
          return(NULL)
        
        DATAOBJS$summary_data <<- read.csv(input$summaryFile$datapath) 
        DATAOBJS$raw_data_path <<- inFile$datapath
        DATAOBJS$raw_summary_data <<- DATAOBJS$summary_data
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
      
      
      observeEvent(input$link, {
        if(input$ramasarId != ""){
          
          df_list <- get_db_data_list(db_con, input)
          ## merge Hybas and wetland data ####
          
          df_list <- list(summary_data = DATAOBJS$raw_summary_data %>% mutate(ramsarid=as.numeric(input$ramasarId), Year=WaterYear)) %>% rlist::list.merge(df_list)      
          merged_df <- df_list %>% purrr::reduce(inner_join,  by=c("ramsarid", "Year"))
          DATAOBJS$summary_data <<- merged_df
          output$dataDF <- DT::renderDataTable(DT::datatable(merged_df, 
                                                             options = list(paging = FALSE,
                                                                            scrollX = TRUE), 
                                                             editable = TRUE))
        }
      })
      
      
      observeEvent(input$dbDownload, {
        if(input$ramasarId != ""){
          
          df_list <- get_db_data_list(db_con, input)
          ## merge Hybas and wetland data ####
          
          merged_df <- df_list %>% purrr::reduce(inner_join,  by=c("ramsarid", "Year"))
          DATAOBJS$summary_data <<- merged_df
          output$dataDF <- DT::renderDataTable(DT::datatable(merged_df, 
                                                             options = list(paging = FALSE,
                                                                            scrollX = TRUE), 
                                                             editable = TRUE))
        }
        
      })
      
    }
  )
}