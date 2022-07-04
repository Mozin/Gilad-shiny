reporting_app_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width=12,
        div(style = 'overflow-x: scroll',DT::dataTableOutput(ns("reportTable")) ) 
      )
    )
  )
}


generate_report_table_row <- function(data_df, row_input_data){
  row_input_data
}


reporting_app_server <- function(id, DATAOBJS, REPORTINGVARS){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observe({
        if(!is.null(DATAOBJS$summary_data) & is.data.frame(DATAOBJS$summary_data) & !is.null(REPORTINGVARS)){
          ambient_df <- data.frame(ecosystem_element = "Ambient environment", indicator = REPORTINGVARS$ambient_var)
          resources_df <- data.frame(ecosystem_element = "Resources", indicator = REPORTINGVARS$resources_var)
          ecosystem_value_df <- data.frame(ecosystem_element = "Ecosystem Value", indicator = REPORTINGVARS$ecosystem_val_var)
          ecosystem_threat_df <- data.frame(ecosystem_element = "Ecosystem Threats", indicator = REPORTINGVARS$ecosystem_threat_var)
          
          indicators_df <- dplyr::bind_rows(ambient_df, resources_df, ecosystem_value_df, ecosystem_threat_df)
          browser()
          output$reportTable <- DT::renderDataTable({
            indicators_df %>% plyr::ddply(c("ecosystem_element", "indicator"), function(df){
              generate_report_table_row(DATAOBJS$summary_data, df)
            }) %>% DT::datatable()
          })
        }
      })
      
    }
  )
  
}