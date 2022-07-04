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


generate_report_table_row <- function(data_df, row_input_data, year_col){
  if(row_input_data$indicator == "") return(data.frame())
  data_vec <- data_df[[row_input_data$indicator]]
  mean_data <- mean(data_vec, na.rm = T)
  sd_data <- sd(data_vec, na.rm = T)
  percentiles = quantile(data_vec, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) %>% paste0(collapse = ", ")
  current_year <- max(data_df[[year_col]])
  row_input_data$mean <- mean_data
  row_input_data$sd <- sd_data
  row_input_data$percentiles <- percentiles
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
          output$reportTable <- DT::renderDataTable({
            indicators_df %>% plyr::ddply(c("ecosystem_element", "indicator"), function(df){
              generate_report_table_row(DATAOBJS$summary_data, df, REPORTINGVARS$year_var)
            }) %>% 
              DT::datatable(extensions = 'Buttons', 
                            options = list(paging = FALSE, 
                                           buttons = list(
                                             list(extend = 'excel', title = "Data")
                                           ),
                                           dom = 'Bfrtip'))
          })
        }
      })
      
    }
  )
  
}