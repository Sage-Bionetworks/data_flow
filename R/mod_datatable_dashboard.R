#' datatable_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_datatable_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("datatable_out"))
  )
}
    
#' datatable_dashboard Server Functions
#' 
#' @param df dataframe containing data to be displayed in dashboard
#' @param config config in `inst/datatable_dashboard_config.json`
#'
#' @noRd 
mod_datatable_dashboard_server <- function(id, df, config){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # render datatable
    
    output$datatable_out <- DT::renderDataTable({
      create_dashboard(df(),
                       config)
    })
 
  })
}
    
## To be copied in the UI
# mod_datatable_dashboard_ui("datatable_dashboard_1")
    
## To be copied in the server
# mod_datatable_dashboard_server("datatable_dashboard_1")
