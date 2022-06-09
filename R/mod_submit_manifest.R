#' submit_manifest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_submit_manifest_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             shinydashboard::box(
               title = "",
               width = 12,
               DT::DTOutput(ns("modified_manifest"))
             ))
    )
 
  )
}
    
#' submit_manifest Server Functions
#'
#' @noRd 
mod_submit_manifest_server <- function(id, manifest, status){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_submit_manifest_ui("submit_manifest_1")
    
## To be copied in the server
# mod_submit_manifest_server("submit_manifest_1")
