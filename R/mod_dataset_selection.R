#' dataset_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dataset_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # select dataset box
    shinydashboard::box(
      title = "Select Dataset",
      width = 6,
      
      # output table
      DT::DTOutput(ns("tbl")),
      
      br(),
      
      # selection action button
      actionButton(ns("button"), "Select Dataset(s)"),
      
      br(),
      )
    )
}
    
#' dataset_selection Server Functions
#'
#' @noRd 
mod_dataset_selection_server <- function(id) {
  
  moduleServer( id, function(input, output, session) {
    
    ns <- session$ns
    
    # create dataset datatable
    # create dummy table
    # this will be a call to synapse to get datasets in a specified fileview
    datasets_col <- paste0(rep("dataset", 200), "_", seq(1:200))
    status_col <- sample(c("Quarantine", "Release 1"), 200, replace = TRUE)
    dummy_data <- data.frame(Dataset = datasets_col, Status = as.factor(status_col))
    
    # render data table with scroll bar, no pagination, and filtering
    output$tbl <- DT::renderDT({
      DT::datatable(dummy_data,
                    option = list(scrollY = 500,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })
      
    
    # when button is pushed
    # if no rows selected: show no dataset selected
    # if rows selected return the selection
    eventReactive(input$button, {
      s <- input$tbl_rows_selected
      if (length(s) == 0) {
        showNotification("No Dataset Selected")
        return(NULL)
      } else{
        return(dummy_data[s,])
        }
      })
    })
  }
  