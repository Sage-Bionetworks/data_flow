# Storage Project Selection Module UI

#' @title select_storage_project_ui and select_storage_project_server
#' @description A shiny module. Outputs a selectInput dropdown of Synapse storage project names to the UI.
#' @return To the server: A list information from the module. `selected_df` - a dataframe with a single row containing the `name` and `id` of the selected storage project. `action_btn` - TRUE/FALSE output from button click.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_storage_project_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Select Project",
      width = NULL,
      
      # Project dropdown
      uiOutput(ns("project_selector")),
      
      # Button to initiate project selection
      actionButton(ns("select_project_btn"),
                   "Submit"),
      
      br()
    )
 
  )
}
    
# Storage Project Selection Module Server
#'
#' @noRd 
mod_select_storage_project_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    schematic_token <- Sys.getenv("schematicToken")
    asset_view <- "syn20446927"
    
    # API CALL : GET STORAGE PROJECTS #######################################################################

    storage_project_list <- storage_projects(asset_view = asset_view,
                                             input_token = schematic_token)

    # name list (required for list_to_dataframe)

    # convert to dataframe
    storage_project_df <- list_to_dataframe(list = storage_project_list,
                                            col_names = c("id", "name"))

    # reorder and add to reactive values
    storage_project_df <- dplyr::select(storage_project_df, name, id)
    
    # DROP DOWN LISTING STORAGE PROJECTS ####################################################################
    
    # render ui for storage project drop down
    output$project_selector <- shiny::renderUI({
      
       selectInput(inputId = ns("selected_project"),
                   label = "Select Project",
                   choices = storage_project_df$name,
                   selectize = FALSE) # must be false or for some reason cannot reference `input$selected_project`
      
    })
    
    # ON BUTTON CLICK RETURN SELECTED PROJECT  ##############################################################
    
    # return project df subsetted by selected project and action button click
    eventReactive(input$select_project_btn, {
      
      selected_project_df <- storage_project_df[ grepl(input$selected_project, storage_project_df$name), ]
      
      return(list(
        selected_df = selected_project_df,
        action_btn = input$select_project_btn
        ))
      })
  })
  }
    
## To be copied in the UI
# mod_select_storage_project_ui("select_storage_project_1")
    
## To be copied in the server
# mod_select_storage_project_server("select_storage_project_1")