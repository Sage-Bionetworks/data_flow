#' select_storage_project UI Function
#'
#' @description A shiny Module.
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
      
      DT::DTOutput(ns("tst_tbl")),
      textOutput(ns("tst_proj")),
      
      br()
    )
 
  )
}
    
#' select_storage_project Server Functions
#'
#' @noRd 
mod_select_storage_project_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    schematic_token <- Sys.getenv("schematicToken")
    asset_view <- "syn20446927"
    
    # API CALL : GET STORAGE PROJECTS #######################################################################

    # storage_project_list <- storage_projects(asset_view = asset_view,
    #                                          input_token = schematic_token)
    # 
    # # name list (required for list_to_dataframe)
    # 
    # # convert to dataframe
    # storage_project_df <- list_to_dataframe(list = storage_project_list,
    #                                         col_names = c("id", "name"))
    # 
    # # reorder and add to reactive values
    # storage_project_df <- dplyr::select(storage_project_df, name, id)
    
    storage_project_df <- data.frame(name = c("HTAN CenterA", "DROPME"),
                                     id = c("synID####", "synID1234"))
    
    # DROP DOWN LISTING STORAGE PROJECTS ####################################################################
    
    output$project_selector <- shiny::renderUI({
      
       selectInput(inputId = ns("selected_project"),
                   label = "Select Project",
                   choices = storage_project_df$name,
                   selectize = FALSE)
      
    })
    
    # ON BUTTON CLICK RETURN SELECTED PROJECT
    # return project df subsetted by selected project
    selected_project_df <- eventReactive(input$select_project_btn, {
      
      storage_project_df[ grepl(input$selected_project, storage_project_df$name), ]
      
      # return(list(
      #   selected_project_df = selected_project_df,
      #   action_btn = input$select_select_project_btn
      #   ))
      })
    
    # return(list(
    #   selected_project_df = reactive({ selected_project_df() }),
    #   action_btn = reactive({ input$select_project_btn })
    # ))
    
    return( reactive({ selected_project_df() }))
  })
  }
    
## To be copied in the UI
# mod_select_storage_project_ui("select_storage_project_1")
    
## To be copied in the server
# mod_select_storage_project_server("select_storage_project_1")