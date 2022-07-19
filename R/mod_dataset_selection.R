#' dataset_selection2 UI Function
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
  
    ## SELECT PROJECT BOX  ####################################################
    
    fluidRow(
      column(width = 12,
             
             mod_select_storage_project_ui(ns("select_storage_project_1")),
             
             br()
             )
      ),
    
    ## SELECT DATASET BOX  ####################################################
    
    fluidRow(
      waiter::useWaiter(),
      column(width = 12,
             div(
               id = ns("select_dataset_wrapper"),
               
               shinydashboard::box(
                 title = "Select Dataset",
                 width = NULL,
                 
                 # Table of storage project datasets
                 DT::DTOutput(ns("dataset_tbl")),

                 br(),
                 
                 # Button to initiate dataset selection
                 actionButton(ns("select_dataset_btn"), "Select Dataset(s)"),
                 
                 br()
                 )
               )
             )
      )
    )
  }
    
#' dataset_selection2 Server Functions
#'
#' @noRd 
mod_dataset_selection_server <- function(id,
                                         asset_view,
                                         input_token) {
  
  moduleServer( id, function(input, output, session) {
    
    ns <- session$ns
    
    # initialize object that contains reactive values
    rv <- reactiveValues()
    
    # initialize waiter
    w <- Waiter$new(id = ns("select_dataset_wrapper"),
                    html = div(
                      style="color:#424874;",
                      waiter::spin_3(),
                      h4("Retrieving datasets...")),
                    color = transparent(.8))
    
    # STORAGE PROJECT SELECTOR MODULE #######################################################################
    # selected_df (dataframe)
    # action_btn (TRUE/FALSE)
    
    select_storage_project <- mod_select_storage_project_server(id = "select_storage_project_1",
                                                                asset_view = asset_view,
                                                                input_token = input_token)
    
    ## ON CLICK DISPLAY STORAGE PROJECT DATASETS  ###########################################################
    # on button click call storage_project_datasets using selected project ID
    
    observeEvent(select_storage_project()$action_btn, {
      
      # show waiter
      w$show()

      # on exit - hide waiter
      on.exit({
        w$hide()
      })
      
      # call schematic API - get datasets for selected storage project
      
      ### COMMENT OUT FOR TESTING
      dataset_list <- storage_project_datasets(asset_view = asset_view,
                                               project_id = select_storage_project()$selected_df$id,
                                               input_token = input_token)

      # schematic outputs a list
      # parse into a dataframe

      rv$dataset_df <- list_to_dataframe(list = dataset_list,
                                      col_names = c("id", "name"))

      rv$dataset_df <- dplyr::select(rv$dataset_df, name, id)

      #rv$dataset_df <- data.frame(name = "HTAN_CenterA_Demographics", id = "syn30028964")

      # render data table with scroll bar, no pagination, and filtering
      output$dataset_tbl <- DT::renderDataTable({
        DT::datatable(rv$dataset_df,
                      selection = "single",
                      option = list(scrollY = 500,
                                    scrollCollapse = TRUE,
                                    bPaginate = FALSE,
                                    dom = "t"),
                      filter = list(position = 'top', clear = TRUE))
      })
    })

    ## ON BUTTON CLICK SUBMIT DATASET SELECTION #############################################################

    # when button is pushed
    # if no rows selected: show no dataset selected
    # if rows selected return the selection
    eventReactive(input$select_dataset_btn, {
      selected <- input$dataset_tbl_rows_selected
      if (length(selected) == 0) {
        showNotification("No Dataset Selected")
        return(NULL)
      } else{
        return(rv$dataset_df[selected,])
        }
      })
 

    })
}
 
    
## To be copied in the UI
# mod_dataset_selection2_ui("dataset_selection2_1")
    
## To be copied in the server
# mod_dataset_selection2_server("dataset_selection2_1")
