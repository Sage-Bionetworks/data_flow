#' file_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_file_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      waiter::use_waiter(),
      column(width = 12,
             # Action button that opens file selector
             # TODO: eventually this will become a toggle similar to data_curator dashboard
             div(
               id = ns("select_files_wrapper"),
               shinydashboard::box(
                 width = NULL,
                 title = "Select Files",
                 
                 actionButton(ns("getfiles_btn"), "Get Files"),
                 
                 DT::DTOutput(ns("manifest_tbl"))
                 )
               )
             )
      )
    )
  }
    
#' file_selection Server Functions
#'
#' @noRd 
mod_file_selection_server <- function(id, dataset, asset_view, input_token) {
  
  moduleServer( id, function(input, output, session) {
    
    ns <- session$ns
    
    w <- Waiter$new(id = ns("select_files_wrapper"),
                    html = div(
                      style="color:#424874;",
                      waiter::spin_3(),
                      h4("Retrieving manifest...")),
                    color = transparent(.8))
    

    # ON CLICK GET MANIFEST FOR SELECTED DATASET ############################################################
    # FIXME: Button click data flow is not ideal
    # Click of show file level view must happen AFTER dataset is selected
    # If you have clicked show file level view and displayed a dataset, but want to
    # change that dataset you need to click "Show file level view" again
    
    manifest <- eventReactive(input$getfiles_btn, {
      
      # show waiter
      w$show()
      
      # on exit - hide waiter
      on.exit({
        w$hide()
      })
      
      ds <- dataset()
      manifest_download_to_df(asset_view = asset_view,
                              dataset_id = ds$id,
                              input_token = input_token)
      
    })
    
    # DISPLAY MANIFEST AS TABLE #############################################################################
    
    output$manifest_tbl <- DT::renderDataTable({
      DT::datatable(manifest(),
                    option = list(scrollY = 500,
                                  scrollX = TRUE,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })
    
    # return a list containing the downloaded manifest and rows selected
    
    return(list(
      manifest = reactive({ manifest() }),
      selected_rows = reactive({ input$manifest_tbl_rows_selected })
    ))
    
  })
}