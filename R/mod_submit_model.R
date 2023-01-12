#' submit_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_submit_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    waiter::useWaiter(),
    actionButton(ns("submit"), "Submit to Synapse")
 
  )
}
    
#' submit_model Server Functions
#'
#' @noRd 
mod_submit_model_server <- function(id, 
                                    dfs_manifest,
                                    data_type,
                                    asset_view,
                                    dataset_id,
                                    manifest_dir = "./manifest",
                                    input_token,
                                    schema_url) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # make sure that manifest folder exists
    if (!file.exists(manifest_dir)) {
      dir.create(manifest_dir)
    }
    
    # on button click submit model to synapse
    observeEvent(input$submit, {
      
      waiter::waiter_show(html = div(
        style="color:#424874;",
        waiter::spin_3(),
        h4("Submitting updated manifest to Synapse...")))
      
      # write manifest table for upload
      path <- file.path(manifest_dir, "synapse_storage_manifest_dataflow.csv")
      
      write.table(dfs_manifest(),
                  path,
                  sep = ",",
                  row.names = FALSE)
      
      # submit model to synapse
      model_submit(data_type = data_type,
                   asset_view = asset_view,
                   dataset_id = dataset_id,
                   restrict_rules = TRUE,
                   file_name = path,
                   input_token = input_token,
                   manifest_record_type = "table",
                   url = "https://schematic.dnt-dev.sagebase.org/v1/model/submit",
                   schema_url = schema_url)
      
      waiter::waiter_hide()
    })
    
 
  })
}
    
## To be copied in the UI
# mod_submit_model_ui("submit_model_1")
    
## To be copied in the server
# mod_submit_model_server("submit_model_1")
