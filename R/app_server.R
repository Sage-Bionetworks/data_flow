#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF ###########################################################################
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  # read in configs
  global_config <- jsonlite::read_json("inst/global.json")
  dash_config <- jsonlite::read_json("inst/datatable_dashboard_config.json")
  

  # DATASET DASH  #######################################################################
  
  # download data flow status manifest
  dfs_manifest <- manifest_download_to_df(asset_view = global_config$asset_view,
                                          dataset_id = global_config$manifest_dataset_id,
                                          input_token = global_config$schematic_token)
  
  dfs_manifest <- manifest_string_to_date(dfs_manifest)
  
  # prepare data to be displayed by mod_datatable
  
  mod_tabbed_dashboard_server("tabbed_dashboard_1", 
                              reactive({dfs_manifest}),
                              jsonlite::read_json("inst/datatable_dashboard_config.json"))

  # ADMINISTRATOR  #######################################################################
  
  # reactive value that holds dfs_manifest 
  rv_manifest <- reactiveVal(dfs_manifest)
  
  # STORAGE PROJECT SELECTION
  
  select_storage_project <- mod_select_storage_project_server(id = "select_storage_project_1",
                                                              asset_view = global_config$asset_view,
                                                              hidden_storage_projects = "HTAN All Projects",
                                                              input_token = global_config$schematic_token)
  
  # DATASET SELECTION
  
  dataset_selection <- mod_dataset_selection_server(id = "dataset_selection_1",
                                                    storage_project_df = select_storage_project,
                                                    asset_view = global_config$asset_view,
                                                    input_token = global_config$schematic_token,
                                                    hidden_datasets = "DataFlowStatus")
  
  # UPDATE DATA FLOW STATUS SELECTIONS 
  updated_data_flow_status <- mod_update_data_flow_status_server("update_data_flow_status_1")
  
  
  # MODIFY MANIFEST
  modified_manifest <- reactive({
    req(updated_data_flow_status())
    
    update_dfs_manifest(dfs_manifest = rv_manifest(),
                        dfs_updates = updated_data_flow_status(),
                        selected_datasets_df = dataset_selection())
  })
  
  
   observeEvent(input$save_update, {
     rv_manifest(modified_manifest())
   })
  
  observeEvent(input$clear_update, {
    rv_manifest(dfs_manifest)
  })
  
  # PREP MANIFEST FOR SYNAPSE SUBMISSION
  
  manifest_submit <- reactive({
    
    manifest_date_to_string(modified_manifest())
  })
  
  # DISPLAY MANIFEST TO BE SUBMITTED
  
  # rearrange manifest so it's more readable
  admin_display_manifest <- reactive({
    rearrange_dataframe(manifest_submit(),
                        names(dash_config))
  })
  
  # get names of selected datasets
  selected_row_names <- reactive({
    dataset_selection()$name
    
  })
  
  mod_highlight_datatable_server("highlight_datatable_1",
                                admin_display_manifest,
                                selected_row_names,
                                "dataset_name")
  
  # SUBMIT MODEL TO SYNAPSE
  # make sure to submit using a manifest that has been run through date to string
  mod_submit_model_server("submit_model_1",
                          dfs_manifest = manifest_submit,
                          data_type = "DataFlow",
                          dataset_id = global_config$manifest_dataset_id,
                          manifest_dir = "./manifest",
                          input_token = global_config$schematic_token,
                          schema_url = global_config$schema_url)

}
