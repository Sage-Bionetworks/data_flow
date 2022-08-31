#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF ###########################################################################
  
  # initialize waiter
  w <- Waiter$new(id = "release_status_wrapper",
                  html = div(
                    style="color:#424874;",
                    waiter::spin_3(),
                    h4("Submitting updated manifest to Synapse...")),
                  color = transparent(.8))
  
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  # read in global config
  global_config <- jsonlite::read_json("inst/global.json")

  # ADMINISTRATOR  #######################################################################
  
  # STORAGE PROJECT SELECTION
  
  select_storage_project <- mod_select_storage_project_server(id = "select_storage_project_1",
                                                              asset_view = global_config$asset_view,
                                                              input_token = global_config$schematic_token)
  
  # DOWNLOAD MANIFEST
  
  # get DFS manifest ID
  
  data_flow_status_id <- reactive({
    req(select_storage_project())

    select_storage_project_df <- select_storage_project()
    
    # get all manifests
    all_manifests_list <- storage_project_manifests(global_config$asset_view, 
                                                    select_storage_project_df$id, 
                                                    global_config$schematic_token)
    
    # get DataFlowStatus idx
    dfs_manifest_idx <- grep("DataFlowStatus", all_manifests_list)
    
    # Return first element from nested list (DataFlowStatus dataset synID)
    all_manifests_list[[dfs_manifest_idx]][[1]][[1]]
    
  })
  
  
  # retrieve manifest
  manifest <- reactive({
    req(data_flow_status_id())
    
    # download data flow status manifest
    dfs_manifest <- manifest_download_to_df(asset_view = global_config$asset_view,
                                            dataset_id = data_flow_status_id(),
                                            input_token = global_config$schematic_token)
    
    manifest_string_to_date(dfs_manifest)
    
  })
  
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
    
    update_dfs_manifest(dfs_manifest = manifest(),
                        dfs_updates = updated_data_flow_status(),
                        selected_datasets_df = dataset_selection())
  })
  
  # PREP MANIFEST FOR SYNAPSE SUBMISSION
  
  manifest_submit <- reactive({
    req(modified_manifest())
    
    manifest_date_to_string(modified_manifest())
  })
  
  # DISPLAY MANIFEST TO BE SUBMITTED
  
  output$tst_manifest_tbl <- renderDataTable({
    manifest_submit()
  })
  
  # SUBMIT MODEL TO SYNAPSE
  # make sure to submit using a manifest that has been run through date to string
  mod_submit_model_server("submit_model_1",
                          dfs_manifest = manifest_submit,
                          data_type = "DataFlow",
                          dataset_id = data_flow_status_id,
                          manifest_dir = "./manifest",
                          input_token = global_config$schematic_token,
                          schema_url = global_config$schema_url)
  
  # DATASET DASH  #######################################################################
  
  
  
  # prepare data to be displayed by mod_datatable

  mod_tabbed_dashboard_server("tabbed_dashboard_1", 
                              dataset_dash_data,
                              jsonlite::read_json("inst/datatable_dashboard_config.json"))
  
}
