#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

app_server <- function( input, output, session ) {
  # Your application server logic
  options(shiny.reactlog = TRUE)

  # AUTHENTICATION
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  redirect_url <- paste0(
    api$access, "?", "redirect_uri=", app_url, "&grant_type=",
    "authorization_code", "&code=", params$code
  )
  
  # get the access_token and userinfo token
  req <- httr::POST(redirect_url, 
                    encode = "form", 
                    body = "", 
                    httr::authenticate(app$key, app$secret, type = "basic"), 
                    config = list())
  # Stop the code if anything other than 2XX status code is returned
  
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
  
  session$userData$access_token <- access_token
  
  # SET UP REACTIVE VALUES  ###################################################
  
  # reactive dash config
  dash_config_react <- reactiveVal(NULL)
  
  # reactive data flow manifest
  df_manifest_react <- reactiveVal(NULL)
  # reactive filtered manifest
  filtered_manifest_react <- reactiveVal(NULL)
  
  # capture selected dcc configuration in a list
  selected_dcc_config_list <- list(
    project_name = reactiveVal(NULL),
    synapse_asset_view = reactiveVal(NULL),
    manifest_dataset_id = reactiveVal(NULL),
    schema_url = reactiveVal(NULL),
    icon = reactiveVal(NULL)
  )
  

  # SELECT A DCC  #############################################################
  mod_select_dcc_out <- dfamodules::mod_select_dcc_server("select_dcc",
                                                          dcc_config,
                                                          access_token)
  
  # CONFIGURE APP ############################################################
  # select a dcc
  # show waiter
  # generate config
  # get manifest
  # update reactive values
  observeEvent(mod_select_dcc_out()$btn_click, {
    
    # move to dashboard page
    shinydashboard::updateTabItems(session = session,
                                   inputId = "tabs",
                                   selected = "tab_dashboard")
    
    # show waiter on button click
    waiter::waiter_show(
      html = shiny::tagList(
        shiny::img(src = "www/loading.gif"),
        shiny::h3("Configuring", style = "color:white;")),
      color="#424874"
    )
    
    # update reactiveVals
    selected_dcc_config_list$synapse_asset_view(mod_select_dcc_out()$selected_dcc_config$synapse_asset_view)
    selected_dcc_config_list$manifest_dataset_id(mod_select_dcc_out()$selected_dcc_config$manifest_dataset_id)
    selected_dcc_config_list$project_name(mod_select_dcc_out()$selected_dcc_config$project_name)
    selected_dcc_config_list$schema_url(mod_select_dcc_out()$selected_dcc_config$schema_url)
    selected_dcc_config_list$icon(mod_select_dcc_out()$selected_dcc_config$icon)
    
    dash_config <- dfamodules::generate_dashboard_config(
      schema_url = selected_dcc_config_list$schema_url(),
      # display_names = list(contributor = "Contributor",
      #                      entityId = "Synapse ID",
      #                      dataset = "Data Type",
      #                      dataset_name = "Dataset Folder Name",
      #                      num_items = "Number of Items in Manifest",
      #                      status = "Status",
      #                      release_scheduled = "Release Date",
      #                      embargo = "Embargo",
      #                      standard_compliance = "QC Checks",
      #                      released = "Released",
      #                      data_portal = "Data Portal",
      #                      Component = NA),
      icon = selected_dcc_config_list$icon(),
      na_replace = list(num_items = "No Manifest",
                        scheduled_release_date = "Not Scheduled",
                        dataset_type = "No Manifest"),
      base_url = schematic_api_url)

    # download manifest
    manifest_obj <- dfamodules::dataset_manifest_download(
      asset_view = selected_dcc_config_list$synapse_asset_view(),
      dataset_id = selected_dcc_config_list$manifest_dataset_id(),
      access_token = access_token,
      base_url = schematic_api_url
      )

    # prep manifest for app
    prepped_manifest <- dfamodules::prep_manifest_dfa(
      manifest = manifest_obj$content,
      config = dash_config
    )
    
    # update reactiveVals
    df_manifest_react(prepped_manifest)
    dash_config_react(dash_config)
    
    # FILTER MANIFEST FOR DASH UI #############################################
    # prepare inputs for filter module
    
    contributor_choices <- unique(prepped_manifest$contributor)
    dataset_choices <- unique(prepped_manifest$dataset_type)
    release_daterange_start <- min(prepped_manifest$scheduled_release_date, na.rm = TRUE)
    release_daterange_end <- max(prepped_manifest$scheduled_release_date, na.rm = TRUE)
    status_choices <- unique(prepped_manifest$status)
      
    filter_inputs <- list(
      contributor_choices,
      dataset_choices,
      release_daterange_start,
      release_daterange_end,
      status_choices)
    
    # FILTER MANIFEST FOR DASH SERVER  ########################################
    
    output$filter_module <- shiny::renderUI({
      filters <- filter_inputs
      dfamodules::mod_datatable_filters_ui(
        "datatable_filters_1",
        contributor_choices = filters[[1]],
        dataset_choices = filters[[2]],
        release_daterange = c(filters[[3]], filters[[4]]),
        status_choices = filters[[5]])
    })

    filtered_manifest <- dfamodules::mod_datatable_filters_server("datatable_filters_1",
                                                                  df_manifest_react)

    
    # DATASET DASH  ###########################################################

    dfamodules::mod_datatable_dashboard_server("dashboard_1",
                                               filtered_manifest,
                                               dash_config_react)
    
    # DATASET DASH VIZ : DISTRIBUTIONS ########################################
    
    dfamodules::mod_distribution_server(id = "distribution_contributor",
                                        df = filtered_manifest,
                                        group_by_var = "contributor",
                                        title = NULL,
                                        x_lab = "Contributor",
                                        y_lab = "Number of Datasets",
                                        fill = "#0d1c38")
    
    dfamodules::mod_distribution_server(id = "distribution_datatype",
                                        df = filtered_manifest,
                                        group_by_var = "dataset_type",
                                        title = NULL,
                                        x_lab = "Type of dataset",
                                        y_lab = "Number of Datasets",
                                        fill = "#0d1c38")
    
   # hide waiter
    waiter::waiter_hide()

  })
  


  


  # SELECT STORAGE PROJECT ###################################################
  
  selected_project_id <- reactiveVal(NULL)

  observe({
  
    if (input$tabs == "tab_administrator") {
      
      mod_select_storage_project_out <- dfamodules::mod_select_storage_project_server(
        id = "select_storage_project_1",
        asset_view = selected_dcc_config_list$synapse_asset_view,
        access_token = access_token,
        base_url = schematic_api_url)
      
      selected_project_id(mod_select_storage_project_out())
    }
  })
  
  mod_dataset_selection_out <- dfamodules::mod_dataset_selection_server(
    id = "dataset_selection_1",
    storage_project_id = selected_project_id,
    asset_view = selected_dcc_config_list$synapse_asset_view,
    access_token = access_token,
    base_url = schematic_api_url
  )

  # ADMINISTRATOR  #######################################################################


  # # UPDATE DATA FLOW STATUS SELECTIONS
  # updated_data_flow_status <- dfamodules::mod_update_data_flow_status_server("update_data_flow_status_1")
  # 
  # 
  # # MODIFY MANIFEST
  # modified_manifest <- shiny::reactive({
  #   shiny::req(updated_data_flow_status())
  # 
  #   dfamodules::update_dfs_manifest(
  #     dfs_manifest = df_manifest_react(),
  #     dfs_updates = updated_data_flow_status(),
  #     selected_datasets_df = mod_dataset_selection_out()$selected_datasets)
  # })
  # 
  # # BUTTON CLICK UPDATE MANIFEST
  # # shiny::observeEvent(input$save_update, {
  # #   rv_manifest(modified_manifest())
  # # })
  # # 
  # # shiny::observeEvent(input$clear_update, {
  # #   rv_manifest(manifest_dfa)
  # # })
  # 
  # 
  # # 
  # # # PREP MANIFEST FOR SYNAPSE SUBMISSION
  # # 
  # manifest_submit <- shiny::reactive({
  #   dfamodules::prep_manifest_submit(modified_manifest(),
  #                                    dash_config_react())
  # })
  # 
  # # get names of selected datasets
  # selected_row_names <- shiny::reactive({
  #   mod_dataset_selection_out()$selected_datasets()$id
  # 
  # })
  # 
  # dfamodules::mod_highlight_datatable_server("highlight_datatable_1",
  #                                            manifest_submit,
  #                                            selected_row_names,
  #                                            "dataset_id")
  # # 
  # # SUBMIT MODEL TO SYNAPSE
  # # make sure to submit using a manifest that has been run through date to string
  # dfamodules::mod_submit_model_server(id = "submit_model_1",
  #                                     dfs_manifest = manifest_submit,
  #                                     data_type = NULL,
  #                                     asset_view = reactive_asset_view,
  #                                     dataset_id = reactive_manifest_id,
  #                                     manifest_dir = "./manifest",
  #                                     access_token = access_token,
  #                                     base_url = schematic_api_url,
  #                                     schema_url = reactive_schema_url)

}
