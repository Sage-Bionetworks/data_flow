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
  req <- httr::POST(
    redirect_url,
    encode = "form",
    body = "",
    httr::authenticate(app$key, app$secret, type = "basic"),
    config = list()
    )
  
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
  
  session$userData$access_token <- access_token
  
  # hide things
  # sidebar
  shinyjs::hide(selector = ".sidebar-menu")
  shinyjs::hide(selector = "a[data-value='tab_administrator']")
  
  # SET UP REACTIVE VALUES  ###################################################
  
  # reactive dash config
  dash_config_react <- reactiveVal(NULL)
  
  # reactive data flow manifest
  original_manifest <- reactiveVal(NULL)
  admin_manifest <- reactiveVal(NULL)

  # reactive filtered manifest
  filtered_manifest_react <- reactiveVal(NULL)
  
  # capture selected dcc configuration in a list
  selected_dcc_config_list <- list(
    synapse_asset_view = reactiveVal(NULL),
    manifest_dataset_id = reactiveVal(NULL),
    schema_url = reactiveVal(NULL)
  )
  
  selected_dcc_config <- reactiveVal(NULL)

  # SELECT A DCC  #############################################################

  mod_select_dcc_out <- dfamodules::mod_select_dcc_server(
    id = "select_dcc",
    tenants_config_path = tenants_config_path ,
    access_token = access_token
  )
  
  # CONFIGURE APP ############################################################
  observeEvent(mod_select_dcc_out()$btn_click, {
    
    # update reactiveVals
    # FIXME: redundant (could get rid of selected_dcc_config_list)
    selected_dcc_config_list$synapse_asset_view(
      mod_select_dcc_out()$selected_dcc_config$dcc$synapse_asset_view
    )
    selected_dcc_config_list$manifest_dataset_id(
      mod_select_dcc_out()$selected_dcc_config$dcc$manifest_dataset_id
    )
    selected_dcc_config_list$schema_url(
      mod_select_dcc_out()$selected_dcc_config$dcc$data_model_url
    )
    
    selected_dcc_config(mod_select_dcc_out()$selected_dcc_config)

    # move to dashboard page
    shinydashboard::updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "tab_dashboard"
    )

    # show waiter on button click
    waiter::waiter_show(
      html = shiny::tagList(
        shiny::img(src = "www/sage-loader.svg"),
        shiny::h3("Getting data. This may take a moment.", style = "color:white;")),
      color="#424874"
    )

    # show sidebar menu
    shinyjs::show(selector = ".sidebar-menu")
    
    # check user's access
    # show admin tab if user has ADMIN access to data flow manifest
    manifest_admin_perm <- dfamodules::synapse_access(
      id = selected_dcc_config()$dcc$manifest_dataset_id,
      access = "CHANGE_PERMISSIONS",
      auth = access_token
    )
    
    if (isTRUE(manifest_admin_perm)) {
      shinyjs::show(selector = "a[data-value='tab_administrator']")
    } else {
      shinyjs::hide(selector = "a[data-value='tab_administrator']")
    }
    
    # Check that user has appropriate permissions to use DFA
    # User must have DOWNLOAD access to the DFA manifest.
    manifest_download_perm <- dfamodules::synapse_access(
      id = selected_dcc_config()$dcc$manifest_dataset_id,
      access = "DOWNLOAD",
      auth = access_token
    )
    if (!isTRUE(manifest_download_perm)) {
      shinypop::nx_report_error(
        title = "Permission error",
        message = tagList(
          shiny::p(
            "You don't have download permission to the DFA manifest",
            shiny::a(
              href = paste0(
                "https://www.synapse.org/#!Synapse:",
                selected_dcc_config()$dcc$manifest_dataset_id),
              selected_dcc_config()$dcc$manifest_dataset_id, 
              taget = "_blank"
            )
          ),
          shiny::p("Refresh the app to try again or contact the DCC for help.")
        )
      )
      shinyjs::hide(selector = "#NXReportButton")
      waiter::waiter_hide()
    }
    
    # generate dashboard config file
    dash_config <- dfamodules::generate_dashboard_config(
      dcc_config = selected_dcc_config(),
      base_url = schematic_api_url
    )

    # FIXME: Show error when manifest download fails
    # download manifest
    manifest_obj <- dfamodules::dataset_manifest_download(
      asset_view = selected_dcc_config()$dcc$synapse_asset_view,
      dataset_id = selected_dcc_config()$dcc$manifest_dataset_id,
      access_token = access_token,
      base_url = schematic_api_url
      )
  
    # prep manifest for app
    prepped_manifest <- dfamodules::prep_manifest_dfa(
      manifest = manifest_obj$content,
      config = dash_config
    )

    # update reactiveVals
    original_manifest(prepped_manifest)
    admin_manifest(prepped_manifest)

    dash_config_react(dash_config)

    # FILTER MANIFEST FOR DASH  ###############################################

    filtered_manifest <- dfamodules::mod_datatable_filters_server(
      "datatable_filters_1",
      original_manifest
      )

    # DATASET DASH  ###########################################################

    dfamodules::mod_datatable_dashboard_server(
      "dashboard_1",
      filtered_manifest,
      dash_config_react
      )

    # DATASET DASH VIZ : DISTRIBUTIONS ########################################

    dfamodules::mod_distribution_server(
      id = "distribution_source",
      df = filtered_manifest,
      group_by_var = "source",
      title = NULL,
      x_lab = "Source",
      y_lab = "Number of Datasets",
      fill = "#0d1c38"
    )

    dfamodules::mod_distribution_server(
      id = "distribution_datatype",
      df = filtered_manifest,
      group_by_var = "dataset_type",
      title = NULL,
      x_lab = "Type of dataset",
      y_lab = "Number of Datasets",
      fill = "#0d1c38"
    )

   # hide waiter
    waiter::waiter_hide()

  })
  
  # ADMINISTRATOR  #############################################################
  # SELECT STORAGE PROJECT #####################################################

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

  # SELECT DATASETS  ##########################################################

  mod_dataset_selection_out <- dfamodules::mod_dataset_selection_server(
    id = "dataset_selection_1",
    storage_project_id = selected_project_id,
    asset_view = selected_dcc_config_list$synapse_asset_view,
    access_token = access_token,
    base_url = schematic_api_url
  )

  # COLLECT WIDGET SELECTIONS  ################################################

  mod_administrator_widgets_out <- dfamodules::mod_administrator_widgets_server(
    id = "update_data_flow_status_1"
    )

  # MAKE UPDATES TO MANIFEST  #################################################

  updated_manifest <- reactive({
    dfamodules::apply_administrator_selections(
      dataflow_manifest = admin_manifest(),
      administrator_widget_output = mod_administrator_widgets_out(),
      dataset_selection_module_output = mod_dataset_selection_out()
    )
  })

  # SHOW MANIFEST PREVIEW  ####################################################
  # get names of selected datasets

  selected_rows <- shiny::reactive({
    mod_dataset_selection_out()$id
  })

  dfamodules::mod_manifest_preview_server(
    id = "highlight_datatable_1",
    df = updated_manifest,
    selection = selected_rows,
    df_match_colname = "dataset_id"
  )

  # BUTTON CLICK ACTIONS  #####################################################

  # save updates for submission to a submission manifest reactiveVal
  # prevents user from having to fully submit a manifest to make new selections
  shiny::observeEvent(input$save_update, {
    admin_manifest(updated_manifest())
  })

  # clear updates
  # also clears the submission manifest of any cached updates
  shiny::observeEvent(input$clear_update, {
    admin_manifest(original_manifest())
  })

  # SUBMIT MODEL TO SYNAPSE   #################################################

  # prep manifest for submission
  manifest_submit <- shiny::reactive({
    req(dash_config_react())
    req(updated_manifest())

    dfamodules::prep_manifest_submit(
      manifest = updated_manifest(),
      config = dash_config_react(),
      na_replace = ""
    )
  })

  dfamodules::mod_submit_model_server(
    id = "submit_model_1",
    dfs_manifest = manifest_submit,
    data_type = NULL,
    asset_view = selected_dcc_config_list$synapse_asset_view,
    dataset_id = selected_dcc_config_list$manifest_dataset_id,
    manifest_dir = "./manifest",
    access_token = access_token,
    base_url = schematic_api_url,
    schema_url = selected_dcc_config_list$schema_url
    )
}
