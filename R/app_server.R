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
  
  # SET UP 
  
  # manifest
  
  
  # SELECT A DCC  #############################################################
  mod_select_dcc_out <- dfamodules::mod_select_dcc_server("select_dcc",
                                                          dcc_config,
                                                          access_token,
                                                          session)
  
  # GENERATE DASHBOARD ON CLICK  ##############################################
  dash_config_react <- reactive({
    
    req(mod_select_dcc_out())
    
    dfamodules::generate_dashboard_config(
      schema_url = mod_select_dcc_out()$selected_dcc_config$schema_url,
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
      icon = mod_select_dcc_out()$selected_dcc_config$icon,
      na_replace = list(num_items = "No Manifest",
                        scheduled_release_date = "Not Scheduled",
                        dataset_type = "No Manifest"),
      base_url = schematic_api_url)
  })

  
  # reactive data flow status manifest object
  df_manifest_obj_react <- reactive({
    req(mod_select_dcc_out())

  # download data flow manifest
  dfamodules::dataset_manifest_download(
    asset_view = mod_select_dcc_out()$selected_dcc_config$synapse_asset_view,
    dataset_id = mod_select_dcc_out()$selected_dcc_config$manifest_dataset_id,
    access_token = access_token,
    base_url = schematic_api_url)
})
  
  # reactive data flow manifest
  df_manifest_react <- reactive({
    req(df_manifest_obj_react())
    req(dash_config_react())
    
    dfamodules::prep_manifest_dfa(manifest = df_manifest_obj_react()$content,
                                  config = dash_config_react())
  })
  
  # FILTER MANIFEST FOR DASH UI ###########################################################

  # prepare inputs for filter module
  filter_inputs <- shiny::reactive({

    contributor_choices <- unique(df_manifest_react()$contributor)
    dataset_choices <- unique(df_manifest_react()$dataset_type)
    release_daterange_start <- min(df_manifest_react()$scheduled_release_date, na.rm = TRUE)
    release_daterange_end <- max(df_manifest_react()$scheduled_release_date, na.rm = TRUE)
    status_choices <- unique(df_manifest_react()$status)

    list(contributor_choices,
         dataset_choices,
         release_daterange_start,
         release_daterange_end,
         status_choices)
  })

  output$filter_module <- shiny::renderUI({
    filters <- filter_inputs()
    dfamodules::mod_datatable_filters_ui("datatable_filters_1",
                                         contributor_choices = filters[[1]],
                                         dataset_choices = filters[[2]],
                                         release_daterange = c(filters[[3]], filters[[4]]),
                                         status_choices = filters[[5]])
    })

  # FILTER MANIFEST FOR DASH SERVER  ####################################################
  filtered_manifest <- dfamodules::mod_datatable_filters_server("datatable_filters_1",
                                                                df_manifest_react)


  # DATASET DASH  #######################################################################

  dfamodules::mod_datatable_dashboard_server("dashboard_1",
                                             filtered_manifest,
                                             dash_config_react)

  # DATASET DASH VIZ : DISTRIBUTIONS ####################################################

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

  # # PREPARE DATA FOR STACKED BAR PLOTS ##################################################
  # # specifically stacked bar plots that show data flow status grouped by contributor
  #
  # stacked_bar_data <- shiny::reactive({
  #
  #   release_status_data <- filtered_manifest() %>%
  #     dplyr::group_by(contributor) %>%
  #     dplyr::group_by(dataset, .add = TRUE) %>%
  #     dplyr::group_by(data_flow_status, .add = TRUE) %>%
  #     dplyr::tally()
  #
  #   # reorder factors
  #   release_status_data$data_flow_status <- factor(release_status_data$data_flow_status,
  #                                                  levels = c("released", "quarantine (ready for release)", "quarantine", "not scheduled"))
  #
  #   release_status_data
  # })
  #
  # dfamodules::mod_stacked_bar_server(id = "stacked_bar_release_status",
  #                                    df = stacked_bar_data,
  #                                    x_var = "contributor",
  #                                    y_var = "n",
  #                                    fill_var = "data_flow_status",
  #                                    title = NULL,
  #                                    x_lab = "Contributors",
  #                                    y_lab = NULL,
  #                                    colors = c("#085631", "#ffa500", "#a72a1e", "#3d3d3d"),
  #                                    coord_flip = TRUE)
  #
  # # drop down for runners plot
  # output$select_project_ui <- shiny::renderUI({
  #
  #   contributors <- unique(filtered_manifest()$contributor)
  #
  #   shiny::selectInput(inputId = "select_project_input",
  #                      label = NULL,
  #                      choices = contributors,
  #                      selectize = FALSE)
  # })
  #
  # # wrangle data for stacked bar plot (runners)
  #
  # release_data_runners <- shiny::reactive({
  #
  #   shiny::req(input$select_project_input)
  #
  #   release_status_data <- filtered_manifest() %>%
  #     dplyr::filter(!is.na(release_scheduled)) %>%
  #     dplyr::filter(contributor == input$select_project_input) %>%
  #     dplyr::group_by(contributor) %>%
  #     dplyr::group_by(release_scheduled, .add = TRUE) %>%
  #     dplyr::group_by(data_flow_status, .add = TRUE) %>%
  #     dplyr::tally()
  #
  #   release_status_data$data_flow_status <- factor(release_status_data$data_flow_status,
  #                                                  levels = c("released", "quarantine (ready for release)", "quarantine"))
  #
  #   release_status_data
  # })
  #
  #
  # dfamodules::mod_stacked_bar_server(id = "stacked_runners",
  #                                    df = release_data_runners,
  #                                    x_var = "release_scheduled",
  #                                    y_var = "n",
  #                                    fill_var = "data_flow_status",
  #                                    title = NULL,
  #                                    x_lab = "Release Dates",
  #                                    y_lab = NULL,
  #                                    x_line = Sys.Date(),
  #                                    colors = c("#085631", "#ffa500", "#a72a1e"),
  #                                    width = 10,
  #                                    date_breaks = "1 month",
  #                                    coord_flip = FALSE)

  # ADMINISTRATOR  #######################################################################
  rv_manifest <- reactiveVal()
  # reactive value that holds manifest_dfa
  observe({
    rv_manifest(df_manifest_react())
    })

  # STORAGE PROJECT SELECTION

  # have to capture in a reactive or else it will not work in select storage module
  # FIXME: Convert to reactive value?
  reactive_asset_view <- reactive({
    mod_select_dcc_out()$selected_dcc_config$synapse_asset_view
  })

  reactive_manifest_id <- reactive({
    mod_select_dcc_out()$selected_dcc_config$manifest_dataset_id
  })

  reactive_schema_url <- reactive({
    mod_select_dcc_out()$selected_dcc_config$schema_url
  })

  mod_select_storage_project_out <- dfamodules::mod_select_storage_project_server(
    id = "select_storage_project_1",
    asset_view = reactive_asset_view,
    access_token = access_token,
    base_url = schematic_api_url)

  # DATASET SELECTION
  reactive_project_id <- reactive({
    mod_select_storage_project_out()
  })

  dataset_selection <- dfamodules::mod_dataset_selection_server(
    id = "dataset_selection_1",
    storage_project_id = reactive_project_id,
    asset_view = reactive_asset_view,
    access_token = access_token,
    base_url = schematic_api_url
  )

  # UPDATE DATA FLOW STATUS SELECTIONS
  updated_data_flow_status <- dfamodules::mod_update_data_flow_status_server("update_data_flow_status_1")


  # MODIFY MANIFEST
  modified_manifest <- shiny::reactive({
    shiny::req(updated_data_flow_status())

    dfamodules::update_dfs_manifest(dfs_manifest = rv_manifest(),
                                    dfs_updates = updated_data_flow_status(),
                                    selected_datasets_df = dataset_selection())
  })

  # BUTTON CLICK UPDATE MANIFEST
  shiny::observeEvent(input$save_update, {
    rv_manifest(modified_manifest())
  })

  shiny::observeEvent(input$clear_update, {
    rv_manifest(manifest_dfa)
  })

  # PREP MANIFEST FOR SYNAPSE SUBMISSION

  manifest_submit <- shiny::reactive({
    dfamodules::prep_manifest_submit(modified_manifest(),
                                     dash_config_react())
  })

  # get names of selected datasets
  selected_row_names <- shiny::reactive({
    dataset_selection()$id

  })

  dfamodules::mod_highlight_datatable_server("highlight_datatable_1",
                                             manifest_submit,
                                             selected_row_names,
                                             "dataset_id")

  # SUBMIT MODEL TO SYNAPSE
  # make sure to submit using a manifest that has been run through date to string
  dfamodules::mod_submit_model_server(id = "submit_model_1",
                                      dfs_manifest = manifest_submit,
                                      data_type = NULL,
                                      asset_view = reactive_asset_view,
                                      dataset_id = reactive_manifest_id,
                                      manifest_dir = "./manifest",
                                      access_token = access_token,
                                      base_url = schematic_api_url,
                                      schema_url = reactive_schema_url)

}
