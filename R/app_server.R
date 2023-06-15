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
  
  # generate dashboard configuration from dataFlow schema
  dash_config <- dfamodules::generate_dashboard_config(schema_url = global_config$schema_url,
                                                       display_names = list(contributor = "Contributor",
                                                                            entityId = "Synapse ID",
                                                                            dataset = "Data Type",
                                                                            dataset_name = "Dataset Folder Name",
                                                                            num_items = "Number of Items in Manifest",
                                                                            release_scheduled = "Release Date",
                                                                            embargo = "Embargo",
                                                                            standard_compliance = "QC Checks",
                                                                            released = "Released",
                                                                            data_portal = "Data Portal",
                                                                            Component = NA),
                                                       icon = TRUE,
                                                       na_replace = list(num_items = "No Manifest",
                                                                         release_scheduled = "Not Scheduled",
                                                                         embargo = "No Embargo",
                                                                         dataset = "No Manifest"),
                                                       base_url = schematic_api_url)
  
  # download data flow status manifest
  manifest_obj <- dfamodules::dataset_manifest_download(asset_view = global_config$asset_view,
                                                        dataset_id = global_config$manifest_dataset_id,
                                                        access_token = access_token,
                                                        base_url = schematic_api_url)
  
  manifest_dfa <- dfamodules::prep_manifest_dfa(manifest = manifest_obj$content,
                                                config = dash_config)
  
  # PREPARE MANIFEST FOR DASH ###########################################################
  
  # add status to manifest
  manifest_w_status <- shiny::reactive({
    
    # add some columns to manifest to make logic easier
    manifest <- manifest_dfa %>%
      dplyr::mutate(scheduled = !is.na(release_scheduled),
                    no_embargo = is.na(embargo) || embargo < Sys.Date(),
                    past_due = !is.na(release_scheduled) && release_scheduled < Sys.Date())
    
    # generate status variable based on some logic that defines various data flow statuses
    status <- sapply(1:nrow(manifest), function(i) {
      row <- manifest[i, ]
      
      if (row$scheduled == FALSE) {
        status <- "not scheduled"
      } else if (row$no_embargo == FALSE || row$standard_compliance == FALSE) {
        status <- "quarantine"
      } else if (row$no_embargo == TRUE & row$standard_compliance == TRUE & row$released == FALSE) {
        status <- "quarantine (ready for release)"
      } else if (row$released == TRUE) {
        status <- "released"
      } else {
        NA
      }
      
      status
    })
    
    # add status to manifest
    manifest$data_flow_status <- status
    
    manifest
  })
  
  # FILTER MANIFEST FOR DASH UI ###########################################################
  
  # prepare inputs for filter module
  filter_inputs <- shiny::reactive({
    
    contributor_choices <- unique(manifest_w_status()$contributor)
    dataset_choices <- unique(manifest_w_status()$dataset)
    release_daterange_start <- min(manifest_w_status()$release_scheduled, na.rm = TRUE)
    release_daterange_end <- max(manifest_w_status()$release_scheduled, na.rm = TRUE)
    status_choices <- unique(manifest_w_status()$data_flow_status)
    
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
                                                                manifest_w_status)
  
  
  # DATASET DASH  #######################################################################
  
  dfamodules::mod_datatable_dashboard_server("dashboard_1",
                                             filtered_manifest,
                                             dash_config)
  
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
                                      group_by_var = "dataset",
                                      title = NULL,
                                      x_lab = "Type of dataset",
                                      y_lab = "Number of Datasets",
                                      fill = "#0d1c38")
  
  # PREPARE DATA FOR STACKED BAR PLOTS ##################################################
  # specifically stacked bar plots that show data flow status grouped by contributor
  
  stacked_bar_data <- shiny::reactive({
    
    release_status_data <- filtered_manifest() %>%
      dplyr::group_by(contributor) %>%
      dplyr::group_by(dataset, .add = TRUE) %>%
      dplyr::group_by(data_flow_status, .add = TRUE) %>%
      dplyr::tally()
    
    # reorder factors
    release_status_data$data_flow_status <- factor(release_status_data$data_flow_status, 
                                                   levels = c("released", "quarantine (ready for release)", "quarantine", "not scheduled"))
    
    release_status_data
  })
  
  dfamodules::mod_stacked_bar_server(id = "stacked_bar_release_status",
                                     df = stacked_bar_data,
                                     x_var = "contributor",
                                     y_var = "n",
                                     fill_var = "data_flow_status",
                                     title = NULL,
                                     x_lab = "Contributors",
                                     y_lab = NULL,
                                     colors = c("#085631", "#ffa500", "#a72a1e", "#3d3d3d"),
                                     coord_flip = TRUE)
  
  # drop down for runners plot
  output$select_project_ui <- shiny::renderUI({
    
    contributors <- unique(filtered_manifest()$contributor)
    
    shiny::selectInput(inputId = "select_project_input",
                       label = NULL,
                       choices = contributors,
                       selectize = FALSE)
  })
  
  # wrangle data for stacked bar plot (runners)
  
  release_data_runners <- shiny::reactive({
    
    shiny::req(input$select_project_input)
    
    release_status_data <- filtered_manifest() %>%
      dplyr::filter(!is.na(release_scheduled)) %>%
      dplyr::filter(contributor == input$select_project_input) %>%
      dplyr::group_by(contributor) %>%
      dplyr::group_by(release_scheduled, .add = TRUE) %>%
      dplyr::group_by(data_flow_status, .add = TRUE) %>%
      dplyr::tally()
    
    release_status_data$data_flow_status <- factor(release_status_data$data_flow_status, 
                                                   levels = c("released", "quarantine (ready for release)", "quarantine"))
    
    release_status_data
  })
  
  
  dfamodules::mod_stacked_bar_server(id = "stacked_runners",
                                     df = release_data_runners,
                                     x_var = "release_scheduled",
                                     y_var = "n",
                                     fill_var = "data_flow_status",
                                     title = NULL,
                                     x_lab = "Release Dates",
                                     y_lab = NULL,
                                     x_line = Sys.Date(),
                                     colors = c("#085631", "#ffa500", "#a72a1e"),
                                     width = 10,
                                     date_breaks = "1 month",
                                     coord_flip = FALSE)
  
  # ADMINISTRATOR  #######################################################################
  
  # reactive value that holds manifest_dfa 
  rv_manifest <- shiny::reactiveVal(manifest_dfa)
  
  # STORAGE PROJECT SELECTION
  
  select_storage_project_out <- dfamodules::mod_select_storage_project_server(id = "select_storage_project_1",
                                                                              asset_view = global_config$asset_view,
                                                                              access_token = access_token,
                                                                              base_url = schematic_api_url)
  
  # DATASET SELECTION
  
  dataset_selection <- dfamodules::mod_dataset_selection_server(id = "dataset_selection_1",
                                                                storage_project_df = select_storage_project_out,
                                                                asset_view = global_config$asset_view,
                                                                access_token = access_token,
                                                                base_url = schematic_api_url)
  
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
                                     dash_config)
  })
  
  # DISPLAY MANIFEST
  admin_display_manifest <- shiny::reactive({
    
    # rearrange manifest so it's more readable
    manifest <- dfamodules::rearrange_dataframe(manifest_submit(),
                                                names(dash_config))
    
    # make columns factors
    factor_cols <- dfamodules::get_colname_by_type(dash_config, type = "drop_down_filter")
    manifest[factor_cols] <- lapply(manifest[,factor_cols], factor)
    
    # return
    manifest
  })
  
  # get names of selected datasets
  selected_row_names <- shiny::reactive({
    dataset_selection()$id
    
  })
  
  dfamodules::mod_highlight_datatable_server("highlight_datatable_1",
                                             admin_display_manifest,
                                             selected_row_names,
                                             "entityId")
  
  # SUBMIT MODEL TO SYNAPSE
  # make sure to submit using a manifest that has been run through date to string
  dfamodules::mod_submit_model_server(id = "submit_model_1",
                                      dfs_manifest = manifest_submit,
                                      data_type = NULL,
                                      asset_view = global_config$asset_view,
                                      dataset_id = global_config$manifest_dataset_id,
                                      manifest_dir = "./manifest",
                                      access_token = access_token,
                                      base_url = schematic_api_url,
                                      schema_url = global_config$schema_url)
  
}
