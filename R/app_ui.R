#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

app_ui <- function() {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # initialize waiter + use preloader
    waiter::useWaiter(),
    waiter::waiterPreloader(
      
        html = tagList(
          img(src = "www/loading.gif"),
          h4("Retrieving Synapse information...", style = "color:white;")),
        color="#424874"),
    
    # define colors for icons in datatable
    # green check
    tags$style(".fa-check {color:#58A158}"),
    # red x
    tags$style(".fa-xmark {color:#B2242A}"),
    
    # Your application UI logic
    
    # dashboardPage
    shinydashboard::dashboardPage(
      
      # dashboardHeader
      shinydashboard::dashboardHeader(
        title = "Data Flow"
      ),
      
      # dashboardSidebar
      shinydashboard::dashboardSidebar(
        
        #sidebarMenu
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Dashboard", 
                                   tabName = "dataset-dashboard",
                                   icon = icon("dashboard")),
          shinydashboard::menuItem("Administrator", 
                                   tabName = "administrator",
                                   icon = icon("cog"))
          
        )
      ),
      
      #dashboardBody
      shinydashboard::dashboardBody(
        
        # implement dca theme module
        dcamodules::use_dca(theme = "sage"),
        
        # initialize shinyjs
        shinyjs::useShinyjs(),
        
        # dashboardTabItems
        shinydashboard::tabItems(
          
          # dataset view dashboard tab
          shinydashboard::tabItem(tabName = "dataset-dashboard",
                                  
                                  shiny::fluidRow(
                                    uiOutput("filter_module")),
                                  
                                  
                                  shiny::fluidRow(
                                    shinydashboard::box(
                                      width = NULL,
                                      title = "Dashboard",
                                      status = "primary",
                                      collapsible = TRUE,
                                      mod_datatable_dashboard_ui("dashboard_1")
                                    )),
                                  
                                  shiny::fluidRow(
                                    shinydashboard::box(
                                      title = "Distribution of datasets by contributor",
                                      status = "primary",
                                      collapsible = TRUE,
                                      mod_distribution_ui("distribution_contributor")),
                                    shinydashboard::box(
                                      title = "Distribution of datasets by data type",
                                      status = "primary",
                                      collapsible = TRUE,
                                      mod_distribution_ui("distribution_datatype")
                                    )),
                                  
                                  shiny::fluidRow(
                                    shinydashboard::box(
                                    title = "Release status of all datasets by contributor",
                                    status = "primary",
                                    collapsible = TRUE,
                                    mod_stacked_bar_ui("stacked_bar_release_status")),                                   
                                  shinydashboard::box(
                                    title = "Data flow status by release date",
                                    status = "primary",
                                    collapsible = TRUE,
                                    
                                    shiny::uiOutput("select_project_ui"),
                                    
                                    mod_stacked_bar_ui("stacked_runners")))
                                  ),
          
          # Administrator tab
          shinydashboard::tabItem(tabName = "administrator",
                                  
                                  fluidPage(
                                    
                                    mod_select_storage_project_ui("select_storage_project_1"),
                                    
                                    mod_dataset_selection_ui("dataset_selection_1"),
                                    
                                    br(),
                                    
                                    mod_update_data_flow_status_ui("update_data_flow_status_1"),
                                    
                                    shinydashboard::box(
                                      
                                      width = NULL,
                                      
                                      mod_highlight_datatable_ui("highlight_datatable_1"),
                                      
                                      br(),
                                      
                                      actionButton("save_update", "Save Updates"),
                                      actionButton("clear_update", "Clear Updates")
                                      
                                    ),
                                
                                    br(),
                                    
                                    mod_submit_model_ui("submit_model_1"))
                                  
          )

          )
        )
      )
    )
  }

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Data Flow'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    authorization_url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
    return(tags$script(HTML(sprintf(
      "location.replace(\"%s\");",
      authorization_url
    ))))
  } else {
    app_ui()
  }
}