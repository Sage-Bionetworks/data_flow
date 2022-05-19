#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    
    # dashboardPage
    shinydashboard::dashboardPage(
      skin = "purple",
      
      # dashboardHeader
      shinydashboard::dashboardHeader(
        title = "Release Administrator"
      ),
      
      # dashboardSidebar
      shinydashboard::dashboardSidebar(
        
        #sidebarMenu
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Administrative", 
                                   tabName = "administrate",
                                   icon = icon("cog")),
          shinydashboard::menuItem("Project Overview Dashboard", 
                                   tabName = "overview-dashboard",
                                   icon = icon("dashboard")),
          shinydashboard::menuItem("Dataset Level Dashboard", 
                                   tabName = "dataset-dashboard",
                                   icon = icon("dashboard"))
          
        )
      ),
      
      #dashboardBody
      shinydashboard::dashboardBody(
        
        # initialize shinyjs
        shinyjs::useShinyjs(),
        
        # dashboardTabItems
        shinydashboard::tabItems(
          
          # administrate tab
          shinydashboard::tabItem(tabName = "administrate",
                                  # dataset and file selection
                                  fluidPage(
                                    waiter::use_waiter(),
                                    waiter::waiter_preloader(),
                                    mod_dataset_selection_ui("dataset_selection_ui_1"),
                                    mod_file_selection_ui("file_selection_ui_1"),
                                    br(),
                                    mod_set_release_status_ui("set_release_status_ui_1"),
                                    DT::DTOutput("modified_manifest"))
                                  ),
          
          # dashboard tab
          shinydashboard::tabItem(tabName = "overview-dashboard",
                                  h2("Coming Soon: Overview Dashbaord!")
                                  ),
          # dashboard tab
          shinydashboard::tabItem(tabName = "dataset-dashboard",
                                  h2("Coming Soon: Dataset Dashbaord!")
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
      app_title = 'release_administratorUI'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}