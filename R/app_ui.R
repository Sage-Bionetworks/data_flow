#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'

app_ui <- function() {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    tags$head(
      tags$link(
        rel="shortcut icon", 
        href=FAVICON_URL
        )
      ),

    # initialize shinyjs
    shinyjs::useShinyjs(),
    
    # Add nx report for error messages
    shinypop::use_notiflix_report(
      width = "500px",
      messageMaxLength = 10000,
      titleMaxLength = 100
    ),

    # define colors for icons in datatable
    # green check
    shiny::tags$style(".fa-check {color:#58A158}"),
    # red x
    shiny::tags$style(".fa-xmark {color:#B2242A}"),

    # Your application UI logic

    # dashboardPage
    shinydashboard::dashboardPage(

      # dashboardHeader
      shinydashboard::dashboardHeader(
        title = "Data Flow" 
      ),

      # dashboardSidebar
      shinydashboard::dashboardSidebar(
        # sidebarMenu
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem("Select a DCC",
            tabName = "tab_select_dcc",
            icon = icon("rocket")
          ),
          shinydashboard::menuItem("Dashboard",
            tabName = "tab_dashboard",
            icon = icon("dashboard")
          ),
          shinydashboard::menuItem("Administrator",
            tabName = "tab_administrator",
            icon = icon("pencil")
          )
        )
      ),

      # dashboardBody
      shinydashboard::dashboardBody(

        # initialize waiter + use preloader
        waiter::use_waiter(),
        waiter::waiter_preloader(
          html = shiny::tagList(
            shiny::img(src = "www/sage-loader.svg"),
            shiny::h4("Retrieving Synapse information...", style = "color:white;")
          ),
          color = "#424874"
        ),

        # implement dca theme module
        dcamodules::use_dca(theme = "sage"),

        # dashboardTabItems
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "tab_select_dcc",
            shiny::fluidRow(
              dfamodules::mod_select_dcc_ui(
                "select_dcc"
              )
            )
          ),

          # dataset view dashboard tab
          shinydashboard::tabItem(
            tabName = "tab_dashboard",
            shiny::fluidRow(
              dfamodules::mod_datatable_filters_ui("datatable_filters_1")
            ),
            shiny::fluidRow(
              shinydashboard::box(
                width = NULL,
                title = "Dashboard",
                status = "primary",
                collapsible = TRUE,
                dfamodules::mod_datatable_dashboard_ui("dashboard_1")
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Distribution of datasets by contributor",
                status = "primary",
                collapsible = TRUE,
                dfamodules::mod_distribution_ui("distribution_source")
              ),
              shinydashboard::box(
                title = "Distribution of datasets by data type",
                status = "primary",
                collapsible = TRUE,
                dfamodules::mod_distribution_ui("distribution_datatype")
              )
            )
          ),

          # Administrator tab
          shinydashboard::tabItem(
            tabName = "tab_administrator",
            shiny::fluidPage(
              dfamodules::mod_select_storage_project_ui("select_storage_project_1"),
              dfamodules::mod_dataset_selection_ui("dataset_selection_1"),
              br(),
              dfamodules::mod_administrator_widgets_ui("update_data_flow_status_1"),
              shinydashboard::box(
                width = NULL,
                dfamodules::mod_manifest_preview_ui("highlight_datatable_1"),
                br(),
                shiny::actionButton("save_update", "Save Updates"),
                shiny::actionButton("clear_update", "Clear Updates")
              ),
              shiny::br(),
              dfamodules::mod_submit_model_ui("submit_model_1")
            )
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(resources_path = app_sys("app/www")),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Data Flow"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

uiFunc <- function(req) {
  if (!has_auth_code(shiny::parseQueryString(req$QUERY_STRING))) {
    authorization_url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
    return(shiny::tags$script(shiny::HTML(sprintf(
      "location.replace(\"%s\");",
      authorization_url
    ))))
  } else {
    app_ui()
  }
}
