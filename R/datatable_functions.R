###############################################
## Functions that create stylized datatables ##
###############################################

#' Create a dashboard style datatable
#'
#' @param df A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#' 
#' @export
#' 

create_dashboard <- function(df,
                             config) {
  
  prepped_df <- prep_df_for_dash(df, config)
  
  style_dashboard(prepped_df, config)
}

#' Add custom styling to dashboard based on contents of config
#'
#' @param df A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#' 
#' @export
#' 

style_dashboard <- function(prepped_dataframe,
                            config) {
  
  # get icon col index
  icon_idx <- match(get_colname_by_type(config, "icon"), names(prepped_dataframe))
  
  # define center styling for icon columns
  center_list <- list(className = 'dt-center', targets = icon_idx)
  
  # index of past_due column created in df prep step
  past_due_idx <- match("past_due", names(prepped_dataframe))
  
  # capture icon and center styling in single variable
  defs <- list(
    center_list,
    # hide past_due column
    list(targets = past_due_idx, visible = FALSE))
  
  # define styling for na_replacement
  na_replace_defs <- get_na_replace_def(config,
                                        prepped_dataframe)
  
  defs <- append(defs, na_replace_defs)
  
  # create datatable
  dt <- DT::datatable(prepped_dataframe,
                      escape = FALSE, 
                      selection = "none",
                      filter = "top",
                      options = list(scrollX = TRUE,
                                     scrollY = 800,
                                     bPaginate = FALSE,
                                     columnDefs = defs))
  
  # FIXME: this is still hardcoded
  if (as.logical(toupper(config$release_scheduled$color_past_due))) {
    
    dt <- DT::formatStyle(table = dt,
                          config$release_scheduled$col_name, "past_due",
                          backgroundColor = DT::styleEqual("pd", "#FF9CA0"))
  }
  
  dt
}

#' Prepare a dataframe for a dashboard style datatable
#'
#' @param df A dataframe with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#' 
#' @export
#' 

prep_df_for_dash <- function(df,
                             config) {
  
  # create past_due column for highlighting release_scheduled
  today <- Sys.Date()
  today <- lubridate::floor_date(today, unit = "month")
  dates <- lubridate::floor_date(df[,"release_scheduled"], unit = "month")
  df$past_due <- ifelse(dates < today, "pd", 
                        ifelse(dates == today, "t", NA))
  
  # convert TRUE / FALSE to icon html
  icon_cols <- get_colname_by_type(config, type = "icon")
  df[icon_cols] <- lapply(df[,icon_cols], true_false_icon)
  
  # convert certain columns to factors 
  # enables drop down selection style filtering for column
  factor_cols <- get_colname_by_type(config, type = "drop_down_filter")
  df[factor_cols] <- lapply(df[,factor_cols], factor)
  
  return(df)
}


## HELPERS ##############################################################################

#' Convert a vector of TRUE/FALSE to icon html
#'
#' @param vec A vector of TRUE/FALSE
#' 
#' @export

true_false_icon <- function(vec) {
  
  # if true assign checkmark icon
  # if false assign x icon
  true_icon <- as.character(icon("check", lib = "font-awesome"))
  false_icon <- as.character(icon("times", lib = "font-awesome"))
  
  ifelse(vec == TRUE, true_icon, false_icon)
}

#' Parse config to get columns types
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param type column type as described in datatable_dashboard_config.json
#' 
#' @export

get_colname_by_type <- function(config,
                                type = c("icon", "drop_down_fliter")) {
  
  # get all elements with 'type'
  type_list <- purrr::map(config, "type")
  types <- unlist(type_list)
  
  #subset types (a names list) where the entry  == type
  col_names <- names(types[types == type])
  
  return(col_names)

}

#' Parse config to get display column names for dashboard
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' 
#' @export

get_renamed_colnames <- function(config) {
  # create a vector of display column names
  col_names<- purrr::map(config, "col_name") 
  purrr::flatten_chr(col_names)
}

#' Parse config to get columns with na_replace specified
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' 
#' @export

get_na_replace_colnames <- function(config) {
  # create a vector of display column names
  col_names <- purrr::map(config, "na_replace") 
  names(purrr::flatten(col_names))
}

#' Parse config to get na replacement definitions with custom javascript. Outputs a list.
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param prepped_dataframe dataframe output by `prep_df_for_dash()`
#' 
#' @export

get_na_replace_def <- function(config,
                               prepped_dataframe) {
  
  # get na_replace columns
  na_replace_cols <- get_na_replace_colnames(config)
  
  # get colname index in prepped dataframe
  na_replace_idx <- match(na_replace_cols, names(prepped_dataframe))
  
  defs <- lapply(seq_along(na_replace_cols), function(i) {
    colname <- na_replace_cols[i]
    replacement <- config[[colname]]$na_replace
    dt_replace_na(na_replace_idx[i],
                  replacement)
  })
  
  return(defs)
}

#' NA replacement - datatable custom JS
#'
#' @param col_index target columns index
#' @param na_replacement text to replace NA
#' 
#' @export
#' 

dt_replace_na <- function(col_index,
                          na_replacement) {
  
  list(targets = col_index, 
       render = DT::JS(
         "function(data, type, row, meta) {",
         glue::glue("return data === null ? '{na_replacement}' : data;"),
         "}"
         ))
}