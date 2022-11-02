#' Generate a data flow status manifest. Fills in the component, contributor, data type, and dataset name columns.
#' 
#' @export

generate_data_flow_manifest <- function(storage_project_id,
                                        asset_view,
                                        input_token,
                                        contributor = NULL,
                                        calc_num_items = FALSE,
                                        testing = FALSE) {
  
  # get all manifests for storage_project
  message(glue::glue("Building data flow status manifest for {storage_project_id}"))
  message("Getting all manifests")
  all_manifests_list <- storage_project_manifests(asset_view, 
                                                  storage_project_id, 
                                                  input_token)
  
  # pull out data from list
  # dataset metadata
  dataset_naming_metadata <- purrr::map(all_manifests_list, 1)
  dataset_ids <- purrr::map_chr(dataset_naming_metadata, 1)
  dataset_names <- purrr::map_chr(dataset_naming_metadata, 2)
  
  # component metadata
  dataset_component_metadata <- purrr::map(all_manifests_list, 3)
  dataset <- purrr::map_chr(dataset_component_metadata, 1)
  dataset <- gsub("^$|^ $", "Not Applicable", dataset)
  
  # calculate number of items
  # pull down each manifest and count each row (1 file / row)
  if (calc_num_items) {
    message("Counting items in each manifest")
    num_items_list <- lapply(dataset_ids, function(id) {
      manifest <- suppressMessages(manifest_download_to_df(asset_view,
                                                           id,
                                                           input_token))
      if (!is.null(manifest)) {
        nrow(manifest)
      } else {
        NA
      }
    })
    
    num_items <- purrr::flatten_int(num_items_list)
    
  } else {
    num_items <- rep("Not Applicable", length(dataset_ids))
  }
  
  # create empty dataframe
  colnames <- c("Component", 
                "contributor",
                "entityId",
                "dataset_name",
                "dataset",
                "num_items",
                "release_scheduled",
                "embargo",
                "standard_compliance",
                "data_portal",
                "released")
  
  df <- data.frame(matrix(nrow = length(dataset_ids), ncol = length(colnames)))
  
  names(df) <- colnames 
  
  # fill in dataframe
  df$Component <- "DataFlow"
  df$dataset_name <- dataset_names
  df$dataset <- dataset
  df$num_items <- num_items
  df$entityId <- dataset_ids
  
  # add contributor
  if (!is.null(contributor)) {
    df$contributor <- rep(contributor, length(dataset_ids))
  }
  
  # if testing = TRUE: fill in w dummy data
  if (testing) {
    df$release_scheduled <- rep("Not Applicable", length(dataset_ids))
    df$embargo <- rep("Not Applicable", length(dataset_ids))
    df$standard_compliance <- rep(FALSE, length(dataset_ids))
    df$data_portal <- rep(FALSE, length(dataset_ids))
    df$released <- rep(FALSE, length(dataset_ids))
  }
  
  return(df)
}

manifest_string_to_date <- function(manifest) {
  
  # convert Not Applicable to NA
  manifest[ manifest == "Not Applicable" ] <- NA
  
  # convert string to date for date cols
  date_cols <- c("release_scheduled", "embargo")
  manifest[date_cols] <- lapply(manifest[,date_cols], as.Date)
  
  return(manifest)
}

#' Convert date columns from date to string
#'
#' @param manifest a data flow status manifest.
#' 
#' @export

manifest_date_to_string <- function(manifest) {
  
  # convert date to string for date cols
  date_cols <- c("release_scheduled", "embargo")
  manifest[date_cols] <- lapply(manifest[,date_cols], as.character)
  
  # convert NA to Not Applicable
  manifest[ is.na(manifest) ] <- "Not Applicable"
  
  return(manifest)
}

#' Rearrange a dataframe. Expected columns are ordered to match `expected_col_names` and moved to the front of the dataframe. Unexpected columns are moved to the back of the dataframe.
#'
#' @param df a dataframe
#' @param expected_col_names a vector of expected column names in the order you would like them to appear.
#' 
#' @export

rearrange_dataframe <- function(df,
                                expected_col_names) {
  
  # capture column names of unexpected columns
  other_column_names <- setdiff(names(df), expected_col_names)
  
  # create separate dataframes
  # order expected columns to match expected_col_names
  exected_df <- df[expected_col_names]
  other_df <- df[other_column_names]
  
  # bind back together
  dplyr::bind_cols(exected_df, other_df)

}
