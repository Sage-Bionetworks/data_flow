#' Prepare a dataframe that has been downloaded from Synapse for the Data Flow App
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#' 
#' @export

prep_manifest_dfa <- function(manifest,
                              config) {
  
  # convert "Not Applicable" to NA
  manifest[ manifest == "Not Applicable" ] <- NA
  
  # convert contribute and dataset to factor
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("drop_down_filter", config),
                                  type = "factor")
  
  # num_items to integer column
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("integer", config),
                                  type = "integer")
  
  # release_scheduled and embargo to date columns
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("date", config),
                                  type = "date")
  
  return(manifest)
}

#' Prepare a dataframe for Synapse submission 
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#' 
#' @export

prep_manifest_submit <- function(manifest,
                                 config) {
  
  # convert columns back to string
  col_names <- c(get_colname_by_type("date", config),
                 get_colname_by_type("drop_down_filter", config),
                 get_colname_by_type("integer", config))
  
  manifest <- convert_column_type(df = manifest,
                                  col_names = col_names,
                                  type = "character")
  
  # convert NA to "Not Applicable"
  manifest[ is.na(manifest) ] <- "Not Applicable"
  
  return(manifest)
}

#' Convert a list to a dataframe
#'
#' @param dfs_status_manifest A data flow status manifest
#' @param dfs_updates Output from mod_update_data_flow_status.R
#' @param selected_datasets_df Output from mod_dataset_selection.R
#' 
#' @export

update_dfs_manifest <- function(dfs_manifest,
                                dfs_updates,
                                selected_datasets_df) {
  
  # remove unchanged attributes from selections
  dfs_updates <- dfs_updates[!unlist(lapply(dfs_updates, is.null))]
  
  # capture column names to update
  col_names <- names(dfs_updates)
  
  # loop over the list of changed attributes
  # for each attribute:
  #   - pull out the original vector
  #   - get the updated entry from the list of attributes
  #   - apply the entry to the selected datasets in dfs manifest
  dfs_manifest[col_names] <- lapply(col_names, function(x) {
    
    # pull out column into a vector
    vec <- dfs_manifest[[x]]
    
    # get entry from updated data flow status attributes list
    entry <- dfs_updates[[x]]
    
    # update vector by index
    manifest_selected_idx <- match(selected_datasets_df$id, dfs_manifest$entityId)
    vec[manifest_selected_idx] <- entry
    
    return(vec)
    
  })
  
  return(dfs_manifest)
}

#' Generate a data flow status manifest skeleton. Fills in the component, contributor, data type, number of items, and dataset name columns.
#' 
#' @param storage_project_list List output from `storage_projects` schematic endpoint
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param calc_num_items TRUE/FALSE. Calculate the number of items in each manifest.
#' 
#' @export

generate_data_flow_manifest_skeleton <- function(storage_project_list,
                                                 asset_view,
                                                 input_token,
                                                 calc_num_items,
                                                 .url = "https://schematic.dnt-dev.sagebase.org/v1/storage/project/manifests") {
  
  # parse storage project list
  storage_project_names <- purrr::map_chr(storage_project_list, 2)
  storage_project_ids <- purrr::map_chr(storage_project_list, 1)
  
  message(paste0("Generating data flow status manifest with ", length(storage_project_ids), " storage project(s)"))
  
  # get manifests for each storage project
  dfs_skeleton_list <- lapply(seq_along(storage_project_ids), function(i) {
    
    storage_project <- storage_project_names[i]
    
    message(glue::glue("Retrieving manifests for {storage_project}"))
    
    manifests <- storage_project_manifests(asset_view,
                                           storage_project_ids[i],
                                           input_token,
                                           .url)
    
    # pull out data from list
    # dataset metadata
    dataset_naming_metadata <- purrr::map(manifests, 1)
    
    # get manifest synid to ID rows with no manifest
    dataset_manifest_metadata <- purrr::map(manifests, 2)
    
    # component metadata
    dataset_component_metadata <- purrr::map(manifests, 3)
    
    # pull together in a dataframe
    data.frame(Component = rep("DataFlow", length(dataset_naming_metadata)),
               contributor = rep(storage_project_names[i], length(dataset_naming_metadata)),
               entityId = purrr::map_chr(dataset_naming_metadata, 1),
               dataset_name = purrr::map_chr(dataset_naming_metadata, 2),
               dataset = purrr::map_chr(dataset_component_metadata, 1),
               manifest_synid = purrr::map_chr(dataset_manifest_metadata, 1))
  })
  
  # combine list into a dataframe
  dfs_manifest <- do.call("rbind", dfs_skeleton_list)
  
  # count rows in each manifest listed
  if (calc_num_items) {
    
    num_items <- sapply(1:nrow(dfs_manifest), function(i) {
      
      contributor <- dfs_manifest[i, "contributor"]
      entity_id <- dfs_manifest[i, "entityId"]

      message(glue::glue("Retrieving manifest: {contributor} {entity_id}"))
      
      # dataset == "" indicates that there is no manifest
      if (dfs_manifest$dataset[i] == "" | dfs_manifest$manifest_synid[i] == "") {
        manifest_nrow <- "Not Applicable"
      } else {
        # download manifest
        manifest <- manifest_download_to_df(asset_view,
                                            dfs_manifest[i, "entityId"],
                                            input_token)
        
        # if no manifest is downloaded, return NA
        # otherwise count rows and return nrow
        manifest_nrow <- ifelse(is.null(manifest), NA, nrow(manifest)) 
      }
      
      return(manifest_nrow)
    })
    # if calc_num_itmes = false, just fill in the column with Not Applicable
  } else {
    num_items <- rep("Not Applicable", nrow(dfs_manifest))
  }
  
  # add to manifest
  dfs_manifest$num_items <- num_items
  
  # add missing columns
  # FIXME: Remove hardcoded column names
  # This function will break if dataflow schema changes
  # Source column names from schema?
  dfs_manifest$release_scheduled <- rep("Not Applicable", nrow(dfs_manifest))
  dfs_manifest$embargo <- rep("Not Applicable", nrow(dfs_manifest))
  dfs_manifest$standard_compliance <- rep(FALSE, nrow(dfs_manifest))
  dfs_manifest$data_portal <- rep(FALSE, nrow(dfs_manifest))
  dfs_manifest$released <- rep(FALSE, nrow(dfs_manifest))
  
  
  ## FIX DATA TYPE FOR NO MANIFEST
  # FIXME: once schematic error is fixed this can be removed
  dfs_manifest$dataset[dfs_manifest$manifest_synid == ""] <- ""
  
  # update empty cells to "Not Applicable"
  dfs_manifest[ dfs_manifest == "" ] <- "Not Applicable"
  
  # remove manifest_synid column
  dfs_manifest <- dfs_manifest[,-grep("manifest_synid", names(dfs_manifest))]
  
  return(dfs_manifest)
}