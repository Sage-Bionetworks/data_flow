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
#' @param dfs_manifest A data flow status manifest
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
#' @param input_token Synapse PAT
#' @param calc_num_items TRUE/FALSE. Calculate the number of items in each manifest.
#' @param base_url Base URL of schematic API
#' 
#' @export

generate_data_flow_manifest_skeleton <- function(storage_project_list,
                                                 asset_view,
                                                 input_token,
                                                 calc_num_items,
                                                 base_url = "https://schematic.dnt-dev.sagebase.org") {

  message(paste0("Generating data flow status manifest with ", length(storage_project_ids), " storage project(s)"))
  
  # get manifests for each storage project
  dfs_manifest <- get_all_manifests(storage_project_list = storage_project_list,
                                    asset_view = asset_view,
                                    input_token = input_token,
                                    base_url = base_url,
                                    verbose = TRUE)
  
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

#' Check synapse for updates to data flow status manifest
#' 
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param manifest_dataset_id Dataset ID for data flow status manifest to be updated
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)
#' 
#' @export

update_data_flow_manifest <- function(asset_view,
                                      manifest_dataset_id,
                                      input_token,
                                      base_url) {
  
  print(paste0("Checking asset view ", asset_view, " for updates"))
  print(paste0("Getting data flow status manifest"))
  # get current data flow manifest
  dfs_manifest <- dataflow::manifest_download_to_df(asset_view = asset_view,
                                                    dataset_id = manifest_dataset_id,
                                                    base_url = base_url,
                                                    input_token = input_token)
  
  
  # get all manifests for each storage project
  print("Getting all manifests")
  synapse_manifests <- get_all_manifests(asset_view = asset_view,
                                         input_token = input_token,
                                         base_url = base_url,
                                         verbose = FALSE)
  
  print("Comparing data flow status manifest to current manifest list")
  # compare recent pull of all manifests to data flow manifest
  missing_datasets_idx <- !synapse_manifests$entityId %in% dfs_manifest$entityId
  missing_datasets <- synapse_manifests[ missing_datasets_idx,]
  
  # if there are missing datasets calculate number of items for each dataset and add in missing information
  if (nrow(missing_datasets) > 0) {
    
    print("New datasets found. Updating data flow status manifest")
    
    num_items <- calculate_items_per_manifest(get_all_manifests_out = missing_datasets,
                                              asset_view = asset_view,
                                              input_token = input_token,
                                              base_url = base_url)
    
    # fill dfs manifest rows for missing datasets
    # FIXME: Remove hardcoded column names
    # This function will break if dataflow schema changes
    # Source column names from schema?
    missing_datasets$release_scheduled <- rep("Not Applicable", nrow(missing_datasets))
    missing_datasets$embargo <- rep("Not Applicable", nrow(missing_datasets))
    missing_datasets$standard_compliance <- rep(FALSE, nrow(missing_datasets))
    missing_datasets$data_portal <- rep(FALSE, nrow(missing_datasets))
    missing_datasets$released <- rep(FALSE, nrow(missing_datasets))
    missing_datasets$num_items <- num_items
    
    # remove uuid
    uuid_idx <- grep("Uuid", names(dfs_manifest))
    dfs_manifest <- dfs_manifest[,-uuid_idx]
    
    # tack on missing datasets to end of dfs_status_manifest
    dfs_manifest <- rbind(dfs_manifest, missing_datasets)
    
    # sort dataframe so that contributor is grouped
    dfs_manifest <- dfs_manifest %>% 
      dplyr::group_by(contributor) %>% 
      dplyr::arrange(contributor)
    
    # convert manifest to json
    dfs_manifest_json <- jsonlite::toJSON(dfs_manifest)
    
    # submit to synapse
    # data_type = NULL until LP can fix model/submit endpoint for large manifests
    # If no datatype indicated no validation will be done
    message("submitting manifest to Synapse")
    
    model_submit(data_type = NULL, 
                 asset_view = asset_view,
                 dataset_id = manifest_dataset_id,
                 json_str = dfs_manifest_json,
                 restrict_rules = TRUE,
                 input_token = input_token,
                 manifest_record_type = "table",
                 base_url = base_url,
                 schema_url = "https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/main/inst/data_flow_component.jsonld",
                 use_schema_label = TRUE)
  } else {
    print("No updates to manifest required at this time")
  }
}