###################################################################
## Functions that build on those defined in schematic_rest_api.R ##
###################################################################

## MANIFEST / DOWNLOAD #############################################################

#' Download a manifest as JSON and parse into a dataframe
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param input_token Synapse login cookie, PAT, or API key.
#' 
#' @export

manifest_download_to_df <- function(asset_view,
                                    dataset_id,
                                    input_token,
                                    base_url) {
  
  # download manifest
  manifest_json <- manifest_download(asset_view = asset_view,
                                     dataset_id = dataset_id,
                                     input_token = input_token,
                                     as_json = TRUE,
                                     new_manifest_name = NULL,
                                     base_url = base_url)
  
  
  # use json lite to parse into dataframe
  content_df <- tryCatch({
    jsonlite::fromJSON(manifest_json)
  },
  error = function(e) {
    message("No manifest available")
    return(NULL)
  })
  
  # if content_df is not NA, modify column names
  if (!is.null(content_df)) {
    names(content_df) <- gsub(" ", ".", names(content_df))
  }
  
  # output df
  return(content_df)
}

#' Call `storage/project/manifests` Schematic endpoint for a given list of storage projects (output from `storage/projects`) and output information as a dataframe
#' 
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API
#' @param verbose T/F for console messages
#' 
#' @export

get_all_manifests <- function(asset_view,
                              input_token,
                              base_url = "https://schematic.dnt-dev.sagebase.org",
                              verbose = FALSE) {
  if (verbose) {
    message(paste0("Getting storage project list for ", asset_view))
  }
  
  # get all storage projects under asset view
  sp <- dataflow::storage_projects(asset_view = config$asset_view,
                                   input_token = token,
                                   base_url = config$api_base_url)
  
  if (verbose) {
    message(paste0("Getting manifests for ", length(sp), " storage project(s)"))
  }
  
  synapse_manifests_list <- lapply(1:length(sp), function(i) {
    
    if (verbose) {
      storage_project <- purrr::map_chr(sp[i], 2)
      
      message(glue::glue("Retrieving manifests for {storage_project}"))
    }
    
    sp_id <- purrr::map_chr(sp[i], 1)
    
    manifest <- dataflow::storage_project_manifests(asset_view = asset_view,
                                                    project_id = sp_id,
                                                    input_token = input_token,
                                                    base_url = base_url)
    # pull out data from list
    # dataset metadata
    dataset_naming_metadata <- purrr::map(manifest, 1)
    
    # get manifest synid to ID rows with no manifest
    dataset_manifest_metadata <- purrr::map(manifest, 2)
    
    # component metadata 
    dataset_component_metadata <- purrr::map(manifest, 3)
    
    # pull together in a dataframe
    data.frame(Component = rep("DataFlow", length(manifest)),
               contributor = rep(purrr::map_chr(sp[i], 2), length(manifest)),
               entityId = purrr::map_chr(dataset_naming_metadata, 1),
               dataset_name = purrr::map_chr(dataset_naming_metadata, 2),
               dataset = purrr::map_chr(dataset_component_metadata, 1))
  })
  
  # return dataframe
  return(do.call("rbind", synapse_manifests_list))
}

#' Call `manifest_download` Schematic endpoint for a given set of manifests and count the number of items in each manifest.
#' 
#' @param  get_all_manifests_out output from `get_all_manifests()`
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API
#' 
#' @export

calculate_items_per_manifest <- function(get_all_manifests_out,
                                         asset_view,
                                         input_token,
                                         base_url) {
  
  
  sapply(1:nrow(get_all_manifests_out), function(i) {
    
    # dataset == "" indicates that there is no manifest
    if (get_all_manifests_out$dataset[i] == "") {
      manifest_nrow <- "Not Applicable"
    } else {
      # download manifest
      manifest <- manifest_download_to_df(asset_view = asset_view,
                                          dataset_id = get_all_manifests_out[i, "entityId"],
                                          input_token = input_token,
                                          base_url = base_url)
      
      # if no manifest is downloaded, return NA
      # otherwise count rows and return nrow
      manifest_nrow <- ifelse(is.null(manifest), "Not Applicable", nrow(manifest)) 
    }
    
    return(manifest_nrow)
  })
}

