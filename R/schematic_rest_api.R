#############################
## Schematic API Functions ##
#############################

# NOTE: functions mostly copied from data_curator/r/schematic_rest_api.R by @afwillia

## MANIFEST OPERATIONS #########################################################

#' Download a manifest
#'
#' @param url URL to schematic API endpoint
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param input_token Synapse login cookie, PAT, or API key.
#' @param as_json Synapse login cookie, PAT, or API key.
#' 
#' @export

manifest_download <- function(asset_view,
                              dataset_id,
                              input_token,
                              as_json,
                              new_manifest_name = NULL,
                              url="https://schematic.dnt-dev.sagebase.org/v1/manifest/download") {
  
  # set up parameters for httr::get call
  params = list(
    `input_token` = input_token,
    `asset_view` = asset_view,
    `dataset_id` = dataset_id,
    `as_json` = as_json,
    `new_manifest_name` = new_manifest_name
  )
  
  # run GET
  res <- httr::GET(url = url, query = params)
  
  # if as_json is TRUE, update type to match
  type <- ifelse(as_json, "application/json", "text/csv")
  
  # pull out content from request
  manifest <- httr::content(res, as = "text", type = type, encoding = "UTF-8")
  
  return(manifest)
}

#' schematic rest api to generate manifest
#'
#' @param title Name of dataset 
#' @param data_type Type of dataset 
#' @param dataset_id Synapse ID of existing manifest
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param oauth true or false STRING passed to python
#' @param use_annotations true or false STRING passed to python 
#' 
#' @returns a URL to a google sheet
#' @export

manifest_generate <- function(title, 
                              data_type,
                              dataset_id,
                              url="https://schematic.dnt-dev.sagebase.org/v1/manifest/generate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #nolint
                              oauth="true",
                              use_annotations="false") {
  
  req <- httr::GET(url,
                   query = list(
                     schema_url=schema_url,
                     title=title,
                     data_type=data_type,
                     oauth=oauth,
                     use_annotations=use_annotations,
                     dataset_id=dataset_id
                   ))
  
  manifest_url <- httr::content(req)
  manifest_url
}

#' Populate a manifest sheet
#'
#' @param data_type Type of dataset
#' @param title Title of csv
#' @param csv_file Filepath of csv to validate
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld
#' 
#' @export

manifest_populate <- function(data_type, 
                              title, 
                              csv_file,
                              url="https://schematic.dnt-dev.sagebase.org/v1/manifest/populate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld") {
  
  req <- httr::POST(url,
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      title=title),
                    body=list(csv_file=httr::upload_file(csv_file))
  )
  req
  
}

## MODEL OPERATIONS ############################################################

#' schematic rest api to validate metadata
#' 
#' @param data_type Type of dataset
#' @param csv_file Filepath of csv to validate
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' 
#' @returns An empty list() if successfully validated. Or a list of errors.
#' @export

manifest_validate <- function(data_type, 
                              csv_file,
                              url="https://schematic.dnt-dev.sagebase.org/v1/model/validate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld") {
  
  req <- httr::POST(url,
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type),
                    body=list(csv_file=httr::upload_file(csv_file))
  )
  
  annotation_status <- httr::content(req)
  annotation_status
}


#' schematic rest api to submit metadata
#' 
#' @param data_type Type of dataset. Set to None for no validation check.
#' @param dataset_id Synapse ID of existing manifest
#' @param restrict_rules If True, validation suite will only run with in-house validation rule. If False, the Great Expectations suite will be utilized and all rules will be available.
#' @param manifest_record_type Manifest storage type. Options: "--", "table" (default), "entity", "both".
#' @param csv_file Filepath of csv to validate
#' @param input_token Synapse login cookie, PAT, or API key
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' 
#' @returns TRUE if successful upload or validate errors if not.
#' @export

model_submit <- function(data_type, 
                         asset_view,
                         dataset_id,
                         restrict_rules,
                         file_name,
                         input_token,
                         manifest_record_type = "table",
                         url="https://schematic.dnt-dev.sagebase.org/v1/model/submit",
                         schema_url="https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/dev/inst/data_flow_component.jsonld") {

  headers = c(
    `accept` = 'application/json',
    `Content-Type` = 'multipart/form-data'
  )
  
  params = list(
    `schema_url` = schema_url,
    `data_type` = data_type,
    `dataset_id` = dataset_id,
    `manifest_record_type` = manifest_record_type,
    `restrict_rules` = restrict_rules,
    `asset_view` = asset_view,
    `input_token` = input_token
  )
  
  files = list(
    `file_name` = httr::upload_file(file_name)
  )
  
  req <- httr::POST(url = url,
                    httr::add_headers(.headers=headers), 
                    query = params, 
                    body = files, 
                    encode = 'multipart')
  
  manifest_id <- httr::content(req)
  manifest_id
}

## SYNAPSE STORAGE  ############################################################

#' Gets all datasets in folder under a given storage project that the current user has access to.
#' 
#' @param asset_view synapse ID of master file view.
#' @param project_id synapse ID of a storage project.
#' @param input_token synapse PAT
#' @param url URL to schematic API endpoint
#'
#'@export

storage_project_datasets <- function(asset_view,
                                     project_id,
                                     input_token,
                                     url="https://schematic.dnt-dev.sagebase.org/v1/storage/project/datasets") {
  
  req <- httr::GET(url,
                   #add_headers(Authorization=paste0("Bearer ", pat)),
                   query=list(
                     asset_view=asset_view,
                     project_id=project_id,
                     input_token=input_token)
  )
  
  httr::content(req)
}

#' Get all storage projects the current user has access to
#' 
#' @param asset_view synapse ID of master file view.
#' @param input_token synapse PAT
#' @param url URL to schematic API endpoint
#'
#' @export

storage_projects <- function(asset_view,
                             input_token,
                             url="https://schematic.dnt-dev.sagebase.org/v1/storage/projects") {
  
  req <- httr::GET(url,
                   query = list(
                     asset_view=asset_view,
                     input_token=input_token
                   ))
  
  httr::content(req)
}

#' /storage/project/manifests
#'
#' @param asset_view synapse ID of master file view.
#' @param dataset_id synapse ID of a storage dataset.
#' @param input_token synapse PAT
#' @param url URL to schematic API endpoint
#' 
#' @export

storage_project_manifests <- function(asset_view,
                                      project_id,
                                      input_token,
                                      url="https://schematic.dnt-dev.sagebase.org/v1/storage/project/manifests") {
  
  require(httr)
  
  headers = c(
    `accept` = 'application/json'
  )
  
  params = list(
    `input_token` = input_token,
    `project_id` = project_id,
    `asset_view` = asset_view
  )
  
  req <- httr::GET(url = url, httr::add_headers(.headers=headers), query = params)
  
  httr::content(req)
  
}

#' /storage/dataset/files
#'
#' @param asset_view synapse ID of master file view.
#' @param dataset_id synapse ID of a storage dataset.
#' @param input_token synapse PAT
#' @param file_names a list of files with particular names (i.e. Sample_A.txt). If you leave it empty, it will return all dataset files under the dataset ID.
#' @param full_path Boolean. If True return the full path as part of this filename; otherwise return just base filename
#' @param url URL to schematic API endpoint
#' 
#' @export

storage_dataset_files <- function(asset_view,
                                  dataset_id,
                                  input_token,
                                  file_names=list(),
                                  full_path=FALSE, 
                                  url="https://schematic.dnt-dev.sagebase.org/v1/storage/dataset/files") {
  
  req <- httr::GET(url,
                   #add_headers(Authorization=paste0("Bearer ", pat)),
                   query=list(
                     asset_view=asset_view,
                     dataset_id=dataset_id,
                     file_names=file_names,
                     full_path=full_path,
                     input_token=input_token))
  httr::content(req)
  
}