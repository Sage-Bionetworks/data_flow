# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# SETUP #################################################################################
# source global vars
source("./testing_variables.R")

# local vars
testing_manifest_path <- "../../inst/testing/synapse_storage_manifest_dataflow.csv"
global_config <- try(jsonlite::read_json("../../inst/global.json"), silent = TRUE)

# CHECK API URL #########################################################################
schematic_url <- "http://0.0.0.0:3001/"
ping <- try(httr::GET(schematic_url), silent = TRUE)

# function that skips test if schematic url is unavailable

ping <- try(httr::GET(schem_url), silent = TRUE)
skip_it <- function(skip=ping) {
  if (inherits(ping, "try-error")) skip(sprintf("schematic server URL unavailable (%s). Is it running locally?", schem_url)) #nolint
}

# TEST API ##############################################################################

test_that("storage_projects returns available projects", {
  skip_it()
  sp <- storage_projects(asset_view = asset_view, # schematic-main all datasets
                         input_token = input_token)
  
  # if api call to storage_project fails there will be a node variable
  # expect node to be null if storage_projects call goes through
  node_null <- is.null(sp$node)
  
  expect_true(node_null)
})

test_that("storage_dataset_files returns some files", {
  skip_it()
  sdf <- storage_dataset_files(asset_view = asset_view,
                               dataset_id = dataset_id,
                               input_token = input_token)
  
  # if api call to storage_dataset_files fails there will be a node variable
  # expect node to be null if storage_dataset_files call goes through
  
  node_null <- is.null(sdf$node)
  
  expect_true(node_null)
})

test_that("manifest_download returns expected dataframe from JSON", {
  skip_it()
  md <- manifest_download(input_token = input_token,
                          asset_view = asset_view,
                          dataset_id = dataset_id,
                          as_json = TRUE)
  
  md_df <- jsonlite::fromJSON(md)
  
  expect_equal(names(md_df), expected_manifest_colnames)
})


test_that("model_submit successfully uploads DataFlow manifest to synapse", {
  skip_it()
  submit <- model_submit(data_type = "DataFlow", 
                         dataset_id = dataset_id,
                         input_token = input_token, 
                         csv_file = testing_manifest_path,
                         restrict_rules = TRUE,
                         schema_url = schema_url)
  
  # check that testing manifest synid is returned (indicates that submission completed)
  is_synid <- grepl("syn41850734", submit)
  
  expect_true(is_synid)
})


# TEST API WRAPPER ######################################################################

test_that("manifest_download_to_df returns a dataframe with expected columns", {
  skip_it()
  mdf <- manifest_download_to_df(input_token = input_token,
                                 asset_view = asset_view,
                                 dataset_id = dataset_id)
  
  expect_equal(names(mdf), expected_manifest_colnames)
})
