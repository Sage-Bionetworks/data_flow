# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# TEST CONFIGURATION ####################################################################

global_config <- try(jsonlite::read_json("../../inst/global.json"), silent = TRUE)
testing_manifest_path <- "../../inst/testing/synapse_storage_manifest_dataflow.csv"
expected_manifest_colnames <- c("Component", "contributor", "data_portal", "dataset",
                                "dataset_name", "embargo", "entityId", "num_items", 
                                "release_scheduled", "released", "standard_compliance")

# check schematic url
schematic_url <- "http://0.0.0.0:3001/"
ping <- try(httr::GET(schematic_url), silent = TRUE)

# function that skips test if: 
#   - schematic url is unavailable
#   - config is not present
#   - token isn't greater than 100 characters (not a pat)
skip_it <- function(ping,
                    global_config) {
  
  if (inherits(ping, "try-error")) {
    skip(sprintf("schematic server URL unavailable (%s). Is it running locally?", schematic_url))
  } else if (inherits(global_config, "try-error")) {
    skip("global_config file is missing.")
  } else if (nchar(global_config$schematic_token) < 100) {
    skip("config$schematic_token is not a valid PAT (<100 chars)")
  }
}

# TEST API ##############################################################################

test_that("storage_projects returns available projects", {
  skip_it(ping, global_config)
  sp <- storage_projects(asset_view = "syn23643253", # schematic-main all datasets
                         input_token = global_config$schematic_token)
  
  # if api call to storage_project fails there will be a node variable
  # expect node to be null if storage_projects call goes through
  node_null <- is.null(sp$node)
  
  expect_true(node_null)
})

test_that("storage_dataset_files returns some files", {
  skip_it(ping, global_config)
  sdf <- storage_dataset_files(asset_view = "syn23643253",
                               dataset_id = "syn23643250",
                               input_token = global_config$schematic_token)
  
  # if api call to storage_dataset_files fails there will be a node variable
  # expect node to be null if storage_dataset_files call goes through
  
  node_null <- is.null(sdf$node)
  
  expect_true(node_null)
})

test_that("manifest_download returns expected dataframe from JSON", {
  skip_it(ping, global_config)
  md <- manifest_download(input_token = global_config$schematic_token,
                          asset_view = "syn23643253",
                          dataset_id = "syn41850334",
                          as_json = TRUE)
  
  md_df <- jsonlite::fromJSON(md)
  
  expect_equal(names(md_df), expected_manifest_colnames)
})


test_that("model_submit successfully uploads DataFlow manifest to synapse", {
  skip_it(ping, global_config)
  submit <- model_submit(data_type = "DataFlow", 
                         dataset_id = "syn41850334",
                         input_token = global_config$schematic_token, 
                         csv_file = testing_manifest_path,
                         restrict_rules = TRUE,
                         schema_url = global_config$schema_url)
  
  # check that testing manifest synid is returned (indicates that submission completed)
  is_synid <- grepl("syn41850734", submit)
  
  expect_true(is_synid)
})


# TEST API WRAPPER ######################################################################

test_that("manifest_download_to_df returns a dataframe with expected columns", {
  skip_it(ping, global_config)
  mdf <- manifest_download_to_df(input_token = global_config$schematic_token,
                                 asset_view = "syn23643253",
                                 dataset_id = "syn41850334")
  
  expect_equal(names(mdf), expected_manifest_colnames)
})
