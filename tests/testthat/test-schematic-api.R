# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# TEST CONFIGURATION ####################################################################

# read in global config
global_config <- try(jsonlite::read_json("../../inst/global.json"), silent = TRUE)

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
  sp <- storage_projects(asset_view="syn23643253", # schematic-main all datasets
                         input_token=global_config$schematic_token)
  
  expect_type(sp, "list")
})

test_that("storage_dataset_files returns files", {
  skip_it(ping, global_config)
  sdf <- storage_dataset_files(asset_view = "syn23643253",
                               dataset_id = "syn23643250",
                               input_token=global_config$schematic_token)
  
  expect_type(sdf, "list")
})

test_that("manifest_download returns json", {
  skip_it(ping, global_config)
  md <- manifest_download(input_token=global_config$schematic_token,
                          asset_view="syn23643253",
                          dataset_id="syn34640850",
                          as_json = TRUE)
  
  expect_type(md, "character")
})

# TEST API WRAPPER ######################################################################

test_that("manifest_download_to_df returns a dataframe", {
  skip_it(ping, global_config)
  mdf <- manifest_download_to_df(input_token=Sys.getenv("schematicToken"),
                                asset_view="syn23643253",
                                dataset_id="syn34640850")
  mdf_class <- class(mdf)
  
  expect_equal(mdf_class, "data.frame")
})
