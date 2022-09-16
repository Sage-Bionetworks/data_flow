# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# check that schematic url is available
schematic_url <- "http://0.0.0.0:3001/"
ping <- try(httr::GET(schematic_url), silent = TRUE)

# function that skips test if schematic url is unavailable
skip_it <- function(skip=ping) {
  if (inherits(ping, "try-error")) skip(sprintf("schematic server URL unavailable (%s). Is it running locally?", schem_url)) #nolint
}

# test schematic api functions

test_that("storage_projects returns available projects", {
  skip_it()
  sp <- storage_projects(asset_view="syn23643253", # schematic-main all datasets
                         input_token=Sys.getenv("schematicToken"))
  
  expect_type(sp, "list")
})

test_that("storage_dataset_files returns files", {
  skip_it()
  sdf <- storage_dataset_files(asset_view = "syn23643253",
                               dataset_id = "syn23643250",
                               input_token=Sys.getenv("schematicToken"))
  
  expect_type(sdf, "list")
})

test_that("manifest_download returns json", {
  skip_it()
  md <- manifest_download(input_token=Sys.getenv("schematicToken"),
                          asset_view="syn23643253",
                          dataset_id="syn34640850",
                          as_json = TRUE)
  
  expect_type(md, "character")
})

# test schematic api wrapper functions

test_that("manifest_download_to_df returns a dataframe", {
  skip_it()
  mdf <- manifest_download_to_df(input_token=Sys.getenv("schematicToken"),
                                asset_view="syn23643253",
                                dataset_id="syn34640850")
  
  mdf_class <- class(mdf)
  
  expect_equal(mdf_class, "data.frame")
})
