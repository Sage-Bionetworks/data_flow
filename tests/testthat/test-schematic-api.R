# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# VARIABLES #############################################################################
# schematic-main/TestDFA
asset_view <- "syn23643253"
storage_project_id <- "syn23643250"
dataset_id <- "syn41850334"
input_token <- Sys.getenv("SYNAPSE_PAT")
schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/43fe7b8d1b649ff9debc5501ef62b3cf108f3eec/inst/data_flow_component.jsonld"
testing_manifest_path <- "../../inst/testing/synapse_storage_manifest_dataflow.csv"

# TEST API ##############################################################################

test_that("storage_projects returns expected projects", {
  sp <- storage_projects(asset_view = asset_view, # schematic-main all datasets
                         input_token = input_token)
  
  # if api call to storage_project is successful the storage project `schematic - main` should be present
  schematic_main_present <- tryCatch({any(grepl("schematic - main", purrr::map_chr(sp, 2)))},
                                     error = function(e) {
                                       return(FALSE)
                                     })
  
  expect_true(schematic_main_present)
})

test_that("storage_dataset_files returns files", {
  sdf <- storage_dataset_files(asset_view = asset_view,
                               dataset_id = storage_project_id,
                               input_token = input_token)
  
  # if api call to storage_dataset_files is successful there will be files present
  files_returned <- tryCatch({length(purrr::map_chr(sdf, 2)) > 0},
                             error = function(e) {
                               return(FALSE)
                             })
  
  expect_true(files_returned)
})

test_that("manifest_download returns expected dataframe from JSON", {
  md <- manifest_download(input_token = input_token,
                          asset_view = asset_view,
                          dataset_id = dataset_id,
                          as_json = TRUE)
  
  md_df <- try(jsonlite::fromJSON(md), silent = TRUE)
  
  expect_true(is.data.frame(md_df))
})

#FIXME: Model submit relies on schematic config file. If DFA is set to look at HTAN
#       schematic config must have fileview set to look at HTAN for submission. Until
#       this issue is resolved cannot test

# test_that("model_submit successfully uploads DataFlow manifest to synapse", {
#   skip_it()
#   submit <- model_submit(data_type = "DataFlow", 
#                          dataset_id = dataset_id,
#                          input_token = input_token, 
#                          csv_file = testing_manifest_path,
#                          restrict_rules = TRUE,
#                          schema_url = schema_url)
#   
#   # check that testing manifest synid is returned (indicates that submission completed)
#   is_synid <- grepl(manifest_id, submit)
#   
#   expect_true(is_synid)
# })


# TEST API WRAPPER ######################################################################

test_that("manifest_download_to_df returns a dataframe with expected columns", {
  mdf <- manifest_download_to_df(input_token = input_token,
                                 asset_view = asset_view,
                                 dataset_id = dataset_id)
  
  expect_true(is.data.frame(mdf))
})
