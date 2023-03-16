# load libraries
library(dataflow)

# set variables
base_url <- "https://schematic-dev.api.sagebionetworks.org/"
secrets <- jsonlite::fromJSON(Sys.getenv("SCHEDULED_JOB_SECRETS"))
input_token <- secrets$pat

# run update manifest functions

# update FAIR demo
tryCatch({
  update_data_flow_manifest(asset_view = "syn50896957",
                            # manifest_dataset_id = "syn50900267",
                            manifest_dataset_id = "syn5090026$",
                            input_token = input_token,
                            base_url = base_url) 
  },
  error=function(e) {
    message("Update to Fair Demo Data (syn50896957) failed")
    message(e)
  }
)


# update INCLUDE
tryCatch({
  update_data_flow_manifest(asset_view = "syn50996463",
                            manifest_dataset_id = "syn51060574",
                            input_token = input_token,
                            base_url = base_url)
  },
  error=function(e) {
    message("Update to INCLUDE (syn50996463) failed")
    message(e)
  }
)

# update HTAN
tryCatch({
  update_data_flow_manifest(asset_view = "syn20446927",
                            manifest_dataset_id = "syn38212343",
                            input_token = input_token,
                            base_url = base_url)
  },
  error=function(e) {
    message("Update to HTAN (syn20446927) failed")
    message(e)
  }
)
