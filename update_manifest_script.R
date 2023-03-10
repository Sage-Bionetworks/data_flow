# load libraries
library(dataflow)

# set variables
# FIXME: How do I put get input_token from docker renviron
asset_view <- "syn50896957"
manifest_id <-"syn50900267"
base_url <- "https://schematic-dev.api.sagebionetworks.org/"
input_token <- Sys.getenv("SYNAPSE_PAT")

# run update manifest function
update_data_flow_manifest(asset_view = asset_view,
                          manifest_dataset_id = manifest_id,
                          input_token = input_token,
                          base_url = base_url) 