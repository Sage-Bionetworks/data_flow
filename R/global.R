## Set Up Virtual Environment
# ShinyAppys has a limit of 7000 files which this app' grossly exceeds
# due to its Python dependencies.  To get around the limit we zip up
# the virtual environment before deployment and unzip it here.

# unzip virtual environment, named as ".venv.zip"
if (!file.exists(".venv")) utils::unzip(".venv.zip")

# We get a '126' error (non-executable) if we don't do this:
system("chmod -R +x .venv")

# Activate virtual env
Sys.unsetenv("RETICULATE_PYTHON")
reticulate::use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)

# set shiny port
if (interactive()) {
  options(shiny.port = 8100)
}

message(Sys.getenv())

OAUTH_LIST <- projectlive.modules::create_oauth_list("oauth_config.yml")
