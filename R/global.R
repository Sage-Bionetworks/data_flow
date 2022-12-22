# unzip virtual environment, named as ".venv.zip"
if (!file.exists(".venv")) utils::unzip(".venv.zip")

# We get a '126' error (non-executable) if we don't do this:
system("chmod -R +x .venv")

# Activate virtual env
Sys.unsetenv("RETICULATE_PYTHON")

# We get a '126' error (non-executable) if we don't do this:
reticulate::use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)


if (interactive()) {
  options(shiny.port = 3978)
} 

OAUTH_LIST <- projectlive.modules::create_oauth_list("inst/oauth_config.yml")