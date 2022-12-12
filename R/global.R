# Activate virtual env
Sys.unsetenv("RETICULATE_PYTHON")
# Note, the name of the virtual environment is defined in the GH Actions workflow
venv_name<-".venv"
# We get a '126' error (non-executable) if we don't do this:
system(sprintf("chmod -R +x %s", venv_name))
reticulate::use_virtualenv(file.path(getwd(),venv_name), required = TRUE)


if (interactive()) {
  options(shiny.port = 3978,
          launch.browser = T)
} 

OAUTH_LIST <- projectlive.modules::create_oauth_list("inst/oauth_config.yml")