FROM rocker/r-ver:4.1.2

LABEL maintainer="Loren loren.wolfe@sagebase.org"

# install libxml2
RUN apt-get update && apt-get -y install libxml2-dev

# install necessary R packages
RUN R -e "install.packages('httr'); \
  install.packages('dplyr'); \
  install.packages('jsonlite'); \
  install.packages('glue'); \
  install.packages('purrr'); \
  install.packages('devtools')"

# install dcamodules (required for dataflow) and dataflow from github
RUN R -e "devtools::install_github('Sage-Bionetworks/dcamodules'); \
  devtools::install_github('Sage-Bionetworks/data_flow', ref = 'pkg-installs')"

# copy data_flow repo
COPY . /data_flow/

# source and run update_manifest_script.R
CMD R -e "source('/data_flow/update_manifest_script.R')"