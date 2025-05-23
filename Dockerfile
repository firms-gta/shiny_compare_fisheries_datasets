FROM rocker/r-ver:4.2.3
#FROM rocker/shiny:4.4.0
#FROM rocker/shiny:latest
#FROM rocker/r-ver:4.4.0
#FROM rocker/r-ver:latest

LABEL org.opencontainers.image.authors="julien.barde@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"
#connect this container (GHitHub package) to the repository
LABEL org.opencontainers.image.source https://github.com/firms-gta/shiny_compare_tunaatlas_datasests

# Update and upgrade the system with option -y to tells apt-get to assume the answer to all prompts is yes.
RUN apt update && apt upgrade -y

# Install system libraries of general use with option -y to tells apt-get to assume the answer to all prompts is yes.

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    git \
    libnetcdf-dev \
    curl \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    cmake
   
# general system libraries
# Note: this includes rdf/redland system libraries
RUN apt-get update && apt-get install -y \
    default-jdk \
    fonts-roboto \
    ghostscript \
    hugo \
    less \
    libbz2-dev \
    libglpk-dev \
    libgmp3-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libhunspell-dev \
    libicu-dev \
    liblzma-dev \
    libmagick++-dev \
    libopenmpi-dev \
    libpcre2-dev \
    libxslt1-dev \
    libzmq3-dev \
    lsb-release \
    qpdf \
    texinfo \
    software-properties-common \
    vim \
    wget
    
RUN install2.r --error --skipinstalled --ncpus -1 redland
RUN apt-get install -y \
    libcurl4 \
    libgit2-dev \
    libxslt-dev \
    librdf0 \
    redland-utils \
    rasqal-utils \
    raptor2-utils
    
## update system libraries
RUN apt update && apt upgrade -y && apt clean

# Set the working directory
WORKDIR /root/shiny_compare_tunaatlas_datasests

# Echo the DOI_CSV_HASH for debugging and to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 
# Copy the CSV containing the data to download
# Copy the script downloading the data from the CSV
COPY data/DOI.csv ./data/DOI.csv
# Add files downloaded from Zenodo DOIs => https://docs.docker.com/reference/dockerfile/#add
ADD https://github.com/firms-gta/shiny_compare_fisheries_datasets/raw/refs/heads/main/data/codelist_species.qs ./data/codelist_species.qs
ADD https://raw.githubusercontent.com/firms-gta/shiny_compare_fisheries_datasets/refs/heads/main/data/cl_tuna_rfmos_enriched.csv ./data/cl_tuna_rfmos_enriched.csv
ADD https://raw.githubusercontent.com/firms-gta/shiny_compare_fisheries_datasets/refs/heads/main/data/codelist_unit_2025.csv ./data/codelist_unit_2025.csv
ADD https://zenodo.org/record/5747175/files/global_catch_firms_level0_view.zip ./data/global_catch_firms_level0_view.zip
ADD https://zenodo.org/record/11410529/files/global_nominal_catch_firms_level0_public.csv ./data/global_nominal_catch_firms_level0_public_11410529.csv
ADD https://zenodo.org/record/14184244/files/global_catch_tunaatlasird_level2.qs ./data/global_catch_tunaatlasird_level2_14184244.qs
ADD https://zenodo.org/record/1164128/files/global_catch_tunaatlasird_level2.csv ./data/global_catch_tunaatlasird_level2_1164128.csv
ADD https://zenodo.org/record/11460074/files/global_catch_firms_level0_public.csv ./data/global_catch_firms_level0_public_11460074.csv
RUN cd ./data && ls -la

# Could also try wget -L -O global_catch_firms_level0_public.csv "https://zenodo.org/record/11460074/files/global_catch_firms_level0_public.csv"

#RUN R -e "install.packages(c('here', 'qs', 'dplyr', 'sf', 'futile.logger', 'purrr', 'tibble', 'readr', 'arrow', 'zen4R', 'lubridate', 'stringr', 'downloader', 'parallel'), repos='http://cran.r-project.org')"
# Install R core package dependencies (we might specify the version of renv package)
#RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# FROM ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests-cache AS base
# Set environment variables for renv cache, see doc https://docs.docker.com/build/cache/backends/
# ARG RENV_PATHS_ROOT
# ARG defines a constructor argument called RENV_PATHS_ROOT. Its value is passed from the YAML file. An initial value is set up in case the YAML does not provide one
ARG RENV_PATHS_ROOT=/root/.cache/R/renv
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

# Set environment variables for renv cache
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

# Echo the RENV_PATHS_ROOT for logging
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}"
RUN echo "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}"

# Define the build argument for the hash of renv.lock to stop cache if renv.lock has changed
ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

# Create the renv cache directory
# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

# Install renv package that records the packages used in the shiny app
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
#COPY renv renv

# Set renv cache location: change default location of cache to project folder
# see documentation for Multi-stage builds => https://cran.r-project.org/web/packages/renv/vignettes/docker.html
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE=renv/.cache

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 
#RUN R -e "renv::repair()" 

#FROM ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests-cache

# Run the data update script Downloading the data (cached if DOI.csv did not change).
##RUN Rscript update_data.R 
# Copy the rest of the application code
COPY . .

RUN ls -la
RUN cd renv/library && ls -la
RUN cd data && ls -la

# COPY R/download_and_process_zenodo_data.R ./R/download_and_process_zenodo_data.R
# COPY R/download_data.R ./R/download_data.R
# COPY R/hotfix.R ./R/hotfix.R
# COPY data/cl_nc_areas_simplfied.gpkg ./data/cl_nc_areas_simplfied.gpkg
# Exécuter le script avec sourcing avant l'appel de la fonction
# RUN Rscript -e "source('R/download_and_process_zenodo_data.R'); source('R/download_data.R'); download_and_process_zenodo_data()"
# COPY create_or_load_default_dataset.R ./create_or_load_default_dataset.R
# COPY data/codelist_species.qs ./data/codelist_species.qs
RUN Rscript ./create_or_load_default_dataset.R
RUN cd data && ls -la


#RUN if [ -d "./data" ]; then \
#      find ./data -type f ! \( \
#        -name "whole_group_df.parquet" \
#        -o -name "filters_combinations.parquet" \
#        -o -name "df_distinct_geom_light.csv" \
#        -o -name "default_df.parquet" \
#        -o -name "DOI.csv" \
#        -o -name "gta_dois.parquet" \
#        -o -name "gta.parquet" \
#      \) -delete; \
#    fi && \
#    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


# Create directories for configuration
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/shiny_compare_tunaatlas_datasests', port=3838, host='0.0.0.0')"]
