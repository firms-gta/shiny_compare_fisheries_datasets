FROM rocker/shiny:4.2.3
#FROM rocker/shiny:latest
#FROM rocker/r-ver:4.4.0
#FROM rocker/r-ver:latest

LABEL org.opencontainers.image.authors="julien.barde@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"
#connect this container (GHitHub package) to the repository
LABEL org.opencontainers.image.source https://github.com/firms-gta/shiny_compare_tunaatlas_datasests

# Update and upgrade the system with option -y to tells apt-get to assume the answer to all prompts is yes.
RUN apt update && apt upgrade -y

# RUN apt install -y \
#    #sudo \
#    #pandoc \
#    pandoc-citeproc \
#    libssl-dev \
#   #libcurl4-openssl-dev \
#    #libxml2-dev \
#    libudunits2-dev \
#    libproj-dev \
#    libgeos-dev \
#    libgdal-dev \
#    libv8-dev \
#    libsodium-dev \
#    libsecret-1-dev \
#    git \
#    libnetcdf-dev \
#    curl \
#    qgis \
#    qgis-plugin-grass \
#    libprotobuf-dev \
# RUN apt-get update && apt-get install -y \
#    sudo \
#    pandoc \
#    pandoc-citeproc \
#    libssl-dev \
#    libcurl4-gnutls-dev \
#    libxml2-dev \
#    libudunits2-dev \
#    libproj-dev \
#    libgeos-dev \
#    libgdal-dev \
#    libv8-dev \
#    libsodium-dev \
#    libsecret-1-dev \
#    git \
#    libnetcdf-dev \
#    curl \
#    libjq-dev \
#    cmake \
#    protobuf-compiler \
#    libprotobuf-dev \
#    librdf0 \
#    librdf0-dev \
#    libjq-dev \
#    libqgis-dev \
#    libicu-dev \ 
#    cmake \
#    redland-utils && \
#    apt-get clean
    
    
# Install system libraries with apt-get -y
RUN apt-get update && \
    apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-gnutls-dev \
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
    qgis \
    qgis-plugin-grass \
    libprotobuf-dev \
    libjq-dev \
    cmake \
    protobuf-compiler \
    librdf0 \
    librdf0-dev \
    libqgis-dev \
    libharfbuzz-dev \
    redland-utils && \
    apt-get clean

    
## update system libraries
RUN apt update && apt upgrade -y && apt clean

# Set the working directory
WORKDIR /root/shiny_compare_tunaatlas_datasests

COPY download_CWP_shapefiles.R ./download_CWP_shapefiles.R

# Run the data to donwload GTA data for species label, species group, cwp_shape
RUN Rscript download_CWP_shapefiles.R

#those packages are essential to download the data in update_data.R, they are ran before renv because the renv.lock would change more than the DOI2.csv
RUN R -e "install.packages('remotes', repos='https://cran.r-project.org/')" 
RUN R -e "remotes::install_version('zen4R', version = '0.10', upgrade = 'never', repos = 'https://cran.r-project.org/')"
RUN R -e "remotes::install_version('readr', version = '2.1.5', upgrade = 'never',  repos = 'https://cran.r-project.org/')"
    
# Echo the DOI_CSV_HASH for debugging and to to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}"

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

# Copy the CSV containing the data to download
# Copy the script downloading the data from the CSV
COPY data/DOI.csv ./data/DOI.csv 
COPY update_data.R ./update_data.R 

# Run the data update script Downloading the data (cached if data/DOI.csv did not change).
RUN Rscript update_data.R 

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
RUN echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}"

# Create the renv cache directory
RUN mkdir -p ${RENV_PATHS_ROOT}

# Install renv package that records the packages used in the shiny app
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 

# Copy the rest of the application code
COPY . .

# Create the default dataset from DOI and GTA data loading to make launching faster (use of qs for loading and data.table for tidying) 
RUN Rscript ./create_parquet_from_DOI.R 

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Create directories for configuration
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/

# Running the library making a lot of time to load as tmap (6 seconds)
# Run the global script to load packages and data prior to running the shiny app 
# Removed as global.R need connection to DB for now to implement everything #not anymore
# RUN Rscript global.R maybe creating errors
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/shiny_compare_tunaatlas_datasests', port=3838, host='0.0.0.0')"]
