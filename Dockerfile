FROM rocker/shiny:4.4.0
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
    cmake \
    curl \
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
    libssl-dev \
    libv8-dev \
    libxml2-dev \
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
    
# Install R core package dependencies (we might specify the version of renv package)
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# FROM ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests-cache AS base
# Set environment variables for renv cache, see doc https://docs.docker.com/build/cache/backends/
ARG RENV_PATHS_ROOT

# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

# Set the working directory
WORKDIR /root/shiny_compare_tunaatlas_datasests

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
RUN R -e "renv::restore()"

#FROM ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests-cache
# Copy the rest of the application code
COPY  . .

# Create directories for configuration
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/

# Expose port 3838 for the Shiny app
EXPOSE 3838
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/shiny_compare_tunaatlas_datasests'"]