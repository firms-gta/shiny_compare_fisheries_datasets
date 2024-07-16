FROM rocker/shiny:4.4.0
#FROM rocker/shiny:latest
#FROM rocker/r-ver:4.4.0
#FROM rocker/r-ver:latest

LABEL org.opencontainers.image.authors="julien.barde@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"
LABEL org.opencontainers.image.source https://github.com/firms-gta/shiny_compare_tunaatlas_datasests

# Update and upgrade the system with option -y to tells apt-get to assume the answer to all prompts is yes.
RUN apt update && apt upgrade -y

# Install system libraries of general use with option -y to tells apt-get to assume the answer to all prompts is yes.
RUN apt install -y \
    #sudo \
    #pandoc \
    pandoc-citeproc \
    libssl-dev \
   #libcurl4-openssl-dev \
    #libxml2-dev \
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
## update system libraries
RUN apt update && \
    apt upgrade -y && \
    apt clean
    
# Install R core package dependencies
#RUN install2.r --error --skipinstalled --ncpus -1 httpuv
#RUN R -e "install.packages(c('renv', 'remotes','jsonlite', 'yaml'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"
#RUN Rscript -e 'renv::restore()'


# git clone https://github.com/firms-gta/shiny_compare_tunaatlas_datasests.git
# Set environment variables for renv cache
ARG RENV_PATHS_ROOT
# RENV_PATHS_ROOT: ~/.cache/R/renv
#ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

# Set the working directory
WORKDIR /root/shiny_compare_tunaatlas_datasests

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY .Rprofile ./
COPY renv renv
# @juldebar COPY renv/settings.json renv/

# Restore renv packages
# RUN R -e "renv::activate()"
RUN R -e "renv::restore()"

# Copy the rest of the application code
COPY . .

# Set renv cache location
ENV RENV_PATHS_CACHE=renv/.cache

# Create directories for configuration
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/



# Expose port 3838 for the Shiny app
EXPOSE 3838
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny'"]
#CMD ["R", "-e", "renv::restore() ; shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]