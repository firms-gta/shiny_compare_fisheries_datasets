FROM rocker/shiny:4.4.0

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
    rasqal-utils \
    raptor2-utils \
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
    dos2unix \
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

WORKDIR /root/shiny_compare_tunaatlas_datasests

# Echo the DOI_CSV_HASH for debugging and to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

COPY data/DOI.csv ./data/DOI.csv

# Appliquer dos2unix pour éviter les problèmes de formatage
RUN dos2unix ./data/DOI.csv && cat -A ./data/DOI.csv

# Télécharger les fichiers depuis Zenodo
RUN echo "Début du téléchargement des fichiers..." \
    && bash -c "tail -n +2 ./data/DOI.csv | tr -d '\r' | while IFS=',' read -r DOI FILE; do \
        echo 'DOI: $DOI, FILE: $FILE'; \
        RECORD_ID=\$(echo \"\$DOI\" | awk -F '/' '{print \$NF}' | sed 's/zenodo\\.//'); \
        echo 'Téléchargement de $FILE depuis Zenodo (Record ID: $RECORD_ID)'; \
        wget -c --retry-connrefused --waitretry=5 --timeout=600 --tries=1 -O \"./data/\$FILE\" \"https://zenodo.org/record/\$RECORD_ID/files/\$FILE?download=1\"; \
        if [ \$? -eq 0 ]; then \
            echo 'Fichier téléchargé : ./data/\$FILE'; \
            FILENAME=\$(echo \"\$FILE\" | sed 's/\..*//'); \
            FILE_MIME=\$(echo \"\$FILE\" | sed 's/.*\.//'); \
            NEWNAME=\"./data/\${FILENAME}_\${RECORD_ID}.\${FILE_MIME}\"; \
            echo 'Renommage : mv ./data/\$FILE \$NEWNAME'; \
            if mv \"./data/\$FILE\" \"\$NEWNAME\"; then \
                echo 'Fichier renommé : '\$NEWNAME; \
            else \
                echo 'Échec du renommage de : ./data/\$FILE -> \$NEWNAME'; \
                ls -lh ./data/; \
            fi; \
        else \
            echo 'Erreur lors du téléchargement de $FILE (Record ID: $RECORD_ID)'; \
        fi; \
    done" \
    && echo "Tous les fichiers ont été traités !"


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
RUN mkdir -p ${RENV_PATHS_ROOT}

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

# Install renv package that records the packages used in the shiny app
RUN R -e "lockfile <- jsonlite::fromJSON('renv.lock'); renv_version <- lockfile$Packages[['renv']]$Version; install.packages('renv', repos='https://cran.r-project.org/', type='source', version=renv_version)"

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 
RUN R -e "renv::repair()" 

COPY . .
RUN ls -la
RUN cd renv/library && ls -la
RUN cd data && ls -la

RUN Rscript -e "source('R/download_and_process_zenodo_data.R'); source('R/download_data.R'); download_and_process_zenodo_data()"

RUN cd data && ls -la

# Create directories for configuration
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/shiny_compare_tunaatlas_datasests', host = '0.0.0.0', port = 3838)"]
