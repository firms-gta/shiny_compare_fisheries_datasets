FROM rocker/r-ver:4.0.5

MAINTAINER Julien Barde "julien.barde@ird.fr"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    texlive-xetex \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-formats-extra \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libv8-dev \
	libsodium-dev \
    libsecret-1-dev \
    git
    
#geospatial
RUN /rocker_scripts/install_geospatial.sh

# install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes','jsonlite','yaml'), repos='https://cran.r-project.org/')"
# clone app
RUN git -C /root/ clone https://github.com/firms-gta/shiny_compare_tunaatlas_datasests && echo "OK!"
RUN ln -s /root/shiny_compare_tunaatlas_datasests /srv/shiny_compare_tunaatlas_datasests
# install R app package dependencies
RUN R -e "source('./srv/shiny_compare_tunaatlas_datasests/install.R')"

#etc dirs (for config)
RUN mkdir -p /etc/shiny_compare_tunaatlas_datasests/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/shiny_compare_tunaatlas_datasests',port=3838,host='0.0.0.0')"]
