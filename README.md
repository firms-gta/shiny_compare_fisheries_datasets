# shiny_compare_tunaatlas_datasests

Shiny app to display and compare the contents of fisheries data : e.g. multiple versions of the same datasest with different levels of processing (raw, raised data..) or datasets collected in different areas.


# Prequesite

This Shiny app has first been built to display data stored in a database but has become more generic to display the content of any dataset as long as its structure is compliant with CWP format for fisheries data.
Inputs data are now delivered by datasets which have been assigned DOIs on Zenodo to foster reproducibility of the work.

See details here : https://github.com/fdiwg 

## Default dataset


![DOI logo](https://upload.wikimedia.org/wikipedia/commons/1/11/DOI_logo.svg)

This app can be customized to visualize, explore and compare any fisheries data complying with the CWP format. However, by default, inputs fisheries data are provided by the Global Tuna Atlas datasets which have been assigned the following DOIs on Zenodo :
 - Level 0 datasets officially endorsed by FIRMS [FIRMS](https://zenodo.org/search?q=metadata.creators.person_or_org.name%3A%22FIRMS%20Global%20Tuna%20Atlas%20Technical%20Working%20Group%22&l=list&p=1&s=10&sort=bestmatch).
  - Upper levels of processing expected by scientists prepared by [IRD](https://zenodo.org/records/14184244).

## Running the app from Rstudio

To run the app from you RStudio, launch the app.R file. You can freely access the RStudio server which has been used to build this app by becoming a member of the [Global Fisheries Atlas Virtual Lab](https://blue-cloud.d4science.org/group/globalfisheriesatlas) provided by [Blue-Cloud 2026](https://blue-cloud.d4science.org) european HORIZON research project which promotes and implements best practices for Open Science and FAIR data management in the marine domain. This Shiny app has been developed and is hosted in this VLab, accessible on this [link](https://shinyproxy.d4science.org/app/compare-tunaatlas-datasets-dev).

Currently the shiny app use the renv package to record state of the packages used to run correctly the application. If you are using a different version of R than the one recorded on the lockfile, error loading the packages can happen.

## Running the app from docker

```
docker pull ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests:latest
docker run --name shiny_compare_datasets -p 3838:3838 ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests
```

And then point your browser to http://localhost:3838

Note: In case of having an alreday existing shiny_compare_tunaatlas_datasests app running on docker, and in order to update the docker app, it will be required to stop and remove the container prior to run the above commands to pull & run the app:

```
docker container stop shiny_compare_datasets
docker container rm shiny_compare_datasets
```

#### Build / Run the image locally

A Dockerfile is provided on GHCR and can be used to build up containers with the application.

To build and run the application issue the following commands
```
sudo docker build -t shiny_compare_datasets <Path of the Dockerfile>
sudo docker run -p 3838:3838 shiny_compare_datasets
```

And then point your browser to http://localhost:3838

As well it can be runned using the command 

```
docker pull ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests_cwp_database:latest
docker run -p 3838:3838 -v path_to_txt/connection_tunaatlas_inv.txt:/root/shiny_compare_tunaatlas_datasests/connection_tunaatlas_inv.txt ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests_cwp_database:latest
```

in a terminal pointing to a .txt that need to contains the following informations:

DB_DRV=
DB_PORT=
DB_HOST=
DB_NAME=
DB_USER_READONLY=
DB_PASSWORD=
