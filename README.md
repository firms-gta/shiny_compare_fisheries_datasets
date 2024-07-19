# shiny_compare_tunaatlas_datasests

Shiny app to display and compare the contents of fisheries data : e.g. multiple versions of the same datasest with different levels of processing (raw, raised data..) or datasets collected in different areas.



![Screenshot from 2021-10-18 09-37-38.png](./doc/Screenshot_from_2021-10-18_09-37-38.png)


# Prequesite

This app is first been built on top of a database but is meant to become more generic to display the content of any dataset as long as its structure is compliant with CWP format for fisheries data.  See details here : https://github.com/fdiwg 

## Running the app from rstudio

To run the app from you RStudio, launch the app.R file.

Currently the shiny app use the renv package to record state of the packages used to run correctly the application. If you are using a different version of R than the one recorded on the lockfile, error loading the packages can happen.

## Running the app from docker

```
docker pull ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests:latest
docker run --name shiny_compare_datasets -p 3839:3838 ghcr.io/firms-gta/shiny_compare_tunaatlas_datasests
```

And then point your browser to http://localhost:3839

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
sudo docker run -p 3839:3838 shiny_compare_datasets
```

And then point your browser to http://localhost:3839

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
