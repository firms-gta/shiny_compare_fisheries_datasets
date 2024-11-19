# Paths for processed files
cl_areal_grid_path <- here::here("data/cl_areal_grid.qs")
cl_nc_areas <- here::here("data/cl_nc_areas.qs")
require(here)
require(futile.logger)
require(qs)
require(readr)
require(sf)
require(tools)
require(dplyr)
require(tibble)

# Load or process cl_areal_grid
if (file.exists(cl_areal_grid_path)) {
  flog.info("Loading processed cl_areal_grid from .qs")
  shapefile.fix <- qs::qread(cl_areal_grid_path)
} else {
  flog.info("Processing cl_areal_grid and saving as .qs")
  shapefile_path <- here::here("data/cl_areal_grid.csv")
  shapefile.fix <- read.csv(shapefile_path)
  shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt")
  if (is.na(st_crs(shapefile.fix))) {
    flog.warn("No CRS found, setting to WGS84")
    st_crs(shapefile.fix) <- 4326
  }
  shapefile.fix <- shapefile.fix[sf::st_is_valid(shapefile.fix),]
  shapefile.fix <- shapefile.fix %>% 
    dplyr::rename(geographic_identifier = CWP_CODE, gridtype = GRIDTYPE) %>% 
    dplyr::select(geographic_identifier, gridtype, geom_wkt) %>%
    dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
  shapefile.fix <- st_as_sf(shapefile.fix)
  qs::qsave(shapefile.fix, cl_areal_grid_path)
  shapefile.fix$geom_wkt <- NULL
  qs::qsave(shapefile.fix, "data/gridtype.qs")
}

# Load or process cl_areal_grid
if (file.exists(cl_nc_areas)) {
  flog.info("Loading processed cl_nc_areas from .qs")
  shapefile.fix <- qs::qread(cl_nc_areas)
} else {
  flog.info("Processing cl_nc_areas and saving as .qs")
  shapefile_path <- here::here("data/cl_nc_areas.csv")
  cl_nc_areas <- read_csv(shapefile_path)
  shapefile.fix <- sf::st_as_sf(cl_nc_areas, wkt = "geom_wkt")
  if (is.na(st_crs(shapefile.fix))) {
    flog.warn("No CRS found, setting to WGS84")
    st_crs(shapefile.fix) <- 4326
  }
  # Function to calculate a bounding box with a 20-degree range around the center
  simplify_to_bbox <- function(geometry) {
    # Ensure geometry is valid
    geometry <- st_make_valid(geometry)
    
    # Calculate the bounding box directly
    bbox <- st_bbox(geometry)
    
    # Create a 20-degree box around the centroid if you want additional padding
    center <- st_centroid(geometry)
    center_coords <- st_coordinates(center)
    xmin <- center_coords[1] - 5
    xmax <- center_coords[1] + 5
    ymin <- center_coords[2] - 5
    ymax <- center_coords[2] + 5
    
    # Create a polygon for the bbox with buffer
    bbox_poly <- st_polygon(list(rbind(
      c(xmin, ymin), c(xmax, ymin),
      c(xmax, ymax), c(xmin, ymax),
      c(xmin, ymin)
    )))
    
    st_sfc(bbox_poly, crs = st_crs(geometry))  # Return as sfc object with the same CRS
  }
  shapefile.fix <- shapefile.fix %>%
    st_cast("POLYGON")
  shapefile.fix <- shapefile.fix %>%
    rowwise() %>%
    dplyr::mutate(geom_wkt = st_make_valid(geom_wkt)) %>%
    ungroup()
  shapefile.fix <- shapefile.fix %>%
    mutate(geom_wkt = lwgeom::lwgeom_make_valid(geom_wkt))
  
  shapefile.fix <- shapefile.fix %>%
    mutate(geom_wkt = st_geometry(geom_wkt)) %>%
    rowwise() %>%
    mutate(geom_wkt = simplify_to_bbox(geom_wkt)) %>%
    ungroup() %>%
    st_as_sf()
  shapefile.fix <- shapefile.fix %>% 
    dplyr::mutate(geographic_identifier = code, gridtype = "NOMINAL_AREA") %>% 
    dplyr::select(geographic_identifier, gridtype, geom_wkt) %>%
    dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
  shapefile.fix <- st_as_sf(shapefile.fix)
  qs::qsave(shapefile.fix, here::here("data/cl_nc_areas.qs"))
}


DOI <- read_csv('data/DOI.csv')
source(here::here("update_data.R"))
load_data <- function(DOI) {
  
  for (filename in DOI$Filename) {
    flog.info("Loading dataset: %s", filename)
    
    # Define file paths
    base_filename <- tools::file_path_sans_ext(filename)
    qs_file_path <- file.path('data', paste0(base_filename, '.qs'))
    csv_file_path <- file.path('data', paste0(base_filename, '.csv'))
    rds_file_path <- file.path('data', paste0(base_filename, '.rds'))
    # Check if .qs file exists
    if (file.exists(qs_file_path)) {
      # Load from .qs if it exists
      data <- qs::qread(qs_file_path)
      flog.info("Loaded %s from .qs", filename)
      
    } else {
      # If .qs does not exist, try to load from CSV or RDS
      if (file.exists(csv_file_path)) {
        # Load from CSV with specific column type
        data <- read_csv(csv_file_path, col_types = cols(gear_type = col_character()))
        flog.info("Loaded %s from CSV", filename)
        
      } else if (file.exists(rds_file_path)) {
        # Load from RDS
        data <- readRDS(rds_file_path)
        flog.info("Loaded %s from RDS", filename)
        
        # Ensure gear_type is character after reading from RDS
        if ("gear_type" %in% names(data)) {
          data$gear_type <- as.character(data$gear_type)
        }
      } else {
        # File not found
        warning(paste('File not found:', csv_file_path, 'or', rds_file_path))
        next
      }
      
      # Save the loaded data to .qs for faster future access
      qs::qsave(data, qs_file_path)
      flog.info("Saved %s as .qs", filename)
      
      # Add to the loaded_data list and assign to global environment
      loaded_data[[base_filename]] <- data
    }
    
    # Assign the loaded data to the global environment
    assign(base_filename, as.data.frame(loaded_data[[base_filename]]), envir = .GlobalEnv)
  }
}

load_data(DOI)

cwp_grid <- qs::qread(cl_areal_grid_path)%>%
  dplyr::mutate(geom_wkt=sf::st_as_text(sf::st_sfc(geom_wkt),EWKT = TRUE))
global_catch_firms_level0_harmonized <- global_catch_firms_level0_harmonized %>%
  dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% 
  dplyr::left_join(cwp_grid)

# gridtype <- qs::qread("data/gridtype.qs")
# global_catch_tunaatlasird_level2 <- global_catch_tunaatlasird_level2 %>%
#   dplyr::left_join(gridtype)

nc_areas <- qs::qread(here::here("data/cl_nc_areas.qs"))%>% 
  dplyr::mutate(geom_wkt = sf::st_simplify(geom_wkt))%>%
  dplyr::mutate(geom_wkt = st_cast(geom_wkt, "MULTIPOLYGON"))%>%
  dplyr::mutate(geom_wkt=sf::st_as_text(sf::st_sfc(geom_wkt),EWKT = TRUE))


global_nominal_catch_firms_level0_harmonized <- global_nominal_catch_firms_level0_harmonized %>% 
  dplyr::left_join(nc_areas)


# # Renommer les colonnes pour `global_catch_tunaatlasird_level2`
# global_catch_tunaatlasird_level2 <- global_catch_tunaatlasird_level2 %>%
#   dplyr::rename(
#     geom_wkt = geom
#   ) %>% dplyr::select(-GRIDTYPE)

# # Ajouter les colonnes manquantes avec NA
# global_catch_tunaatlasird_level2$measurement <- NULL
# global_catch_tunaatlasird_level2$measurement_type <- NULL
# global_catch_tunaatlasird_level2$measurement_status <- NULL

# Ajouter les colonnes manquantes avec NA
global_nominal_catch_firms_level0_harmonized$measurement <- NULL
global_nominal_catch_firms_level0_harmonized$measurement_unit <- "t"
global_nominal_catch_firms_level0_harmonized$measurement_type <- NULL
global_nominal_catch_firms_level0_harmonized$measurement_status <- NULL

# Ajouter les colonnes manquantes avec NA
global_catch_firms_level0_harmonized$measurement <- NULL
global_catch_firms_level0_harmonized$measurement_type <- NULL
global_catch_firms_level0_harmonized$measurement_status <- NULL

# global_catch_tunaatlasird_level2$Gear <- NULL
# global_catch_tunaatlasird_level2$geographic_identifier_nom <- NULL
# global_catch_tunaatlasird_level2$species_group <- NULL

if (!file.exists(here::here("gta.parquet"))) {
  
  binded <- do.call(rbind, lapply(DOI$Filename, function(x) {
    # Récupérer le nom sans extension
    dataset_name <- tools::file_path_sans_ext(x)
    
    data <- get(dataset_name)
    data$dataset <- dataset_name
    
    return(data)
  }))
  binded <-binded %>%
    as.data.frame() %>%
    dplyr::rename(geom = geom_wkt, codesource_area = geographic_identifier) %>% 
    dplyr::mutate(ogc_fid = codesource_area)%>%
    mutate(year = as.numeric(format(time_start, "%Y"))) %>% 
    mutate(month =  as.numeric(format(time_start, "%m"))) %>% 
    mutate(count = 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(geom = gsub("^SRID=4326;", "", geom))
  
  binded <- as.data.frame(binded)
  attr(binded, "sf_column") <- NULL
  attr(binded, "agr") <- NULL
  binded <- binded %>% tibble::as.tibble()
  qs::qsave(binded, here::here("all.qs"))
  arrow::write_parquet(binded, here::here("gta.parquet"))
}


# binded <- binded %>%
#   mutate(
#     geom = st_as_text(geom),               # Convert geometry to WKT character
#     ogc_fid = as.numeric(ogc_fid),          # Ensure ogc_fid is numeric
#     year = as.integer(year),                # Ensure year is integer
#     month = as.integer(month),              # Ensure month is integer
#     measurement_value = as.numeric(measurement_value), # Ensure measurement_value is numeric
#     count = as.numeric(count)               # Ensure count is numeric
#   ) %>%
#   st_drop_geometry() 


# reloaded_data = arrow::read_parquet("gta.parquet") %>% st_as_sf(wkt="geom", crs = 4326)

# Write to Parquet with WKB geometry
# arrow::write_parquet(object_parquet, "gta.parquet")
# arrow::write_parquet(object, "gta.parquet")
