# Paths for processed files
cl_areal_grid_path <- here::here("data/cl_areal_grid.qs")
cl_nc_areas <- here::here("data/cl_nc_areas.qs")

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
    rename(geographic_identifier = CWP_CODE, gridtype = GRIDTYPE) %>% 
    select(geographic_identifier, gridtype, geom_wkt) %>%
    mutate(geographic_identifier = as.character(geographic_identifier))
  shapefile.fix <- st_as_sf(shapefile.fix)
  qs::qsave(shapefile.fix, cl_areal_grid_path)
  shapefile.fix$geom_wkt <- NULL
  qs::qsave(shapefile.fix, "data/gridtype.qs")
}

# Load or process cl_areal_grid
if (file.exists(cl_nc_areas)) {
  flog.info("Loading processed cl_areal_grid from .qs")
  shapefile.fix <- qs::qread(cl_nc_areas)
} else {
  flog.info("Processing cl_areal_grid and saving as .qs")
  shapefile_path <- here::here("data/cl_nc_areas.csv")
  shapefile.fix <- read.csv(shapefile_path)
  shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt")
  if (is.na(st_crs(shapefile.fix))) {
    flog.warn("No CRS found, setting to WGS84")
    st_crs(shapefile.fix) <- 4326
  }
  shapefile.fix <- shapefile.fix[sf::st_is_valid(shapefile.fix),]
  shapefile.fix <- shapefile.fix %>% 
    rename(geographic_identifier = code, gridtype = "NOMINAL_AREA") %>% 
    select(geographic_identifier, gridtype, geom_wkt) %>%
    mutate(geographic_identifier = as.character(geographic_identifier))
  shapefile.fix <- st_as_sf(shapefile.fix)
  qs::qsave(shapefile.fix, cl_nc_areas)
}


DOI <- read_csv('data/DOI.csv')
source(here::here("update_data.R"))
load_data <- function(DOI) {
  loaded_data <- list()
  
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
      loaded_data[[base_filename]] <- qs::qread(qs_file_path)
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

cwp_grid <- qs::read(cl_areal_grid_path)
global_catch_firms_level0_harmonized <- global_catch_firms_level0_harmonized %>% dplyr::left_join(cwp_grid)

gridtype <- qs::read("data/gridtype.qs")
global_catch_tunaatlasird_level2 <- global_catch_tunaatlasird_level2 %>% dplyr::left_join(gridtype)

nc_areas <- qs::read(cl_nc_areas)

global_nominal_catch_firms_level0_harmonized <- global_nominal_catch_firms_level0_harmonized %>% 
  dplyr::left_join(nc_areas)

if(!file.exists(here::here("gta.parquet"))){
object <- do.call(rbind, lapply(DOI$Filename, function(x) {
  tools::file_path_sans_ext(x)
}))
}
arrow::write_parquet(object, "gta.parquet")
