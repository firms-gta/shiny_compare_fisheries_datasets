load_spatial_data <- function(codesource_area=NULL,mode) {
  this_areas <- codesource_area
  if(!file.exists(here::here("data/gta_geom.qs"))){
    df_distinct_geom_spatial <- read_csv("https://github.com/fdiwg/fdi-codelists/raw/refs/heads/main/global/cwp/cl_areal_grid.csv") %>% 
      dplyr::mutate(codesource_area = as.character(code)) %>% 
      dplyr::select(codesource_area,GRIDTYPE,geom_wkt) %>% 
      dplyr::rename('gridtype' = GRIDTYPE, geom=geom_wkt)  %>% 
      dplyr::mutate('gridtype' = case_when(gridtype == '1deg_x_1deg' ~ '1deg_x_1deg',
                                           gridtype == '5deg_x_5deg' ~ '5deg_x_5deg',
                                           TRUE ~ 'others')) %>%
      st_as_sf(wkt="geom", crs=4326)
    
    if(!is.null(this_areas)){
      df_distinct_geom_spatial <- df_distinct_geom_spatial  %>% dplyr::filter(codesource_area %in% this_areas)
    }
    qsave(df_distinct_geom_spatial, here::here("data/gta_geom.qs"))
    }
    
if(mode!="DOI"){
  flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
  df_distinct_geom <- df_sf %>% as.data.frame() %>% 
    dplyr::filter(codesource_area %in% this_areas) %>% 
    dplyr::group_by(codesource_area,gridtype,geom) %>%
    filter(!is.na(gridtype)) %>% filter(!is.na(geom)) %>%
    dplyr::summarise(ogc_fid = first(ogc_fid)) %>% ungroup() %>% st_as_sf(wkt="geom",crs=4326) 
}else{
  # saveRDS(df_distinct_geom, "gta_geom.RDS")
  if(!file.exists(here::here("data/gta_geom_new.qs"))){
    df_distinct_geom_spatial <- qs::qread(here::here("data/gta_geom.qs"))
    
    if(!file.exists(here::here("data/cl_nc_areas_simplfied.gpkg"))){
      df_distinct_geom_nominal <- read_csv("https://github.com/fdiwg/fdi-codelists/raw/main/global/firms/gta/cl_nc_areas.csv") %>% 
        dplyr::mutate('geom'=geom_wkt)  %>%
        sf::st_as_sf(wkt="geom",crs=4326)  %>% st_simplify(dTolerance = 0.5) 
      st_write(df_distinct_geom_nominal,dsn = here::here("data/cl_nc_areas_simplfied.gpkg"))
    }else{
      df_distinct_geom_nominal <- sf::read_sf(here::here("data/cl_nc_areas_simplfied.gpkg"))
      }
    df_distinct_geom_nominal <- df_distinct_geom_nominal %>% st_centroid() %>% 
      st_buffer(units::set_units(1, degree))  %>% 
      dplyr::rename('codesource_area'= code)   %>% 
      dplyr::mutate('gridtype'="nominal")  %>%
      # dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom),EWKT = TRUE)) %>% dplyr::as_tibble() %>%  st_as_sf(wkt="geom_wkt", crs=4326) %>% 
      dplyr::select(codesource_area,gridtype)
    
    df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
    dplyr::mutate('ogc_fid'= row_number(codesource_area)) 
    
  
  # qs::qsave(df_distinct_geom, "data/gta_geom_new.qs")  
  # arrow::write_parquet(df_distinct_geom, "data/gta_geom_new.parquet")
  qsave(df_distinct_geom, here::here("data/gta_geom_new.qs"))
  }else{
    # df_distinct_geom <- arrow::read_parquet("gta_geom_new.parquet") 
    df_distinct_geom <- qread(here::here("data/gta_geom_new.qs"))
  }
  
  # df_distinct_geom_nominal <- read.csv("cl_nc_areas.csv") %>% sf::st_as_sf(wkt="geom_wkt",crs=4326)   %>% 
  #   dplyr::mutate('geom'=st_bbox(),'codesource_area'=geographic_identifier)
  # arrow::write_parquet(df_distinct_geom, "gta_geom.parquet")
}
  
  return(df_distinct_geom)
}