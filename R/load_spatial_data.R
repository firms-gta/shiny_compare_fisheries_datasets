load_spatial_data <- function(df_sf,mode) {
  
  if(!file.exists("data/gta_geom.RDS")){
    df_distinct_geom <- qread("data/global_catch_tunaatlasird_level2_14184244.qs") %>%
      dplyr::select(geographic_identifier, GRIDTYPE) %>% 
      dplyr::mutate(ogc_fid = 1) %>% 
      dplyr::rename(codesource_area=geographic_identifier,gridtype=GRIDTYPE,geom=geom_wkt) %>%
      mutate(ogc_fid=row_number(codesource_area)) %>% 
      dplyr::group_by(codesource_area,gridtype,geom) %>% dplyr::summarise(count = sum(ogc_fid)) %>% ungroup() %>%  st_set_crs(4326)
    #%>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom),EWKT = TRUE)) %>% dplyr::as_tibble() # st_as_sf(wkt="geom_wkt", crs=4326)
    saveRDS(df_distinct_geom, "data/gta_geom.RDS")   
  } 
    
if(mode!="DOI"){
  flog.info("Store distinct geometries in the dedicaded sf object 'df_distinct_geom' to perform faster spatial analysis")
  df_distinct_geom <- df_sf %>% as.data.frame() %>% dplyr::group_by(codesource_area,gridtype,geom) %>%
    filter(!is.na(gridtype)) %>% filter(!is.na(geom)) %>%
    dplyr::summarise(ogc_fid = first(ogc_fid)) %>% ungroup() %>% st_as_sf(wkt="geom",crs=4326) 
}else{
  # saveRDS(df_distinct_geom, "gta_geom.RDS")
  if(!file.exists("data/gta_geom_new.qs")){
    df_distinct_geom_spatial <- readRDS("data/gta_geom.RDS") %>% dplyr::select(-c(count)) 
  
  df_distinct_geom_nominal <- sf::read_sf("data/cl_nc_areas_simplfied.gpkg") %>% 
    dplyr::rename('codesource_area'= code)   %>% 
    dplyr::mutate(geom=st_buffer(st_centroid(geom),dist=1),'gridtype'="nominal")  %>% 
    # dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom)),EWKT = TRUE) %>% 
    dplyr::select(codesource_area,gridtype)
  
  df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
    dplyr::mutate('ogc_fid'= row_number(codesource_area)) 
  
  # saveRDS(df_distinct_geom, "data/gta_geom_new.RDS")  
  # arrow::write_parquet(df_distinct_geom, "data/gta_geom_new.parquet")
  qsave(df_distinct_geom, "data/gta_geom_new.qs")
  }else{
    # df_distinct_geom <- arrow::read_parquet("data/gta_geom_new.parquet") 
    df_distinct_geom <- qread("data/gta_geom_new.qs") 
    
    
  }
  
  # df_distinct_geom_nominal <- read.csv("data/cl_nc_areas.csv") %>% sf::st_as_sf(wkt="geom_wkt",crs=4326)   %>% 
  #   dplyr::mutate('geom'=st_bbox(),'codesource_area'=geographic_identifier)
  # arrow::write_parquet(df_distinct_geom, "gta_geom.parquet")
  
}
  
  return(df_distinct_geom)
}