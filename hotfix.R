if(exists(here::here("data/df_distinct_geom_light.csv"))){
  unlink(here::here("data/df_distinct_geom_light.csv"))

df_distinct_geom_spatial <- qs::qread(here::here("data/gta_geom.qs")) %>% dplyr::select(-c(count)) 

flog.info("Add spatial geometries 1")

# https://github.com/fdiwg/fdi-codelists/raw/main/global/firms/gta/cl_nc_areas.csv
df_distinct_geom_nominal <- sf::read_sf(here::here("data/cl_nc_areas_simplfied.gpkg")) %>%
  dplyr::rename(codesource_area = code) %>%
  dplyr::mutate(geom = st_make_valid(geom)) %>%  
  dplyr::mutate(geom = st_collection_extract(geom, "POLYGON")) %>%
  dplyr::mutate(geom = st_centroid(geom)) %>%  
  dplyr::mutate(geom = st_buffer(geom, dist = 1)) %>%  
  dplyr::mutate(gridtype = "nominal") %>%
  dplyr::select(codesource_area, gridtype)
flog.info("Add spatial geometries 2")

df_distinct_geom <- rbind(df_distinct_geom_spatial,df_distinct_geom_nominal)  %>% 
  dplyr::mutate('ogc_fid'= row_number(codesource_area)) 
rm(df_distinct_geom_nominal)
gc()
flog.info("Add spatial geometries 3")

df_distinct_geom_light <- df_distinct_geom %>% dplyr::mutate(geom_wkt=st_as_text(st_sfc(geom))) %>% 
  st_drop_geometry()  %>% dplyr::as_data_frame()
qs::qsave(df_distinct_geom_light, here::here("data/df_distinct_geom_light.qs"))
rm(df_distinct_geom)
gc()

}
