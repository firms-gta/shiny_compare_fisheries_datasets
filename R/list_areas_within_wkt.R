########################################################## Function for spatial filters ########################################################## 
process_list_areas <- function(df_distinct_geom, wkt,list_gridtype) {
  
  
  current_selection <- st_sf(st_as_sfc(wkt, crs = 4326))
  current_df_distinct_geom <- df_distinct_geom %>% dplyr::filter(gridtype %in% list_gridtype)
  
  # Vérifier si qgisprocess est installé
  if (requireNamespace("qgisprocess", quietly = TRUE) && spatial_processing_mode=="QGIS") {
    
    # Essayer de configurer qgisprocess pour voir s'il fonctionne
    qgis_path <- try(qgisprocess::qgis_configure(), silent = TRUE)
    flog.info("QGIS is used to select remaing data within the default or newly dran polygon !")
    
    if (!inherits(qgis_path, "try-error") && !is.null(qgis_path)) {
      # Utiliser qgisprocess si disponible et configuré
      message("Utilisation de qgisprocess pour traiter les données.")
      list_areas <- df_distinct_geom %>%
        dplyr::filter(!is.na(gridtype)) %>%
        qgisprocess::qgis_run_algorithm(
          "native:extractbylocation",
          INPUT = .,
          PREDICATE = "are within",
          INTERSECT = st_sf(current_selection)
        ) %>% 
        sf::st_as_sf()
      
      flog.info("Remaining number of different areas within this WKT: %s", nrow(list_areas))
      
      within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
        rename_at(1,~"codesource_area") %>% dplyr::select(codesource_area) %>% pull()
      return(within_areas)
    }
  }
  
  # Si qgisprocess n'est pas disponible ou configuré, utiliser sf
  message("qgisprocess non disponible ou non configuré. Utilisation de sf pour traiter les données.")
  flog.info("using sf instead of QGIS")
  list_areas <- df_distinct_geom %>%
    dplyr::filter(!is.na(gridtype)) %>%
    dplyr::filter(sf::st_within(., st_sf(current_selection), sparse = FALSE)) %>%
    sf::st_as_sf()
  
  within_areas <- unique(list_areas$codesource_area) %>% as.data.frame() %>%
    rename_at(1,~"codesource_area") %>% dplyr::select(codesource_area) %>% pull()
  return(within_areas)
}