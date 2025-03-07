load_codelists <- function(list_values_dimensions,list_dimensions=NULL){
  
  # From Data Structure Description: https://github.com/fdiwg/fdi-formats/blob/main/cwp_rh_generic_gta_taskI.json
  if(grepl("species",list_dimensions)){
    #library(worrms)
    cl_asfis_species <- read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/refs/heads/main/global/cwp/cl_asfis_species.csv")
    # name <- cl_asfis_species$taxon_scientific_name 
    # wm_name2id(name)
    # wm_name2id(name = "Rhincodon")
    # for(i in 1:length(name)){
    #   cat(wm_name2id_(name=as.vector(cl_asfis_species$taxon_scientific_name)))
    # }
    # cl_asfis_species_worms <- cl_asfis_species %>% dplyr::mutate(worms_id=getWormsID(cl_asfis_species$taxon_scientific_name))
    # View(cl_asfis_species_worms)
    # cl_asfis_species_worms$worms_id
    
    standard_species <- list_values_dimensions$species %>% as_tibble() %>%
      dplyr::rename(code=value) %>%
      dplyr::left_join(y = cl_asfis_species,by = "code")  
    # %>%     dplyr::mutate(worms_id=wm_name2id_(taxon_scientific_name))
    
    for(i in 1:length(standard_species$taxon_scientific_name)){
      # if(!(standard_species$taxon_scientific_name[i] %in% c("Thunnini"))){
      flog.info("Count %s:", i)
      wres <- getWormsID(standard_species$taxon_scientific_name[i])
      flog.info("WormsID %s:", wres)
      flog.info("Next species %s:", standard_species$taxon_scientific_name[i+1])
      standard_species$AphiaID[i] <-  wres
      # }else{
      #   flog.info("Problematic species %s !!!!", standard_species$taxon_scientific_name[i])
      #   standard_species$AphiaID[i] <-  NA
      # }
    }
    # View(standard_species)
    standard_species <- standard_species %>% 
      mutate(label = ifelse(is.na(label), code, label))
    qs::qsave(standard_species, here::here(file.path("data","codelist_species.qs")))
    }
  
  
  if(grepl("gear",list_dimensions)){
    standard_gear <- list_values_dimensions$gear_type %>% as_tibble() %>%
      dplyr::rename(gear_type=value) %>%
      dplyr::mutate(code=gear_type)  %>%
      dplyr::left_join(y = read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv"),by = "code")
    # View(read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv"))
    qs::qsave(standard_gear, here::here(file.path("data","codelist_gear.qs")))
    }
  
  
  if(grepl("source_authority",list_dimensions)){
    standard_source_authority <- list_values_dimensions$source_authority %>% as_tibble() %>%
      dplyr::rename(source_authority=value) %>%
      dplyr::mutate(code=source_authority)  %>%
      dplyr::left_join(y = read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_tuna_rfmos.csv"),by = "code")
    # View(read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv"))
    qs::qsave(standard_source_authority, here::here(file.path("data","codelist_source_authority.qs")))
  }
  
  
  if(grepl("fishing_fleet",list_dimensions)){
    standard_fishing_fleet <- list_values_dimensions$fishing_fleet %>% as_tibble() %>%
      dplyr::rename(fishing_fleet=value) %>%
      dplyr::mutate(code=fishing_fleet)  %>%
      dplyr::left_join(y = read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv"),by = "code")
    # View(read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv"))
    standard_fishing_fleet <- standard_fishing_fleet %>% 
      mutate(label = ifelse(is.na(label), code, label))
      
    qs::qsave(standard_fishing_fleet, here::here(file.path("data","codelist_fishing_fleet.qs")))
  }
  
  
 
  
  
  
  # qs::qsave(standard_species, here::here(file.path("data","codelist_species.qs")))
  # codelist_species <- qs::qread(here::here(file.path("data","codelist_species.qs"))) %>% 
  #   dplyr::select(c("AphiaID","code","taxon_code","taxon_scientific_name","name_en","name_fr","name_fr","name_es","name_fr")) 
  # # %>%
  # #   as_tibble() %>%
  # #   as_tibble() %>%
  # #   as_tibble() %>%
  # # time_series_ids <- c(1,2,3,4,5)
  # # names(time_series_ids) <- c('a','b','c','d','e')
  # species_ids <- codelist_species$code
  # names(species_ids) <- codelist_species$taxon_scientific_name
  # species_ids
  #   dplyr::rename(gear_type=value) %>%
  #   dplyr::mutate(code=gear_type)  %>%
  # read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv")
  # fishing_mode
  # read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv")
  # source_authority
  # read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_tuna_rfmos.csv")
  
}
