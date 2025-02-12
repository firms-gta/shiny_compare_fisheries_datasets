########################################################## Load data from a list of DOIs ########################################################## 
list_DOIs <-"data/DOI.csv"
DOIs <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
list_dataframes <- load_data(mode=mode)