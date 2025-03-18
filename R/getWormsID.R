# #An vector of names we wan't to match (can be one item only!)
# namesToMatch <- c("Solea soleo", "Abra Albo", "sdqkljflkfjqsmlfds")
# namesToMatch <- as.vector(standard_species$taxon_scientific_name)
# c("Thunnini","Lamnidae","Lamnidae","Scombroidei")
# testworms <- getWormsID("Scombroidei")
# from https://www.marinespecies.org/aphia/webservice/Aphia_webservice_R_elaborate.txt
getWormsID  <- function(namesToMatch){
  # #Use the libraries
  # library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
  # library(httr)
  #Convert the namesToMatch to a valid REST-url
  if(namesToMatch!='Thunnini' && !is.na(namesToMatch)){
    urlNamesPart <- ""
    for (index in 1:length(namesToMatch)) {
      urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[index]);
    }
    
    #The url can contain special characters that need to be converted
    urlNamesPart <- URLencode(urlNamesPart)
    
    #The dyanmic build of the URL causes an obsolete '&' at the beginning of the string, so remove it
    urlNamesPart <- substring(urlNamesPart, 2)
    
    #Build the final REST-url
    url <- sprintf("https://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);
    # url <- "https://www.marinespecies.org/rest/AphiaRecordsByMatchNames?scientificnames[]=Thunnus%20alalunga&scientificnames[]=Alopias%20vulpinus&scientificnames[]=Thunnus%20obesus"
    #Get the actual data from the URL
    matches <- fromJSON(url)
    
    #Handle the data (each requested name has an list of results)
    for (matchesindex in 1:length(namesToMatch)) {
      #Get the results for the current index
      currentResultList = matches[[matchesindex]]
      
      #Get the number of list entries for the first column
      numberOfResults <- length(currentResultList[[1]])
      
      #Handle empty data due to no matches found
      if (is.na(currentResultList[[1]][[1]])) {
        numberOfResults <- 0
      }
      print(sprintf("%d Result(s) found for %s", numberOfResults, namesToMatch[matchesindex]))
      if (numberOfResults > 0) {
        for (listentry in 1:numberOfResults) {
          print(sprintf("IDstandard_species$taxon_scientific_name: %d, SCIENTIFICNAME: %s, MATCH_TYPE: %s",
                        currentResultList[["AphiaID"]][listentry],
                        currentResultList[["scientificname"]][listentry],
                        currentResultList[["match_type"]][listentry]
          ));
        }
      }
    }
    res <- currentResultList[["AphiaID"]][listentry]
  }else{
    res <-NA
  }
  
  return(res)
}