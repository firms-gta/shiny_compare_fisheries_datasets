#' Nettoyage du dossier data
#'
#' Cette fonction supprime tous les fichiers du dossier `data/`, à l'exception de certains fichiers spécifiés, afin de réduire la taille d'un dockerfile. Elle n'est lancée que lors du changement du docker.
#'
#' @param data_path Chemin du dossier contenant les fichiers à nettoyer. Par défaut, `here::here("data")`.
#'
#' @return Ne retourne rien, mais supprime les fichiers non listés dans la liste de conservation.
#' @export
#'
#' @examples
#' clean_data_folder()
clean_data_folder <- function(data_path = here::here("data")) {
  # Fichiers à conserver
  keep_files <- c("whole_group_df.parquet",
                  "filters_combinations.parquet",
                  "df_distinct_geom_light.csv",
                  "default_df.parquet",
                  "DOI.csv")
  
  # Lister les fichiers dans le dossier
  files <- list.files(data_path, full.names = TRUE)
  
  # Identifier les fichiers à supprimer
  files_to_delete <- files[!basename(files) %in% keep_files]
  
  # Supprimer les fichiers inutiles
  if (length(files_to_delete) > 0) {
    file.remove(files_to_delete)
    message(length(files_to_delete), " fichiers supprimés.")
  } else {
    message("Aucun fichier à supprimer.")
  }
}