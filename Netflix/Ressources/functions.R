# Fonction pour formatter une valeur en pourcentage ( round + format)

Pourcentage_formatting <- function(raw_number) {
  rounded_number <- round(raw_number, digits=2)
  formatted_number <- format(rounded_number, decimal.mark = ',')
  return (formatted_number)
}

#Fonction pour extraire seulent l'année dans la colonne "date_added"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

duration_formatting <- function(data) {
  if(grepl("Seasons",data)){
    season_string_removed <- sub("Seasons", "",data)
    season_string_removed <- gsub(" ", "", season_string_removed, fixed = TRUE)
    formatted_data <- as.numeric(season_string_removed)*360
    
  } else if(grepl("Season",data)){
    season_string_removed <- sub("Season", "",data)
    season_string_removed <- gsub(" ", "", season_string_removed, fixed = TRUE)
    formatted_data <- as.numeric(season_string_removed)*360
    
  } else if(grepl("min",data)) {
    movie_string_removed <- sub("min", "",data)
    formatted_data <- gsub(" ", "", movie_string_removed, fixed = TRUE)
  }
  return (formatted_data)
}