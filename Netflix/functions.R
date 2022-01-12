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

