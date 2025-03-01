# NETFLIX

# On télécharge le csv de données Netflix

netflixData <- read.csv('Ressources/netflix_data.csv',TRUE,sep = ",")

# PRE-TRAITEMENT
# DONNES MANQUANTES

# On va d'abord calculer l'impact des données manquantes colonne par colonne
# On calcule le nombre total de lignes 

TotalRowNumber <- nrow(netflixData)

# On calcule ensuite le nombre de données manquantes pour les 4 colonnes qui
# vont nous intéresser pour notre étude

missingDataForTypeColumn <- (sum(is.na(netflixData$type) | netflixData$type == "")/TotalRowNumber)*100

# Le pourcentage de données manquantes pour la colonne "type" est 0 %

missingDataForDateAddedColumn <- (sum(is.na(netflixData$date_added) | netflixData$date_added == "")/TotalRowNumber)*100

# on fait un rapide formatage pour une meilleur lisibilité

missingDataForDateAddedColumn <- Pourcentage_formatting(missingDataForDateAddedColumn)

# Le pourcentage de données manquantes pour la colonne "date_added" est 0.03 %

missingDataForDurationColumn <- (sum(is.na(netflixData$duration) | netflixData$duration == "")/TotalRowNumber)*100

# Le pourcentage de données manquantes pour la colonne "date_added" est 0.11 %

missingDataForReleaseYearColumn <- (sum(is.na(netflixData$release_year) | netflixData$release_year == "")/TotalRowNumber)*100

# Le pourcentage de données manquantes pour la colonne "release_year" est de 0 %

missingDataForListedInColumn <- (sum(is.na(netflixData$listed_in) | netflixData$listed_in == "")/TotalRowNumber)*100

# Le pourcentage de données manquantes pour la colonne "listed_in" est de 0 %

# On constate qu'il y a très peu de données manquantes et qu'il n'est donc 
# pas nécessaire de traiter cette base de données dans ce sens et on va simplement supprimer les lignes concernées par la suite

NAValueForDurationColumn <- (sum(is.na(netflixData$duration)))
# On constate qu'il n'y a aucune valeur NA dans la colonne duration mais seulement des valeurs manquantes, on va donc supprimer 
# les lignes avec une valeur manquante pour duration.

netflixData <- netflixData[!(netflixData$duration == ""),]

# On va donc maintenant diviser la base de données en deux parties, les données 
# des programmes ajoutés à Netflix du début à 2020, netflixData.H et les données des programmes
# ajoutés en 2021, netflixData.A

netflixData.H <- subset(netflixData, substrRight(netflixData$date_added,4) < 2021)

netflixData.A <- subset(netflixData, substrRight(netflixData$date_added,4) == 2021)

#On reformatte la colonne date_added des deux bases de données pour n'avoir que l'année 

netflixData.A$date_added <- substrRight(netflixData.A$date_added,4)

netflixData.H$date_added <- substrRight(netflixData.H$date_added,4)

# On va ensuite enlèver les lignes avec une case "date_added" ayant comme valeur NA
# On peut utiliser cette technique car le % de données manquante pours cette colonne est d'à peine 0,11%

netflixData.H <- netflixData.H[complete.cases(netflixData.H), ]

netflixData.A <- netflixData.A[complete.cases(netflixData.A), ]


# TRAITEMENT PREMIERE QUESTION

# Pour la première question de l'étude, il nous ai demandé de comparer le temps moyen entre la sortie
# des films et leur ajout au carnet Netflix pour valider une hypothèse, on doit donc traiter les données
# et créer une nouvelle base de données pour chaque partie, dans laquelle ne figure que les films.

netflixData.H.Movies <- subset(netflixData.H,netflixData.H$type =='Movie')

netflixData.A.Movies <- subset(netflixData.A,netflixData.A$type =='Movie')

# On peut maintenant exploiter les données de ces deux bases pour le début de l'étude.


# On supprime les écarts négatives que l'on peut trouver dûs probablement à une erreur de saisie 

netflixData.H.Movies <- subset(netflixData.H.Movies,(as.numeric(netflixData.H.Movies$date_added)-as.numeric(netflixData.H.Movies$release_year))>=0)


#TRAITEMENT QUATRIEME QUESTION

# Pour la quatrième question, on doit formater les durées des films et des saisons, pour les films, on enlève le "min" et pour les saisons
# on va convertir le nombre de saisons en min, en partant du principe que 1 saison c'est 12 épisodes de 30 minutes.
# On n'a développé une fonction pour ça, la fonction "duration_formatting"


netflixData.A$new_duration <- sapply(netflixData.A$duration, duration_formatting)

# pour NetflixData.H :
# On peut maintenant appliquer la fonction sur netflixData.H également et créer cette nouvelle colonne

netflixData.H$new_duration <- sapply(netflixData.H$duration, duration_formatting)









