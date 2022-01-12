#1. Est-ce que c’est vrai que le temps moyen entre la sortie du film et son ajout au carnet Netflix s’est
#   raccourci ?

# On commence par calculer le temps moyen avant 2021 :

tempsMoyenSortieFilmAvant2021 <- sum(as.numeric(netflixData.H.Movies$date_added)-as.numeric(netflixData.H.Movies$release_year))/nrow(netflixData.H.Movies)

# On calcule le temps moyen pour l'année 2021 :

tempsMoyenSortieFilmEn2021 <- sum(as.numeric(netflixData.A.Movies$date_added)-as.numeric(netflixData.A.Movies$release_year))/nrow(netflixData.A.Movies)

# L'hypothèse n'est donc pas validé, on remarque que le temps moyen entre la sortie du film et son ajout a, au contraire, augmenté.

# 2. A votre avis quelle est la cause de la tendance que vous remarquez au point précèdent ? Proposez une ´
#    solution pour pouvoir donner une reponse plus réaliste au point 1) et refaites les calculs en ce sens.
