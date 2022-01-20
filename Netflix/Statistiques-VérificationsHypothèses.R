#1. Est-ce que c’est vrai que le temps moyen entre la sortie du film et son ajout au carnet Netflix s’est
#   raccourci ?

# H pour calculer les quantités statistique remarquables
# A est l'échantillon pour les tests statistiques


# H0 : moyenne entre la sortie en salle et sur Netxflix en 2021 (7.7) > Moyenne entre la sortie en salle et sur netflix avant 2021 (5.3)
# H1 : Moyenne entre la sortie en salle et sur netflix avant 2021 (5.3) > moyenne entre la sortie en salle et sur Netxflix en 2021 (7.7)


# On commence par calculer le temps moyen avant 2021 :

tempsMoyenSortieFilmAvant2021 <- sum(as.numeric(netflixData.H.Movies$date_added)-as.numeric(netflixData.H.Movies$release_year))/nrow(netflixData.H.Movies)


# On calcule la moyenne, l'écart type ...


netflixData.A.Movies$ecart <- as.numeric(netflixData.A.Movies$date_added)-as.numeric(netflixData.A.Movies$release_year)


moyenne <- mean(netflixData.A.Movies$ecart)

variance <- var(netflixData.A.Movies$ecart)

ecarttype <- sd(netflixData.A.Movies$ecart)

t <- (moyenne - tempsMoyenSortieFilmAvant2021)/(ecarttype/sqrt(nrow(netflixData.A.Movies)))

# intervalle de confiance 

t.test(netflixData.A.Movies$ecart, conf.level=0.95)

#t appartient à l'intervalle de confiance donc l'hypothèse H0 est validé. H1 n'est pas validé 

# On calcule le temps moyen pour l'année 2021 :

tempsMoyenSortieFilmEn2021 <- sum(as.numeric(netflixData.A.Movies$date_added)-as.numeric(netflixData.A.Movies$release_year))/nrow(netflixData.A.Movies)

# L'hypothèse n'est donc pas validé, on remarque que le temps moyen entre la sortie du film et son ajout a, au contraire, augmenté.

# 2. A votre avis quelle est la cause de la tendance que vous remarquez au point précèdent ? Proposez une ´
#    solution pour pouvoir donner une reponse plus réaliste au point 1) et refaites les calculs en ce sens.
# etude de corélation ))