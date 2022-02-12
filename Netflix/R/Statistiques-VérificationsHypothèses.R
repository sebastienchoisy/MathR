#1. Est-ce que c’est vrai que le temps moyen entre la sortie du film et son ajout au carnet Netflix s’est
#   raccourci ?

# H pour calculer les quantités statistique remarquables
# A est l'échantillon pour les tests statistiques

#Par convention, on prend l'hyothèse null: mu1 = mu2
# H0 : Le temps moyen entre la sortie du film et son ajout au carnet Netflix n'est pas raccourci
# Moyenne entre la sortie en salle et sur Netxflix en 2021 = Moyenne entre la sortie en salle et sur netflix avant 2021


# H1 : Le temps moyens entre la sortie du film et son ajout au carnet Netflix s'est raccourci 
# Moyenne entre la sortie en salle et sur netflix avant 2021 > Moyenne entre la sortie en salle et sur Netxflix en 2021 


# La variable pour le temps de sortie du film est release year
# La variable pour le moment de son ajout au carnet Netflix est date_added
# On commence par calculer le temps moyen avant 2021 :
# Il faut calculer leur différence, notée diff comme variable


netflixData.H.Movies$Dif <- as.numeric(netflixData.H.Movies$date_added) - netflixData.H.Movies$release_year
netflixData.A.Movies$Dif <- as.numeric(netflixData.A.Movies$date_added) - netflixData.A.Movies$release_year

# On calcule la moyenne, l'écart type ...
# avant 2021:

summary(netflixData.H.Movies$Dif)
mean(netflixData.H.Movies$Dif)
var(netflixData.H.Movies$Dif)
sd(netflixData.H.Movies$Dif)

# en 2021:

summary(netflixData.A.Movies$Dif)
mean(netflixData.A.Movies$Dif)
var(netflixData.A.Movies$Dif)
sd(netflixData.A.Movies$Dif)


# D'après les statistiques, il semble qu'en moyenne, Dif est plus petit avant 2021 (moyenne 5.34) qu'en 2021 (moyenne 7.696)

# Par ailleurs, leurs variances ne sont pas égaux, on va ajouter cette information dans le test de Student
# intervalle de confiance

t.test(x=netflixData.H.Movies$Dif, 
       y=netflixData.A.Movies$Dif, 
       alternative = "greater",  # car H1 est le temps moyens entre la sortie du film et son ajout au carnet Netflix s'est raccourci
       var.equal = FALSE,        # car les variances ne sont pas égaux
       paired = FALSE,      
       conf.level=0.95)

# La p-valeur est > 0,05, donc on ne peut pas rejeter H0 que le temps moyen entre la sortie du film et son ajout ne change pas pour les années avant 2021 et en 2021. 
# Le temps moyen entre la sortie du film et son ajout au carnet Netflix n'est pas raccourci

# 2. A votre avis quelle est la cause de la tendance que vous remarquez au point précèdent ? Proposez une ´
#    solution pour pouvoir donner une reponse plus réaliste au point 1) et refaites les calculs en ce sens.
# etude de corélation ))

# # Nous avons remarqué qu'en fait le temps moyen entre la sortie du film et son ajout s'est prolongé au lieu de raccourci. 

# Par convention, on prend hypothèse null: mu1 = mu2
# H0 : Le temps moyen entre la sortie du film et son ajout au carnet Netflix n'est pas raccourci, 
# moyenne entre la sortie en salle et sur Netflix en 2021 = Moyenne entre la sortie en salle et sur netflix avant 2021

# H1 : le temps moyens entre la sortie du film et son ajout au carnet Netflix est prolongé
# Moyenne entre la sortie en salle et sur netflix avant 2021 < moyenne entre la sortie en salle et sur Netflix en 2021

# on refait le test dans ce sens
t.test(x=netflixData.H.Movies$Dif, 
       y=netflixData.A.Movies$Dif, 
       alternative = "less",  # car H1 est le temps moyens entre la sortie du film et son ajout au carnet Netflix est prolongé
       var.equal = FALSE,     # car les variances ne sont pas égaux
       paired = FALSE,      
       conf.level=0.95)

# La p-valeur est < 0,05, donc on rejète H0 que le temps moyen entre la sortie du film et son ajout ne change pas pour les années avant 2021 et en 2021. 
# Moyenne entre la sortie en salle et sur netflix avant 2021 < moyenne entre la sortie en salle et sur Netxflix en 2021

# Afin de comprendre la cause de ce changement, nous allons effectuer une étude des relations entre d'autres variables et la différence (Dif)

# Etude de rating et date_added #######
# avant 2021
# on regarde le pourcentage de chaque rating pour la base de données avant 2021
prop.table(table(netflixData.H.Movies$rating)) * 100

# en 2021
# on regarde le pourcentage de chaque rating pour la base de données en 2021
prop.table(table(netflixData.A.Movies$rating)) * 100

# il y a différence, mais en 2021, il n'y a pas beaucoup plus de films "non-appropriés" qu'avant 2021

# Etude de type et date_added #######
# les pourcentages de TV Show et film en 2021: 
prop.table(table(netflixData.A$type)) * 100

# les pourcentages de TV Show et film avant 2021: 
prop.table(table(netflixData.H$type)) * 100

# On remarque qu'il y a plus de TV Show que Movie en pourcentage
# On va réaliser un test dans la question 3 afin de confirmer cette hypothèse. 


# Etude de la durée en minute et date_added #########

# séparer valeur et "min", puis convertir en numérique
netflixData.A.Movies$min <- as.numeric(sapply(strsplit(x=netflixData.A.Movies$duration, split = " "),`[`, 1))
netflixData.H.Movies$min <- as.numeric(sapply(strsplit(x=netflixData.H.Movies$duration, split = " "),`[`, 1))

summary(netflixData.A.Movies$min)
summary(netflixData.H.Movies$min)

# On remarque que la moyenne des films durent plus longtemps en 2021
# 2021 la durée moyenne des films est 102.8
# Avant 2021 la durée moyenne des films est de 98.95

# calculer la corrélation entre la durée en minute et Dif : 
# on concatène d'abord les deux bases de données
netflixData.all.Movies <- rbind(netflixData.A.Movies, netflixData.H.Movies)

cor.test(netflixData.all.Movies$min, 
         netflixData.all.Movies$Dif, 
         use="complete.obs",
         method = "pearson")

# Le coefficient de corrélation entre la variable durée en minute et le dif est de 0,22. 
# La p-valeur associée est < 2.2e-16
# Donc ces deux variables sont faiblement positivement corrélée.
# Mais il est à noter que la corrélation n'implique pas la causalité.  

# On va réaliser un test dans la question 4 afin de confirmer l'hypothèse que la durée en minute a augmenté. 


# 3.Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) ? 

# On regarde la proportion des programmes: 
# avant 2021: 
prop.table(table(netflixData.H$type)) * 100
# Il y a uniquement deux types de programmes: Movie (70%) et TV Show(30%). 

# en 2021: 
prop.table(table(netflixData.A$type)) * 100
# Il y a les mêmes types de programmes: Movie (66%) et TV Show(34%). 


# Hypothèse: 
# H0: la proportion de TV Show reste identiquement avant 2021 (p1) et en 2021 (p2) : p1 = p2
# H1: la proportion de TV Show a augmenté en 2021 qu'avant 2021 : p1 < p2 (less) 

# Pour cela, on va effectuer test de proportion

# d'abord, on calcule le nombre de TV Show dans la base de données avant 2021 et celui dans la base de données en 2021 
nb.TVShow.H <- sum(netflixData.H$type=="TV Show")
nb.TVShow.H

nb.TVShow.A <- sum(netflixData.A$type=="TV Show")
nb.TVShow.A

# ensuite, il faut aussi compter le nombre total de la base de données avant 2021 et celui en 2021
nb.total.H <- nrow(netflixData.H)
nb.total.H

nb.total.A <- nrow(netflixData.A)
nb.total.A


# On peut maintenant effectuer test de proportion: 
prop.test(x = c(nb.TVShow.H, nb.TVShow.A), 
          n = c(nb.total.H, nb.total.A), 
          alternative = "less")   # "less" car H1 est proportion de TV Show avant 2021 < celle en 2021

# La p-valeur est 0,001174 < 0,05
# donc on rejète hypothèse nulle que la proportion de TV Show est identique avant 2021 et en 2021.
# la proportion de TV Show a augmenté en 2021 qu'avant 2021


# 4.même question que la précédente mais en prenant en compte la durée en minutes 
# Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) en minutes ? 


# On regarde la proportion des programmes: 
# avant 2021:

# On calcule le total de minutes pour H :

totalMinutes.H <- sum(as.numeric(netflixData.H$new_duration))

# On calcule le total de minutes pour les TvShow de H d'une part et pour les Movies de H d'autre part :

totalTvShowMinutes.H <- sum(as.numeric(netflixData.H[netflixData.H$type == "TV Show", 13]))
totalMovieMinutes.H <- sum(as.numeric(netflixData.H[netflixData.H$type == "Movie", 13]))

# On peut maintenant calculer la proportion:

proportionTvShowMinutes.H <- (totalTvShowMinutes.H/totalMinutes.H)*100
proportionMovieMinutes.H <- (totalMovieMinutes.H/totalMinutes.H)*100

# Il y a 72.6% de TvShows et 27.4% de Movies avant 2021 en minutes.

# On refait les calculs identiques pour l'année 2021 :

totalMinutes.A <- sum(as.numeric(netflixData.A$new_duration))

totalTvShowMinutes.A <- sum(as.numeric(netflixData.A[netflixData.A$type == "TV Show", 13]))
totalMovieMinutes.A <- sum(as.numeric(netflixData.A[netflixData.A$type == "Movie", 13]))

proportionTvShowMinutes.A <- (totalTvShowMinutes.A/totalMinutes.A)*100
proportionMovieMinutes.A <- (totalMovieMinutes.A/totalMinutes.A)*100

# Il y a 77.7% de TvShows et 22.3% de Movies en minutes.

# Hypothèse: 
# H0: la proportion de TV Show en minutes reste identique avant 2021 (p1) et en 2021 (p2), i.e. p1 = p2
# H1: la proportion de TV Show en minutes a augmenté en 2021 qu'avant 2021, i.e. p1 < p2 (less) 


prop.test(x = c(totalTvShowMinutes.H,totalTvShowMinutes.A),
          n = c(totalMinutes.H,totalMinutes.A),
          alternative="less")

# La p-valeur est 2.2e-16 < 0,05
# On peut donc rejeter l'hypothèse nulle que la proportion de TV Show en minutes reste identique
# avant 2021 et en 2021




