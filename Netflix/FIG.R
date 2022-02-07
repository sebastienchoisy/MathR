#Installation des packages 
install.packages("plyr")
install.packages("dplyr")
install.packages("plotly")
library(plyr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(plotly)

# Question 1 : 
#1. Est-ce que c’est vrai que le temps moyen entre la sortie du film et son ajout au carnet Netflix s’est
#   raccourci ?

#Affichage des écarts entre la date de production et sortie pour les films avant 2021 :

a <-  table(netflixData.H.Movies$Dif)
plot(a, xlab = "écart entre la date de production et sortie du film en année",ylab = "nombre de films")

#Affichage des écarts entre la date de production et sortie pour les films après 2021 :

b <-  table(netflixData.A.Movies$Dif)
plot(b, xlab = "écart entre la date de production et sortie du film en année",ylab = "nombre de films")

x = c(mean(netflixData.H.Movies$Dif),mean(netflixData.A.Movies$Dif))

barplot(x,col=c(2,3),legend.text = c("avant 2021","après 2021"),args.legend=list(x="topleft"),main="Moyenne des écarts entre 
        date de production et sortie des films") 


# Question 2 : 
# 2. A votre avis quelle est la cause de la tendance que vous remarquez au point précèdent ? Proposez une ´
#    solution pour pouvoir donner une reponse plus réaliste au point 1) et refaites les calculs en ce sens.
# etude de corélation ))


# on regarde le pourcentage de chaque rating pour la base de données avant 2021
c <- prop.table(table(netflixData.H.Movies$rating)) * 100

plot(c,xlab = "différent rating",ylab = "nombre de films en pourcentage", main="Le pourcentage de film avant 2021 selon le rating")

# on regarde le pourcentage de chaque rating pour la base de données en 2021
d <- prop.table(table(netflixData.A.Movies$rating)) * 100

plot(d,xlab = "différent rating",ylab = "nombre de films en pourcentage", main="Le pourcentage de film en 2021 selon le rating")

# Etude de la durée en minute et date_added
e <- mean(netflixData.A.Movies$min)
f <- mean(netflixData.H.Movies$min)

y = c(mean(netflixData.H.Movies$min),mean(netflixData.A.Movies$min))

barplot(y,col=c(2,3),legend.text = c("avant 2021","après 2021"),args.legend=list(x="topright"),main="Moyenne de la durée des films") 


# Question 3 : 
# 3.Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) ? 


# 4.même question que la précédente mais en prenant en compte la durée en minutes 
# Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) en minutes ? 