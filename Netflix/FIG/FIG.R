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
plot(a, xlab = "écart entre la date de production et sortie du film en année",ylab = "nombre de films",main="Le nombre de film selon son écart avant 2021")

#Affichage des écarts entre la date de production et sortie pour les films après 2021 :

b <-  table(netflixData.A.Movies$Dif)
plot(b, xlab = "écart entre la date de production et sortie du film en année",ylab = "nombre de films", main="nombre de films",main="Le nombre de film selon son écart en 2021")


#Affichage des deux moyennes des écarts entre la date de production et sortie des films
x = c(mean(netflixData.H.Movies$Dif),mean(netflixData.A.Movies$Dif))

barplot(x,col=c(2,3),legend.text = c("avant 2021","après 2021"),args.legend=list(x="topleft"),main="Moyenne des écarts entre 
        date de production et sortie des films") 


# Question 2 : 
# 2. A votre avis quelle est la cause de la tendance que vous remarquez au point précèdent ? Proposez une ´
#    solution pour pouvoir donner une reponse plus réaliste au point 1) et refaites les calculs en ce sens.
# etude de corélation ))

#Affichage du pourcentage de chaque rating 
#Avant 2021

proH <- prop.table(table(netflixData.H.Movies$rating)) * 100

barplot(proH,main="Le pourcentage de chaque rating pour la base de données avant 2021")

#En 2021
proA <- prop.table(table(netflixData.A.Movies$rating)) * 100
barplot(proA,main="Le pourcentage de chaque rating pour la base de données en 2021")


#Affichage du pourcentage de chaque type: 
#En 2021

typeA <- prop.table(table(netflixData.A$type)) * 100

barplot(typeA,main="Le pourcentage de TV Show et film en 2021")

#Avant 2021
typeH <- prop.table(table(netflixData.H$type)) * 100

barplot(typeH,main="Le pourcentage de TV Show et film avant 2021")


#Affichage des moyennes sur la durée des fims en minute 

e <- mean(netflixData.A.Movies$min)
f <- mean(netflixData.H.Movies$min)

y = c(mean(netflixData.H.Movies$min),mean(netflixData.A.Movies$min))

barplot(y,col=c(2,3),legend.text = c("avant 2021","en 2021"),args.legend=list(x="topright"),main="La durée moyenne des films") 



# Question 3 : 
# 3.Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) ? 

#Affichage de la proportion de showTV et Movie avant 2021
proportionTypeH <- prop.table(table(netflixData.H$type)) * 100

pie(proportionTypeH,col=c("#AAFFAA","#FFEE44"),main="La proportion des Movies et TV avant 2021",cex=1.5)


#Affichage de la proportion de showTV et Movie en 2021
proportionTypeA <- prop.table(table(netflixData.A$type)) * 100

pie(proportionTypeA,col=c("#AAFFAA","#FFEE44"),main="La proportion des Movies et TV en 2021",cex=1.5)


# 4.même question que la précédente mais en prenant en compte la durée en minutes 
# Est ce que c'est vrai que la proportion de show télévisés qui sont mis au programme a considérablement augmenté 
# par rapport aux autres programmes (films, séries) en minutes ? 

proportionTvShowMinutes.H <- (totalTvShowMinutes.H/totalMinutes.H)*100


proportionTvShowMinutes.A <- (totalTvShowMinutes.A/totalMinutes.A)*100

z = c(proportionTvShowMinutes.H,proportionTvShowMinutes.A)

barplot(z,col=c(2,3),legend.text = c("avant 2021","après 2021"),args.legend=list(x="topright"),main="La proportion de TV Show en minutes") 
