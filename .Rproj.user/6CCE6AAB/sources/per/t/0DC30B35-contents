# En tête ---- 

#Desc : Prépa TP1 ADM
#Date: 02/10/2024
#Auteur : Louison GILLET

# Partie 1 : dataframe ---- 

Datagenus <- read.csv("~/Louison/M1/ADM/DM1/data/Datagenus.csv", sep=";")

# On enlève la ligne 1001 
data <- Datagenus[1:1000,]
str(data)
summary(data)
head(data)

##### Question 1 #### 

# Sélectionner les colonnes des espèces
especes <- paste0("gen", 1:27)
#especes

# Calculer la densité de peuplement pour chaque espèce
densite_especes <- data[especes] / data$surface
head(densite_especes)

# Moyenne des densités par parcelle 
densite_especes$densite_moyenne <- rowMeans(densite_especes)
head(densite_especes)

# Centrer les densités 
densite_c <- densite_especes - densite_especes$densite_moyenne
head(densite_c)
rowMeans(densite_c)

# Réduire les densités 
densite_cr <- densite_c / sd

# Calculer l'inertie totale du nuage de points
inertie_totale <- sum(densite_cr^2) 
inertie_totale  # Doit être proche de 27


