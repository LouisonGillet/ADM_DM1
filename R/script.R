# En tête ---- 

#Desc : DM1 ADM
#Date: 02/10/2024
#Auteur : EL MAZZOUJI Wahel & GILLET Louison 

# Dataframe ---- 

Datagenus <- read.csv("data/Datagenus.csv", sep=";")
#str(Datagenus)
data <- Datagenus[1:1000,] # On ne prend pas la ligne 1001 

# Partie 1 ---- 

### 1.0 Sélection des colonnes des espèces

especes <- paste0("gen", 1:27)

#### 1.1 Calcul de la densité de peuplement pour chaque espèce (gen1 à gen27) ####

densite_peuplement <- as.matrix(data[especes] / data$surface) # Conversion en matrice 

#### 1.2 Centrage et réduction avec des opérations matricielles ####

### Calcul des moyennes pour chaque espèce (colonne)

moyennes_especes <- (colMeans(densite_peuplement))

### Calcul des écarts-types pour chaque espèce (colonne) 
n <- nrow(densite_peuplement)
p <- ncol(densite_peuplement)
mat_moyenne <- matrix(moyennes_especes, nrow = n, ncol = p, byrow = TRUE)
#remplit chaque ligne avec la densité de la colonne 

sd_especes <- sqrt(colSums((densite_peuplement - mat_moyenne)^2) / (n - 1)) #racine de la variance sans-biais 

### Centrage et réduction : on soustrait les moyennes et on divise par l'écart-type

mat_sd <- matrix(sd_especes, nrow = n, ncol = p, byrow = TRUE)
#idem avec l'écart-type 

densite_centree_reduite <- (densite_peuplement - mat_moyenne) / mat_sd 

#### 1.3 Barycentre et inertie #### 

### 1.3.1 Barycentre à l'origine (moyennes des colonnes proches de 0)
#summary(densite_centree_reduite)
moyennes_apres_centrage <- colMeans(densite_centree_reduite)
test_barycentre_a_l_origine <- all(abs(moyennes_apres_centrage) < 1e-10)
test_barycentre_a_l_origine

### 1.3.2 Inertie totale (variances des colonnes proches de 1)
# Variance par colonne 
variances_apres_centrage <- colSums(densite_centree_reduite^2) / (n-1)
#variances_apres_centrage

# Inertie totale = somme des variances
inertie_totale <- sum(variances_apres_centrage)
#inertie_totale 


