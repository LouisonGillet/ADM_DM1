# En tête ---- 

#Desc : TP1 ADM
#Date: 02/10/2024
#Auteur : Wahel/Louison 

# Partie 1 : dataframe ---- 

# Lecture des données
data <- read.csv("data/Datagenus.csv", sep = ";", encoding = "UTF-8")

# Sélection des colonnes : espèces (gen1 à gen27), surface, forêt, géologie
colonnes_selectionnees <- c(paste0("gen", 1:27), "surface", "forest", "geology")
donnees_filtrees <- data[, colonnes_selectionnees]

#### 1. Calcul de la densité de peuplement pour chaque espèce (gen1 à gen27) #### 
# On divise le nombre d'individus d'une espèce par la surface de la parcelle pour obtenir la densité
densite_peuplement <- as.matrix(donnees_filtrees[, paste0("gen", 1:27)]) / donnees_filtrees$surface
print("Calcul de la densité de peuplement terminé")

#### 2. Centrage et réduction avec des opérations matricielles ####

# Calcul des moyennes pour chaque colonne (espèce)
moyennes_especes <- colMeans(densite_peuplement)

# Calcul des écarts-types pour chaque colonne (espèce)
n <- nrow(densite_peuplement)
ecarts_types_especes <- sqrt(colSums((densite_peuplement - matrix(moyennes_especes, nrow = n, ncol = ncol(densite_peuplement), byrow = TRUE))^2) / (n - 1))

# Centrage et réduction : on soustrait les moyennes et on divise par l'écart-type
densite_centree_reduite <- (densite_peuplement - matrix(moyennes_especes, nrow = n, ncol = ncol(densite_peuplement), byrow = TRUE)) / 
  matrix(ecarts_types_especes, nrow = n, ncol = ncol(densite_peuplement), byrow = TRUE)

print("Centrage et réduction des densités terminé")

#### 3. Vérification : Barycentre à l'origine ####

# Vérification si les moyennes des colonnes centrées-réduites sont proches de zéro
moyennes_apres_centrage <- colMeans(densite_centree_reduite)
barycentre_a_l_origine <- all(abs(moyennes_apres_centrage) < 1e-10)
print(paste("Le barycentre est-il à l'origine ? :", barycentre_a_l_origine))

#### 4. Vérification : Inertie totale ####
# Calcul des variances après centrage et réduction (elles devraient être proches de 1)
variances_apres_centrage <- colSums(densite_centree_reduite^2) / (n - 1)

# Inertie totale = somme des variances
inertie_totale <- sum(variances_apres_centrage)
print(paste("Inertie totale (somme des variances) :", inertie_totale))

#### 5. Calcul des barycentres et normes euclidiennes des types forestiers ####

# Identification des types forestiers
types_forestiers <- unique(donnees_filtrees$forest)

# Création d'une matrice pour les poids, barycentres et normes euclidiennes carrées
poids_forestiers <- numeric(length(types_forestiers))
barycentres_forestiers <- matrix(0, nrow = length(types_forestiers), ncol = ncol(densite_centree_reduite))
normes_euclidiennes_carre <- numeric(length(types_forestiers))

# Calcul par opérations matricielles
for (i in 1:length(types_forestiers)) {
  # Filtrer les parcelles appartenant à chaque type forestier
  parcelles_type_forestier <- densite_centree_reduite[donnees_filtrees$forest == types_forestiers[i], ]
  
  # Calculer le poids : proportion des parcelles de ce type
  poids_forestiers[i] <- nrow(parcelles_type_forestier) / nrow(densite_centree_reduite)
  
  # Calcul du barycentre pour chaque type forestier (moyenne par espèce)
  barycentres_forestiers[i, ] <- colMeans(parcelles_type_forestier)
  
  # Calcul de la norme euclidienne carrée pour chaque type forestier
  normes_euclidiennes_carre[i] <- sum(barycentres_forestiers[i, ]^2)
}

# Affichage des résultats
print("Poids des types forestiers :")
print(poids_forestiers)

print("Barycentres des types forestiers :")
print(barycentres_forestiers)

print("Normes euclidiennes carrées des barycentres :")
print(normes_euclidiennes_carre)

#### 6. Calcul de l'inertie inter-types ####
inertie_inter_types <- sum(poids_forestiers * normes_euclidiennes_carre)
print(paste("Inertie inter-types :", inertie_inter_types))

#### 7. Calcul du R2 (coefficient de détermination) ####
R2 <- inertie_inter_types / inertie_totale
print(paste("Coefficient de détermination R2 :", R2))

#### 8. Calcul du pourcentage d'information expliqué par la partition ####
pourcentage_information <- R2 * 100
print(paste("Pourcentage d'information expliqué par la partition :", pourcentage_information, "%"))

#### 9. Calcul de la variance totale et de la variance inter-types pour chaque espèce ####
variance_totale_par_espece <- colSums(densite_centree_reduite^2) / (n - 1)
variance_inter_types_par_espece <- numeric(ncol(densite_centree_reduite))

# Boucle pour chaque espèce
for (j in 1:ncol(densite_centree_reduite)) {
  # On calcule la variance inter-types pour l'espèce j
  variance_inter_types_par_espece[j] <- sum(poids_forestiers * (barycentres_forestiers[, j]^2))
}

#### 10. Calcul du R2 pour chaque espèce ####
R2_par_espece <- variance_inter_types_par_espece / variance_totale_par_espece

#### 11. Identification des espèces les plus et les moins liées au type forestier ####
especes_most_liees <- names(sort(R2_par_espece, decreasing = TRUE))[1:5]  # Les 5 espèces les plus liées
especes_least_liees <- names(sort(R2_par_espece, decreasing = FALSE))[1:5]  # Les 5 espèces les moins liées

# Affichage des résultats
print("R2 par espèce (densité de peuplement) :")
print(R2_par_espece)

print("Les 5 espèces les plus liées au type forestier :")
print(especes_most_liees)

print("Les 5 espèces les moins liées au type forestier :")
print(especes_least_liees)

#### 12. Calcul des R2 pour chaque espèce ####
R2_par_espece <- variance_inter_types_par_espece / variance_totale_par_espece

#### 13. Calcul de la moyenne arithmétique des R2 des espèces ####
moyenne_R2_especes <- mean(R2_par_espece)

#### 14. Vérification que le R2 de la partition est égal à la moyenne des R2 des variables ####
verification_R2 <- R2 == moyenne_R2_especes

# Affichage des résultats
print(paste("R2 de la partition :", R2))
print(paste("Moyenne des R2 des espèces :", moyenne_R2_especes))
print(paste("Le R2 de la partition est-il égal à la moyenne des R2 des espèces ? :", verification_R2))