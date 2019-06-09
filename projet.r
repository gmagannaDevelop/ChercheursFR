################## UNIVERSITE DE MONS  ##################################
################## FACULTE DE SCIENCES ##################################
# COURS : Statistique multidimensionnelle
# Professeur : Michel VOUE
# Etudiants : Simon MATTENS
#             Joachim SNEESSENS
#             Gustavo MAGAÑA LOPEZ


## libraries necessaires :
library(ade4)
library(xlsx)
library(factoextra)
library(dplyr)
library(FactoMineR)

## Fonction pour nettoyer les donnees :
clean.names <- function(x){
  strsplit(x, 
           split = "(",
           fixed = TRUE)[[1]][[1]]
}

# Importation des donnees depuis le fichier :
dataSet <- read.xlsx("data.xlsx",sheetIndex=1)

# Construction de la matrice (DataFrame) :
chercheurs <- dataSet
row.names(chercheurs) <- chercheurs$geo.time
row.names(chercheurs) <- unlist(lapply(row.names(chercheurs), clean.names))
chercheurs <- subset(chercheurs, select = -c(geo.time))

summary(chercheurs)

# On centre les donnees
chercheurs.scaled <- scale(chercheurs)

# Effet de la transformation sur la distribution des donnees :
boxplot(chercheurs, main = "avant centrage des donnees")
boxplot(chercheurs.scaled, main = "apres centrage des donnees")

#matrice des correlations :
chercheurs.corr <- cor(chercheurs.scaled)

# pourcentage d'inertie de chacun des axes, pour pouvoir decider combien
# d'axes nous allons conserver pour faire analyse
ch.fr.scaled.cor.eigen <- eigen(chercheurs.corr)
ch.fr.scaled.inertia <- ch.fr.scaled.cor.eigen$values / sum(ch.fr.scaled.cor.eigen$values)
ch.fr.scaled.inertia 
barplot(ch.fr.scaled.cor.eigen$values, main = "Valeurs propres")
# Remarquons qu avec les 2 premiers axes on apporte suffisement d inertie : surtout le premier.

# ACP au moyen de ade4::dudi.pca
ch.fr.pca <- dudi.pca(chercheurs.scaled, scann=FALSE, nf = 2)
fviz_eig(ch.fr.pca) # C'est comme le barplot mais plus jolie.

ch.fr.var <- get_pca_var(ch.fr.pca)
ch.fr.var

fviz_pca_ind(
  ch.fr.pca,
  col.ind = "cos2", # Color by the quality of representation
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(
  ch.fr.pca, 
  repel = TRUE,
  col.var = "#2E9FDF", # Variables color
  col.ind = "#696969"  # Individuals color
)

# Contributions of variables to PC1
fviz_contrib(ch.fr.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(ch.fr.pca, choice = "var", axes = 2, top = 10)

# Contributions of individuals to PC1
fviz_contrib(ch.fr.pca, choice = "ind", axes = 1, top = 10)
# Contributions of individuals to PC2
fviz_contrib(ch.fr.pca, choice = "ind", axes = 2, top = 10)

ch.fr.hcps <- HCPC(PCA(chercheurs.scaled))


if (T){
  # On a besoin de faire clustering pour faire cette visualisation
fviz_pca_ind(ch.fr.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = ch.fr.hcps$data.clust$clust, # color by groups
             palette = rainbow(length(ch.fr.hcps$data.clust$clust)),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
}

# les variables correlees sont tres proche l une de l autre
# plus la distance a l origine est frande meilleure est la projection
# co = column coordinate
# c1 = column normed scores ie principal axes
s.corcircle(ch.fr.pca$co)


