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
library(corrplot)
library(ggplot2)
library(ez)

## Fonction pour nettoyer les donnees :
clean.names <- function(x){
  strsplit(x, 
           split = "(",
           fixed = TRUE)[[1]][[1]]
}

clean.years <- function(x){
  as.numeric(
    strsplit(x, "X")[[1]][[2]]
  )
}

ggplotRegression <- function (fit, title) {
# Fonction originale : https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/  
  require(ggplot2)
  
  print(
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red") +
      labs(title = paste(title, "  :   ",
                         "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                         " P-value =",signif(summary(fit)$coef[2,4], 5)
                        )
          )
    )
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

### Tout ce qu'il y a dans le if (FALSE)
### c'est l'ancien analyse que l'on avait fait.
### Cette mise a jour commence a partir de la ligne 166
if (FALSE){
# Effet de la transformation sur la distribution des donnees :
boxplot(chercheurs, main = "avant centrage des donnees")
boxplot(chercheurs.scaled, main = "apres centrage des donnees")

#matrice des correlations :
chercheurs.corr <- cor(chercheurs.scaled)
corrplot(chercheurs.corr)

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

# On obtient les parametres diagnostiques :
ch.fr.var <- get_pca_var(ch.fr.pca)
ch.fr.var

# La qualite de la representation de chaque individu (region de la France)
fviz_pca_ind(
  ch.fr.pca,
  col.ind = "cos2", # Color by the quality of representation
  gradient.cols = rainbow(10),
  repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(ch.fr.pca, col.ind="cos2") +
  scale_color_gradient2(low="red", mid="blue",
                        high="green", midpoint=0.85)

fviz_pca_biplot(ch.fr.pca, col.ind="contrib", repel = TRUE) +
  scale_color_gradient2(low="blue", mid="green",
                        high="red", midpoint=15)

# On peut voir les vecteurs des anciennes variables sur les composants principaux.
fviz_pca_biplot(
  ch.fr.pca, 
  repel = TRUE,
  col.var = "#2E9FDF", # Variables color
  col.ind = "#696969"  # Individuals color
)

# Contributions des variables au CP1
fviz_contrib(ch.fr.pca, choice = "var", axes = 1, top = 10, title="Contributions à la 1ère CP")
# Contributions des variables au CP2
fviz_contrib(ch.fr.pca, choice = "var", axes = 2, top = 10, title="Contributions à la 2ème CP")

# Contributions des individus (regions) au CP1
fviz_contrib(ch.fr.pca, choice = "ind", axes = 1, top = 10, title="Contributions à la 1ère CP")
# Contributions des individus (regions) to CP2
fviz_contrib(ch.fr.pca, choice = "ind", axes = 2, top = 10, title="Contributions à la 2ème CP")

# Classification hierarchique :
ch.fr.pca2 <- PCA(chercheurs.scaled, graph = FALSE)
ch.fr.hcpc <- HCPC(ch.fr.pca2, nb.clust = 3, graph = FALSE)

fviz_dend(ch.fr.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(ch.fr.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

plot(ch.fr.hcpc, choice = "3D.map")

# Visualisation de la classification hierarchique sur le ACP plot.
fviz_pca_ind(ch.fr.pca,
             repel = TRUE,
             geom.ind = c("point", "text"), # show points only (nbut not "text")
             col.ind = ch.fr.hcpc$data.clust$clust, # color by groups
             palette = c("red", "blue", "green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)


# les variables correlees sont tres proche l une de l autre
# plus la distance a l origine est frande meilleure est la projection
# co = column coordinate
# c1 = column normed scores ie principal axes
s.corcircle(ch.fr.pca$co)
}


### 
### Extra : analyse de la stabilite temporelle du profil.
### Au moyen de regression lineaire.
my.var <- apply(chercheurs, 2, mean)
my.var <- as.data.frame(my.var)
my.var$year <- unlist(lapply(rownames(my.var), clean.years))
rownames(my.var) <- NULL
colnames(my.var) <- c("moyenne", "annee")

regression1 <- lm(moyenne ~ annee, data = my.var)
summary(regression1)

#ggplot(my.var, aes(x=annee, y=moyenne)) +
 # geom_point(shape=1) +
  #  geom_smooth(method=lm)
ggplotRegression(regression1, "Moyenne de toutes les regions")


## Nous trouvons une evolution lineaire au fil du temps, ce qui est contraire
## a notre premiere conclusion. Meme si c'est vrai que l'on trouve plus
## de variance dans les groupes qu'au fil du temps, on peut voir une croissance
## stable et significative du pourcentage des chercheurs en fonction du temps.

my.years <- unlist(
              lapply(colnames(chercheurs), clean.years)
            )

# Faire une regression lineaire pour chaque region
regressions <- vector("list", length = length(rownames(chercheurs)))
for (i in 1:length(rownames(chercheurs))){
  regressions[[i]] <- lm(as.numeric(chercheurs[i, ]) ~ my.years)
}

# Afficher les resultats
# !!!!!!! ATTENTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# Effacer tous les plots existents avant d'executer !
# 22 plots seront crees !
for (i in 1:length(rownames(chercheurs))){
  ggplotRegression(regressions[[i]], rownames(chercheurs)[i])
}

# On peut bien voir qu'il y a des regions
# ou l'on trouve un comportement de croissance claire
# tandis qu'autres varient beaucoup.
