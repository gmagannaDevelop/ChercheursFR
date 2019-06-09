library(ade4)
library(xlsx)

dataSet <- read.xlsx("data.xlsx",sheetIndex=1)
summary(dataSet)


#je transforme la premiere colonne de string en nombre
dataSet$geo.time <- as.numeric(dataSet$geo.time)

#je centre mes donnees
dataScaled <- scale(dataSet)

boxplot(dataSet, main = "avant centrage des donnees")
boxplot(dataScaled, main = "apres centrage des donnees")

#matrice des correlations
MatrixOfCorrelation <- cor(dataScaled)
MatrixOfCorrelation


# pourcentage d inertie de chacun des axes, pour pouvoir decider combien
#d axes nous allons conserver pour faire analyse

dataScaled_Cor_Eigen <- eigen(MatrixOfCorrelation)
dataScaled_inertia <- dataScaled_Cor_Eigen$values / sum(dataScaled_Cor_Eigen$values)
dataScaled_inertia 

barplot(dataScaled_Cor_Eigen$values, main = "Valeurs propres")
#remarquons qu avec les 2 premiers axes on apporte suffisement d inertie

data_pca <- dudi.pca(dataScaled, scann=F, nf = 2)

# les variables correlees sont tres proche l une de l autre
# plus la distance a l origine est frande meilleure est la projection
# co = column coordinate
# c1 = column normed scores ie principal axes
s.corcircle(data_pca$co)


