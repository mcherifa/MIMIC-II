#######################################################
#-------- Selection de variables 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

library(corrplot)
library(FactoMineR) 
library(caret)

# library(devtools)
# install_github("ggbiplot", "vqv")
# library(ggbiplot)

#######################################
# Lecture
#######################################

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"
load(file.path(chemin,"4_data.RData"))

# centrée et réduire toutes les variables

dat <- data.frame(scale(df[,16: ncol(df)],
                        center = TRUE, scale = TRUE))

#######################################
# Corrélation entre les variables 
#######################################

corMatMy <- cor(dat)
corrplot(corMatMy, order = "hclust")

# Filtre sur les corrélations > 0.7
highlyCor <- findCorrelation(corMatMy, 0.70)
datMyFiltered.scale <- dat[,-highlyCor]
corMatMy <- cor(datMyFiltered.scale)
corrplot(corMatMy, order = "hclust")

#######################################
# Analyse en composantes principales
#######################################

pca <- PCA(dat, scale.unit=TRUE, ncp=5, graph=T)
summary(pca)

#######################################œ
# Data final 
#######################################

# df_final <- cbind( df[,1:13],datMyFiltered.scale)
# df_final <- cbind( df[,1:15],dat)



save(df, file = "~/Recherche/Mimic-II/data/clean/mimic2/df_modelisation.RData")



