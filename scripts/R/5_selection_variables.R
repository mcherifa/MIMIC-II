#######################################################
#-------- Selection de variables 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

library(corrplot)
library(FactoMineR) 
# library(devtools)
# install_github("ggbiplot", "vqv")
# library(ggbiplot)

#######################################
# Lecture
#######################################

df <- na.omit(cbind(
  readRDS(paste0(dest_data,"df_resume.rds"))[,11:26],
  readRDS(paste0(dest_data,"df_resume.rds"))[,1:10],
  readRDS(paste0(dest_data,"df_lineaire.rds"))[,1:10],
  readRDS(paste0(dest_data,"df_arima.rds"))[,1:15],
  readRDS(paste0(dest_data,"df_haar.rds"))[,1:10])
  )

# centrée et réduire toutes les variables

dat <- data.frame(scale(df[,17: ncol(df)],
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

#######################################
# Data final 
#######################################

# df_final <- cbind( df[,1:13],datMyFiltered.scale)
df_final <- cbind( df[,1:16],dat)

# saveRDS(df_final,paste0(dest_data,"df_modelisation.rds"))
saveRDS(df,paste0(dest_data,"df_modelisation.rds"))


