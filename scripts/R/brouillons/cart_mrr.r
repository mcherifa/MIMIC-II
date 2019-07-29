
source("Mimic/scripts/R/toolbox/les_sources.R",encoding="unknown")

library(rpart)

#######################################
# DATA
#######################################

df_wide 				<- readRDS(paste0(dest_data,"fichier_wide_periode.rds"))
df_wide_resume 	<- readRDS(paste0(dest_data,"df_resume.rds"))
df_wide_para 		<- readRDS(paste0(dest_data,"df_para.rds"))
df_wide_arma 		<- readRDS(paste0(dest_data,"df_arima.rds"))
df_wide_fourier <- readRDS(paste0(dest_data,"df_fourier.rds"))
df_wide_haar4 	<- readRDS(paste0(dest_data,"df_haar4.rds"))
df_wide_haar5 	<- readRDS(paste0(dest_data,"df_haar5.rds"))

# Management
df_wide <- df_wide[which(df_wide$eevent == 0),]
df_wide <- subset(df_wide, select = c( -id ,-periode, -identif,-eevent))

# Variables factors
df_wide[,c(1:5)]  <- data.frame(lapply(df_wide[,c(1:5)], as.factor))

#######################################
# MODELE
#######################################
A1 <- arbre(df_wide)
A2 <- arbre(df_wide_resume)
A3 <- arbre(df_wide_para)
A4 <- arbre(df_wide_arma)
A5 <- arbre(df_wide_haar4)
A6 <- arbre(df_wide_haar5)
A7 <- arbre(df_wide_fourier)
#######################################
# MODELE CROSS-VALIDATION
#######################################
	
	
	
	
	
	
	
	
	
	
	
