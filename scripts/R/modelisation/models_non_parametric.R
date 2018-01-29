#######################################################
# Modelisation 
# 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/mcherifa/Mimic/scripts/R/toolbox/les_sources.R")

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
df_wide <- subset(df_wide, select = c( -id ,-periode, - identif,-eevent))

# Variables factors
df_wide[,c(1:5)]  <- data.frame(lapply(df_wide[,c(1:5)], as.factor))

df_wide$admission_type_descr				<- droplevels(df_wide$admission_type_descr)
df_wide_resume$admission_type_descr <- droplevels(df_wide_resume$admission_type_descr)
df_wide_para$admission_type_descr 	<- droplevels(df_wide_para$admission_type_descr)
df_wide_arma$admission_type_descr 	<- droplevels(df_wide_arma$admission_type_descr)
df_wide_fourier$admission_type_descr<- droplevels(df_wide_fourier$admission_type_descr)
df_wide_haar4$admission_type_descr 	<- droplevels(df_wide_haar4$admission_type_descr)
df_wide_haar5$admission_type_descr	<- droplevels(df_wide_haar5$admission_type_descr)

#######################################
# Monte Carlo cross validation - Reseau de Neurone 
#######################################

d <- read.csv("/home/mcherifa/Mimic/resultats/neurone4.csv")

data <- df_wide[,1:8]

beta <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping(data, siz)
}
moyenne = mean(beta[[1]]$AUC)

resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										l = moyenne - 1.96 * sd(beta[[1]]$AUC), 
										s = moyenne + 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)			
		 
d1 <- rbind(d,resume)
write.csv(resume,"/home/mcherifa/Mimic/resultats/neurone4.csv",row.names=F)

















beta <- rbind(k_bootstrapping(df_wide[,1:8], 1),
 k_bootstrapping(df_wide_resume, 1),
 k_bootstrapping(df_wide_para, 1),
 k_bootstrapping(df_wide_arma, 1),
 k_bootstrapping(df_wide_fourier, 1),
 k_bootstrapping(df_wide_haar4, 1),
 k_bootstrapping(df_wide_haar5, 1))
write.csv(beta,"/home/mcherifa/Mimic/resultats/neurone_41.csv",row.names=F)
































# tourner une fois 

beta <- rbind(k_bootstrapping_arbre(df_wide[,1:8], 1),
 k_bootstrapping_arbre(df_wide_resume, 1),
 k_bootstrapping_arbre(df_wide_para, 1),
 k_bootstrapping_arbre(df_wide_arma, 1),
 k_bootstrapping_arbre(df_wide_fourier, 1),
 k_bootstrapping_arbre(df_wide_haar4, 1),
 k_bootstrapping_arbre(df_wide_haar5, 1))
write.csv(beta,"/home/mcherifa/Mimic/resultats/res_neurone_41.csv",row.names=F)


data <- read.csv("/home/mcherifa/Mimic/resultats/res_neurone_4.csv", row.names=1)

arrondie <-  function (d){
	arrond_d <- data.frame(mapply(function(x) round(x,2), d))
	row.names(arrond_d) <- row.names(d)
	write.csv(arrond_d,"/home/mcherifa/Mimic/resultats/res_neurone_4.csv",row.names=T)
}

arrondie(data)

d <- read.csv("/home/mcherifa/Mimic/resultats/arbre_001.csv")
d <- read.csv("/home/mcherifa/Mimic/resultats/arbre_001.csv")
d <- read.csv("/home/mcherifa/Mimic/resultats/arbre_001.csv")
d <- read.csv("/home/mcherifa/Mimic/resultats/arbre_001.csv")
d <- read.csv("/home/mcherifa/Mimic/resultats/res_neurone_4.csv")


#######################################
# Monte Carlo cross validation - arbre
#######################################

d <- read.csv("/home/mcherifa/Mimic/resultats/arbre.csv")

data <- df_wide_resume

s <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping_arbre(data, siz)
}

resume <- data.frame(moyenne = mean(s[[1]]$AUC),
										ecart.type = 1.96 * sd(s[[1]]$AUC), 
										time = mean(s[[1]]$Time_to_predict)
)					 
d1 <- rbind(d,resume)
write.csv(resume,"/home/mcherifa/Mimic/resultats/o.csv",row.names=F)




