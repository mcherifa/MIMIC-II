#######################################################
# Script combine parametric models for numerics 
#                 series and AHE event 
# 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/mcherifa/Mimic/scripts/R/toolbox/les_sources.R")

#######################################
# DATA
#######################################

df_wide 				<- readRDS(paste0(dest_data,"fichier_wide_periode.rds"))
df_wide_resume 	<- readRDS(paste0(dest_data,"df_resume_para.rds"))
df_wide_para 		<- readRDS(paste0(dest_data,"df_para_para.rds"))
df_wide_arma 		<- readRDS(paste0(dest_data,"df_arima_para.rds"))
df_wide_fourier <- readRDS(paste0(dest_data,"df_fourier_para.rds"))
df_wide_haar4 	<- readRDS(paste0(dest_data,"df_haar4_para.rds"))
df_wide_haar5 	<- readRDS(paste0(dest_data,"df_haar5_para.rds"))

# Management
df_wide <- df_wide[which(df_wide$eevent == 0),]
df_wide <- subset(df_wide, select = c(-periode, - identif,-eevent))

# Variables factors
df_wide[,c(1:5)]  <- data.frame(lapply(df_wide[,c(1:5)], as.factor))


#######################################
# Sélection de variables significatives
#######################################  

un <- model_parametrique(df_wide_resume,variables[[1]])
deux <-model_parametrique(df_wide_para,variables[[2]])
trois <- model_parametrique(df_wide_arma,variables_arma[[3]])
quatre<- model_parametrique(df_wide_fourier,variables[[4]])
cinq <- model_parametrique(df_wide_haar4,variables[[5]])
six <- model_parametrique(df_wide_haar5,variables[[6]])
sept <- model_parametrique(df_wide,variables[[7]])


#######################################
# Sélection de variables significatives
#######################################  

variables_wide <- as.character(selection(df_wide))
variables_resume <- as.character(selection(df_wide_resume))
variables_para <- as.character(selection(df_wide_para))
variables_arma <- as.character(selection(df_wide_arma))
variables_fourier <- as.character(selection(df_wide_fourier))
variables_haar4 <- as.character(selection(df_wide_haar4))
variables_haar_5 <- as.character(selection(df_wide_haar5))

variables <- list()
variables[[1]] <- variables_resume
variables[[2]] <- variables_para
variables[[3]] <- variables_arma
variables[[4]] <- variables_fourier
variables[[5]] <- variables_haar4
variables[[6]] <- variables_haar_5
variables[[7]] <- variables_wide

saveRDS(variables,"/home/mcherifa/Mimic/data/clean/variables.rds")

variables <- readRDS(paste0(dest_data,"variables.rds"))       

#######################################
# Modele lineaire mixte multivariés
#######################################  

df <- df_wide 
cov <- c("gender","ventilation", "amine", 
	"admission_type_descr", "age", "sapsi_first", "sofa_first")
	
# données brutes 

beta <- foreach(siz = 1) %dopar% 
{
   k_bootstrapping_para(df_wide, cov ,siz)
   
}
  
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		

write.table(resume, file = "/home/mcherifa/Mimic/resultats/brute_para.csv", 
            sep = "\t", row.names = F)
           
# données résumées 
 
beta <- foreach(siz = 1) %dopar% 
{
   k_bootstrapping_para(df_wide_resume, variables[[1]], siz)
   
}
  
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		

write.table(resume, file = "/home/mcherifa/Mimic/resultats/resume_para.csv", 
            sep = "\t", row.names = T)


# données linéaires 

beta <- foreach(siz = 1) %dopar% 
{
   k_bootstrapping_para(df_wide_para,variables[[2]] ,siz)
}
  
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		

write.table(resume, file = "/home/mcherifa/Mimic/resultats/lineaire_para.csv", 
            sep = "\t", row.names = F)
            
                     
# données arma 

beta <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping_para(df_wide_arma,variables[[3]], siz)
}

resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		
 
write.table(resume, file = "/home/mcherifa/Mimic/resultats/arma.csv", 
            sep = "\t", row.names = F)
             
# données ondelettes 4

beta <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping_para(df_wide_haar4,variables[[5]], siz)
}
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		
write.table(resume, file = "/home/mcherifa/Mimic/resultats/haar4.csv", 
            sep = "\t", row.names = F)
            
            
# données ondelettes 5

beta <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping_para(df_wide_haar5,variables_haar_5, siz)
}
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		
write.table(resume, file = "/home/mcherifa/Mimic/resultats/haar5.csv", 
            sep = "\t", row.names = F)
                     
# données fourier 

beta <- foreach(siz = 200) %dopar% 
{
   k_bootstrapping_para(df_wide_fourier, variables_fourier ,siz)
}
resume <- data.frame(moyenne = mean(beta[[1]]$AUC),
										ecart.type = 1.96 * sd(beta[[1]]$AUC), 
										time = mean(beta[[1]]$Time_to_predict)
)		
write.table(resume, file = "/home/mcherifa/Mimic/resultats/fourier.csv", 
            sep = "\t", row.names = F)



beta <- rbind(k_bootstrapping_para(df_wide, cov ,1)
k_bootstrapping_para(df_wide_resume, variables_resume  ,1)
k_bootstrapping_para(df_wide_para,  variables_para ,1)
k_bootstrapping_para(df_wide_arma,variables_arma ,1)
k_bootstrapping_para(df_wide_fourier,  variables_fourier ,1)
k_bootstrapping_para(df_wide_haar4, variables_haar4 ,1)
k_bootstrapping_para(df_wide_haar5,  variables_haar_5 ,1)
)

write.table(beta, file = "/home/mcherifa/Mimic/resultats/logistic_1.csv", 
            sep = "\t", row.names = F)		


















































































