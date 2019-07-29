#######################################################
# Analyse de sensibilité pour gap de 50 min
#######################################################

# Application de différentes méthodes de feature sélection

rm(list = ls())

library(dplyr)
library(wavelets)

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"
load(file.path(chemin,"3_numerics.RData"))
load(file.path(chemin,"4_data.RData"))

df_information <- df_final_wide_60min[,c(1:which(names(df_final_wide_60min) == "hr.t1")-1)]

###
# MOYENNE - VARIANCE 
###

df_final_wide_mean <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data=df_numerics_observation_50_min, mean, na.rm = T)
colnames(df_final_wide_mean) <- c("periode", "id", "mean_hr","mean_spo2","mean_abpm","mean_abpd","mean_abps")

df_final_wide_var <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                               data=df_numerics_observation_50_min, var, na.rm = T)
colnames(df_final_wide_var) <- c("periode", "id", "var_hr","var_spo2","var_abpm","var_abpd","var_abps")

###
# MODELE LINEAIRE 
###

df_final_wide_lineaire <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                    data = df_numerics_observation_50_min, estimate_tendance, temps_observation = 50) 
df_final_wide_lineaire <- do.call(data.frame, df_final_wide_lineaire)
colnames(df_final_wide_lineaire) <- c("periode", "id",
                                      "alpha_hr",  "beta_hr",
                                      "alpha_spo2","beta_spo2",
                                      "alpha_abpm","beta_abpm",
                                      "alpha_abpd","beta_abpd",
                                      "alpha_abps","beta_abps")
###                                      
#  ARIMA (1,0,1)
###
df_final_wide_arma <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_observation_50_min, para_arima,temps_observation = 50) 
df_final_wide_arma <- do.call(data.frame, df_final_wide_arma)
colnames(df_final_wide_arma) <- c("periode","id","ar_hr",  "ma_hr",  "inter_hr",
                                  "ar_spo2","ma_spo2","inter_spo2",
                                  "ar_abpm","ma_abpm","inter_abpm",
                                  "ar_abpd","ma_abpd","inter_abpd",
                                  "ar_abps","ma_abps","inter_abps")
###
# HAAR : 5
###

df_final_wide_haar <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_observation_50_min, para_haar) 
df_final_wide_haar <- do.call(data.frame, df_final_wide_haar)
colnames(df_final_wide_haar) <- c("periode","id","W1_hr",  "V1_hr",
                                  "W1_spo2","V1_spo2",
                                  "W1_abpm","V1_abpm",
                                  "W1_abpd","V1_abpd",
                                  "W1_abps","V1_abps")

df_final_wide <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                        list(df_final_wide_mean, df_final_wide_var, df_final_wide_lineaire,
                             df_final_wide_arma, df_final_wide_haar))

df_final <- merge(df_information, df_final_wide, by = c("id", "periode"))
df_final <- df_final[order(df_final$id,df_final$periode),]

#######################################################
# Preparation de toutes les données 
# pour l'utilisation du superlearner
rm(list = ls())
library(caret)
library(SuperLearner)
library(plyr)
library(parallel)
library(doParallel)
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)

set.seed(1)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#arma_data <- data.frame(mapply(FUN = na.zero, 
#                               subset(df_final, select = c("ar_hr", "ma_hr", "inter_hr", "ar_spo2",
#                                                           "ma_spo2", "inter_spo2", "ar_abpm",              
#                                                           "ma_abpm","inter_abpm", "ar_abpd",
#                                                           "ma_abpd","inter_abpd","ar_abps", 
#                                                           "ma_abps","inter_abps"))))

#df_final <- df_final %>% dplyr::select(-c(ar_hr, ma_hr, inter_hr, ar_spo2,
#                                          ma_spo2, inter_spo2, ar_abpm,              
#                                          ma_abpm,inter_abpm, ar_abpd,
#                                          ma_abpd,inter_abpd,ar_abps, 
#                                          ma_abps,inter_abps))
#df_final <- cbind(df_final, arma_data)


#save(df_final, file ="df_final_50.Rdata")

load("df_final_50.Rdata")

df_modelisation <- df_final %>%  
  subset(select = -c(event_cum, admission_type_descr, date_admission_icu,
										 date_admission_hos, date_discharge_icu, date_discharge_hos ,
   									 delais_hospital, delais_icu, admission_icu, discharge_icu, 
   									 admission_hos, discharge_hop, debut_periode,start, end,
                     periode, bmi, temps_ahe_observation, temps_ahe_prediction,
                     los_icu, los_hospital, icu_death, hospital_death,
                     j28_death, fin_periode, date_debut_periode  )) %>%
  mutate(gender  =  as.numeric(ifelse(gender == "M",1,0)),
         care_unit = as.factor(care_unit),
         eevent = ifelse(as.numeric(as.character(eevent)) > 0, 1, as.numeric(as.character(eevent))),
         event = ifelse(as.numeric(as.character(event)) > 0, 1, as.numeric(as.character(event))),
         id =  as.numeric(as.character(id)))

#   Nombre de patients 
length(unique(df_modelisation$id))

#  Dummy variable care_unit
df_modelisation <- data.frame(stats::model.matrix( ~.-1, data = df_modelisation))
df_modelisation_mimic <- subset(df_modelisation, select = -care_unit6) %>% 
  na.omit() 

#  Tirage au sort une ligne par id 
df_sample_mimic <- df_modelisation_mimic %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()
table(df_sample_mimic$event)# 0, 1, 976 221 

# Une période par patient # 1197 avant 1151
setdiff(df_modelisation_mimic$id, df_sample_mimic$id)

##################
################## 2  LECTURE FABRICE
##################

# Données de Fabrice : 13 events, 65 patients
df_validation <-  read.csv(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/clean/df_modelisation.csv", 
  header = T, sep = " ")

df_information <-  read.csv(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/liste_patient.csv", 
  header = T )
df_information$care_unit
names(df_information) <- c("id","num_archive","nom","time_and_date","age",
                           "gender","sofa_first","sapsi_first","amine","sedate","venti")

df_information <- subset(df_information, 
                         select = -c(num_archive, nom, time_and_date))
df_information$care_unit1 <- rep(0, 55)
df_information$care_unit2 <- rep(0, 55)
df_information$care_unit4 <- rep(0, 55)

# Df_validation final 
df_validation <- right_join(df_information, df_validation, by = "id")
df_modelisation_fabrice <- subset(df_validation, select = -c(periode))

arma_data <- data.frame(mapply(FUN = na.zero, 
                               subset(df_modelisation_fabrice, select = c("ar_hr", "ma_hr", "inter_hr", "ar_spo2",
                                                                          "ma_spo2", "inter_spo2", "ar_abpm",              
                                                                          "ma_abpm","inter_abpm", "ar_abpd",
                                                                          "ma_abpd","inter_abpd","ar_abps", 
                                                                          "ma_abps","inter_abps"))))

df_modelisation_fabrice <- df_modelisation_fabrice %>% dplyr::select(-c(ar_hr, ma_hr, inter_hr, ar_spo2,
                                                                        ma_spo2, inter_spo2, ar_abpm,              
                                                                        ma_abpm,inter_abpm, ar_abpd,
                                                                        ma_abpd,inter_abpd,ar_abps, 
                                                                        ma_abps,inter_abps))

df_modelisation_fabrice <- cbind(df_modelisation_fabrice, arma_data) %>% na.omit()


colnames(df_modelisation_fabrice) <-  c("id",          "age",         "gender",      "sofa_first", 
                                        "sapsi_first", "amine",       "sedation",    "ventilation",      
                                        "care_unit1",  "care_unit2",  "care_unit4",  "event",      
                                        "mean_hr",     "var_hr",      "mean_spo2",   "var_spo2",     
                                        "mean_abpm",   "var_abpm",    "mean_abpd",   "var_abpd",     
                                        "mean_abps",   "var_abps",    "beta_hr",     "alpha_hr",   
                                        "beta_spo2",   "alpha_spo2",  "beta_abpm",   "alpha_abpm", 
                                        "beta_abpd",   "alpha_abpd",  "beta_abps",   "alpha_abps", 
                                        "W1_hr" , 		 "V1_hr",       "W1_spo2",     "V1_spo2",     "W1_abpm",    
                                        "V1_abpm",     "W1_abpd",     "V1_abpd",     "W1_abps",    
                                        "V1_abps", 		 "ar_hr",       "ma_hr",       "inter_hr",    "ar_spo2", 
                                        "ma_spo2",     "inter_spo2",  "ar_abpm",     "ma_abpm",   
                                        "inter_abpm",  "ar_abpd",     "ma_abpd",     "inter_abpd", 
                                        "ar_abps",     "ma_abps",     "inter_abps")

# Ecriture de la base de données de validation finale
# library("xlsx")
# write.xlsx(df_validation, "Data_larib.xlsx", sheetName = "Sheet1", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)

# Avoir au moins 8 events
data_event <-  df_modelisation_fabrice[df_modelisation_fabrice$event ==1,]
data_event <- data_event %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()

df_sample_fabrice <- df_modelisation_fabrice %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()
  
# on enleve les patients présents dans data_events
df_sample_fabrice <- subset(df_sample_fabrice, !(id %in% data_event$id))
# Recolle le tout
df_sample_fabrice <- na.omit(rbind(df_sample_fabrice, data_event))

#######################################################
# Modelisation_superlearner
# save(df_final, file ="df_final_40.Rdata")

options(na.action='na.pass')

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")

data_mimic <- generation.data(data = df_modelisation_mimic)
data_fabrice <- generation.data(data = df_modelisation_fabrice)


d <- Modelisation_superlearner_ma(data_train = data_mimic[[3]],
                               data_val = data_fabrice[[3]],
                               eval_cv = FALSE,
                               folds = 5)
                               
f <-  Modelisation_superlearner_ma(data_train = data_mimic[[5]],
                                data_val = data_fabrice[[5]],
                                eval_cv = FALSE,
                                folds = 5)
                                
h <-  Modelisation_superlearner_ma(data_train = data_mimic[[7]],
                                data_val = data_fabrice[[7]],
                                eval_cv = FALSE,
                                folds = 5)
                                
j <-  Modelisation_superlearner_ma(data_train = data_mimic[[9]],
                                data_val = data_fabrice[[9]],
                                eval_cv = FALSE,
                                folds = 5)
                                
save(d, f, h, j, file = "toutes_periodes_50.Rdata")
#load(file = "toutes_periodes_50.Rdata")
lettres = c("d","f","h","j")

# Récupération AUC
for(ll in lettres){ 
	cat(get(ll)$auc_apprentissage, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$auc_validation, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$auc_fabrice, "\n")
}

#######################################################
# Modelisation_superlearner une période
# save(df_final, file ="df_final_50.Rdata")
 
data_mimic <- generation.data(data = df_sample_mimic)
data_fabrice <- generation.data(data = df_sample_fabrice)

d <- Modelisation_superlearner(data_train = data_mimic[[3]],
                               data_val = data_fabrice[[3]],
                               eval_cv = FALSE,
                               folds = 10)
                               
f <-  Modelisation_superlearner(data_train = data_mimic[[5]],
                                data_val = data_fabrice[[5]],
                                eval_cv = FALSE,
                                folds = 10)
                                
h <-  Modelisation_superlearner(data_train = data_mimic[[7]],
                                data_val = data_fabrice[[7]],
                                eval_cv = FALSE,
                                folds = 10)
                                
j <-  Modelisation_superlearner(data_train = data_mimic[[9]],
                                data_val = data_fabrice[[9]],
                                eval_cv = FALSE,
                                folds = 10)
                                
save(d, f, h, j, file = "une_periode_50.Rdata")
# load(file = "une_periode_50.Rdata")
lettres = c("d","f","h","j")

# Récupération AUC
for(ll in lettres){ 
	cat(get(ll)$auc_apprentissage, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$auc_validation, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$auc_fabrice, "\n")
}
                               

