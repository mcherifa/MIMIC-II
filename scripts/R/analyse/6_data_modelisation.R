########################
# Preparation de toutes les données 
# pour l'utilisation du superlearner
########################

rm(list = ls())
set.seed(1)

library(caret)
library(SuperLearner)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)

options(na.action='na.pass')

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"
load(file.path(chemin,"4_data.RData"))

numberAHE <- function(df, time){
	all_outcome = NULL
	# Par id 
	for(ii in unique(df$id)){		
		dat <- df[df$id == ii, ]
		outcome_time = NULL
  	dat$debut_periode <- as.POSIXct(strptime(dat$debut_periode,"%Y-%m-%d %H:%M:%S"))
  	dat$jour1  <- as.POSIXct(strptime(dat$debut_periode,"%Y-%m-%d %H:%M:%S"))
  	# 60 * 60 = 1H et time = Nombre d'heure
  	dat$jour2  <-  dat$jour1 - ( 60 * 60 * time)
  	for (i in 1: nrow(dat)){
    temp <- dplyr::filter(dat, (dat$debut_periode < dat$jour1[i] &
                                  dat$debut_periode >= dat$jour2[i]))
		outcome_time <- c(outcome_time, sum(temp$event))
  }
  all_outcome <- c(all_outcome, outcome_time)
 }
  return(all_outcome)
}

##################
################## 1  LECTURE MIMIC
##################

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

arma_data <- data.frame(mapply(FUN = na.zero, 
                               subset(df_final, select = c("ar_hr", "ma_hr", "inter_hr", "ar_spo2",
                                                           "ma_spo2", "inter_spo2", "ar_abpm",              
                                                           "ma_abpm","inter_abpm", "ar_abpd",
                                                           "ma_abpd","inter_abpd","ar_abps", 
                                                           "ma_abps","inter_abps"))))

df_final <- df_final %>% dplyr::select(-c(ar_hr, ma_hr, inter_hr, ar_spo2,
                                          ma_spo2, inter_spo2,ar_abpm,              
                                          ma_abpm,inter_abpm, ar_abpd,
                                          ma_abpd,inter_abpd,ar_abps, 
                                          ma_abps,inter_abps))
df_final <- cbind(df_final, arma_data)
df_final$event_24h <- numberAHE(df = df_final, time = 24)

df_modelisation <- df_final %>%  
  subset(select = -c(admission_type_descr, date_admission_icu,
										 date_admission_hos, date_discharge_icu, date_discharge_hos ,
   									 delais_hospital, delais_icu, admission_icu, discharge_icu, 
   									 admission_hos, discharge_hop, debut_periode,start, end,
                     periode, bmi, temps_ahe_observation, temps_ahe_prediction,
                     los_icu, los_hospital, icu_death, hospital_death,
                     j28_death, fin_periode, date_debut_periode)) %>%
  mutate(gender  =  as.numeric(ifelse(gender == "M",1,0)),
         care_unit = as.factor(care_unit),
         eevent = ifelse(as.numeric(as.character(eevent)) > 0, 1, as.numeric(as.character(eevent))),
         event = ifelse(as.numeric(as.character(event)) > 0, 1, as.numeric(as.character(event))),
         id =  as.numeric(as.character(id)))

df_modelisation$event_cum <- ifelse(df_modelisation$event_cum <0, 0, df_modelisation$event_cum)
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
  header = T, sep = "," )

df_information <-  read.csv(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/liste_patient.csv", 
  header = T )
df_information$care_unit
names(df_information) <- c("id","num_archive","nom","time_and_date","age",
                           "gender","sofa_first","sapsi_first","amine","sedate","venti")

df_information <- subset(df_information, 
                         select = -c(num_archive, nom, time_and_date))
                         
df_information$care_unit1 <- rep(0, 55)
df_information$care_unit2 <- rep(1, 55)
df_information$care_unit4 <- rep(0, 55)

# Df_validation final 
df_validation <- right_join(df_information, df_validation, by = "id")
df_modelisation_fabrice <- subset(df_validation, select = -c(periode))
df_modelisation_fabrice$event_24h <- numberAHE(df = df_modelisation_fabrice, time = 24)

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

df_modelisation_fabrice <- cbind(df_modelisation_fabrice, arma_data) %>%
												   na.omit() %>%
													 dplyr::select(-c(eevent,temps_ahe_observation, temps_ahe_prediction, debut_periode, identif))

colnames(df_modelisation_fabrice) <-  c("id",          "age",         "gender",      "sofa_first", 
                                        "sapsi_first", "amine",       "sedation",    "ventilation",      
                                        "care_unit1",  "care_unit2",  "care_unit4",  "event", "event_cum",     
                                        "mean_hr",     "var_hr",      "mean_spo2",   "var_spo2",     
                                        "mean_abpm",   "var_abpm",    "mean_abpd",   "var_abpd",     
                                        "mean_abps",   "var_abps",    "beta_hr",     "alpha_hr",   
                                        "beta_spo2",   "alpha_spo2",  "beta_abpm",   "alpha_abpm", 
                                        "beta_abpd",   "alpha_abpd",  "beta_abps",   "alpha_abps", 
                                        "W1_hr" , 		 "V1_hr",       "W1_spo2",     "V1_spo2",     "W1_abpm",    
                                        "V1_abpm",     "W1_abpd",     "V1_abpd",     "W1_abps",    
                                        "V1_abps", 		 
                                        "event_24h",
                                        "ar_hr",       "ma_hr",       "inter_hr",    "ar_spo2", 
                                        "ma_spo2",     "inter_spo2",  "ar_abpm",     "ma_abpm",   
                                        "inter_abpm",  "ar_abpd",     "ma_abpd",     "inter_abpd", 
                                        "ar_abps",     "ma_abps",     "inter_abps")

# Ecriture de la base de données de validation finale
# library("xlsx")
# write.xlsx(df_validation, "Data_larib.xlsx", sheetName = "Sheet1", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)

df_modelisation_fabrice$event_cum <- ifelse(df_modelisation_fabrice$event_cum <0, 0, df_modelisation_fabrice$event_cum)
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

save(df_modelisation_mimic, df_sample_mimic, df_modelisation_fabrice, df_sample_fabrice,
     file = "~/Recherche/Mimic-II/data/clean/mimic2/df_modelisation.RData")







