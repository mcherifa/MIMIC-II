#####
# Analyse de sensibilité PAM < 60 mmHG
#####

#######################################################
#-------- Combiner les données cliniques
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")

library(parallel)
library(doParallel)
library(data.table)
library(dplyr)

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"

# Numerics 
numerics <- readRDS(paste0(chemin,"/numerics.rds"), refhook = NULL)

# Données réactualisées
load(file.path(chemin,"sedation.RData"))
load(file.path(chemin,"amine.RData"))
load(file.path(chemin,"ventilation.RData"))

# infos_admission de mimic 2
load(file.path(chemin,"infos_admission.RData"))
length(unique(infos_admission$subject_id))

# Différence entre patients avec numerics et sans numerics 
# Patients qui ont des donneés numerics mais pas des infos cliniques
setdiff(numerics$id, infos_admission$subject_id)

# Patients qui ont des donneés infos cliniques mais pas de numerics
setdiff(infos_admission$subject_id, numerics$id)

#- 1  Recherche episode hypotensif de 5 min (drapeau)
 no_cores <- detectCores() - 1
 cl <- makeCluster(no_cores, type="FORK")
 registerDoParallel(cl)
 v_id <- unique(numerics$id)
 recherche_episode <- foreach(sujet = v_id,.combine = rbind) %dopar% {
   periodeAHE(dossier_patient = sujet, outcome = 60)
 }
#load("~/Recherche/Mimic-II/resultats_rewieving/recherche_episode.RData")
recherche_episode$debut_periode <-  as.POSIXct(strptime(recherche_episode$debut_periode,"%Y-%m-%d %H:%M:%S"))
recherche_episode <- na.omit(recherche_episode) # 63463

# - 2 Médicaments pdt la période
# Vasopresseurs
ap = se = vp = NULL
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  ap <- rbind(ap, medoc_periode(df1 = recherche_episode, df2 = amine, sujet = sujet, type = "amine"))
}
# Sedation
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  se <- rbind(se, medoc_periode(df1 = recherche_episode, df2 = sedation, sujet = sujet, type = "sedation"))
} 

# Jointure
recherche_episode$subject_id <- recherche_episode$id
recherche_episode$start <- recherche_episode$debut_periode
df <- recherche_episode %>%  
  full_join(ap,  by = c("start","subject_id")) %>%
  full_join(se,  by = c("start","subject_id","end"))

# - 3 Ventilation dans la période
ventilation <- subset(ventilation, mechvent == 1)

# Ventilation
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  vp <- rbind(vp,  ventilation_periode(df = df, sujet = sujet))
}

# 3 - Liaison entre sejour et numerics et outcome
patient.record <- unique(vp$id)
all_sejour <- NULL

for(pp in patient.record){ #10419
  cat(pp, "\t")
  clinical <- subset(infos_admission, subject_id == pp) %>%
    dplyr::mutate(
      date_admission_icu = as.POSIXct(strptime(admission_icu,"%Y-%m-%d")),
      date_admission_hos = as.POSIXct(strptime(admission_hos,"%Y-%m-%d")),
      date_discharge_icu  = as.POSIXct(strptime(discharge_icu,"%Y-%m-%d")),
      date_discharge_hos  = as.POSIXct(strptime(discharge_hop,"%Y-%m-%d"))
    )
  date_admission_sejour <-  data.frame(hos = clinical$date_admission_hos,
                                       icu = clinical$date_admission_icu)
  date_admission_sejour <-  unique(transform(date_admission_sejour, min = pmin(hos, icu))$min)
  
  date_sortie_sejour <-  data.frame(hos = clinical$date_discharge_hos,
                                    icu = clinical$date_discharge_icu)
  date_sortie_sejour <-   unique(transform(date_sortie_sejour, max = pmax(hos, icu))$max)
  
  out_come <- na.omit(subset(vp, id == pp)) %>%
    dplyr::mutate(debut_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d %H:%M:%S")),
                  fin_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d %H:%M:%S")) + (60 * 90),
                  date_debut_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d")))

  
  for(i in 1:length(date_admission_sejour)){
  	all_sejour_patient <- NULL
    tmp_out_come <-  dplyr::filter(out_come,(date_debut_periode >= date_admission_sejour[i]))
    tmp_out_come <-  dplyr::filter(tmp_out_come,(fin_periode <= date_sortie_sejour[i]))
    
    sejour <- merge(clinical, out_come, by.x ="subject_id", by.y = "subject_id")
    all_sejour_patient <- rbind(all_sejour_patient, sejour)
  }
  all_sejour <- rbind(all_sejour, all_sejour_patient)
}

# 4 - Informations sur les patients 
df_information <- all_sejour
#  select(-c(admission_icu, discharge_icu, admission_hos, discharge_hop,
#            debut_periode, id, start, end))

df_information <- unique(df_information[order(df_information$subject_id,df_information$periode),])

# 4 - Sauvegarde
save(df_information, numerics, vp, recherche_episode,
     file = "~/Recherche/Mimic-II/data/clean/mimic2_60/2_mimic.RData")
     
###############################################################################
###############################################################################

rm(list = ls())

library(stringr)
library(plyr)
library(gdata)
library(parallel)
library(doParallel)
library(data.table)
library(dplyr)
    
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")    
source("~/Recherche/Mimic-II/scripts/R/toolbox/to.balanced.R")  

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"

load(file.path(chemin,"2_mimic.RData"))
load("~/Recherche/Mimic-II/data/clean/mimic2_60/2_mimic.RData")

df_information <- df_information %>% 
  mutate(
    event = ifelse(event > 0, 1, 0), # 10800 events
    periode = as.character(periode),
    periode = gsub(" ","",periode),
    identif = paste(as.character(subject_id),
                    as.character(periode),sep="-"),
    id = as.character(subject_id))  # 1276 individus

numerics <- subset(numerics,id %in% df_information$id) 

### ------- Découpage des numérics par périodes d'observation

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)
v_id <- unique(numerics$id)

df_numerics_observation_60_min <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  observation_periode(df = numerics,
                      sujet = sujet, 
                      duree_obs = 60)
}

stopCluster(cl)    
     

###########################
# Observation 60 min 
###########################

df_numerics_sp <- subset(df_numerics_observation_60_min, !is.na(periode))
# Nombre de période de 60 min
nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode) == 60)

df_numerics_sp <- df_numerics_sp %>%
  mutate(
    time = rep(x = seq(1,60,1), nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"))

###
### ------- Data final : wide
###

v_id <- unique(df_numerics_sp$id)
transp = NULL
for (sujet in v_id){
  cat(sujet,"\t")
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                                      match("identif",names(df_numerics_sp)),
                                      match("time", names(df_numerics_sp)),
                                      match(c("hr","spo2","abpsys","abpdias",
                                              "abpmean"),
                                            names(df_numerics_sp))))
}

transp <- transp %>% 
  mutate(
    identif = as.character(identif),
    id = as.character(identif),
    id = as.character(str_match(id,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2]))

df_final_wide_60min <- dplyr::inner_join(df_information, transp, 
                            by = c("identif","id","periode")) %>%
  select(-c(identif, icustay_id, rank, subject_id))
  
save(df_numerics_observation_60_min, df_final_wide_60min,
  file = "~/Recherche/Mimic-II/data/clean/mimic2_60/3_numerics.RData") 
     
###############################################################################
###############################################################################

rm(list = ls())

library(stringr)
library(plyr)
library(gdata)
library(parallel)
library(doParallel)
library(data.table)
library(dplyr)
library(wavelets)


source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
chemin <- "~/Recherche/Mimic-II/data/clean/mimic2_60"
load(file.path(chemin,"3_numerics.RData"))


df_information <- df_final_wide_60min[,c(1:which(names(df_final_wide_60min) == "hr.t1")-1
)]

###
# MOYENNE - VARIANCE 
###

df_final_wide_mean <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data=df_numerics_observation_60_min, mean, na.rm = T)
colnames(df_final_wide_mean) <- c("periode", "id", "mean_hr","mean_spo2","mean_abpm","mean_abpd","mean_abps")

df_final_wide_var <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                               data=df_numerics_observation_60_min, var, na.rm = T)
colnames(df_final_wide_var) <- c("periode", "id", "var_hr","var_spo2","var_abpm","var_abpd","var_abps")

###
# MODELE LINEAIRE 
###

df_final_wide_lineaire <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                    data = df_numerics_observation_60_min, estimate_tendance, temps_observation = 60) 
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
                                data = df_numerics_observation_60_min, para_arima,temps_observation = 60) 
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
                                data = df_numerics_observation_60_min, para_haar) 
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

save(df_final_wide_mean,
     df_final_wide_var,
     df_final_wide_lineaire, 
     df_final_wide_arma,
     df_final_wide_haar,  
     df_final,
     file = "~/Recherche/Mimic-II/data/clean/mimic2_60/4_data.RData")
     
###############################################################################
###############################################################################

rm(list = ls())
set.seed(1)

library(caret)
library(SuperLearner)   
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)


options(na.action='na.pass')

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2_60"
load(file.path(chemin,"4_data.RData"))

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
                                          ma_spo2, inter_spo2, ar_abpm,              
                                          ma_abpm,inter_abpm, ar_abpd,
                                          ma_abpd,inter_abpd,ar_abps, 
                                          ma_abps,inter_abps))
df_final <- cbind(df_final, arma_data)

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

save(df_modelisation_mimic, df_sample_mimic, df_modelisation_fabrice, df_sample_fabrice,
     file = "~/Recherche/Mimic-II/data/clean/mimic2_60/df_modelisation.RData")
     
###############################################################################
###############################################################################

# Modelisation_superlearner toutes les periodes

rm(list = ls())
set.seed(1)

library(caret)
library(SuperLearner)   
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)


options(na.action='na.pass')

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")
load("~/Recherche/Mimic-II/data/clean/mimic2_60/df_modelisation.RData")

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
                                
save(d, f, h, j, file = "toutes_periodes_60pam.Rdata")
load(file = "toutes_periodes_60pam.Rdata")
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



# Récupération Brier score
for(ll in lettres){ 
	cat(get(ll)$brier_score_train, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$brier_score_val, "\n")
}
for(ll in lettres){ 
	cat(get(ll)$brier_score_fab, "\n")
}


#######################################################
# Modelisation_superlearner une période

rm(list = ls())
set.seed(1)

library(caret)
library(SuperLearner)   
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)


options(na.action='na.pass')

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")
load("~/Recherche/Mimic-II/data/clean/mimic2_60/df_modelisation.RData")

 
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
                                
save(d, f, h, j, file = "une_periode_60pam.Rdata")
load(file = "une_periode_60pam.Rdata")
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


                       

     
     

