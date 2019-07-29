#######################################################
# Analyse en sousgroupe 
#######################################################

##########
# Modelisation Superlearner
# une periode par patient
##########

rm(list = ls())

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

source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")

# options(na.action='na.pass')
load(file = "~/Recherche/Mimic-II/data/clean/mimic2/df_modelisation.RData")

### Fonction pour les données
### Type analyse : one_periode, all_periode, ou data_fabrice
### data : df_sample, ou df_modelisation

## Data 1 : baseline carac
## Data 2 : baseline carac + traitement
## Data 3 : baseline carac + traitement + mean var sans pam
## Data 4 : baseline carac + traitement + mean var avec pam
## Data 5 : baseline carac + traitement + lineaire sans pam
## Data 6 : baseline carac + traitement + lineaire avec pam
## Data 7 : baseline carac + traitement + arma sans pam
## Data 8 : baseline carac + traitement + arma avec pam
## Data 9 : baseline carac + traitement + haar sans pam
## Data 10 : baseline carac + traitement + haar avec pam

# Ajout des diagnostics
library(xlsx)
diag = read.xlsx("/home/menyssa/Recherche/Mimic-II/scripts/R/description/diagnostic_160318_AB.xlsx",
                 sheetIndex = 2)
length(unique(diag$subject_id))
diag = subset(diag, is.na(diagnosis) == F)
diag = diag[!duplicated(diag$subject_id),]

df_sample_mimic = merge(x = df_sample_mimic,
										y = diag, 
										by.x = c("id"),
										by.y = c("subject_id"))

# Regroupement des diagnostiques
df_sample_mimic[df_sample_mimic$diagnosis %in% c("CCV?","CCVs","CS", "SCA"),"diagnosis"] = "CCV"
df_sample_mimic[df_sample_mimic$diagnosis == "Trauma","diagnosis"] = "trauma"
df_sample_mimic[df_sample_mimic$diagnosis %in% c("AKI", "IMV","ACR"),"diagnosis"] = "med"
df_sample_mimic[df_sample_mimic$diagnosis %in% c("cancer", "chir","Burn", "AHF  " ),"diagnosis"] = "other"
df_sample_mimic$diagnosis = droplevels(df_sample_mimic$diagnosis)

# Analyse CCV : 
# 0   1 
# 447 115 

data_mimic <- generation.data(data = subset(df_sample_mimic, diagnosis == "CCV"))
data_fabrice <- generation.data(data = df_sample_fabrice)

# Modelisation caracteristiques, traitements, tous les resumés
d <- Modelisation_superlearner(data_train =  subset(data_mimic[[3]],select = -c(event_cum, event_24h)),
                               data_val =  subset(data_fabrice[[3]],select = -c(event_cum, event_24h)),
                               eval_cv = FALSE,
                               folds = 10)
########## lineaire
# Modelisation caracteristiques, traitements, tous les resumés
f <-  Modelisation_superlearner(data_train = subset(data_mimic[[5]], select = -c(event_cum, event_24h)),
                                data_val = subset(data_fabrice[[5]], select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 10)
########## ARMA
# Modelisation caracteristiques, traitements, tous les resumés
h <-  Modelisation_superlearner(data_train =  subset(data_mimic[[7]],select = -c(event_cum, event_24h)),
                                data_val =  subset(data_fabrice[[7]],select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 10)
########## haar
# Modelisation caracteristiques, traitements, tous les resumés
j <-  Modelisation_superlearner(data_train =  subset(data_mimic[[9]],select = -c(event_cum, event_24h)),
                                data_val =  subset(data_fabrice[[9]],select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 10)

save(d, f, h, j,
     file = "/home/menyssa/Recherche/Mimic-II/resultats_rewieving/une_periode_ccv.Rdata")

##########
# Modelisation Superlearner
# toutes périodes
##########

df_modelisation_mimic = merge(x = df_modelisation_mimic,
										y = diag, 
										by.x = c("id"),
										by.y = c("subject_id"))

# Regroupement des diagnostiques
df_modelisation_mimic[df_modelisation_mimic$diagnosis %in% c("CCV?","CCVs","CS", "SCA"),"diagnosis"] = "CCV"
df_modelisation_mimic[df_modelisation_mimic$diagnosis == "Trauma","diagnosis"] = "trauma"
df_modelisation_mimic[df_modelisation_mimic$diagnosis %in% c("AKI", "IMV","ACR"),"diagnosis"] = "med"
df_modelisation_mimic[df_modelisation_mimic$diagnosis %in% c("cancer", "chir","Burn", "AHF  " ),"diagnosis"] = "other"
df_modelisation_mimic$diagnosis = droplevels(df_modelisation_mimic$diagnosis)

data_mimic <- generation.data(data = subset(df_modelisation_mimic, diagnosis =='CCV'))
data_fabrice <- generation.data(data = df_modelisation_fabrice)
# Nb events
# 0     1 
#25842  6385 

########## Resume
# Modelisation caracteristiques, traitements, tous les resumés
d <- Modelisation_superlearner_ma(data_train = subset(data_mimic[[3]],select = -c(event_cum, event_24h)),
                               data_val = subset(data_fabrice[[3]],select = -c(event_cum, event_24h)),
                               eval_cv = FALSE,
                               folds = 5)
########## lineaire
# Modelisation caracteristiques, traitements, tous les resumés
f <-  Modelisation_superlearner_ma(data_train = subset(data_mimic[[5]], select = -c(event_cum, event_24h)),
                                data_val = subset(data_fabrice[[5]], select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 5)
########## ARMA
# Modelisation caracteristiques, traitements, tous les resumés
h <-  Modelisation_superlearner_ma(data_train =  subset(data_mimic[[7]], select = -c(event_cum, event_24h)),
                                data_val = subset(data_fabrice[[7]], select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 5)
########## haar

# Modelisation caracteristiques, traitements, tous les resumés
j <-  Modelisation_superlearner_ma(data_train = subset(data_mimic[[9]], select = -c(event_cum, event_24h)),
                                data_val = subset(data_fabrice[[9]], select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 5)
                                
save(d, f, h, j, file = "toutes_periodes_CCV.Rdata")

############################
# Evaluation
############################

load("/home/menyssa/Recherche/Mimic-II/resultats_rewieving/une_periode_ccv.Rdata")      
lettres = c("d","f","h","j")
# Récupération AUC
for(ll in lettres){ 
	cat(get(ll)$auc_validation, "\n")
}
# Récupération Brier score
for(ll in lettres){ 
	cat(get(ll)$brier_score_val, "\n")
}

load("toutes_periodes_CCV.Rdata")
lettres = c("d","f","h","j")
# Récupération AUC
for(ll in lettres){ 
	cat(get(ll)$auc_validation, "\n")
}
# Récupération Brier score
for(ll in lettres){ 
	cat(get(ll)$brier_score_val, "\n")
}
