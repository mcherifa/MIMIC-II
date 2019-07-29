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

data_mimic <- generation.data(data = df_sample_mimic)
data_fabrice <- generation.data(data = df_sample_fabrice)

#############################
#### Modelisation 9 modèles
#############################

# Modelisation juste caracteristiques à l'admission
a <- Modelisation_superlearner(data_train = data_mimic[[1]],
                               data_val = data_fabrice[[1]],
                               eval_cv = FALSE,
                               folds = 10)
                               
# Modelisation juste caracteristiques et traitements
b <- Modelisation_superlearner(data_train = data_mimic[[2]],
                               data_val = data_fabrice[[2]],
                               eval_cv = FALSE,
                               folds = 10)
########## Resume
# Modelisation caracteristiques, traitements, tous les resumés sauf ceux associés a la pam
c <- Modelisation_superlearner(data_train = data_mimic[[4]],
                               data_val = data_fabrice[[4]],
                               eval_cv = FALSE,
                               folds = 10)
# Modelisation caracteristiques, traitements, tous les resumés
d <- Modelisation_superlearner(data_train = data_mimic[[3]],
                               data_val = data_fabrice[[3]],
                               eval_cv = FALSE,
                               folds = 10)
########## lineaire
# Modelisation caracteristiques, traitements, tous les resumés sauf ceux associés a la pam
e <-   Modelisation_superlearner(data_train = data_mimic[[6]],
                                 data_val = data_fabrice[[6]],
                                 eval_cv = FALSE,
                                 folds = 10)
# Modelisation caracteristiques, traitements, tous les resumés
f <-  Modelisation_superlearner(data_train = subset(data_mimic[[5]], select = -c(event_cum, event_24h)),
                                data_val = subset(data_fabrice[[5]], select = -c(event_cum, event_24h)),
                                eval_cv = FALSE,
                                folds = 10)
########## ARMA
# Modelisation caracteristiques, traitements, tous les resumés sauf ceux associés a la pam
g <-  Modelisation_superlearner(data_train = data_mimic[[8]],
                                data_val = data_fabrice[[8]],
                                eval_cv = FALSE,
                                folds = 10)
# Modelisation caracteristiques, traitements, tous les resumés
h <-  Modelisation_superlearner(data_train = data_mimic[[7]],
                                data_val = data_fabrice[[7]],
                                eval_cv = FALSE,
                                folds = 10)
########## haar
# Modelisation caracteristiques, traitements, tous les resumés sauf ceux associés a la pam
i <- Modelisation_superlearner(data_train = data_mimic[[10]],
                               data_val = data_fabrice[[10]],
                               eval_cv = FALSE,
                               folds = 10)
# Modelisation caracteristiques, traitements, tous les resumés
j <-  Modelisation_superlearner(data_train = data_mimic[[9]],
                                data_val = data_fabrice[[9]],
                                eval_cv = TRUE,
                                folds = 10)

save(a, b, c, d, e, f, g, h, i, j,
     file = "/home/menyssa/Recherche/Mimic-II/resultats_rewieving/modelisation_une_periode.Rdata")

load(file = "/home/menyssa/Recherche/Mimic-II/resultats_rewieving/modelisation_une_periode.Rdata")

lettres = letters[1:10]

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

# Recupération des valeurs apprentissage
data = NULL
for(ll in lettres){ 
	lol <- na.omit(get(ll)$apprentissage)
	data <- rbind( data, round(lol[which(lol$f1_score == max(lol$f1_score)),], 2))
}
write.csv(data, "/home/menyssa/results_apprentissage.csv")
data$letters <- lettres

# Recupération des valeurs validation
data_autre = NULL
for(ll in lettres){ 
 data_autre <- round(rbind(data_autre , subset(get(ll)$validation, seuil_prediction == as.character(subset(data, letters == ll, seuil_prediction)))),2)
}
write.csv(data_autre, "/home/menyssa/results_validation.csv")

# Recupération des valeurs validation fabrice
data_autre = NULL
for(ll in lettres){ 
 data_autre <- round(rbind(data_autre , subset(get(ll)$validation_fab, seuil_prediction == as.character(subset(data, letters == ll, seuil_prediction)))),2)
}
write.csv(data_autre, "/home/menyssa/results_validation_fab.csv")

#############################
#### Modelisation 27 modèles
#############################

# Modelisation juste caracteristiques à l'admission
a <- Modelisation_superlearner_full_models(data_train = data_mimic[[1]],
                               data_val = data_fabrice[[1]],
                               eval_cv = FALSE,
                               folds = 10)      
                                                        
# Modelisation juste caracteristiques et traitements
b <- Modelisation_superlearner_full_models(data_train = data_mimic[[2]],
                               data_val = data_fabrice[[2]],
                               eval_cv = FALSE,
                               folds = 10)
########## Resume
# Modelisation caracteristiques, traitements, tous les resumés
d <- Modelisation_superlearner_full_models(data_train = data_mimic[[3]],
                               data_val = data_fabrice[[3]],
                               eval_cv = FALSE,
                               folds = 10)
########## lineaire

f <-  Modelisation_superlearner_full_models(data_train = data_mimic[[5]],
                                data_val = data_fabrice[[5]],
                                eval_cv = FALSE,
                                folds = 10)
########## ARMA                      
# Modelisation caracteristiques, traitements, tous les resumés
h <-  Modelisation_superlearner_full_models(data_train = data_mimic[[7]],
                                data_val = data_fabrice[[7]],
                                eval_cv = FALSE,
                                folds = 2)
########## haar
# Modelisation caracteristiques, traitements, tous les resumés
j <-  Modelisation_superlearner_full_models(data_train = data_mimic[[9]],
                                data_val = data_fabrice[[9]],
                                eval_cv = FALSE,
                                folds = 10)

save(a, b, d, f, h, j,
     file = "/home/menyssa/Recherche/Mimic-II/resultats_rewieving/modelisation_une_periode_27.Rdata")
     
load("/home/menyssa/Recherche/Mimic-II/resultats_rewieving/modelisation_une_periode_27.Rdata")

lettres = c("a","b","d","f","h","j")

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

# Recupération des valeurs apprentissage
data = NULL
for(ll in lettres){ 
	lol <- na.omit(get(ll)$apprentissage)
	data <- rbind( data, round(lol[which(lol$f1_score == max(lol$f1_score)),], 2))
}
write.csv(data, "/home/menyssa/results_apprentissage.csv")
data$letters <- lettres

# Recupération des valeurs validation
data_autre = NULL
for(ll in lettres){ 
 data_autre <- round(rbind(data_autre , subset(get(ll)$validation, seuil_prediction == as.character(subset(data, letters == ll, seuil_prediction)))),2)
}
write.csv(data_autre, "/home/menyssa/results_validation.csv")

# Recupération des valeurs validation fabrice
data_autre = NULL
for(ll in lettres){ 
 data_autre <- round(rbind(data_autre , subset(get(ll)$validation_fab, seuil_prediction == as.character(subset(data, letters == ll, seuil_prediction)))),2)
}
write.csv(data_autre, "/home/menyssa/results_validation_fab.csv")

### Cross-validation models
fit.cv$table <- data.frame(fit.cv$table)
res = data.frame(
	Models = fit.cv$table$Algorithm,
	AUC = paste0(round(fit.cv$table$Ave,3)," (", round(fit.cv$table$Min,3),"-", round(fit.cv$table$Max,3),")")
)

### Cross validation 27 modeles 

load("/home/menyssa/cv_superlearner_27modeles.RData")
fit.cv$table <- data.frame( summary(fit.cv)$Table)

res = data.frame(
	Models = fit.cv$table$Algorithm,
	AUC = paste0(round(fit.cv$table$Ave,3)," (", round(fit.cv$table$Min,3),"-", round(fit.cv$table$Max,3),")"),
	coefs =  c(NA,NA,colMeans(fit.cv$coef)) 
)
# Poids 
res = data.frame(
		models = fit.cv$table$Algorithm,		
		AUC = paste0(round(fit.cv$table$Ave,3)," (", round(fit.cv$table$Min,3),"-", round(fit.cv$table$Max,3),")"),
		poids = c(NA,NA,round(colMeans(fit.cv$modele$coef),2)),
		poids_min = c(NA,NA,round(apply(fit.cv$modele$coef,2,min),2)),
		poids_max = c(NA,NA,round(apply(fit.cv$modele$coef, 2, max),2))
		)





