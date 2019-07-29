########################
# Modelisation avec le superlearner 
# toutes les périodes par patient
########################

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

options(na.action='na.pass')
source("~/Recherche/Mimic-II/scripts/R/toolbox/library_superlearner.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions_superlearner.R")
load(file = "~/Recherche/Mimic-II/data/clean/mimic2/df_modelisation.RData")

data_mimic <- generation.data(data = df_modelisation_mimic)
data_fabrice <- generation.data(data = df_modelisation_fabrice)

#############################
#### Modelisation
#############################

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
                                
save(d, f, h, j, file = "toutes_periodes.Rdata")
load(file = "toutes_periodes.Rdata")

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
##########
# Graphiques
##########

blancheur <- theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # legend.title = element_blank(),
  legend.justification = c(1, 0.25),
  legend.position = "right",
  #legend.position = c(1, 0.25), 
  legend.background = element_rect(fill="gainsboro",
                                   size=0.5, linetype="solid", 
                                   colour ="black"),
  # legend.position = "none",
  panel.background = element_rect(fill = "white",
                                  colour = "white"),
  axis.line = element_line(colour = "black")
)

##########
# Validation
##########

#  1 - Seuils pour la sensibilité et la specificité
temp <- rbind(a_ma$performances_validation$matrice_resultats,
              b_ma$performances_validation$matrice_resultats,
              c_ma$performances_validation$matrice_resultats,
              d_ma$performances_validation$matrice_resultats)

temp$type <- c(
  rep("Haar",51), 
  rep("ARMA",51), 
  rep("Linear",51), 
  rep("Statistical measures",51))
write.csv(temp, "performance_validation_ma.csv", row.names = F)

temp  <- subset(temp, select = - accur)
mdata <- melt(temp, id=c("seuil_prediction", "type"))
mdata$id <- paste0(mdata$type,"_",mdata$variable)

sensiplot <- ggplot(mdata, aes(x = seuil_prediction, y = value,colour=id))+
  geom_line(aes(linetype=variable, color=type))+blancheur +
  labs(title=" ", y="Value", x="Thresholds")+
  scale_x_continuous(  expand = c(0, 0),limits=c(0,0.5)) +
  scale_y_continuous(  expand = c(0, 0),limits=c(0,1))+
  scale_color_manual(name="Preprocessing Methods", 
                     labels = c("ARMA", "Haar", "Linear", "Statistical measures"), 
                     values = c("ARMA"="black", "Haar"="grey", "Linear"="orange","Statistical measures"="red"))+
  scale_linetype_discrete(name = "Measures", labels = c("Sensitivity", "Specificity"))


#  2 - Plot pour les AUC
temp <- data.frame(
  fpr = c(a_ma$performance_validation@x.values[[1]],
          b_ma$performance_validation@x.values[[1]],
          c_ma$performance_validation@x.values[[1]],
          d_ma$performance_validation@x.values[[1]]),
  tpr = c(a_ma$performance_validation@y.values[[1]],
          b_ma$performance_validation@y.values[[1]],
          c_ma$performance_validation@y.values[[1]],
          d_ma$performance_validation@y.values[[1]]),
  type = c(rep("Haar",15321), rep("ARMA",15321), 
           rep("Linear",15321), rep("Statistical measures",15321))
)

aucplot <- ggplot(temp, aes(x = fpr, y = tpr, colour =type))+
  geom_line()+ 
  blancheur + 
  geom_abline(intercept=0,slope=1, linetype="dashed",size= 0.2) +
  scale_x_continuous("False Positive Rate (1 - Specificity)",
                     expand = c(0, 0), limits=c(0,1)) +
  scale_y_continuous("True Positive Rate (Sensitivity)",
                     expand = c(0, 0), limits=c(0,1)) +
  scale_color_manual(name="Preprocessing Methods", 
                     labels = c("ARMA", "Haar", "Linear", "Statistical measures"), 
                     values = c("ARMA"="black", "Haar"="grey", "Linear"="orange","Statistical measures"="red"))

##########
# Apprentissage 
##########

temp_train <- rbind(a_ma$performances_apprentissage$matrice_resultats,
                    b_ma$performances_apprentissage$matrice_resultats,
                    c_ma$performances_apprentissage$matrice_resultats,
                    d_ma$performances_apprentissage$matrice_resultats)

temp_train$type <- c(
  rep("Haar",51), 
  rep("ARMA",51), 
  rep("Linear",51), 
  rep("Statistical measures",51))

write.csv(temp_train, "performance_apprentissage_ma.csv", row.names = F)

#  1 - Seuils pour la sensibilité et la specificité
temp_train  <- subset(temp_train, select = - accur)
mdata <- melt(temp_train, id=c("seuil_prediction", "type"))
mdata$id <- paste0(mdata$type,"_",mdata$variable)

sensiplot <- ggplot(mdata, aes(x = seuil_prediction, y = value,colour=id))+
  geom_line(aes(linetype=variable, color=type))+blancheur +
  labs(title =" ", y="Value", x="Thresholds")+
  scale_x_continuous(  expand = c(0, 0),limits=c(0,0.5)) +
  scale_y_continuous(  expand = c(0, 0),limits=c(0,1))+
  scale_color_manual(name="Preprocessing Methods", 
                     labels = c("ARMA", "Haar", "Linear", "Statistical measures"), 
                     values = c("ARMA"="black", "Haar"="grey", "Linear"="orange","Statistical measures"="red"))+
  scale_linetype_discrete(name = "Measures", labels = c("Sensitivity", "Specificity"))

#  2 - Plot pour les AUC
temp_train <- data.frame(
  fpr = c(a_ma$preformance_apprentissage@x.values[[1]],
          b_ma$preformance_apprentissage@x.values[[1]],
          c_ma$preformance_apprentissage@x.values[[1]],
          d_ma$preformance_apprentissage@x.values[[1]]),
  tpr = c(a_ma$preformance_apprentissage@y.values[[1]],
          b_ma$preformance_apprentissage@y.values[[1]],
          c_ma$preformance_apprentissage@y.values[[1]],
          d_ma$preformance_apprentissage@y.values[[1]]),
  type = c(rep("Haar",37225), rep("ARMA",37225), 
           rep("Linear",37225), rep("Statistical measures",37225))
)

aucplot <- ggplot(temp_train, aes(x = fpr, y = tpr, colour =type))+
  geom_line()+ 
  blancheur + 
  geom_abline(intercept=0,slope=1, linetype="dashed",size= 0.2) +
  scale_x_continuous("False Positive Rate (1 - Specificity)",
                     expand = c(0, 0), limits=c(0,1)) +
  scale_y_continuous("True Positive Rate (Sensitivity)",
                     expand = c(0, 0), limits=c(0,1)) +
  scale_color_manual(name="Preprocessing Methods", 
                     labels = c("ARMA", "Haar", "Linear", "Statistical measures"), 
                     values = c("ARMA"="black", "Haar"="grey", "Linear"="orange","Statistical measures"="red"))


##########
# Cross_validation
##########

a_ma.cv <- Modelisation_superlearner_ma.cv(df = data[[10]], folds = 5, V = 10, sl = librairie_mixed)
b_ma.cv <- Modelisation_superlearner_ma.cv(df = data[[7]], folds = 5, V = 10,  sl = librairie_mixed)
c_ma.cv <- Modelisation_superlearner_ma.cv(df = data[[4]], folds = 5, V = 10, sl = librairie_mixed)
d_ma.cv <- Modelisation_superlearner_ma.cv(df = data[[1]],  folds = 5, V = 10, sl = librairie_mixed)

#################
# Résultats
#################

temp = rbind(d_ma.cv_10$data, c_ma.cv_10$data, b_ma.cv_10$data, a_ma.cv$data)
temp$auc <- paste0(round(temp$Ave,3),"[",round(temp$Min,3), "-",round(temp$Max,3) ,"]")
temp$names <- c(rep("resume",5), rep("lineaire",5), rep("arma",5), rep("haar",5))
write.csv(temp[,c(1,6,7)], "cross_validation_ma_10.csv", row.names = F)

temp = rbind(d_ma.cv$data,c_ma.cv$data,b_ma.cv$data,a_ma.cv$data)
temp$auc <- paste0(round(temp$Ave,3),"[",round(temp$Min,3), "-",round(temp$Max,3) ,"]")
temp$names <- c(rep("resume",5), rep("lineaire",5),rep("arma",5), rep("haar",5))
write.csv(temp[,c(1,6,7)], "cross_validation_ma_5.csv", row.names = F)


# Poids 
res = data.frame(
		models = fit.cv$table$Algorithm,		
		AUC = paste0(round(fit.cv$table$Ave,3)," (", round(fit.cv$table$Min,3),"-", round(fit.cv$table$Max,3),")"),
		poids = c(NA,NA,round(colMeans(fit.cv$modele$coef),2)),
		poids_min = c(NA,NA,round(apply(fit.cv$modele$coef,2,min),2)),
		poids_max = c(NA,NA,round(apply(fit.cv$modele$coef, 2, max),2))
		)














