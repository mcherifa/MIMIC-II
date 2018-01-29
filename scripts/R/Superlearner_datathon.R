#######################################################
#-------- Modelisation Super Learner 
# By Menyssa CHERIFA 
#######################################################

library(SuperLearner)
library(glmnet)
library(ggplot2)
library(nnet)
library(randomForest)
library(ROCR)
library(gbm)
library(polspline)
library(gam)
library(arm)
library(ipred)
# library(xgboost)
library(pROC)
library(verification)
library(caret)
library(plyr)

#######################################################
#-------- Data management
#######################################################

# Tout recoder en binaire pour conversion matrice (SL.library nnet et xgboost)

data <- read.csv("~/Datathon/data/data_complete.csv")

data <- data[!is.na(data$mean_temperature),]
data <- data[!is.na(data$mean_glucose),]
data <- data[!is.na(data$mean_resprate),]
data <- data[!is.na(data$mean_abpsys),]

data <- subset(data,select = -c(subject_id,mean_glucose,
                                mean_pulse,mean_temperature,mean_resprate,
                                mean_abpmean,mean_dias,mean_abpsys,mean_hr,
                                max_pulse))

# Tout convertir en numérique 
data$gender <- ifelse(data$gender == "M",1,0)
data$admission_type <- ifelse(data$admission_type == "ELECTIVE",0,1)

levels(data$first_careunit) <- c(levels(data$first_careunit),0,1,2,3,4)

data$first_careunit[data$first_careunit == "CCU"] <- 0
data$first_careunit[data$first_careunit == "CSRU"] <- 1
data$first_careunit[data$first_careunit == "MICU"] <- 2
data$first_careunit[data$first_careunit == "SICU"] <- 3
data$first_careunit[data$first_careunit == "TSICU"] <- 4
data <- droplevels(data)
data$first_careunit <- as.numeric(as.character(data$first_careunit))

# NA par variable
lapply(lapply(data, is.na),table)

# Data final
datasave <- data

#######################################################
#-------- Essai réseau neurone
#######################################################

# 1 - Réseau de neurone : exemple 

neuronal <- nnet(formula = data$outcome ~., 
                 data  = subset(data, select = -c(outcome)),
                 size = 10, 
                 MaxNWts = 1e+05, 
                 decay = 1.6,
                 maxit = 500,
                 trace = T)

predictions <- predict(neuronal, data)
AUC <- roc(data$outcome, as.numeric(predictions),ci = T)

# 2 - Réseau de neurone  : entrainement 20 fois sur la précision

results <- data.frame(h = NA,acc = NA,acc.l=NA,acc.u=NA)

for (h in 1:20) {
  cat("Run", h, "\n")
  # With range
  
  ann <- nnet(data$outcome ~., 
              data = subset(data, select = -c(outcome)), 
              size = h, 
              decay = 5e-4, 
              rang  =.5, 
              maxit = 200)
  
  pred <- (predict(ann, data = data, type="raw")>0.5)+0
  pred <- confusionMatrix(pred,data$outcome)
  acc <- pred$overall["Accuracy"]
  acc.l <- pred$overall["AccuracyLower"]
  acc.u <- pred$overall["AccuracyUpper"]
  results <- rbind(results, c(h, acc, acc.l, acc.u))
}

plot(results[,2] ~ results[,1], 
     ylim=c(0,1), 
     xlab="Hidden neurons", 
     ylab="Weighted accuracy"
)
rm(results,ann,pred,acc,acc.l,acc.u)

# 3-  Réseau de neurone  : entrainement 20 * sur l'AUC 

results_2  <- data.frame(h = NA, AUC = NA)

for (h in 1:20) {
  
  cat("Run", h, "\n")
  # With range
  
  ann <- nnet(formula = data$outcome ~., 
              data  = subset(data, select = -c(outcome)),
              size = 10, 
              MaxNWts = 1e+05, 
              decay = 1.6,
              maxit = 500,
              trace = T)
  
  predictions <- predict(ann, data)
  AUC <- roc(data$outcome, as.numeric(predictions),ci = T)
  
  results_2 <- rbind(results_2, c(h, AUC$auc))
}

plot(results_2[,2] ~ results_2[,1], 
     ylim=c(0,1), 
     xlab="Hidden neurons", 
     ylab="AUC"
)


#######################################################
#--------- Création des wrappers
#######################################################

create.SL.glmnet <- function(alpha = c(0.25, 0.50, 0.75)) {
  for(mm in seq(length(alpha))){
    eval(parse(text = 
                 paste('SL.glmnet.', 
                       alpha[mm], 
                       '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')),
         envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet(alpha = c(0.2, 0.4, 0.6, 0.8))

SL.gbm.1 <- function(..., interaction.depth = 1) {
  SL.gbm(..., interaction.depth = interaction.depth)
}

create.SL.nnet <- function(size = c(3, 4, 5)) {
  for(mm in seq(length(size))) {
    eval(parse(text = 
                 paste('SL.nnet.', size[mm],
                       '<- function(..., size = ', size[mm], ') SL.nnet(..., size = size)', 
                       sep = '')),
         envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.nnet()

SL.gam.3 <- function(..., deg.gam = 3) {
  SL.gam(..., deg.gam = deg.gam)
}

create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10),
                                               nodesize = c(1, 5, 10))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) { 
    eval(parse(file = "", 
               text = paste("SL.randomForest.",
                            mm,
                            "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") 
                            SL.randomForest(..., mtry = mtry, nodesize = nodesize)",
                            sep = "")), 
         envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest(list(mtry = c(2, 5, 7, 12), 
                            nodesize = c(5, 10, 30)))

create.SL.xgboost(list(ntrees = c(100, 500), 
                       max_depth = c(1, 2),
                       minobspernode = 10,
                       shrinkage = c(0.1, 0.01, 0.001)),
                  detailed_names = F, 
                  env = .GlobalEnv,
                  name_prefix = "SL.xgb")

#######################################################
#--------- Définir la library du SuperLearner
#######################################################

SL.library <- c("SL.stepAIC",
                "SL.rpart",
                "SL.gam",
                "SL.glm",
                "SL.glm.interaction"
                # "SL.nnet.3",
                # "SL.nnet",
                # "SL.nnet.3",
                # "SL.nnet.4",
                # "SL.nnet.5",
                # "SL.glmnet",
                # "SL.glmnet.0.6",
                # "SL.glmnet.0.4",
                # "SL.glmnet.0.8",
                # "SL.glmnet.0.2",
                # "SL.randomForest.5", 
                # "SL.randomForest.9",                    
                # "SL.randomForest.6",
                # "SL.randomForest.3",
                # "SL.randomForest.1",
                # "SL.bayesglm",
                # "SL.xgb.5",
                # "SL.xgb.9"
)

#######################################################
#--------- Apprentissage
#######################################################

# Cross validation 
control.SL <- SuperLearner.CV.control(V = 10, 
                                      stratifyCV = TRUE,
                                      shuffle = TRUE,
                                      validRows = NULL)
# 1- Apprentissage  
fit.mod1 <- mcSuperLearner(Y = data$outcome,
                           X = subset(data, select = -c(outcome)),
                           SL.library = SL.library,
                           family     = binomial(),
                           method     ="method.AUC",
                           verbose    = TRUE,
                           cvControl  = control.SL)
# Predictions  
out <- predict(fit.mod1)


# 2 - Apprentissage  : cross validation 10 folds
V = 10
fit.mod1.AUC <- CV.SuperLearner(Y = data$outcome,
                                X = subset(data, select = -c(outcome)),
                                SL.library  = SL.library,
                                family      = binomial(),
                                method      = "method.AUC",
                                verbose     = TRUE,
                                V           = V, 
                                parallel    = 'multicore')

summary(fit.mod1.AUC)

# 3 - Apprentissage  : cross validation 2 folds
V = 2
fit.mod2.AUC <- CV.SuperLearner(Y = data$outcome,
                                X = subset(data, select = -c(outcome)),
                                SL.library = SL.library,
                                family     = binomial(),
                                method     = "method.AUC",
                                verbose    = TRUE, 
                                V          = V, 
                                parallel   = 'multicore')
summary(fit.mod2.AUC)

#######################################################
#--------- Graphique
#######################################################

fit.plot <- summary(fit.mod1.AUC)
fit.plot <- fit.plot$Table
fit.plot <- fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- factor(fit.plot$Algorithm,
                             levels = fit.plot$Algorithm[order(fit.plot$Ave, decreasing = F)])

# Renomme les Algorithmes (légende et axe Y)

fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.rpart_All", "Recursive Partitioning")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glm_All", "Generalized Linear\nModel")
# fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glmnet_All", "Penalized GLM")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glmnet.0.6_All", "Penalized\nGeneralized Linear Model")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.nnet.3_All", "Neural Network")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.stepAIC_All", "Stepwise regression")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.gbm.1_All", "Gradient Boosting")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.gam_All", "Generalized Additive\nModel")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.randomForest.5_All", "Random Forest")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.bayesglm_All", "Bayesian\nGeneralized Linear Model")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.ipredbagg_All", "Bagging\nclassifier trees")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glm.interaction_All", "GLM interaction")
fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.xgb.5_All", "XG boost")


fit.plot <- fit.plot[fit.plot$Algorithm != "SL.randomForest.9_All" &
                       fit.plot$Algorithm != "SL.xgb.9_All",] 
#& fit.plot$Algorithm != "SL.nnet.3_All",]

# Produire le plot AUC
ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0, 1) +
  xlab(paste(10, "fold Cross-Validated AUC")) +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black") +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "light grey") +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "light grey") +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12))
