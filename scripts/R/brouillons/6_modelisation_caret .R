########################
# Modelisation à l'aide du package caret 
########################

# model : reseau de neurone, SVM, KNN, arbre, regression logistique, neuro fuzzy network 

library(tidyverse)
library(origami)
library(sl3)
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
library(xgboost)
library(pROC)
library(verification)
library(caret)
library(plyr) 
library(dplyr)
library(doParallel)

 
registerDoParallel(cores = 3)

df_modelisation <-
    readRDS("/home/menyssa/Mimic/data/clean/mimic2/df_modelisation.rds") %>%      
    subset(age < 100 & eevent == 0) %>%
    subset(select = c(-identif, -eevent, -care_unit, -periode)) %>%
    mutate(
    id = as.numeric(as.character(id)),
    # periode = as.numeric(as.character(periode)),
    event   = as.factor(as.character(event)),
    gender  = factor(gender, levels = c("F","M"), labels = c("1","0")),
    amine   = factor(amine,  levels =  c(0,1), labels = c("0","1")),
    curare  = factor(curare, levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate, levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,  levels =  c(0,1), labels = c("0","1"))
  )

sample_resume	<- subset(df_sample, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                              care_unit1,care_unit4,amine, sedate, venti, 
                                              grep("m_",names(df_sample)),
                                              grep("v_",names(df_sample))))







# Tirage au sort une ligne par id 

df_sample <- df_modelisation %>% 
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()

  
#  Sous df_sample en fonction méthodes pour résumer
sample_resume 	<- df_sample[,c(1:12,
                               grep("m_",names(df_sample)),
                               grep("v_",names(df_sample)))]

sample_lineaire <- df_sample[,c(1:12,
                                grep("alpha_",names(df_sample)),
                                grep("beta_",names(df_sample)))]

sample_arma 		<- df_sample[,c(1:12,
                              grep("ar_",names(df_sample)),
                              grep("ma_",names(df_sample)),
                              grep("inter_",names(df_sample)))]

sample_haar 		<- df_sample[,c(1:12,
                              grep("W1_",names(df_sample)),
                              grep("V1_",names(df_sample))
)]

Modelisation <- function(data){
  data = sample_sans_pam
  data <- subset(data, select = -id)
  data$event <- as.factor(ifelse(data$event == 0, "class_1", "class_0"))
  
  inTrain <- createDataPartition(data$id, p = 0.7)[[1]]
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  numFolds <- trainControl(method = 'cv',
                           number = 5,
                           classProbs = TRUE,
                           verboseIter = TRUE,
                           summaryFunction = twoClassSummary)
  
  # glm
  
  fit <- train(event ~ .,
               data = training,
               method = 'glm',
               metric = "ROC",
               preProcess = c('center', 'scale'),
               trControl = numFolds)
  
  out_glm <- fit$results
  
  # mlp
  
  fit1 <- train(event ~ .,
                data = training,
                method = 'mlp',
                metric = "ROC",
                preProcess = c('center', 'scale'),
                tuneGrid = expand.grid(size = 6),
                trControl = numFolds)
  
  out_mlp <- fit1$results
  
  
  # rn
  
  fit2 <- train(event ~ .,
                data = training,
                method = 'nnet',
                metric = "ROC",
                maxit = 1000,
                preProcess = c('center', 'scale'),
                trControl = numFolds,
                tuneGrid = expand.grid(size = 6, decay = c(1)))
  
  out_nnet <- fit2$results
  
  # rf
  
  mtry <- sqrt(ncol(training))
  
  fit3 <- train(event ~.,
                data = training,
                method = "rf",
                metric = "ROC",
                tuneGrid = expand.grid(.mtry=mtry),
                trControl = numFolds)
  
  out_rf <- fit3$results
  
  
  probs <- predict(fit, newdata = testing, type='prob')
  aire1  <- roc(as.numeric(testing$event), as.numeric(max.col(probs)),ci = T)
  probs <- predict(fit1, newdata = testing, type='prob')
  aire2  <- roc(as.numeric(testing$event), as.numeric(max.col(probs)),ci = T)
  probs <- predict(fit2, newdata = testing, type='prob')
  aire3  <- roc(as.numeric(testing$event), as.numeric(max.col(probs)),ci = T)
  probs <- predict(fit3, newdata = testing, type='prob')
  aire4  <- roc(as.numeric(testing$event), as.numeric(max.col(probs)),ci = T)
  
  
  
  names <- data.frame(noms = c("glm","mlp","rf","nnet"))
  out   <- rbind.fill(out_glm,out_mlp,out_rf, out_nnet)
  out   <- cbind(names,out)  
  
  out <- data.frame(out, 
                    auc = c(aire1$ci[2],aire2$ci[2],aire3$ci[2],aire4$ci[2]),
                    auc_lo = c(aire1$ci[1],aire2$ci[1],aire3$ci[1],aire4$ci[1]),
                    auc_up = c(aire1$ci[3],aire2$ci[3],aire3$ci[3],aire4$ci[3]))
  return(out)
}

##################
# Modelisation sans superlearner 
##################


lineaire    <-  Modelisation(data = sample_lineaire)
lineaire    <- cbind(type = rep("Lineraire",4), lineaire)
statistique <-  Modelisation(data = sample_resume)
statistique <- cbind(type = rep("statistique",4), statistique)
arma    <-  Modelisation(data = sample_arma)
arma    <- cbind(type = rep("arma",4), arma)
haar    <-  Modelisation(data = sample_haar)
haar    <- cbind(type = rep("haar",4), haar)

##################
# Resultats
##################

output <- rbind(statistique,lineaire,arma,haar)
write.csv(output, file='/Users/menyssa/Desktop/output_caret.csv', row.names = FALSE, quote = FALSE)

