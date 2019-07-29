########################
#### Modelisation Boris
# Toutes les périodes
########################
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

Modelisation_superlearner_ma <- function(data_train, data_val, folds, eval_cv){
  
  set.seed(25)
  
  # data_train = data_mimic[[9]]
  # data_val = data_fabrice[[9]]
  # folds = 10
  
  # Pour que les periodes d'un patient toutes dans l'apprentissage 
  # soit toutes dans la validation
  etiquette <- c("apprentissage","validation")
  
  train_test = data.frame(
    numero = sample(etiquette, length(unique(data_train$id)), replace = T, prob = c(0.7,0.3)),
    id = unique(data_train$id))
  
  df <- merge(x = data_train, y = train_test, by = "id")
  data2 <- subset(df, select = c(-event))
  
  inTrain <- which(df$numero == "apprentissage")
  
  # Covariables
  X <- subset(data2, numero == "apprentissage")
  X <- subset(X, select = -c(numero))
  X_holdout <- subset(data2, numero == "validation")
  X_holdout <- subset(X_holdout, select = -c(numero))
  
  # Outcome
  outcome <- as.numeric(df$event == "1")
  Y <-  outcome[inTrain]
  Y_holdout <-  outcome[-inTrain]
  
  # Clean up
  rm(outcome)
  
  control.SL <- SuperLearner.CV.control(V = folds)
  # Classification
  fit <- SuperLearner(Y = Y,
                      X = X,
                      family = binomial(),
                      method = "method.AUC",
                      SL.library = c("My.SL.gls", "My.SL.glmmPQL", "My.SL.glmmML"),
                      id = X$id,
                      cvControl = control.SL)
  if(eval_cv){
    fit.cv <- Modelisation_superlearner_cv(features = X, outcome  = Y, folds = 10)  
  }else{
    fit.cv = NULL
  }
  
  ### Performance training
  pred_train <- predict.SuperLearner_ma(fit, newdata = X)
  pred <- prediction( as.vector(pred_train$pred), Y)
  auc_train <- roc(Y, as.vector(pred_train$pred),ci = TRUE)
  auc_train <- paste0(round(auc_train$ci[2],3)," [", round(auc_train$ci[1],3),"-",round(auc_train$ci[3],3), "]")
  apprentissage <- Performance_superlearner(observations = Y, predictions = pred_train$pred)
  brier_score_train = mean((Y - pred_train$pred)**2 )
  
  ### Performance Validation
  pred_test <- predict.SuperLearner_ma(fit, newdata = X_holdout)
  pred <-  prediction( as.vector(pred_test$pred), Y_holdout)
  perf_val <- performance(pred,"tpr","fpr")
  auc_val <- roc(Y_holdout, as.vector(pred_test$pred),ci = TRUE)
  auc_val <- paste0(round(auc_val$ci[2],3)," [", round(auc_val$ci[1],3),"-",round(auc_val$ci[3],3), "]")
  validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)
  brier_score_val = mean((Y_holdout - pred_test$pred)**2 )
  
  # ### Performance Fabrice
  pred_fab <- predict.SuperLearner_ma(fit, newdata = dplyr::select(data_val, -c(event)))
  pred <-  prediction( as.vector(pred_fab$pred), data_val$event)
  perf_fab <- performance(pred ,"tpr","fpr")
  auc_fab <- roc(data_val$event, as.vector(pred_fab$pred),ci = TRUE)
  auc_fab <- paste0(round(auc_fab$ci[2],3)," [", round(auc_fab$ci[1],3),"-",round(auc_fab$ci[3],3), "]")
  validation_fab <- Performance_superlearner(observations = data_val$event, predictions = pred_fab$pred)
  brier_score_fab = mean((data_val$event - pred_fab$pred)**2)
  
  return(list(
    performances_modeles = fit.cv,
    auc_apprentissage = auc_train, 
    auc_validation = auc_val,
    auc_fabrice = auc_fab,
    apprentissage = apprentissage,
    validation = validation,
    validation_fab = validation_fab,
    brier_score_train = brier_score_train,
    brier_score_val = brier_score_val,
    brier_score_fab = brier_score_fab,
    prediction_train = pred_train$pred,
    prediction_val = pred_val$pred,
    prediction_fabrice = pred_fab$pred
    #train_plot = plot_data_train,
    #internal_validation_plot = plot_data_validation,
    #external_validation_plot = plot_data_fab
  ))
  # return(list(auc_train, brier_score_train))
}

predict.SuperLearner_ma <- function (object, newdata) {
  #object = fit
  #newdata = X
  k <- length(object$libraryNames)
  predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  colnames(predY) <- object$libraryNames
  #whichLibrary <-  which(object$coef > 0)
  #predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  for (mm in 1:k) {
    # print(mm)
    newdataMM <- subset(newdata, select = object$whichScreen[object$SL.library$library[mm, 2], ])
    newdataMM <- subset(newdataMM, select = c(-id))
    newdataMM <- data.frame(model.matrix(~ ., data = newdataMM)[,])
    if(mm == 1){
      predY[, mm] <- crossprod(object$fitLibrary[[mm]]$object$coefficients , y = t(newdataMM))
    }else if(mm == 2){
      predY[, mm] <- crossprod(object$fitLibrary[[mm]]$object$coefficients$fixed, y = t(newdataMM))
    }else{
      predY[, mm] <- crossprod(object$fitLibrary[[mm]]$object$coefficients,y = t(newdataMM))
    }
  }
  getPred <- object$method$computePred(predY = predY, 
                                       coef = object$coef, 
                                       control = object$control)
  
  out <- list(pred = getPred, library.predict = predY)
  
  return(out)
}

# SL.fit.gls
write.SL.template(file = 'My.SL.gls')
My.SL.gls <- function (Y, X, newX, family, obsWeights, id){
  require(nlme)
  
  if (family$family == "binomial") {
    fit.gls <- suppressWarnings(gls(as.formula(paste('Y~',paste0(names(subset(X,select = -id)), 
                                                                 collapse='+'))), 
                                    data = cbind(Y,X), 
                                    corAR1(form = ~ 1 | id)))
  }
  id <- id
  newX = subset(newX, select = -id)
  pred <- predict(fit.gls, newdata = newX)
  fit <- list(object = fit.gls)
  class(fit) <- c("SL.fit.gls")
  out <- list(pred = pred, fit = fit)
  return(out)
}

# SL.fit.glmmPQL
write.SL.template(file = 'My.SL.glmmPQL')
My.SL.glmmPQL <- function (Y, X, newX, family, obsWeights,id)
{
  require(nlme); require(MASS)
  
  if (family$family == "binomial") {
    fit.glmmPQL <- suppressWarnings(glmmPQL(
      as.formula(paste('Y~', paste0(names(subset(X, select = -id)), collapse='+'))), 
      random = ~ 1 | id,
      data=cbind(Y,X), niter = 10 ,family='binomial'))
  }
  id = id
  newX = subset(newX, select = -id)
  # pred <- predict(fit.glmmPQL, newdata = newX, type = c("response"),level = 0)
  pred <- predict(fit.glmmPQL, newdata = newX)
  fit <- list(object=fit.glmmPQL)
  class(fit) <- c("SL.fit.glmmPQL")
  out <- list(pred = pred, fit = fit)
  return(out)
}

# SL.fit.glmmML
write.SL.template(file = 'My.SL.glmmML')
My.SL.glmmML <- function (Y, X, newX, family, obsWeights,id) {
  require(glmmML)
  if (family$family == "binomial") {
    fit.glmmML <- suppressWarnings(glmmML(as.formula(paste('Y~',
                                                           paste0(names(subset(X,select = -id)), collapse='+'))),
                                          family = binomial,
                                          boot = 10,
                                          data = cbind(Y,X),
                                          cluster = id))
  }
  id = id
  newX = subset(newX, select = -id)
  pred <- predict.glmmML(fit.glmmML, newdata = newX)
  fit  <- list(object=fit.glmmML)
  class(fit) <- c("SL.fit.glmmML")
  out <- list(pred = pred, fit = fit)
  return(out)
}

predict.glmmML <- function(object, newdata){
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.gls <- function(object, newdata){
  # object = fit.gls
  # newdata = subset(X, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  # p2      <-  crossprod(x = fit.gls$coefficients ,y = t(newdata))
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.glmmPQL <- function(object, newdata){
  # object = fit.glmmPQL
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients$fixed %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

######### --------------------- 

predict.SL.fit.glmmML <- function(object, newdata){
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.SL.fit.gls <- function(object, newdata){
  # object = fit.gls
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.SL.fit.glmmPQL <- function(object, newdata){
  # object = fit.glmmPQL
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients$fixed %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

generation.data <- function(data){
  
  characteristics <- subset(data, select = c(id, event,age, gender, sapsi_first, sofa_first,
                                             care_unit1, care_unit2, care_unit4))
  
  characteristics_treatments <- subset(data, select = c(id, event,age, gender, sapsi_first, sofa_first,
                                                        care_unit1, care_unit2, care_unit4, 
                                                        ventilation, amine, sedation))
  # Resume 
  resume	<- subset(data, select = c(id, event,age, gender,sapsi_first, sofa_first,
                                    care_unit1,care_unit2, care_unit4,amine, sedation, ventilation, 
                                    grep("mean_",names(data)),
                                    grep("var_",names(data))))
  resume_sans_pam <- subset(data, select = c(id, event,age, gender, sapsi_first, sofa_first,
                                             care_unit1, care_unit2, care_unit4, 
                                             ventilation, amine, sedation, mean_hr, 
                                             mean_spo2, var_hr, var_spo2))
  # Linéaire 
  lineaire <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                      care_unit1,care_unit4, care_unit2, amine, sedation, ventilation,
                                      grep("alpha_",names(data)),
                                      grep("beta_",names(data))))
  
  lineaire_sans_pam <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                               care_unit1,care_unit4, care_unit2, amine, sedation, ventilation,
                                               grep("alpha_",names(data)),
                                               grep("beta_",names(data))))%>%
    dplyr::select(-c(alpha_abpm, alpha_abps, alpha_abpd, 
                     beta_abpm, beta_abps, beta_abpd))
  #  Modele ARMA
  arma  <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                   care_unit1,care_unit2,care_unit4,amine, sedation, ventilation, 
                                   grep("ar_",names(data)),
                                   grep("ma_",names(data)),
                                   grep("inter_",names(data)))) %>%
    dplyr::select(-c(var_hr,var_spo2,var_abpm,var_abpd, var_abps ))
  
  arma_sans_pam <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                           care_unit1,care_unit2,care_unit4, 
                                           amine, sedation, ventilation, 
                                           grep("ar_",names(data)),
                                           grep("ma_",names(data)),
                                           grep("inter_",names(data)))) %>% 
    dplyr::select(-c(var_hr, var_spo2, var_abpm, var_abps, var_abpd,
                     ar_abpm, ar_abps, ar_abpd,
                     ma_abpm, ma_abps, ma_abpd,
                     inter_abpm, inter_abps, inter_abpd))
  #  Ondelette de Haar 
  haar <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                  care_unit1,care_unit2, care_unit4,amine, sedation, ventilation,
                                  grep("W1_",names(data)),
                                  grep("V1_",names(data))))
  haar_sans_pam <- subset(data, select = c(id,event,age, gender,sapsi_first, sofa_first,
                                           care_unit1,care_unit2, care_unit4,amine, sedation, ventilation,
                                           grep("W1_",names(data)),
                                           grep("V1_",names(data)))) %>%
    dplyr::select(-c(W1_abpm,W1_abps,W1_abpd, V1_abpm, V1_abps,V1_abpd ))
  
  return(list(
    characteristics, characteristics_treatments, resume, resume_sans_pam,
    lineaire, lineaire_sans_pam, arma, arma_sans_pam,
    haar, haar_sans_pam  ))
}

#####################
# MAIN
#####################

options(na.action='na.pass')
load(file = "~/Recherche/Mimic-II/data/df_modelisation.RData")

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
data_mimic <- generation.data(data = df_modelisation_mimic)
data_fabrice <- generation.data(data = df_modelisation_fabrice)
# rm(list = c("df_modelisation", "df_sample"))

a <- Modelisation_superlearner_ma(data_train = data_mimic[[1]],
                                  data_val = data_fabrice[[1]],
                                  eval_cv = FALSE,
                                  folds = 10)
b <- Modelisation_superlearner_ma(data_train = data_mimic[[2]],
                                  data_val = data_fabrice[[2]],
                                  eval_cv = FALSE,
                                  folds = 10)
