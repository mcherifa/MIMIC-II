
#########################
# Toutes les fonctions pour le superlearner
#########################



#############################################
# Fonction qui génère les types de données:
#############################################
generation.data <- function(data){
  
  characteristics <- subset(data, select = c(id, event, event_cum, event_24h,
  																						age, gender, sapsi_first, sofa_first,
                                             care_unit1, care_unit2, care_unit4))
  
  characteristics_treatments <- subset(data, select = c(id, event, event_cum, event_24h,
  																											age, gender, sapsi_first, sofa_first,
                                                        care_unit1, care_unit2, care_unit4, 
                                                        ventilation, amine, sedation))
  # Resume 
  resume	<- subset(data, select = c(id, event, event_cum, event_24h, 
  																	age, gender,sapsi_first, sofa_first,
                                    care_unit1,care_unit2, care_unit4,
                                    amine, sedation, ventilation, 
                                    grep("mean_",names(data)),
                                    grep("var_",names(data))))
  resume_sans_pam <- subset(data, select = c(id, event, event_cum, event_24h,
  																					age, gender, sapsi_first, sofa_first,
                                             care_unit1, care_unit2, care_unit4,
                                             ventilation, amine, sedation, mean_hr, 
                                             mean_spo2, var_hr, var_spo2))
  # Linéaire 
  lineaire <- subset(data, select = c(id, event, event_cum, event_24h,
  																		age, gender,sapsi_first, sofa_first,
                                      care_unit1,care_unit4, care_unit2,
                                      amine, sedation, ventilation,
                                      grep("alpha_",names(data)),
                                      grep("beta_",names(data))))
  
  lineaire_sans_pam <- subset(data, select = c(id,event, event_cum, event_24h,
  																						 age, gender,sapsi_first, sofa_first,
                                               care_unit1,care_unit4, care_unit2,
                                               amine, sedation, ventilation,
                                               grep("alpha_",names(data)),
                                               grep("beta_",names(data))))%>%
    dplyr::select(-c(alpha_abpm, alpha_abps, alpha_abpd, 
                     beta_abpm, beta_abps, beta_abpd))
  #  Modele ARMA
  arma  <- subset(data, select = c(id,event, event_cum, event_24h,
 																	 age, gender,sapsi_first, sofa_first,
                                   care_unit1,care_unit2,care_unit4,
                                   amine, sedation, ventilation, 
                                   grep("ar_",names(data)),
                                   grep("ma_",names(data)),
                                   grep("inter_",names(data)))) %>%
    dplyr::select(-c(var_hr,var_spo2,var_abpm,var_abpd, var_abps ))
  
  arma_sans_pam <- subset(data, select = c(id,event, event_cum, event_24h,
  																				 age, gender,sapsi_first, sofa_first,
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
  haar <- subset(data, select = c(id,event, event_cum, event_24h,
 																  age, gender,sapsi_first, sofa_first,
                                  care_unit1,care_unit2, care_unit4,amine, sedation, ventilation,
                                  grep("W1_",names(data)),
                                  grep("V1_",names(data))))
  haar_sans_pam <- subset(data, select = c(id,event, event_cum, event_24h,
  																				age, gender,sapsi_first, sofa_first,
                                          care_unit1,care_unit2, care_unit4,
                                          amine, sedation, ventilation,
                                           grep("W1_",names(data)),
                                           grep("V1_",names(data)))) %>%
    dplyr::select(-c(W1_abpm,W1_abps,W1_abpd, V1_abpm, V1_abps,V1_abpd ))
  
  return(list(
    characteristics, characteristics_treatments, resume, resume_sans_pam,
    lineaire, lineaire_sans_pam, arma, arma_sans_pam,
    haar, haar_sans_pam  ))
}

#############################################
# Fonction qui calcule la sensibilité et la spécifictié pour différents seuils:
#############################################

Performance_superlearner <- function(observations, predictions){
  
  # observations = factor(Y_holdout, levels = c(0,1),labels = c("NoAHE", "AHE"))
  # predictions = as.vector(pred_test$pred)
  
  observations = factor(observations, levels = c(0,1),labels = c("NoAHE", "AHE"))
  predictions = as.vector(predictions)
  
  seuil_prediction <- seq(0, 1, 0.05)
  sensi = speci = accur = vpp = vpn = f1_score = vector()
  
  
  for( i in 1:length(seuil_prediction)){
    # Predictions binaires
    
    pred <- factor(ifelse(predictions < seuil_prediction[i], 0, 1), 
                   levels = c(0,1),
                   labels = c("NoAHE", "AHE"))
    # Table de confusion
    tab <- table(pred, observations) 
    table_results = confusionMatrix(tab, positive = "AHE")
    sensi[i] = table_results$byClass[1]
    speci[i] = table_results$byClass[2]
    accur[i] = table_results$overall[1]
    vpp[i] = table_results$byClass[3]
    vpn[i] = table_results$byClass[4]
    f1_score[i] =  table_results$byClass[7]
  }
  matrice = data.frame(cbind(seuil_prediction, sensi, speci,accur, vpp, vpn, f1_score))
  # mdata <- melt(matrice, id=c("seuil_prediction"))
  # 
  # sensiplot = ggplot(mdata, aes(x = seuil_prediction, y = value, color = variable))+
  #   geom_line() + 
  #   theme_classic() +
  #   labs(title="Super Learner's Performances", y="Value", x="Thresholds")+
  #   scale_color_manual(name="Measures", 
  #                      labels = c("Sensitivity", 
  #                                 "Specificity", 
  #                                 "Accuracy"), 
  #                      values = c("sensi"="blue", 
  #                                 "speci"="red", 
  #                                 "accur"="green"))
  return(matrice)
  #plot = sensiplot, 
  #matrice_resultats = matrice
  #))
}
# apprentissage <- Performance_superlearner(observations = Y, predictions = Y)
# validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)

#############################################
# Superlearner
#############################################


#### Modelisation 1 période

Modelisation_superlearner <- function(data_train, data_val, folds, eval_cv){ 
  
  set.seed(25)
  
	#data_train = subset(data_mimic[[9]], select = -c(event_24h, event_cum))
	#data_val = subset(data_fabrice[[9]], select = -c(event_24h, event_cum))
	#folds = 10

  # Split
  inTrain  <- createDataPartition(data_train$id, p = 0.7)[[1]]
  data2 <- subset(data_train, select = -c(id, event))
  
  # Covariables
  X <- data2[inTrain,]
  X_holdout <- data2[-inTrain,]
  
  # Outcome
  outcome <- as.numeric(data_train$event == "1")
  Y <-  outcome[inTrain]
  Y_holdout <-  outcome[-inTrain]
  
  # Clean up #Currently only works with gaussian data SL.loess, SL.leekasso
  rm(outcome)
 
  # Cross validation
  control.SL <- SuperLearner.CV.control(V = folds,
                                        stratifyCV = TRUE,
                                        shuffle = FALSE,
                                        validRows = NULL)
  
  # 9 modèles
  librarie <- c("SL.nnet","SL.glm","SL.glmnet",
  							"SL.rpart","SL.gbm","SL.bayesglm",
  							"SL.randomForest", "SL.xgboost",
  							"SL.gam")
  
  ### Apprentissage
  fit <- SuperLearner(Y = Y,
                      X = X,
                      family = binomial(),
                      method = "method.AUC",
                      SL.library = librarie,
                      control = list(saveFitLibrary = TRUE, trimLogit = 0.001),
                      cvControl  = control.SL)
  if(eval_cv){
    fit.cv <- Modelisation_superlearner_cv(features = X, outcome  = Y, folds = 10)  
  }else{
    fit.cv = NULL
  }
  # temps <- fit$times$everything
  
  ### Performance training
  # onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
  pred_train <- predict(fit, newdata = X , type ='prob', onlySL = FALSE)
  pred <- prediction( as.vector(pred_train$pred), Y)
  auc_train <- roc(Y, as.vector(pred_train$pred),ci = TRUE, conf.level = 0.975)
  auc_train <- paste0(round(auc_train$ci[2],3)," [", round(auc_train$ci[1],3),"-",round(auc_train$ci[3],3), "]")
  apprentissage <- Performance_superlearner(observations = Y, predictions = pred_train$pred)
  brier_score_train = mean((Y - pred_train$pred)**2 )
  
  ### Performance Validation
  pred_test <- predict(fit, newdata = X_holdout , type='prob', onlySL = FALSE)
  pred <-  prediction( as.vector(pred_test$pred), Y_holdout)
  perf_val <- performance(pred,"tpr","fpr")
  auc_val <- roc(Y_holdout, as.vector(pred_test$pred),ci = T)
  auc_val <- paste0(round(auc_val$ci[2],3)," [", round(auc_val$ci[1],3),"-",round(auc_val$ci[3],3), "]")
  validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)
  brier_score_val = mean((Y_holdout - pred_test$pred)**2 )
  
  # ### Performance Fabrice
  pred_fab <- predict(fit, newdata = dplyr::select(data_val, -c(id, event)), type='prob', onlySL = FALSE)
  pred <-  prediction( as.vector(pred_fab$pred), data_val$event)
  perf_fab <- performance(pred ,"tpr","fpr")
  auc_fab <- roc(data_val$event, as.vector(pred_fab$pred),ci = T)
  auc_fab <- paste0(round(auc_fab$ci[2],3)," [", round(auc_fab$ci[1],3),"-",round(auc_fab$ci[3],3), "]")
  validation_fab <- Performance_superlearner(observations = data_val$event,predictions = pred_fab$pred)
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
    outcome_train = Y,
    outcome_test = Y_holdout,
    outcome_fab = data_val$event,
    pred_train = pred_train$pred,
    pred_test = pred_test$pred,
    pred_fab = pred_fab$pred
  ))
}
############################################# 
# Modelisation superlearner tous les modeles
#############################################

Modelisation_superlearner_full_models <- function(data_train, data_val, folds, eval_cv){ 
  
  set.seed(25)

#  data_train = data_mimic[[9]]
#  data_val = data_fabrice[[9]]
#  folds = 10
  
  # Split
  inTrain  <- createDataPartition(data_train$id, p = 0.7)[[1]]
  data2 <- subset(data_train, select = -c(id, event))
  
  # Covariables
  X <- data2[inTrain,]
  X_holdout <- data2[-inTrain,]
  
  # Outcome
  outcome <- as.numeric(data_train$event == "1")
  Y <-  outcome[inTrain]
  Y_holdout <-  outcome[-inTrain]
  
  # Clean up #Currently only works with gaussian data SL.loess, SL.leekasso
  rm(outcome)
 
  # Cross validation
  control.SL <- SuperLearner.CV.control(V = folds,
                                        stratifyCV = TRUE,
                                        shuffle = FALSE,
                                        validRows = NULL)
              
  librarie <- c("SL.bartMachine","SL.bayesglm","SL.biglasso",  						
                "SL.extraTrees","SL.gam","SL.gbm",
                "SL.glm","SL.glm.interaction", "SL.glmnet",
                "SL.ipredbagg","SL.ksvm", "SL.mean",
                "SL.nnet", "SL.nnls", "SL.polymars",
                "SL.randomForest","SL.ranger","SL.rpart",
                "SL.rpartPrune","SL.speedglm","SL.step",
                "SL.stepAIC", "SL.step.forward", "SL.step.interaction",
                "SL.xgboost", "SL.caret.rpart")  
                
   fit <- SuperLearner(Y = Y,
                       X = X,
                       family = binomial(),
                       method = "method.AUC",
                       SL.library = librarie,
                       control = list(saveFitLibrary = TRUE, trimLogit = 0.001),
                       cvControl  = control.SL)
  if(eval_cv){
    fit.cv <- Modelisation_superlearner_cv(features = X, outcome  = Y, folds = 10)  
  }else{
    fit.cv = NULL
  }
  # temps <- fit$times$everything
  
  ### Performance training
  # onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
  pred_train <- predict(fit, newdata = X , type ='prob', onlySL = FALSE)
  pred <- prediction( as.vector(pred_train$pred), Y)
  auc_train <- roc(Y, as.vector(pred_train$pred),ci = TRUE, conf.level = 0.975)
  auc_train <- paste0(round(auc_train$ci[2],3)," [", round(auc_train$ci[1],3),"-",round(auc_train$ci[3],3), "]")
  apprentissage <- Performance_superlearner(observations = Y, predictions = pred_train$pred)
  brier_score_train = mean((Y - pred_train$pred)**2 )
  
  ### Performance Validation
  pred_test <- predict(fit, newdata = X_holdout , type='prob', onlySL = FALSE)
  pred <-  prediction( as.vector(pred_test$pred), Y_holdout)
  perf_val <- performance(pred,"tpr","fpr")
  auc_val <- roc(Y_holdout, as.vector(pred_test$pred),ci = T)
  auc_val <- paste0(round(auc_val$ci[2],3)," [", round(auc_val$ci[1],3),"-",round(auc_val$ci[3],3), "]")
  validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)
  brier_score_val = mean((Y_holdout - pred_test$pred)**2 )
  
  # ### Performance Fabrice
  pred_fab <- predict(fit, newdata = dplyr::select(data_val, -c(id, event)), type='prob', onlySL = FALSE)
  pred <-  prediction( as.vector(pred_fab$pred), data_val$event)
  perf_fab <- performance(pred ,"tpr","fpr")
  auc_fab <- roc(data_val$event, as.vector(pred_fab$pred),ci = T)
  auc_fab <- paste0(round(auc_fab$ci[2],3)," [", round(auc_fab$ci[1],3),"-",round(auc_fab$ci[3],3), "]")
  validation_fab <- Performance_superlearner(observations = data_val$event,predictions = pred_fab$pred)
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
    outcome_train = Y,
    outcome_test = Y_holdout,
    outcome_fab = data_val$event,
    pred_train = pred_train$pred,
    pred_test = pred_test$pred,
    pred_fab = pred_fab$pred
   ))
}
      
#############################################
# Cross validation : performances de tous les modèles
#############################################

Modelisation_superlearner_cv <- function(features, outcome, folds){
  
  set.seed(25)
  
  control.SL <- SuperLearner.CV.control(V = folds,
                                        stratifyCV = TRUE,
                                        shuffle = FALSE,
                                        validRows = NULL)
  # Classification
  fit.cv <- CV.SuperLearner(Y = Y,
                            X = X,
                            family     = binomial(),
                            method   = "method.NNLS",
                            #method     = "method.AUC",
#                         	SL.library = c("SL.nnet","SL.glm","SL.glmnet",
#  																			 "SL.rpart","SL.gbm","SL.bayesglm",
#  																			 "SL.randomForest", "SL.xgboost",
#  																			 "SL.gam"),  
#														SL.library = c("SL.bartMachine","SL.bayesglm","SL.biglasso",  						
#               															 "SL.extraTrees","SL.gam","SL.gbm",
#               															 "SL.glm","SL.glm.interaction", "SL.glmnet",
#               															 "SL.ipredbagg","SL.ksvm", "SL.mean",
#                														 "SL.nnet", "SL.nnls", "SL.polymars",
#              															 "SL.randomForest","SL.ranger","SL.rpart",
#               															 "SL.rpartPrune","SL.speedglm","SL.step",
#            																 "SL.stepAIC", "SL.step.forward", 																						 "SL.step.interaction", "SL.xgboost", "SL.caret.rpart") ,
  													SL.library = c("My.SL.gls", "My.SL.glmmPQL", "My.SL.glmmML"),
                     				#id = X$id,
                            cvControl =  control.SL,
                            innerCvControl = list(control.SL),
                            verbose    = TRUE,
                            parallel   = 'seq')
  
  
  # library(ck37r)
  # cvsl_plot_roc(fit)
  
  fit.plot <- summary(fit.cv)
  fit.plot <- fit.plot$Table
#  fit.plot <- fit.plot[order(fit.plot$Ave, decreasing = T),]
#  fit.plot <- subset(fit.plot , Algorithm != "SL.mean_All")
#  fit.plot$Algorithm <- factor(fit.plot$Algorithm,
#                               levels = fit.plot$Algorithm[order(fit.plot$Ave, decreasing = F)])
#  
#  # Renomme les Algorithmes (légende et axe Y)
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glm_All", "Logistic Regression")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.glmnet_All", "Penalized Logistic Regression")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.rpart_All", "Recursive Partitioning")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.gbm_All", "Gradient Boosting")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.nnet_All", "Neural Network")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.bayesglm_All", "Bayesian Generalized Linear Regression")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.randomForest_All", "Random Forest")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"SL.xgboost_All", "XGboost")
#  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")  
# Produire le plot AUC"
# exemples
# fit.plot = a_cv_5$graphique$data[-10,]
# fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
#  graphique <- ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
#    geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
#    geom_point(size = 2.5, shape = 20, fill = "white") +
#    xlim(0.6, 1) +
#    xlab(paste(folds, "fold Cross-Validated AUROC")) + 
#    ylab("Algorithms") +
#    geom_text(aes(label = round(Ave,3), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black") +
#    geom_text(aes(label = round(Min,3), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "light grey") +
#    geom_text(aes(label = round(Max,3), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "light grey") +
#    theme_bw() +
#    scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
#    theme(legend.position = "none",
#          axis.text.y = element_text(size = 10),
#          axis.title = element_text(face = "bold", size = 12))
	return(list(modele = fit.cv, table = fit.plot))
  
  # return(list(table = fit.plot, graphique = graphique))
}

#########################
# Modelisation toutes périodes
#########################

# Cross validation : performances du superlearner
Modelisation_superlearner_ma <- function(data_train, data_val, folds, eval_cv){
  
  set.seed(25)
 
data_train = subset(data_mimic[[9]], select = -c(event_24h, event_cum))
data_val = subset(data_fabrice[[9]], select = -c(event_24h, event_cum))
folds = 10
  
  # Creation de la variable care_unit
  data_train$care_unit <- ifelse(data_train$care_unit1 == 1,1, 
																ifelse(data_train$care_unit2 == 1, 2, 3))
	data_val$care_unit <- ifelse(data_val$care_unit1 == 1,1, 
																ifelse(data_val$care_unit2 == 1, 2, 3))
	data_train <- data_train %>% 
		dplyr::select(c(-care_unit1,-care_unit2, -care_unit4))
	data_val <- data_val %>% 
		dplyr::select(c(-care_unit1,-care_unit2, -care_unit4))
																																		
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
  
  set.seed(25)
    
  # Classification
  fit <- SuperLearner(Y = Y,
                      X = X,
                      family = binomial(),
                      method = "method.AUC",
                      SL.library = c("My.SL.gls", "My.SL.glmmPQL", "My.SL.glmmML"),
                      id = X$id,
                      cvControl = control.SL)
  if(eval_cv){
    fit.cv <- Modelisation_superlearner_cv(features = X, outcome  = Y, folds = 5)  
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
  
  ### Performance Fabrice
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
		outcome_train = Y,
    outcome_test = Y_holdout,
    outcome_fab = data_val$event,
    pred_train = pred_train$pred,
    pred_test = pred_test$pred,
    pred_fab = pred_fab$pred
    ))
}


# Prediction avec le SuperLearner 
# # essayer avec crossprod sinon
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
     pi <- crossprod(object$fitLibrary[[mm]]$object$coefficients , y = t(newdataMM))
     predY[, mm] <- 1 / (1 + exp(-pi))
      
    }else if(mm == 2){
      pi <- crossprod(object$fitLibrary[[mm]]$object$coefficients$fixed, y = t(newdataMM))
      predY[, mm] <- 1 / (1 + exp(-pi))
    }else{
      pi <- crossprod(object$fitLibrary[[mm]]$object$coefficients,y = t(newdataMM))
      predY[, mm] <- 1 / (1 + exp(-pi))
    }
  }
  getPred <- object$method$computePred(predY = predY, 
                                       coef = object$coef, 
                                       control = object$control)
  
  out <- list(pred = getPred, library.predict = predY)
  
  return(out)
}

# Cross validation : performances de tous les modèles
Modelisation_superlearner_ma.cv <- function(df, V, folds, sl ){
  
  # df = data[[10]]
  # folds = 10
  # V = 1
  # sl =  librairie_mixed 
  
  etiquette <- c("apprentissage","validation")
  
  train_test = data.frame(
    numero = sample(etiquette, length(unique(df$id)), replace = T, prob = c(0.7,0.3)),
    id = unique(df$id))
  
  df <- merge(x = df, y = train_test, by = "id")
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
  
  # Cross validation
  control.SL <- SuperLearner.CV.control(V = folds,
                                        stratifyCV = TRUE,
                                        shuffle = FALSE,
                                        validRows = NULL)
  # Classification
  fit1 <- CV.SuperLearner(Y = Y,
                          X = X,
                          SL.library = sl,
                          family     = binomial(),
                          method     = "method.AUC",
                          cvControl  = SuperLearner.CV.control(V = V,
                                                               stratifyCV = TRUE,
                                                               shuffle = FALSE,
                                                               validRows = NULL),
                          innerCvControl = list(control.SL),
                          verbose    = TRUE,
                          parallel   = "seq")
  
  fit.plot <- summary(fit1)
  fit.plot <- fit.plot$Table
  fit.plot <- fit.plot[order(fit.plot$Ave, decreasing = T),]
  fit.plot$Algorithm <- factor(fit.plot$Algorithm,
                               levels = fit.plot$Algorithm[order(fit.plot$Ave, decreasing = F)])
  
  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"My.SL.gls_All", "Generalized Least Squares")
  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"My.SL.glmmPQL_All", "Penalized Quasi-Likelihood GLM")
  fit.plot$Algorithm <- mapvalues(fit.plot$Algorithm,"My.SL.glmmML_All", "Maximum Likelihood GLM")
  
  graphique <- ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
    geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
    geom_point(size = 2.5, shape = 20, fill = "white") +
    xlim(0, 1) +
    xlab(paste(V, "fold Cross-Validated AUC")) +
    geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black") +
    geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "light grey") +
    geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "light grey") +
    theme_bw() +
    scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 10),
          axis.title = element_text(face = "bold", size = 12))
  
  return(graphique)
}

#########################
# Modelisation fabrice
#########################
Modelisation_superlearner_one_fabrice <- function(df, validation, variables, V , sl){
  
  set.seed(25)
  
  # Pour que les periodes d'un patient toutes dans l'apprentissage 
  # soit toutes dans la validation
  # df = data[[4]]
  # validation = na.omit(df_sample_validation)
  # variables = c( "age", "gender","sapsi_first","sofa_first",
  #                "care_unit1", "care_unit4", "amine", "sedate","venti",
  #                "alpha_hr", "alpha_spo2","alpha_abpm", "alpha_abpd", "alpha_abps",
  #                "beta_hr", "beta_spo2", "beta_abpm", "beta_abpd","beta_abps")
  # V = 10
  # sl = librairie
  
  # Split
  inTrain  <- createDataPartition(df$id, p = 0.7)[[1]]
  #data2 <- na.omit(subset(data, select = -c(id, event)))
  data2 <- subset(df, select = -c(id, event))
  
  # Covariables
  X <- data2[inTrain,variables]
  X_holdout <- subset(validation, select = c(variables))
  
  # Outcome
  outcome <- as.numeric(df$event == "1")
  outcome_val <- as.numeric(validation$event == 1)
  Y <-  outcome[inTrain]
  Y_holdout <-  outcome_val
  
  # Clean up
  rm(outcome)
  
  # Cross validation
  control.SL <- SuperLearner.CV.control(V = V,
                                        stratifyCV = TRUE,
                                        shuffle = FALSE,
                                        validRows = NULL)
  # Classification
  sl <- mcSuperLearner(Y = Y,
                       X = X,
                       family = binomial(),
                       SL.library = sl,
                       cvControl =  control.SL)
  
  pred_test  <- predict.SuperLearner(sl, newdata = X_holdout)
  pred       <-  prediction( as.vector(pred_test$pred), Y_holdout)
  perf_val   <- performance(pred,"tpr","fpr")
  auc_val <- roc(Y_holdout, as.vector(pred_test$pred),ci = T)
  auc_val <- paste0(round(auc_val$ci[2],3)," [", round(auc_val$ci[1],3),"-",round(auc_val$ci[3],3), "]")
  validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)
  return(list(
    performance_validation    = perf_val,
    prediction_validation     = pred_test$pred,
    auc_validation            = auc_val,
    performances_validation   = validation
  ))
}

Modelisation_superlearner_fabrice <- function(df, validation, variables, V , sl){
  
  # Pour que les periodes d'un patient toutes dans l'apprentissage 
  # soit toutes dans la validation
  df = data[[10]]
  validation = df_validation
  variables = c("age", "gender","sapsi_first","sofa_first",
                "care_unit1", "care_unit4", "amine", "sedate","venti",
                "W1_hr","W1_spo2","W1_abpm","W1_abpd","W1_abps",
                "V1_hr","V1_spo2","V1_abpm","V1_abpd","V1_abps")
  V = 10
  sl = librairie_mixed
  
  set.seed(25)
  
  etiquette <- c("apprentissage","validation")
  
  train_test = data.frame(
    numero = sample(etiquette, length(unique(df$id)), replace = T, prob = c(0.7,0.3)),
    id = unique(df$id))
  
  df <- merge(x = df, y = train_test, by = "id")
  data2 <- subset(df, select = c(-event))
  
  inTrain <- which(df$numero == "apprentissage")
  
  # Covariables
  X <- subset(data2, numero == "apprentissage")
  X <- subset(X, select = -c(numero))
  X_holdout <- subset(validation, select = c("id",variables))
  # Outcome
  outcome <- as.numeric(df$event == "1")
  outcome_val <- as.numeric(validation$event == 1)
  Y <-  outcome[inTrain]
  Y_holdout <-  outcome_val
  
  # Cross validation
  control.SL <- SuperLearner.CV.control(V = V,
                                        stratifyCV = TRUE,
                                        shuffle = TRUE,
                                        validRows = NULL)
  # Classification
  sl <- mcSuperLearner(Y = Y,
                       X = X,
                       family = binomial(),
                       SL.library = sl,
                       cvControl =  control.SL)
  
  pred_test  <- predict.SuperLearner_ma(sl, newdata = X_holdout)
  pred       <-  prediction( as.vector(pred_test$pred), Y_holdout)
  perf_val   <- performance(pred,"tpr","fpr")
  auc_val <- roc(Y_holdout, as.vector(pred_test$pred),ci = T)
  auc_val <- paste0(round(auc_val$ci[2],3)," [", round(auc_val$ci[1],3),"-",round(auc_val$ci[3],3), "]")
  validation <- Performance_superlearner(observations = Y_holdout,predictions = pred_test$pred)
  return(list(
    performance_validation    = perf_val,
    prediction_validation     = pred_test$pred,
    auc_validation            = auc_val,
    performances_validation    = validation
  ))
}
