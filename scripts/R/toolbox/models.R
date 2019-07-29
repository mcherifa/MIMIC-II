model_logistique <- function(df, cut_data){
  
  before <- Sys.time()
  
  # Echantillon test et apprentissage en respectant l'appartenance des id 
  
  nID <- length(unique(df$id))
  
  inTrainID <- sample(unique(df$id), round(nID * cut_data), replace = FALSE)
  train <- subset(df, id %in% inTrainID) %>% 
    subset(select = c(-id))
  test  <- subset(df, !(id %in% inTrainID))%>% 
    subset(select = c(-id))
  
  # Modele 
  
  GHQ <- glm(formula = event ~ .,
             family = binomial,
             data = train)
  
  
  # Predictions
  
  fitted.results <- predict(GHQ, newdata = test, type = "response")
  AUC <- roc(test$event, fitted.results,ci = T)
  CI.AUC <- ci.auc(AUC)
  
  after <- Sys.time() 
  
  # Temps
  
  time <- after - before
  
  return(list(
    AUC = CI.AUC[2],
    CI_lower = CI.AUC[1],
    CI_upper =  CI.AUC[3],
    time = time))
}


reseau_neurone <- function( liste = train_test ){
  
  # set.seed(20191) 
  
  RES <- NULL
  
  # jeu test 
  
  X.test <- subset(liste$testDesc, select = -id)
  y.test <- liste$testClass
  
  # jeu d'apprentissage
  
  X.app <- subset(liste$trainDesc, select = -id)
  y.app <- liste$trainClass
  
  before <- Sys.time()
  
  model <- nnet(y.app  ~ ., 
                data = cbind(X.app, y.app),
                size = 3, 
                MaxNWts = 1e+05, 
                decay = 1.6,
                maxit = 200,
                trace = T)
  
  predictions <- predict(model, cbind(X.test,y.test))
  
  after <- Sys.time()
  
  class_predict_3 <- max.col(predictions)
  target <- max.col(y.test)
  
  # matrice_confusion <- CrossTable(class_predict_3,target,
  #                               dnn = c("Predicted","Observed"))
  
  # Accuracy 
  matrice_confusion <- as.matrix(table(actual = target,
                                       predicted = class_predict_3))
  # print(matrice_confusion)
  n <- sum(matrice_confusion)
  diag <- diag(matrice_confusion)
  precision <- sum(diag) / n
  
  # interval de confiance 
  
  AUC <- roc(as.numeric(target), as.numeric(class_predict_3),ci = T)
  CI.AUC <- ci.auc(AUC)
  
  # Time
  time <- after - before
  
  # Result 
  
  RES <- list(
    AUC = CI.AUC[2],
    CI_lower = CI.AUC[1],
    CI_upper =  CI.AUC[3],
    ACCURACY = precision,
    Time_to_predict = time
    #,
    #table = matrice_confusion
  )
  
  return(RES)
}

arbre_classification <- function (df,cut_data,control_parameter){
  
  before = Sys.time()
  nID <- length(unique(df$id))
  
  inTrainID <- sample(unique(df$id), round(nID * cut_data), replace = FALSE)
  train <- subset(df, id %in% inTrainID) %>% 
    subset(select = c(-id))
  test  <- subset(df, !(id %in% inTrainID))%>% 
    subset(select = c(-id))
  
  
  model <- rpart(event ~ ., 
                 data = train,
                 control=list(cp=control_parameter))
  
  predictions <- predict(model, test,type = "prob")
  
  after <- Sys.time()
  
  class_predict_3 <- predictions[,2]
  target <- test$event
  
  AUC <- roc(as.numeric(target), as.numeric(class_predict_3),ci = T)
  CI.AUC <- ci.auc(AUC)
  time = after-before 
  
  return(list(
    AUC = CI.AUC[2],
    CI_lower = CI.AUC[1],
    CI_upper =  CI.AUC[3],
    time = time))
  
} 

model_superlearner <- function(liste, model){
  
  before <- Sys.time()
  
  # Echantillon test et apprentissage en respectant l'appartenance des id 
  
  X_holdout <- subset(liste$testDesc, select = -id)
  Y_holdout <- liste$testClass[,2]
  
  # jeu d'apprentissage
  
  X_train <- subset(liste$trainDesc, select = -id)
  Y_train <- liste$trainClass[,2]
  
  # Modele 
  
  sl = SuperLearner(Y = Y_train,
                    X = X_train, 
                    family = binomial(),
                    SL.library = model)
  
  after <- Sys.time()
  
  pred = predict(sl, X_holdout)
  
  pred_rocr = ROCR::prediction(pred$pred, Y_holdout)
  auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
  
  
  AUC <- roc(Y_holdout, pred$pred,ci = T)
  CI.AUC <- ci.auc(AUC)
  time = after-before 
  
  return(list(
    AUC = CI.AUC[2],
    CI_lower = CI.AUC[1],
    CI_upper =  CI.AUC[3],
    time = time))
  
}



model_effet <- function(df, cut_data){
  
  before <- Sys.time()
  
  # Echantillon test et apprentissage en respectant l'appartenance des id 
  
  nID <- length(unique(df$id))
  inTrainID <- sample(unique(df$id), round(nID * cut_data),
                      replace = FALSE)
  train <- subset(df, id %in% inTrainID) 
  test  <- subset(df, !(id %in% inTrainID))
  
  # Modele 
  
  GHQ <- glmer(event ~  event_cum + event_24h + m_hr +
                 m_abpm + v_abpm + v_abpd + m_abps + v_abps + beta_hr +
                 beta_abpm + beta_abpd + beta_abps + ar_hr + ma_hr + 
                 inter_hr + inter_abpm + inter_abpd + ar_abps + 
                 W1_abpm + V1_abps + (1|id),
               data = train,
               family = binomial(link = "logit"))
  
  # Predictions
  
  fitted.results <- predict(GHQ, newdata = test, type = "response",
                            allow.new.levels=TRUE)
  
  AUC <- roc(test$event, fitted.results,ci = T)
  CI.AUC <- ci.auc(AUC)
  
  after <- Sys.time() 
  
  # Temps
  time <- after - before
  
  return(list(
    AUC = CI.AUC[2],
    CI_lower = CI.AUC[1],
    CI_upper =  CI.AUC[3],
    time = time))
}


























