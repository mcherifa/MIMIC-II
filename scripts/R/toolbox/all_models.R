library(magrittr)

all_models <- function(data, cut_data, control_parameter){
  
  # set.seed(1)
  
  aire <- list()
  
  #######################################	
  # Echantillon Apprentissage et Test 
  #######################################

  nID   <- length(unique(data$id))
  
  inTrainID <- sample(unique(data$id), round(nID * cut_data), replace = FALSE)
  
  train <- subset(data, id %in% inTrainID) %>% 
    subset(select = c(-id))
  
  test  <- subset(data, !(id %in% inTrainID))%>% 
    subset(select = c(-id))
  
  # Echantillon test et apprentissage 
  
  y <- matrix(0, ncol = 2, nrow = nrow(data))
  y[which(data$event == "0"), 1] <- 1
  y[which(data$event == "1"), 2] <- 1 
  
  temp <- cbind(y, data$id) %>% 
    data.frame() %>%  
    set_colnames(c("0", "1", "id"))
  
  
  # jeu test
  
  X.test <- subset(test, select = -event)
  y.test <- as.matrix(subset(temp, !(id %in% inTrainID))[,1:2])
  target <- max.col(y.test)
  
  # jeu d'apprentissage
  
  X.app <- subset(train, select = -event)
  y.app <- as.matrix(subset(temp, (id %in% inTrainID))[,1:2])
  
  #######################################	
  # Modélisation
  #######################################
  
  # Logistique 
  
  fit.glm <- glm(formula = as.factor(event) ~ .,
                 family = binomial,
                 data = train)
  
  # Réseau neurone
  
  fit.rn <- nnet(y.app  ~ ., 
                 data = cbind(X.app, y.app),
                 size = 3, 
                 MaxNWts = 1e+05, 
                 decay = 1,
                 maxit = 100,
                 trace = T)
  
  # fit.rn <- nnet(Species  ~ ., 
  #                data = iris,
  #                size = 3, 
  #                MaxNWts = 1e+05, 
  #                decay = 1,
  #                maxit = 200,
  #                trace = T)
  
  # Arbre de classification              
  
  fit.arbre <- rpart(as.factor(event) ~ ., 
                     data = train,
                     control = list(cp = control_parameter)
  )
  
  #######################################	
  # Evaluation performances
  #######################################
  
  pred.glm <- predict(fit.glm , newdata = test, type = "response")
  
  pred.rn  <- predict(fit.rn, cbind(X.test,y.test, type = "raw"))
  class_predict.rn <- max.col(pred.rn)
  
  pred.arbre <- predict( fit.arbre, test, type = "prob")
  class_predict.arbre <- pred.arbre[,2]
  
  aire[[1]] <- roc(test$event, pred.glm,ci = T)
  
  aire[[2]] <- roc(as.numeric(target), as.numeric(class_predict.rn),ci = T)
  
  aire[[3]] <- roc(as.numeric(target), as.numeric(class_predict.arbre),ci = T)
  
  aire <- list(logistique           = aire[[1]], 
              reseau_neurone        = aire[[2]], 
              arbre_classification  = aire[[3]])
  
  return(aire)
  
}
































