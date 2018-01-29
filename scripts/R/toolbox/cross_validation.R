############
# Boot par fold = id 
############

k_bootstrapping_logistique <-  function(df, c, k){
  
 	progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  RES = aggregation = NULL
  list <- 1:k
  
  for ( i in 1:k){
   
    # Modele
    
    RES <- rbind(RES,model_logistique(df,c))
   
    progress.bar$step()
  }
  
  RES	<- data.frame(matrix(unlist(RES), ncol = 4, nrow=k))
  
  colnames(RES) <- c( "AUC","cil","ciu","Time_to_predict")
 
  
  return(RES)
    
}

############
# Boot par fold = id 
############

k_bootstrapping_effet <-  function(a, v, k){
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  RES = aggregation = NULL
  list <- 1:k
  
  for ( i in 1:k){
    
    # Modele
    
    RES <- rbind(RES,model_effet(a,v))
    
    progress.bar$step()
  }
  
  RES	<- data.frame(matrix(unlist(RES), ncol = 4, nrow=k))
  
  colnames(RES) <- c( "AUC","cil","ciu","Time_to_predict")
  
  return(RES)
  
}


bootstrapping <- function(df){

  X <- subset(df, select = c( - event))
  Y <- subset(df, select = c( event))
  
  y = matrix(0, ncol=2, nrow=dim(Y)[1])
  y[which(df$event == "0"),1] <- 1
  y[which(df$event == "1"), 2] <- 1 
  
  # jeu test
  
  ind.test <- sample(1:dim(Y)[1],size=dim(Y)[1]/3)
  X.test <- X[ind.test,]
  y.test <- y[ind.test,]
  
  # jeu d'apprentissage
  
  X.app <- X[-ind.test,]
  y.app <- y[-ind.test,]
  
  liste <- list (testDesc = X.test,
                 trainDesc = X.app,
                 testClass = y.test,
                 trainClass = y.app 
  )
	
  return(liste)
}

k_bootstrapping <-  function(df, k){
  
  coeur <- parallel::makeCluster(parallel::detectCores()-1)
	doParallel::registerDoParallel(coeur)
  
  RES = aggregation = NULL
  list <- 1:k
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  for ( i in 1:k){
    
    # Bootstrap
    
    train_test <- bootstrapping(df)
    
    # Modele
    
    RES <- rbind(RES,deeplearning(liste = train_test))
   
    progress.bar$step()
  }
  
  RES	<- data.frame(matrix(unlist(RES), ncol = 5, nrow=k))
  
  colnames(RES) <- c( "AUC",  "CI_lower",  "CI_upper", 
  										 "ACCURACY","Time_to_predict")
 
    
	stopCluster(coeur)
	
  
  return(RES)
    
}




k_bootstrapping_arbre <-  function(df, k){
  
  cl <- makeCluster(detectCores()-1)
	registerDoParallel(cl)
	getDoParWorkers()
  
  RES = aggregation = NULL
  list <- 1:k
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  for ( i in 1:k){
    
    # Modele
    
    RES <- rbind(RES,arbre(df))
   
    progress.bar$step()
  }
  
  RES	<- data.frame(matrix(unlist(RES), ncol = 4, nrow=k))
  
  colnames(RES) <- c( "CI_L","AUC", "CI_U","Time_to_predict")
 
    
	stopCluster(cl)
	
  
  return(RES)
    
}


k_fold_meny <- function(df, k){
  
  aggregation_moyenne = NULL
  coeur <- parallel::makeCluster(detectCores()-1)
  registerDoParallel(coeur)
  
  # attribution du fold 
  
  df$appartenance <- sample(1:k, nrow(df), replace = TRUE) 
  list <- 1:k
  
  RES <- NULL
  
  # Bar de progression 
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  for (i in 1:k){
    
    # jeu de validation
    
    ind.test <- df[which(df$appartenance == i),]
    X.test <-  subset(ind.test, select = c( - event))
    
    Y <- ind.test[,2]
    y = matrix(0, ncol=2, nrow=length(Y))
    y[which(ind.test$event == " 0"),1] <- 1
    y[which(ind.test$event == " 1"), 2] <- 1 
    y.test <- y
    
    # jeu d'apprentissage
    
    ind.app <- df[which(df$appartenance != i),]
    X.app <- subset(ind.app, select = c( - event))
    
    Y <- ind.app[,2]
    y = matrix(0, ncol=2, nrow=length(Y))
    y[which(ind.app$event == " 0"),1] <- 1
    y[which(ind.app$event == " 1"), 2] <- 1 
    y.app <- y
    
    train_test <- list (testDesc = X.test,
                        trainDesc = X.app,
                        testClass = y.test,
                        trainClass = y.app )
    
    RES <- rbind(RES,deeplearning(liste = train_test))
    
    progress.bar$step()
    
  }
  stopCluster(coeur)
  
  RES	<- data.frame(matrix(unlist(RES), ncol = 5, nrow=k))
  
  aggregation_moyenne <- data.frame(mapply(mean, RES))
  
  return(aggregation_moyenne)
}


stratified_k_fold_meny <- function(df, k ){
  
  coeur <- parallel::makeCluster(detectCores()-1)
  registerDoParallel(coeur)
  
  # Stratifié : 14 / 15 event par fold 
  
  folds <- createFolds(factor(df$event), k = 10, list = FALSE)
  df$fold <- folds
  
  # table(df$event,df$fold)
  list <- 1:k
  
  RES <- NULL
  
  # Bar de progression 
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  
  for (i in 1:k){  
    
    # jeu de validation
    
    ind.test <- df[which(df$fold == i),]
    X.test <-  subset(ind.test, select = c( - event, - fold ))
    
    Y <- ind.test[,2]
    y = matrix(0, ncol=2, nrow=length(Y))
    y[which(ind.test$event == " 0"),1] <- 1
    y[which(ind.test$event == " 1"), 2] <- 1 
    y.test <- y
    
    # jeu d'apprentissage
    
    ind.app <- df[which(df$fold != i),]
    X.app <- subset(ind.app, select = c( - event, - fold))
    
    Y <- ind.app[,2]
    y = matrix(0, ncol=2, nrow=length(Y))
    y[which(ind.app$event == " 0"),1] <- 1
    y[which(ind.app$event == " 1"), 2] <- 1 
    y.app <- y
    
    train_test <- list (testDesc = X.test,
                        trainDesc = X.app,
                        testClass = y.test,
                        trainClass = y.app )
    
    RES <- rbind(RES,deeplearning(liste = train_test))
    
    progress.bar$step()
    }
  
  stopCluster(coeur)
  return()
}






















