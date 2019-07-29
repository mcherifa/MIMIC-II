#######################################################
# Script models optimisation
#                 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/afroinshape/AHE.git
#######################################################


#######################################
# # Lecture
#######################################

dest_r <- "/home/mcherifa/Mimic/scripts/R/toolbox/"
dest_d <- "/home/mcherifa/Mimic/data/clean/"
source(paste0(dest_r,"packages.R"))
source(paste0(dest_r,"fonctions.R"))

# Chargement des données 
df <- readRDS(paste0(dest_d,"fichier_wide_periode.rds"))

# Management
df <- df[which(df$eevent == 0),]
df <- subset(df, select = c( - id, - periode, - identif.x,- identif.y,-eevent))

# Variables factors
df[,c(1:5)]  <- data.frame(lapply(df[,c(1:5)], as.factor))

# Premières lignes du data 
# head(df)


#######################################
# Deep learning optimisation
#######################################

# 1 - Function f( size, decay)

deep_optimisation <- function(i,j){ 
  
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  set.seed(graine)
  
  rn <- nnet::nnet(event ~ ., data = train ,size = i,
                   decay = j ,MaxNWts = 100000, maxit = 100, trace = F)
  

  
  AUC <-  pROC::roc(as.numeric(test$event), 
                    as.numeric(predict(rn,newdata = test,type = "raw")))$auc
  stopCluster(cl)
  return(AUC)
}

# 2 - Function f(maxit)

deep_optimisation_maxit <- function(i){ 
  
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  set.seed(graine)
  
  rn <- nnet::nnet(event ~ ., data = train ,size = 13,
                   decay = 1.6 ,MaxNWts = 100000, maxit = i, trace = F)
  
  AUC <- pROC::roc(as.numeric(test$event), 
                   as.numeric(predict(rn,newdata = test,type = "raw")))$auc
  
  stopCluster(cl)
  
  return(AUC)
}

############ 1 

i <- seq(1, 20,by = 1)
j <- seq(0, 5,by = 0.1 )

beta <- foreach(siz = i,.combine = 'cbind') %:% 
  foreach(dec = j, .combine ='c') %dopar% {
    deep_optimisation(siz,dec)
  }

size <- i[which(beta == max(beta), arr.ind = T)[1]]
decay <- j[which(beta == max(beta), arr.ind = T)[2]]
print(size) # 13
print(decay) # 1.6


############ 2

#i <- seq(100, 1000,by = 50)

#beta <- foreach(iter = i,.combine = 'c') %dopar% {
#  deep_optimisation_maxit(iter)
#}


