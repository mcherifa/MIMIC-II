#######################################################
#-------- Modelisation  paramétrique 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

set.seed (1)
rm(list = ls())

source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")
source("/home/menyssa/Mimic/scripts/R/toolbox/all_models.R")

library(tidyverse)
library(SuperLearner)
library(origami)
library(sl3)
library(gbm)
library(polspline)
library(gam)
library(arm)
library(ipred)
# library(verification)
library(caret)
library(plyr)

#######################################
# Lecture
#######################################

df <- readRDS(paste0(dest_data,"df_modelisation.rds")) %>%    
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(- identif, - eevent, - care_unit)) %>%
  mutate(
    periode = as.numeric(as.character(periode)),
    id      = as.numeric(as.character(id)),
    gender  = factor(gender, levels = c("F","M"), labels = c("1","0")),
    event   = factor(event,  levels = c(0,1), labels = c("0","1")),
    amine   = factor(amine,  levels = c(0,1), labels = c("0","1")),
    curare  = factor(curare, levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate, levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,  levels = c(0,1), labels = c("0","1"))
  )

df_modelisation <-  readRDS(paste0(dest_data,"df_modelisation.rds")) %>%    
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(- identif, - eevent, - care_unit)) %>%
  mutate(
    periode = as.numeric(as.character(periode)),
    id      = as.numeric(as.character(id)),
    gender  = as.numeric(ifelse(gender == "M",1,0))
  )

# Tirage au sort une ligne par id 

df_sample <- df_modelisation %>% 
  group_by(id) %>% 
  sample_n(size = 1)  %>%
  data.frame()


#  Sous df_sample en fonction méthodes pour résumer

sample_resume 	<- df_sample[,c(1:13,
                               grep("m_",names(df_sample)),
                               grep("v_",names(df_sample)))]

# 2 - Modelisation avec superlearner

# glm, rpart, nnet 
SL.library <- c("SL.randomForest",
                "SL.glm",
                "SL.nnet"
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

# 1- Apprentissage  

V = 2
data = sample_resume
nID   <- length(unique(data$id))
cut_data <- 0.7

inTrainID <- sample(unique(data$id), round(nID * cut_data), replace = FALSE)

train <- subset(data, id %in% inTrainID) %>% 
  subset(select = c(-id))

test  <- subset(data, !(id %in% inTrainID))%>% 
  subset(select = c(-id))


result = SuperLearner(Y = train$event,
                        X = train[,-6], 
                        SL.library = SL.library)

fit.mod1.AUC <- CV.SuperLearner(Y = train$event,
                                X = train[,-6],
                                SL.library  = SL.library,
                                family      = binomial(),
                                V           = V)
pred = predict(result, test[,-6], onlySL = T)

pred_rocr = ROCR::prediction(pred$pred, test$event)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc


fit_glm_sl       <- model_superlearner(bootstrapping(df_sample), "SL.glm")
fit_randomForest <- model_superlearner(bootstrapping(df_sample), "SL.randomForest")
fit_randomForest <- model_superlearner(bootstrapping(df_sample), "SL.nnet")

