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

sample_lineaire <- df_sample[,c(1:13,
	grep("alpha_",names(df_sample)),
	grep("beta_",names(df_sample)))]

sample_arma 		<- df_sample[,c(1:13,
	grep("ar_",names(df_sample)),
	grep("ma_",names(df_sample)),
	grep("inter_",names(df_sample)))]

sample_haar 		<- df_sample[,c(1:13,
	grep("W1_",names(df_sample)),
	grep("V1_",names(df_sample))
)]


#######################################
# Modelisation
#######################################


# 1 - Modelisation sans superlearner

res <- list()
res[[1]] <- all_models(data = sample_resume, cut_data = 0.7,control_parameter = 0.01)
res[[2]] <- all_models(data = sample_lineaire, cut_data = 0.7,control_parameter = 0.01)
res[[3]] <- all_models(data = sample_arma, cut_data = 0.7,control_parameter = 0.01)
res[[4]] <- all_models(data = sample_haar, cut_data = 0.7,control_parameter = 0.01)

res <- list(resume   = res[[1]], 
            lineaire = res[[2]], 
            arma     = res[[3]], 
            haar     = res[[4]])









