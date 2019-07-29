#######################################################
#-------- Numerics par patients
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

rm(list = ls())
# source("~/Recherche/Mimic-II/scripts/R/analyse/2_mimic.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/to.balanced.R")

library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(gdata)

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"

load(file.path(chemin,"2_mimic.RData"))
load("~/Recherche/Mimic-II/data/clean/mimic2/2_mimic.RData")

df_information <- df_information %>% 
  mutate(
    event = ifelse(event > 0, 1, 0), # 10800 events
    periode = as.character(periode),
    periode = gsub(" ","",periode),
    identif = paste(as.character(subject_id),
                    as.character(periode),sep="-"),
    id = as.character(subject_id))  # 1276 individus

numerics <- subset(numerics,id %in% df_information$id) 

### ------- Découpage des numérics par périodes d'observation

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)
v_id <- unique(numerics$id)

df_numerics_observation_60_min <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  observation_periode(df = numerics,
                      sujet = sujet, 
                      duree_obs = 60)
}

df_numerics_observation_50_min <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  observation_periode(df = numerics,
                      sujet = sujet, 
                      duree_obs = 50)
}

df_numerics_observation_40_min <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  observation_periode(df = numerics,
                      sujet = sujet, 
                      duree_obs = 40)
}
stopCluster(cl)

###########################
# Observation 60 min 
###########################

df_numerics_sp <- subset(df_numerics_observation_60_min, !is.na(periode))
# Nombre de période de 60 min
nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode) == 60)

df_numerics_sp <- df_numerics_sp %>%
  mutate(
    time = rep(x = seq(1,60,1), nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"))

###
### ------- Data final : wide
###

v_id <- unique(df_numerics_sp$id)
transp = NULL
for (sujet in v_id){
  cat(sujet,"\t")
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                                      match("identif",names(df_numerics_sp)),
                                      match("time", names(df_numerics_sp)),
                                      match(c("hr","spo2","abpsys","abpdias",
                                              "abpmean"),
                                            names(df_numerics_sp))))
}

transp <- transp %>% 
  mutate(
    identif = as.character(identif),
    id = as.character(identif),
    id = as.character(str_match(id,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2]))

df_final_wide_60min <- dplyr::inner_join(df_information, transp, 
                            by = c("identif","id","periode")) %>%
  select(-c(identif, icustay_id, rank, subject_id))

###########################
# Observation 50 min 
###########################
df_numerics_sp <- subset(df_numerics_observation_50_min, !is.na(periode))
# Nombre de période de 50 min
nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode) == 50)

df_numerics_sp <- df_numerics_sp %>%
  mutate(
    time = rep(x = seq(1,50,1), nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"))
### ------- Data final : wide
v_id <- unique(df_numerics_sp$id)
transp = NULL
for (sujet in v_id){
  cat(sujet,"\t")
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                                      match("identif",names(df_numerics_sp)),
                                      match("time", names(df_numerics_sp)),
                                      match(c("hr","spo2","abpsys","abpdias",
                                              "abpmean"),
                                            names(df_numerics_sp))))
}

transp <- transp %>% 
  mutate(
    identif = as.character(identif),
    id = as.character(identif),
    id = as.character(str_match(id,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2]))

df_final_wide_50min <- inner_join(df_information, transp, 
                                  by = c("identif","id","periode")) %>%
  select(-c(identif, icustay_id, rank, subject_id))



###########################
# Observation 40 min 
###########################
df_numerics_sp <- subset(df_numerics_observation_40_min, !is.na(periode))
# Nombre de période de 40 min
nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode) == 40)

df_numerics_sp <- df_numerics_sp %>%
  mutate(
    time = rep(x = seq(1,40,1), nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"))
### ------- Data final : wide
v_id <- unique(df_numerics_sp$id)
transp = NULL
for (sujet in v_id){
  cat(sujet,"\t")
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                                      match("identif",names(df_numerics_sp)),
                                      match("time", names(df_numerics_sp)),
                                      match(c("hr","spo2","abpsys","abpdias",
                                              "abpmean"),
                                            names(df_numerics_sp))))
}

transp <- transp %>% 
  mutate(
    identif = as.character(identif),
    id = as.character(identif),
    id = as.character(str_match(id,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2]))

df_final_wide_40min <- inner_join(df_information, transp, 
                                  by = c("identif","id","periode")) %>%
  select(-c(identif, icustay_id, rank, subject_id))

###
### ------- Data final : long pour les 60 premieres minutes.
###

df_numerics_sp <- subset(df_numerics_observation_60_min, !is.na(periode))
# Nombre de période de 60 min
nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode) == 60)

df_numerics_sp <- df_numerics_sp %>%
  mutate(
    time = rep(x = seq(1,60,1), nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"))
                    
df_final_long_60min <- merge(df_information, df_numerics_sp, 
               by.x =c("identif", "id","periode"),
               by.y = c("identif", "id","periode"),
               all.x = T,all.y = F) %>% 
   subset(id %in% df_final_wide_60min$id)    
   
      
save(df_numerics_observation_60_min,
	df_numerics_observation_50_min,
 	df_numerics_observation_40_min,
  df_final_wide_40min,
  df_final_wide_50min,
  df_final_wide_60min,
  df_final_long_60min,
  file = "~/Recherche/Mimic-II/data/clean/mimic2/3_numerics.RData")                       

# temp_num <- temp[,c("identif","hr", "abpsys", "abpdias","abpmean", "spo2")]
# 
# # moyenne par periode et par patient
# temp_long <- aggregate(. ~ identif, data = temp_num, mean)
# 
# df_final_long <- merge( df_information, temp_long,
#                         by.x ="identif",
#                         by.y="identif",
#                         all.x = T ,all.y = T) %>%
#   subset(select = -identif)
