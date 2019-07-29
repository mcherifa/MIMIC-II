##############################
# Analyse fichier validation
# Données de Lariboisière
# Ményssa CHERIFA 
##############################

#### Fonction qui réécrit les fichiers si variables sont présentes
# dossier lecture: destination ou sont les fichiers
# dossier ecriture: destination ou vont les fichiers propres ( meme que lecture)
# vecteur.variables: variables devant être présente dans les fichiers csv

reecriture.fichier <- function(num.chambre,
                               dossier.lecture,
                               dossier.ecriture,
                               vecteur.variables){
  dest.room <- paste0(dossier.lecture, num.chambre) 
  if(dossier.lecture == dossier.ecriture){dossier.ecriture <- dest.room}
  csv.list <- list.files(path = dest.room, pattern =".csv", full.names=T)
  # Lecture de tous les fichiers
  for ( i in csv.list){
    print(paste0("fichier : ", i))
    data <- fread(i, na.strings ='-', header=T, sep=";")
    data <- data[which(!is.na(str_match(data$HEURE,"\\d+:\\d+:00"))==T),]
    data$TIME_AND_DATE <- as.POSIXct(
      strptime(paste0(data$DATE," " ,data$HEURE),"%d/%m/%Y %H:%M:%S"))
    # Recodage des noms de colonnes
    noms <- as.character(names(data))
    cc <- gsub("\\.", " ", noms, perl=TRUE)
    part <- colsplit(string=cc, pattern=" ", names=c("Part1", "Part2"))
    colnames(data) <- part$Part1
    # Garde uniquement les fichiers qui ont toutes les variables d'intérêt 
    contenu <- sum(vecteur.variables %in% colnames(data))
    if(contenu == length(vecteur.variables)){
      data <- subset(data, select = vecteur.variables)
      names <- str_split(i, "/")
      names <- names[[1]][9]
      write.csv(data, paste0(dossier.ecriture,"/",names), row.names = F)
    }else{
      file.remove(i)
      print(paste0("Fichier : ", i, " effaçé"))
    }
  }
  return('Fin de lecture')
}

#### Fonction qui combine tous les fichiers d'une chambre de réa
combiner.fichier <- function(num.chambre, 
                             dossier.lecture, 
                             dossier.ecriture){
  dest.room <- paste0(dossier.lecture, num.chambre) 
  if(file.exists(dest.room)){
    csv.list <- list.files(path = dest.room , pattern =".csv", full.names = T)
    for ( i in csv.list){
      data <- rbind.fill(lapply(csv.list, fread, header = T))
    }
    # Ceux qui ont une PAM et un NOM
    data$NOM <- ifelse(data$NOM == " ", NA, data$NOM)
    data <- data[which((!is.na(data$PAm) & !is.na(data$NOM))),]
    # Si artérielle manquantes alors brassard
    data$ARTd <- ifelse(is.na(data$ARTd), data$PAd, data$ARTd)
    data$ARTm <- ifelse(is.na(data$ARTm), data$PAm, data$ARTm)
    data$ARTs <- ifelse(is.na(data$ARTs), data$PAs, data$ARTs)
    # Identifiant patient
    data <- transform(data, id = as.numeric(factor(data$NOM)))
    colnames(data) <- tolower(colnames(data))
    data$hr <- data$fc 
    data$abpmean <- data$artm
    data$abpsys <- data$arts
    data$abpdias <- data$artd
    data <- subset(data, select = c(lit, nom, id, time_and_date, hr, spo2, abpmean, abpsys, abpdias))
    data <- data[order(data$id, data$time_and_date),]
    write.table(data, paste0(dossier.ecriture,"/",num.chambre,".csv"), row.names = F, na = "NA")
    return(data)
  }else{
    print( paste0("Pas de dossier correspondant à la chambre:", num.chambre))
    return(0)
  }
}

# cohorte <- function(num.chambre, 
#                     dossier.lecture){
#   dest.room <- paste0(dossier.lecture, num.chambre) 
#   if(file.exists(dest.room)){
#     csv.list <- list.files(path = dest.room , pattern =".csv", full.names = T)
#     for ( i in csv.list){
#       data <- rbind.fill(lapply(csv.list, fread, header = T))
#     }
#     # Ceux qui ont une PAM et un NOM
#     data$NOM <- ifelse(data$NOM == " ", NA, data$NOM)
#   }
#   return(data)
# }
# 
# all_num = NULL
# for(i in 1:22){
#   all_num = rbind.fill(all_num,cohorte(num.chambre = i, dossier.lecture = lecture))
# }
# length(unique(all_num$NOM)) 219 patients

##############################
##############################  1 - Generation des numerics
##############################

library(reshape2)
library(data.table)
library(tidyverse)
library(plyr)
library(wavelets)

lecture <- "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/CH_"
ecriture <- "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/CH_"
ecriture.fichier <-  "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/clean/"

# Lecture / Ecriture / Combinaison de tous les fichiers par chambre
for(i in 1:9){
  print(paste0("chambre : ", i))
  reecriture.fichier(num.chambre = i,
                     dossier.lecture = lecture, 
                     dossier.ecriture =  ecriture,
                     vecteur.variables = c("IDT","TIME_AND_DATE","N", 
                                           "NOM","LIT", "FC", "SpO2",
                                           "ARTd" , "ARTm","ARTs","FR",
                                           "PAd","PAs","PAm"))
  combiner.fichier(num.chambre = i, 
                   dossier.lecture = lecture,
                   dossier.ecriture = ecriture.fichier)
  
}


# Numerics final avec tous les fichiers par chambre
csv.list <- list.files(path = ecriture.fichier , pattern =".csv", full.names = T)
for ( i in csv.list){
  print(i)
  numerics <- rbind.fill(lapply(csv.list, fread, header = T))
}

numerics <- transform(numerics, id = as.numeric(factor(numerics$nom)))
numerics <- numerics[order(numerics$id, numerics$time_and_date),]
write.table(numerics, paste0(ecriture.fichier,"numerics_fabrice.csv"), row.names = F, na = "NA")

##############################
##############################  2 - Lecture des numerics
##############################

rm( list = ls())

library(reshape2)
library(data.table)
library(tidyverse)
library(plyr)
library(parallel)
library(doParallel)
library(gdata)

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/to.balanced.R")

# Numerics avec la pam enregistrée
numerics <- data.frame(fread(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/clean/numerics_fabrice.csv", 
  header = T))

# Recherche episodes hypotensifs
v_id <- unique(numerics$id)
recherche_episode = re = NULL
for( sujet in v_id){
	cat(sujet, "\t")
	re <- periodeAHE(dossier_patient = sujet,  outcome = 65)
	recherche_episode <- rbind(recherche_episode, re)
}

recherche_episode$debut_periode <-  as.POSIXct(strptime(recherche_episode$debut_periode,"%Y-%m-%d %H:%M:%S"))
recherche_episode <- na.omit(recherche_episode) 

##############################
##############################  3 - Numerics observation
##############################

df_information <- recherche_episode %>% 
  mutate(
    event = ifelse(event > 0, 1, 0),
    periode = as.character(periode),
    periode = gsub(" ","",periode),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"),
    id = as.character(id))  

numerics <- subset(numerics, id %in% df_information$id) 

v_id <- unique(numerics$id)
df_numerics_observation_60_min <-  NULL
transp <-  NULL
for ( i in v_id){
  print(i)
  df_numerics_observation_60_min <- rbind(
    df_numerics_observation_60_min, 
    observation_periode(numerics, i, duree_obs = 60))
    
}

# Nombre de période d'observation de 60 min 
nombre_time <- sum(table(df_numerics_observation_60_min$id,
                         df_numerics_observation_60_min$periode) == 60)

df_numerics_sp <- df_numerics_observation_60_min %>%
  mutate(
    id = drop.levels(id),
    time = rep(seq(1, 60, 1), nombre_time),
    identif = paste(as.character(id), as.character(periode),sep="-"))

# Transpose les données pour avoir un fichier wide
transp = NULL
for (sujet in unique(df_numerics_sp$id)){
  print(sujet)
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                                      match("identif",names(df_numerics_sp)),
                                      match("time", names(df_numerics_sp)),
                                      match(c("hr","spo2","abpsys","abpdias",
                                              "abpmean"),
                                            names(df_numerics_sp))))
	}

transp <- transp %>% 
  mutate(
    id = as.character(str_match(identif,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2]))


df_final_wide <- inner_join(df_information, transp, 
                            by = c("identif","id","periode")) %>%
  select(-c(eevent, temps_ahe_observation, temps_ahe_prediction, 
            identif)) %>%
  na.omit()

# Conversion en numeric
n = ncol(df_final_wide)
number = ncol(df_final_wide[,c(1:which(names(df_final_wide) == "hr.t1")-1)])
df_final_wide[,number:n] <- apply(df_final_wide[,number:n],2, as.numeric)

##############################
##############################  4 - Résumé des signaux
##############################
df_numerics_sp  = na.omit(df_numerics_sp)
nombre_time <- sum(table(df_numerics_sp$id,
                         df_numerics_sp$periode) == 60)

df_numerics_sp[, c("hr","spo2","abpmean","abpsys","abpdias")] <- apply(df_numerics_sp[, c("hr","spo2","abpmean","abpsys","abpdias")] ,2, as.numeric)

# MOYENNE - VARIANCE 
df_final_wide_mean <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_sp, mean, na.rm = T)
colnames(df_final_wide_mean) <- c("periode", "id", "mean_hr","mean_spo2","mean_abpm","mean_abpd","mean_abps")

df_final_wide_var <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                               data=df_numerics_sp, var, na.rm = T)
colnames(df_final_wide_var) <- c("periode", "id", "var_hr","var_spo2","var_abpm","var_abpd","var_abps")

# MODELE LINEAIRE 
df_final_wide_lineaire <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                    data = df_numerics_sp, estimate_tendance, temps_observation = 60) 
df_final_wide_lineaire <- do.call(data.frame, df_final_wide_lineaire)
colnames(df_final_wide_lineaire) <- c("periode", "id",
                                      "alpha_hr",  "beta_hr",
                                      "alpha_spo2","beta_spo2",
                                      "alpha_abpm","beta_abpm",
                                      "alpha_abpd","beta_abpd",
                                     "alpha_abps","beta_abps")                                
# ARIMA (1,0,1)
df_final_wide_arma <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_sp, para_arima, temps_observation = 60) 
df_final_wide_arma <- do.call(data.frame, df_final_wide_arma)
colnames(df_final_wide_arma) <- c("periode","id","ar_hr",  "ma_hr",  "inter_hr",
                                  "ar_spo2","ma_spo2","inter_spo2",
                                  "ar_abpm","ma_abpm","inter_abpm",
                                  "ar_abpd","ma_abpd","inter_abpd",
                                  "ar_abps","ma_abps","inter_abps")
                                  
                                  
                                
# HAAR : 5
library(wavelets)  
para_haar = function(x,prof=5){
	if( length(x) == 60){
		x <- c(unlist(x))
	  y <- dwt(x, filter="haar", boundary="periodic")
  	coeffs <- c(y@W[[prof]],y@V[[prof]])
  	return(coeffs)
  }else 
  	return(c(NA,NA))
}

df_final_wide_haar <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_sp, para_haar)                                
                                
#hey = unique(df_numerics_sp$identif)
#res = NULL
#for (ii in hey){
#	cat(ii, "\n")
#	res = para_haar(base::subset(df_numerics_sp, identif == ii,  select = hr ))
#}

df_final_wide_haar <- do.call(data.frame, df_final_wide_haar)
colnames(df_final_wide_haar) <- c("periode","id","W1_hr",  "V1_hr",
                                  "W1_spo2","V1_spo2",
                                  "W1_abpm","V1_abpm",
                                  "W1_abpd","V1_abpd",
                                  "W1_abps","V1_abps")

df_final_wide <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                        list(df_final_wide_mean,
                         df_final_wide_var,
                         df_final_wide_lineaire,
                         df_final_wide_arma, 
                         df_final_wide_haar))

df_final <- merge(df_information, df_final_wide, by = c("id", "periode"))
df_final <- df_final[order(df_final$id,df_final$periode),]

ecriture.fichier <-  "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/clean/"
write.csv(df_final, paste0(ecriture.fichier,"df_modelisation.csv"), row.names = F)

