#######################################################
# -------- Traitement des fichiers "numerics" 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
source("~/Recherche/Mimic-II/scripts/R/toolbox/text_cleaning.R")

library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(gdata) 

chemin <-  "~/Recherche/Mimic-II/data/numerics"

####################
#---- Fichiers téléchargés
####################

# Fichiers qui comprends les numéros de dossiers 
# ou il y a des waveforms et des numerics 
fichiers_numeric <- read.table("~/Recherche/Mimic-II/data/tools/mimic2_RECORDS-numerics.txt")[1]
nombre_patients <- as.factor(str_extract(fichiers_numeric$V1,".*(?=\\/)")) 
nombre_patients <- gsub("s0+",'' ,nombre_patients)
nombre_patients <- gsub("s",'' , nombre_patients)
nombre_patients_file <- str_extract(nombre_patients, "\\d+")
print(length(unique(nombre_patients))) # 2808 patients

# Fichiers numerics téléchargés
nombre_patients <-  list.files(path = chemin,
                               pattern =".csv", full.names = T) %>%
  str_extract(.,"\\w*.(?=.csv)") %>% 
  gsub("s0+",'',.) %>%
  gsub("s",'',. ) %>% 
  str_extract(.,"\\d+")
print(length(unique(nombre_patients))) # 2808 patients

# Différence entre le téléchargement et le fichier d'origine 
setdiff(nombre_patients_file, nombre_patients)

####################
#---- Clean csv
####################

# Nombre de patients final
df <- clean_csv(chemin = chemin) ## uniquement sur les fichiers téléchargés 
numerics <- data.frame(df$data)
length(unique(df$data$id)) # 1351 patients

# Nombre de patients supprimés sans ABPMean
nombre_patients_sup <- df$n_fichier_sup  %>%
  str_extract(.,"\\w*.(?=.csv)") %>% 
  gsub("s0+",'',.) %>%
  gsub("s",'',. ) %>% 
  str_extract(.,"\\d+")
print(length(unique(nombre_patients_sup)))

# Sauvegarde
save(numerics, file = "~/Recherche/Mimic-II/data/clean/mimic2/numerics.RData")

print("------------------------------------------------------------")
print("------------------ CLEAN NUMERICS DONE----------------------")
print("------------------------------------------------------------")
