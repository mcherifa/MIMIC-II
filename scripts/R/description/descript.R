#######################################################
# Patients characteristics
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

rm(list = ls())

set.seed(10)
library(tidyverse)

# Fonctions
source("/home/menyssa/Recherche/Mimic-II/scripts/R/toolbox/utils.r")
source("/home/menyssa/Recherche/Mimic-II/scripts/R/toolbox/les_sources.R")

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"

# Traitements
load(file.path(chemin,"sedation.RData"))
load(file.path(chemin,"amine.RData")) 
amine <- amine %>% subset(duration > 0)
load(file.path(chemin,"ventilation.RData"))
ventilation <- subset(ventilation, mechvent == 1)


# Infos mimic2 pour tous les patients 
load(file.path(chemin,"infos_admission.RData")) 
infos_admission$gender <- ifelse(infos_admission$gender == "F", 1, 0) 
infos_admission$icu_death <- ifelse(infos_admission$icu_death == "Y", 1, 0) 
infos_admission$hospital_death <- ifelse(infos_admission$hospital_death == "Y", 1, 0) 
infos_admission$vasopresseurs <-  ifelse(infos_admission$subject_id %in% amine$subject_id, 1, 0) 
infos_admission$sedation <- ifelse(infos_admission$subject_id %in% sedation$subject_id, 1, 0) 
infos_admission$ventilation <- ifelse(infos_admission$subject_id %in% ventilation$subject_id, 1, 0) 

##################################
# Analyse de tous les patients 
##################################

# Infos mimic 2 pour les patients avec numerics
load(file.path(chemin,"df_modelisation.RData"))

# Prendre tous les 2808 patients de mimic 2 subset matched
fichiers_numeric <- read.table("~/Recherche/Mimic-II/data/tools/mimic2_RECORDS-numerics.txt")[1]
nombre_patients <- as.factor(str_extract(fichiers_numeric$V1,".*(?=\\/)")) 
nombre_patients <- gsub("s0+",'' ,nombre_patients)
nombre_patients <- gsub("s",'' , nombre_patients)
print(nombre_patients) 
df <- subset(infos_admission, subject_id %in% nombre_patients) 				 

# Patients avec numerics  On prend que les 1151 patients initiaux
df_wide <- readRDS(paste0(dest_data,"fichier_wide_periode.rds")) 
df$had_numerics_90min <- ifelse(df$subject_id %in% df_wide$id, 1,0 )

# Unique par ID
df <- df %>%
	group_by(subject_id) %>%
	sample_n(size = 1)  %>%
  data.frame()

# Table caracteristiques pour les numerics
# Overall
RES <- NULL 
RES <-  descr(x="age", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES <-  descr(x="gender", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")                               
RES <-  descr(x="bmi", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon")                                
RES <-  descr(x="sapsi_first", y = NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="sofa_first", y = NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="admission_type_descr", y = NULL, dat=df, RES, type="m", categ=NULL, prec=1, test="chisq")    
RES <-  descr(x="care_unit", y = NULL, dat=df, RES, type="m", categ=NULL, prec=1, test="chisq")    
RES <-  descr(x="los_hospital", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="los_icu", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon")
RES <-  descr(x="vasopresseurs", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="sedation", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="ventilation", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="rrt", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="icu_death", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="hospital_death", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="j28_death", y = NULL, dat=df, RES, type="b", categ=1, desc="med", prec=1, test="chisq")        

# VS numerics
RES1 <- NULL				 
RES1 <-  descr(x="age",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES1 <-  descr(x="gender", y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")                               
RES1 <-  descr(x="bmi",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon")                                
RES1 <-  descr(x="sapsi_first",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sofa_first",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="admission_type_descr",  y = "had_numerics_90min", dat=df, RES1, type="m", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="care_unit",  y = "had_numerics_90min", dat=df, RES1, type="m", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="los_hospital",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="los_icu",  y = "had_numerics_90min", dat=df, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon")
RES1 <-  descr(x="vasopresseurs",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="sedation",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="ventilation",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="rrt",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="icu_death",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="hospital_death",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="j28_death",  y = "had_numerics_90min", dat=df, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")

# Ecriture 
RES_all <- data.frame(cbind(RES, RES1[,-1]))
colnames(RES_all) <- c("Variables","All patients","No event","Event","P_value")
RES_all$Variables <- toupper(RES_all$Variables)
RES_all$P_value <- as.numeric(as.character(RES_all$P_value))
RES_all$P_value <- ifelse( RES_all$P_value < 0.001, "< 0.001",RES_all$P_value)
write.table(RES_all,"~/Recherche/Mimic-II/resultats_rewieving/caracteristiques_patients_numerics.csv",sep="\t",row.names=F,dec = ".")

##################################
# Analyse des patients avec numerics
##################################

df_numerics <- subset(df, subject_id %in% df_wide$id)
n <- length(unique(df_numerics$subject_id)) #1151 patients

# AHE 
AHE_patients <- subset(df_modelisation_mimic, id %in% df_wide$id) %>%
	group_by(id) %>%
	summarise(AHE = ifelse(sum(event) > 0,1,0))
	
df_numerics <- merge(df_numerics, AHE_patients, by.x = "subject_id", by.y = "id")

# Diagnostique
# temp = unique(df[,c("id","description")])
# tab = table(temp$description)
# tab = as.matrix(tab)
# write.csv(x = temp,file = "diagnostic.csv")
library(xlsx)
diag = read.xlsx("/home/menyssa/Recherche/Mimic-II/scripts/R/description/diagnostic_160318_AB.xlsx",
                 sheetIndex = 2)
length(unique(diag$subject_id))
diag = subset(diag, is.na(diagnosis) == F)
diag = diag[!duplicated(diag$subject_id),]

df_numerics = merge(x = df_numerics, y = diag, by.x = c("subject_id"),by.y = c("subject_id"))
             
             
# Tu peux regrouper tous les CCV ensemble,
# tous les trauma ensemble.
# Mettre CS dans CCV
# Regrouper AKI, IMV, ACR dans med
# Mettre cancer, chir et burn dans other

# Regroupement des diagnostiques
df_numerics[df_numerics$diagnosis %in% c("CCV?","CCVs","CS", "SCA"),"diagnosis"] = "CCV"
df_numerics[df_numerics$diagnosis == "Trauma","diagnosis"] = "trauma"
df_numerics[df_numerics$diagnosis %in% c("AKI", "IMV","ACR"),"diagnosis"] = "med"
df_numerics[df_numerics$diagnosis %in% c("cancer", "chir","Burn", "AHF  " ),"diagnosis"] = "other"
df_numerics$diagnosis = droplevels(df_numerics$diagnosis)

# Table caracteristiques pour les AHE
# Overall
RES <- NULL 
RES <-  descr(x="age", y=NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES <-  descr(x="gender", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")                               
RES <-  descr(x="bmi", y=NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon")                                
RES <-  descr(x="sapsi_first", y = NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="sofa_first", y = NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="diagnosis", y = NULL, dat=df_numerics, RES, type="m", categ=NULL, prec=1, test="chisq")    
RES <-  descr(x="admission_type_descr", y = NULL, dat=df_numerics, RES, type="m", categ=NULL, prec=1, test="chisq")    
RES <-  descr(x="care_unit", y = NULL, dat=df_numerics, RES, type="m", categ=NULL, prec=1, test="chisq")
RES <-  descr(x="vasopresseurs", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="sedation", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="ventilation", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="rrt", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="los_icu", y=NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon")
RES <-  descr(x="los_hospital", y=NULL, dat=df_numerics, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="icu_death", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="hospital_death", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")
RES <-  descr(x="j28_death", y = NULL, dat=df_numerics, RES, type="b", categ=1, desc="med", prec=1, test="chisq")        

# VS event
RES1 <- NULL				 
RES1 <-  descr(x="age",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES1 <-  descr(x="gender", y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")                               
RES1 <-  descr(x="bmi",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon")                                
RES1 <-  descr(x="sapsi_first",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sofa_first",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="diagnosis",  y = "AHE", dat=df_numerics, RES1, type="m", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="admission_type_descr",  y = "AHE", dat=df_numerics, RES1, type="m", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="care_unit",  y = "AHE", dat=df_numerics, RES1, type="m", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="vasopresseurs",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="sedation",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="ventilation",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="rrt",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="los_icu",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon")
RES1 <-  descr(x="los_hospital",  y = "AHE", dat=df_numerics, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="icu_death",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="hospital_death",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="j28_death",  y = "AHE", dat=df_numerics, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")


# Ecriture 
RES_all <- data.frame(cbind(RES, RES1[,-1]))
colnames(RES_all) <- c("Variables","All patients","No event","Event","P_value")
RES_all$Variables <- toupper(RES_all$Variables)
RES_all$P_value <- as.numeric(as.character(RES_all$P_value))
RES_all$P_value <- ifelse( RES_all$P_value < 0.001, "< 0.001",RES_all$P_value)
write.table(RES_all,"~/Recherche/Mimic-II/resultats_rewieving/caracteristiques_patients_AHE.csv",sep="\t",row.names=F,dec = ".")

##################################
# Analyse des patients fabrice
##################################

rm(list = ls())

library(reshape2)
library(data.table)
library(tidyverse)
library(plyr)
library(wavelets)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

# Données de Fabrice : numerics
df_validation <-  read.csv(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/clean/df_modelisation.csv", 
  header = T, sep = " ")

# Données de Fabrice : cliniques
df_information <-  read.csv(
  file = "/home/menyssa/Recherche/Mimic-II/data/fabrice_data/liste_patient.csv", 
  header = T )
df_information$care_unit
names(df_information) <- c("id","num_archive","nom","time_and_date","age",
                           "gender","sofa_first","sapsi_first","amine","sedate","venti")

df_information <- subset(df_information, 
                         select = -c(num_archive, nom, time_and_date))
df_information$care_unit1 <- rep(0, 55)
df_information$care_unit2 <- rep(1, 55)
df_information$care_unit4 <- rep(0, 55)

# Df_validation final 
df_validation <- right_join(df_information, df_validation, by = "id")
df_modelisation_fabrice <- subset(df_validation, select = -c(periode))

arma_data <- data.frame(mapply(FUN = na.zero, 
                               subset(df_modelisation_fabrice, select = c("ar_hr", "ma_hr", "inter_hr", "ar_spo2",
                                                                          "ma_spo2", "inter_spo2", "ar_abpm",              
                                                                          "ma_abpm","inter_abpm", "ar_abpd",
                                                                          "ma_abpd","inter_abpd","ar_abps", 
                                                                          "ma_abps","inter_abps"))))

df_modelisation_fabrice <- df_modelisation_fabrice %>% dplyr::select(-c(ar_hr, ma_hr, inter_hr, ar_spo2,
                                                                        ma_spo2, inter_spo2, ar_abpm,              
                                                                        ma_abpm,inter_abpm, ar_abpd,
                                                                        ma_abpd,inter_abpd,ar_abps, 
                                                                        ma_abps,inter_abps))

df_modelisation_fabrice <- cbind(df_modelisation_fabrice, arma_data) %>% na.omit()


colnames(df_modelisation_fabrice) <-  c("id",          "age",         "gender",      "sofa_first", 
                                        "sapsi_first", "amine",       "sedation",    "ventilation",      
                                        "care_unit1",  "care_unit2",  "care_unit4",  "event",      
                                        "mean_hr",     "var_hr",      "mean_spo2",   "var_spo2",     
                                        "mean_abpm",   "var_abpm",    "mean_abpd",   "var_abpd",     
                                        "mean_abps",   "var_abps",    "beta_hr",     "alpha_hr",   
                                        "beta_spo2",   "alpha_spo2",  "beta_abpm",   "alpha_abpm", 
                                        "beta_abpd",   "alpha_abpd",  "beta_abps",   "alpha_abps", 
                                        "W1_hr" , 		 "V1_hr",       "W1_spo2",     "V1_spo2",     "W1_abpm",    
                                        "V1_abpm",     "W1_abpd",     "V1_abpd",     "W1_abps",    
                                        "V1_abps", 		 "ar_hr",       "ma_hr",       "inter_hr",    "ar_spo2", 
                                        "ma_spo2",     "inter_spo2",  "ar_abpm",     "ma_abpm",   
                                        "inter_abpm",  "ar_abpd",     "ma_abpd",     "inter_abpd", 
                                        "ar_abps",     "ma_abps",     "inter_abps")
                                        
data_event <-  df_modelisation_fabrice[df_modelisation_fabrice$event ==1,]
data_event <- data_event %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()

df_sample_fabrice <- df_modelisation_fabrice %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()
  
# on enleve les patients présents dans data_events
df_sample_fabrice <- subset(df_sample_fabrice, !(id %in% data_event$id))
# Recolle le tout
df_sample_fabrice <- na.omit(rbind(df_sample_fabrice, data_event))

# Fonctions
source("/home/menyssa/Recherche/Mimic-II/scripts/R/toolbox/utils.r")
source("/home/menyssa/Recherche/Mimic-II/scripts/R/toolbox/les_sources.R")

RES1 <- NULL				 
RES1 <-  descr(x="age",  y = NULL, dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES1 <-  descr(x="gender",  y = NULL, dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")               
RES1 <-  descr(x="sapsi_first",  y = NULL, dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sofa_first",   y = NULL , dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="care_unit2",   y = NULL, dat=df_sample_fabrice, RES1, type="b", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="amine",  y = NULL, dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="sedation",   y = NULL, dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="ventilation",   y = NULL, dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")

RES1 <- NULL				 
RES1 <-  descr(x="age",  y = "event", dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1,test="wilcoxon") 
RES1 <-  descr(x="gender", y = "event", dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")               
RES1 <-  descr(x="sapsi_first", y = "event", dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sofa_first", y = "event", dat=df_sample_fabrice, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="care_unit2",   y = "event", dat=df_sample_fabrice, RES1, type="b", categ=NULL, prec=1, test="chisq")    
RES1 <-  descr(x="amine", y = "event", dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="sedation",  y = "event", dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")
RES1 <-  descr(x="ventilation",  y = "event", dat=df_sample_fabrice, RES1, type="b", categ=1, desc="med", prec=1, test="chisq")

