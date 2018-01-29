#######################################################
# -------- Traitement des fichiers "numerics" 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Mimic/scripts/R/toolbox/les_sources.R")

#######################################################
# -------- Fonctions
#######################################################

suppression <- function (dest){
  
  csv.list <- list.files(path = dest, pattern =".csv", full.names=T)
  
  for ( i in csv.list){
    
    print(i)
    
    data <- try(fread(i, na.strings ='-',header=T), silent = T) 
    
    if(attr(data ,"class")[1]=="try-error") file.remove(i)
    
    else(attr(data ,"class")[1]!="try-error")
    {
      # Si pas de colonne ABPMean alors 1 sinon 0
      flag <- ifelse((sum(colnames(data)=="'ABPMean'",na.rm = T)==0),1,0)
      
      if(flag) file.remove(i)
      
      else{write.csv(data, i)}
    }
  }
}

rbind_all_numeric <- function(dest){
  
  csv.list <- list.files(path = dest, pattern =".csv", full.names = T)
  
  data <- rbind.fill(lapply(csv.list, fread, na.strings ='-', header = T)) %>%
    clean_colnames() 
  
  # Recodage des dates 
  h <- str_extract(data$time_and_date,"\\d+:\\d+:\\d+")
  d <- str_extract(data$time_and_date,"[0-9]{2}/[0-9]{2}/[0-9]{4}")
  dh <- paste0(d," ",h)
  data$time_and_date <- as.POSIXct(strptime(dh,"%d/%m/%Y %H:%M:%S"))
  
  # Definition de l'ID du patient 
  data$id <- as.factor(str_extract(data$id,"\\w*.(?=.csv)"))
  data$id <- gsub("s0+",'' ,data$id)
  data$id <- gsub("s",'' ,data$id)
  data$id <- str_extract(data$id, "\\d+")
  data$id <- as.factor(data$id)
  
  data <- try(base::subset(data, select = c(id,time_and_date,hr,abpsys,abpdias,
                                            abpmean,spo2)), 
              silent = TRUE)
  
  data[,c('hr','abpsys','abpmean','abpdias','spo2')] <- data.frame(
    mapply(function(x){as.numeric(as.character(x))},
           data[,c('hr','abpsys','abpmean','abpdias','spo2')]))
  
  data <- data[order(data$id,data$time_and_date),]
  
  return(data)
}

#######################################################
# -------- Main 
#######################################################

chemin <- "~/Mimic/data/numerics"

# Fichiers qui comprends les numéros de dossiers 
# ou il y a des waveforms et des numerics 

fichiers_numeric <- read.table("~/Mimic/data/numerics/RECORDS-numerics.txt")[1]
nombre_patients <- as.factor(str_extract(fichiers_numeric$V1,".*(?=\\/)")) 
nombre_patients <- gsub("s0+",'' ,nombre_patients)
nombre_patients <- gsub("s",'' , nombre_patients)
nombre_patients_file <- str_extract(nombre_patients, "\\d+")
print(length(unique(nombre_patients))) # 2808 patients

# Fichiers numerics téléchargés
nombre_patients <-  list.files(path = chemin, pattern =".csv", full.names = T) %>%
  str_extract(.,"\\w*.(?=.csv)") %>% 
  gsub("s0+",'',.) %>%
  gsub("s",'',. ) %>% 
  str_extract(.,"\\d+")
print(length(unique(nombre_patients))) # 2808 patients

# Différence entre le téléchargement et le fichier d'origine 
setdiff(nombre_patients_file, nombre_patients)


# 1 - Supprime les fichiers qui n'ont pas d'ABP ou a 0 sur tout le fichier 
suppression(dest = chemin)  # 1381 patients

# 2 - Grande base de donnée qui contient tous les numerics

df_long_n <- rbind_all_numeric(chemin)
length(unique(df_long_n$id))

# 3 - Conserve les lignes où 35 < abpmean < 140 
# et sans données manquantes d'enregistrement

# df <- df_long_n %>% 
#   subset(abpmean > 35 & abpmean < 140) %>%
#   na.omit() %>%
#   mutate( id = as.numeric(as.character(id)))

df <- df_long_n %>% 
  subset(abpmean > 0 ) %>%
  na.omit() %>%
  mutate(id = as.numeric(as.character(id)))

# Nombre de patients 
length(unique(df$id)) # 1351 patients

# Sauvegarde
saveRDS(df,"~/Mimic/data/clean/mimic2/numerics.rds")

print("------------------------------------------------------------")
print("------------------ CLEAN NUMERICS DONE----------------------")
print("------------------------------------------------------------")
