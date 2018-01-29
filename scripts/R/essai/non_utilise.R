# Lecture fichier rbind.fill des fichiers du challenge
lire.fichier.fill <- function(dest1, dest2, w=F){
  # dest1 <- "/home/mcherifa/Mimic/data/raw/training_csv/numerics"
  # dest2 <- "/home/mcherifa/Mimic/data/raw/test_csv/numerics"
  w = T 
  if (w==T){
    csv.list <- c(list.files(path=dest1,pattern =".csv",full.names=T),
                  list.files(path=dest2,pattern =".csv",full.names=T))
  }else{
    csv.list <- list.files(path=dest1,pattern =".csv",full.names=T)
  }
  
  data <- rbind.fill(lapply(csv.list,fread,header=T)) %>% clean_colnames() 
  
  position_id <- which(names(data) %in% "id")
  
  data <- data[-1,c(1:position_id)]
  
  h <- str_extract(data$timeanddate,"\\d+:\\d+:\\d+")
  d <- str_extract(data$timeanddate,"[0-9]{2}/[0-9]{2}/[0-9]{4}")
  dh <- paste0(d," ",h)
  
  data$timeanddate <- as.POSIXct(strptime(dh,"%d/%m/%Y %H:%M:%S"))
  
  data[3:ncol(data)-1]<- data.frame(
    lapply(data[3:ncol(data)-1],function (x){
      y <- as.numeric(as.character(x))
      return(y)
    }))
  
  data$id <- as.factor(str_extract(data$id,"\\w*.(?=.csv)"))
  
  return(data)
} 


##############################################################################################################
# Fichiers Fabrice 
##############################################################################################################

lire.fichier.fabrice <- function(){
  
  dest <- "/home/mcherifa/Mimic/data/raw/BLOC 4"
  csv.list <- list.files(path=dest,pattern =".csv",full.names=T)
  
  for ( i in csv.list){
    print(i)
    data <- read.csv(i,na.strings ='-',header=T,sep=";")
    
    # On garde que les numerics
    data <- data[which(!is.na(str_match(data$HEURE,"\\d+:\\d+:00"))==T),]
    
    # names data 
    noms <- as.character(names(data))
    cc <- gsub("\\.", " ", noms, perl=TRUE)
    part <- colsplit(string=cc, pattern=" ", names=c("Part1", "Part2"))
    colnames(data) <- part$Part1
    
    # colonnes d'intéret
    data <- subset(data, select=c(IDT,FC,SpO2,ARTd ,ARTm ,ARTs,FR))
    
    # suppresion lignes na
    data$count <- rowSums(is.na(data))
    data <- subset(data, count < 1)
    
    write.csv(data,i,row.names = F)
  }
}

# suppresion lignes NA
dest <- "/home/mcherifa/Mimic/data/raw"
csv.list <- list.files(path=dest,pattern =".csv",full.names=T)

for ( i in csv.list){
  data <- read.csv(i)
  data$count <- rowSums(is.na(data))
  data <- subset(data, count < 1)
  
  write.csv(data,i,row.names = F)
}

# Rbind les fichiers 
dest <- "/home/mcherifa/Mimic/data/raw/BLOC 4"
csv.list <- list.files(path=dest,pattern =".csv",full.names=T)
for ( i in csv.list){ data <- rbind.fill(lapply(csv.list,fread,header=T))}
write.csv(data,"/home/mcherifa/Mimic/data/clean/numeric_fabrice.csv",row.names = F)


#######################################################
# -------- MAIN 1 --------
#######################################################

#
# Permet d'obtenir 4 fichiers aggrégés num, wave, train, test
#

########### TRAINING 

#dest <- "/home/mcherifa/Mimic/data/raw/training_csv/numerics"
#dflong_train.n <- lire.fichier.fill(dest)
#saveRDS(dflong_train.n,"/home/mcherifa/Mimic/data/clean/training_numerics.rds")

# waveform 
#dest <- "/home/mcherifa/Mimic/data/raw/training_csv/waveform"
#dflong_train.w <- lire.fichier.fill(dest)
#saveRDS(dflong_train.w,"/home/mcherifa/Mimic/data/clean/training_waveform.rds")

########### TEST  

# numeric
#dest <- "/home/mcherifa/Mimic/data/raw/test_csv/numerics"
#dflong_test.n <- lire.fichier.fill(dest)
#saveRDS(dflong_test.n, "/home/mcherifa/Mimic/data/clean/test_numerics.rds")

# waveform 
#dest <- "/home/mcherifa/Mimic/data/raw/test_csv/waveform"
#dflong_test.w <- lire.fichier.fill(dest)
#saveRDS(dflong_test.w, "/home/mcherifa/Mimic/data/clean/test_waveform.rds")

#######################################################
# -------- MAIN 2 --------
#######################################################

#
# Permet d'obtenir 2 fichiers aggrégés : num et waveform
#

########### NUMERICS  

#dest1 <- "/home/mcherifa/Mimic/data/raw/training_csv/numerics"
#dest2 <- "/home/mcherifa/Mimic/data/raw/test_csv/numerics"
#dflong_n <- lire.fichier.fill(dest1,dest2,w=T)
#saveRDS(dflong_n, "/home/mcherifa/Mimic/data/clean/numerics.rds")

########### WAVEFORM

#dest1 <- "/home/mcherifa/Mimic/data/raw/training_csv/waveform"
#dest2 <- "/home/mcherifa/Mimic/data/raw/test_csv/waveform"
#dflong_w <- lire.fichier.fill(dest1,dest2,w=T)
#saveRDS(dflong_w,"/home/mcherifa/Mimic/data/clean/waveform.rds")

########### -> Fichiers dans data/clean 


#######################################################
# -------- MAIN 3 --------
#######################################################
#
# Permet d'obtenir 1 fichier qui contient tous les numerics
#

########### NUMERICS  

dest <- "/home/mcherifa/Mimic/data/raw/numerics"
dflong_n <- lire.fichier.num.fill(dest)
saveRDS(dflong_n, "/home/mcherifa/Mimic/data/clean/numerics.rds")

