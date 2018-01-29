#######################################################
# -------- Data Clean 
#######################################################

# Amélioration finale du texte avec :
# - retrait des accents
# - passage en majuscule
# - retrait des mots usuels en anglais et français (of, to, de, ...)
# - retrait des url
# - retrait des chiffres
# prend en entrée une chaine de caractère ou un vecteur de caractère

improve_text <- function(text) {
  library(tm)
  # retrait des accents
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  # Passage en majuscule
  text <- toupper(text)
  # retrait ponctuation
  text = gsub("[[:punct:]]", "", text)
  # retrait chiffres
  text = gsub("[[:digit:]]", "", text)
  # remove links
  text = gsub("http\\w+", "", text)
  # retrait des mots 'usuels'
  text <- removeWords(text,stopwords("english"))
  text <- removeWords(text,stopwords("french"))
  return(text)
}

try_to_lower <- function(x) {
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# Permet de faire des csv propres
clean_text_cols <- function(data){
  clean_text <- function(col){
  #on remplace les retours à la ligne et les ; par des espaces
    return(gsub("[\r\n;]+"," ",col))
  }
  data[,colnames(Filter(is.character,data))] <- apply(Filter(is.character,data), 2, clean_text)
  return(data)
}

# Renommer les fichiers qui n'ont pas d'ABP_Mean enregistrée
# Ou ABP_Mean  a 0
renom <- function (dest){
   csv.list <- list.files(path=dest,pattern =".csv",full.names=T)
   for ( i in csv.list){
 	#i = "s00001.csv"
     print(i)
     data <- fread(i,na.strings ='-',header=T)
     flag <- ifelse((sum(colnames(data)=="'ABP_Mean'")==0),1,0)		
     if(flag ==1) {write.csv(data, paste0(i,"non"))}
     else{write.csv(data, i)}
   }
 }

renom(dest = "/home/mcherifa/Mimic/data/raw/numerics" )


# Garder que les lignes du csv d'un patient avec ABP > 35 
nettoyage <- function(){
   dest <- "/home/mcherifa/Mimic/data/raw/numerics"
   csv.list <- list.files(path=dest,pattern =".csv",full.names=T)
   for ( i in csv.list){
     #    i = "s00079.csv"
     print(i)
     data <- read.csv(i,header=T,sep=',')
     data <- data[data$X.ABPMean. > 35,]
     write.csv(data,i)
   }
 }

nettoyage()


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

# Lecture fichiers numerics telechargés
lire.fichier.num.fill <- function(dest){
  dest <- "/home/mcherifa/Mimic/data/raw/numerics"
  csv.list <- list.files(path=dest,pattern =".csv",full.names=T)
  data <- rbind.fill(lapply(csv.list,fread,na.strings ='-',header=T)) %>% clean_colnames() 
  
  position_id <- which(names(data) %in% "id")
  
  h <- str_extract(data$timeanddate,"\\d+:\\d+:\\d+")
  d <- str_extract(data$timeanddate,"[0-9]{2}/[0-9]{2}/[0-9]{4}")
  dh <- paste0(d," ",h)
  data$time_and_date <- as.POSIXct(strptime(dh,"%d/%m/%Y %H:%M:%S"))
  
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




