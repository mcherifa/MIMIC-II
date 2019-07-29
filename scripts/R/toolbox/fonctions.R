#######################################################
# Toutes les fonctions à l'analyse de MIMIC II
# By Menyssa CHERIFA 
# GITHUB : https://github.com/afroinshape/AHE.git
#######################################################

# ------------------------------------------------
# ----- Script 1 Clean Numerics 
# ------------------------------------------------ 

# Supprime les fichiers de patients qui n'ont pas de 
# pression enregistrée
# 1- donner juste le chemin vers le dossier qui contient les fichiers csv
suppression <- function (dest){
  csv.list <- list.files(path = dest, pattern =".csv", full.names=T)
  for ( i in csv.list){
    print(i)
    # data <- try(fread(i, na.strings ='-',header=T), silent = T) 
    # if(attr(data ,"class")[1]=="try-error") file.remove(i)
    # else(attr(data ,"class")[1]!="try-error")
    
    data <- fread(i, na.strings ='-',header=T)
    {
      # Si pas de colonne ABPMean alors 1 sinon 0
      flag <- ifelse((sum(colnames(data)=="'ABPMean'",na.rm = T)==0),1,0)
      if(flag) file.remove(i)
      else{write.csv(data, i)}
    }
    csv.list_apres <- list.files(path = dest, pattern =".csv", full.names=T)
    n_fichier_sup <- setdiff(csv.list, csv.list_apres)
  }
  return(n_fichier_sup)
}

# Rassemble à la suite en un fichier tous les 
# fichiers des patients avec des numerics
# 1- Donner uniquement le chemin vers les fichiers a combiner
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
  
  data <- try(base::subset(data, select = c(id, time_and_date, hr, abpsys, abpdias,
                                            abpmean,spo2)), 
              silent = TRUE)
  
  data[,c('hr','abpsys','abpmean','abpdias','spo2')] <- data.frame(
    mapply(function(x){as.numeric(as.character(x))},
           data[,c('hr','abpsys','abpmean','abpdias','spo2')]))
  
  data <- data[order(data$id,data$time_and_date),]
  
  return(data)
}

# Supprime et clean les csv pour les rbind_all
# 1 - Donner juste le chemin vers le dossier qui contient les numerics
clean_csv <- function(chemin){
  setwd(dir = chemin)
  # 1- Supprimer la ligne 2 de chaque csv qui contient 
  # les unités de mesure des variables enregistrées
  system(paste(
    "for csv in *.csv; do sed -i '/mmHg/d' $csv; 
    done"))
  
  # 2-  Ajout colonne : ID avec en contenu dans le nom du fichier 
  system(paste(
    "for csv in *.csv
  do
  awk 'NR==1{print $0 \",id\"}NR>1{print $0 \",\"FILENAME}' \"$csv\" > tempfile && mv tempfile \"$csv\"
  done"
  ))
  
  # 3 - Enlever les espaces dans les colonnes
  # et les remplacer par '_'
  system(paste(
    "for csv in *.csv
   do
   awk 'NR==1{gsub(/ /, \"_\", $0); print} NR>1{print $0}' \"$csv\" > tempfile && mv tempfile \"$csv\"
   done"
  ))
  
  # 4 - Supprime les fichiers qui n'ont pas d'ABP ou a 0 sur tout le fichier 
  n_fichier_sup <- suppression(dest = chemin)  # 1381 patients
  
  # 5 - Grande base de donnée qui contient tous les numerics
  # de tous les patients ayant une APBMean
  df_long_n <- rbind_all_numeric(chemin)
  
  df <- df_long_n %>% 
    subset(abpmean > 0 ) %>%
    na.omit() %>%
    mutate(id = as.numeric(as.character(id)))
  
  return(list(data = df,
              n_fichier_sup = n_fichier_sup))
}

# ------------------------------------------------
# ----- Script 2 Données mimic
# ------------------------------------------------ 
# Numerics de toute la période de 90 min
periode_90min <- function(fichier){
  if(nrow(fichier) >= 90){
    fichier$time_and_date <- as.POSIXct(strptime(fichier$time_and_date,"%Y-%m-%d %H:%M:%S"))
    premier.point   <- as.POSIXlt(fichier$time_and_date)[1]
    dernier.point   <-as.POSIXlt(fichier$time_and_date)[nrow(fichier)]
    difference      <- difftime(dernier.point, premier.point,units="hours")
    nombre.periode  <- as.numeric(difference) * 60 / 90 
    timebreak       <- premier.point + (seq(0,nombre.periode) * 90 * 60)
    fichier$periode <- cut(fichier$time_and_date, breaks = timebreak,
                           labels = 1 : nombre.periode)
    
    return(fichier)
  }
}

# Numerics de la période d'observation
# ("duree_obs" premieres valeurs de la période ) 
observation_periode <- function(df, sujet, duree_obs){
  data = dfset = NULL
  fichier <- periode_90min(base::subset(numerics, id %in% sujet))
  for( i in unique(fichier$periode)){
    dfset <- subset(fichier, periode %in% i)
    if(nrow(dfset)==90){
      data <- rbindlist(list(data, dfset[1:duree_obs,]))
    }
  }
  return(data.frame(data))
}

# Recherche episode hypotensif par période de 90 min
# Recherche une MAP < 65mmHg pendant au moins "le drapeau".
# dans la periode de prédiction pour chaque periode d'un patient : "fichier"
## Data = numerics
## Drapeau = duree de l'hypotension
## Outcome = 65 mmHg 
## Duree phase d'observation = 60, 50, 40 min 
periodeAHE <- function(data = numerics, 
                       dossier_patient,
                       outcome,
                       duree_obs = 60){
  
#  dossier_patient = 11
#  outcome = 65
#  duree_obs = 60
  
  fichier = unique(base::subset(numerics, id %in% dossier_patient))
  variable = "abpmean"
  
  drapeau = NULL
  p = mat_event = temp = event = event_24h = observation_ahe =  periode_ahe = NULL 
  somme = event_cum = temp_eevent = temps_event = 0
  
  if(nrow(fichier) > 90 ){
    data <- periode_90min(fichier)
    # Pour chaque période  
    for ( i in levels(data$periode)){
      temp <- base::subset(data, periode %in% i)
      if(nrow(temp) == 90) {
        drapeau <- 5
      } else drapeau <- 5 * 60 # Si numerics en secondes car 300 sec egale 5 min
      p <- i
      debut_periode <- temp$time_and_date[1]
      if(nrow(temp) >= 90){
        # Observation window
        fin_periode_ecoute  <- debut_periode + (60 * duree_obs)
        observation         <- dplyr::filter(temp,(time_and_date >= debut_periode & time_and_date < fin_periode_ecoute ))
        observation[,variable] <- as.numeric(as.character(observation[,variable]))
        # AHE pendant l'observation
        # Si apbmean et nabpmean a 0 ou NA de calcul
        if(sum(observation[,variable], na.rm = T) > 0){
          for ( i in 1 : length(observation[,variable])){
            v <- ifelse(observation[i,variable] < outcome & ! is.na(observation[i,variable]), 1, 0)
            if(v == 1) somme <- somme + v
            else somme <- 0
            observation_ahe[i] <- somme
          }
          # Nombre de AHE pdt l'observation
          eevent <- sum(observation_ahe == drapeau)
          # Combien de temps dure l'hypotension pendant l'observation
          if(eevent != 0 && drapeau == 5) {
            temps_eevent <- drapeau * eevent
          }else if (eevent != 0 && drapeau == 300){
            temps_eevent <- drapeau * eevent / 60
          }else temps_eevent <- 0
        }else{
          eevent <- 0
          temps_eevent <- 0 
        }
        # Prediction window
        zone.ahe <- debut_periode + 70 * 60
        periode <- dplyr::filter(temp,(time_and_date >= zone.ahe & time_and_date <= zone.ahe + 20 * 60))
        periode[,variable] <- as.numeric(as.character(periode[,variable]))
        somme <- 0
        #if(nrow(periode) != 0){
        for ( i in 1 : length(periode[, variable])){
          v <- ifelse(periode[i,variable] < outcome & !is.na(periode[i,variable]),1,0)
          if(v == 1 ) somme <- somme + 1
          else somme <- 0
          periode_ahe[i] <- somme
        }
        # AHE in prediction window
        event <- sum(periode_ahe == drapeau) 
        if(event != 0 && drapeau == 5) {
          temps_event <- drapeau * event
        }else if (event != 0 && drapeau == 300){ 
          temps_event <- drapeau * event / 60
        }else temps_event = 0
        # AHE cumulé
        event_cum = event_cum + event
        # Resultat
        mat_event <- c(mat_event,
                       eevent, temps_eevent,
                       event, temps_event,
                       event_cum, 
                       p, 
                       as.character(debut_periode))
      }else{mat_event <- c(mat_event,
                           rep(NA,5),
                           p,
                           as.character(debut_periode))
      }
    }# ferme le for 
  }else{mat_event <- c(mat_event,
                       rep(NA,5),
                       NA,
                       NA)}
  mat_event <- data.frame(matrix(mat_event, ncol = 7, byrow = T))
  colnames(mat_event) <-c("eevent","temps_ahe_observation",
                          "event","temps_ahe_prediction",
                          "event_cum","periode", "debut_periode")
  mat_event$event     <- as.numeric(as.character(mat_event[,'event']))
  mat_event$event_cum <- as.numeric(as.character(mat_event[,'event_cum']))
  mat_event$event_cum <- mat_event$event_cum - 1
  mat_event$periode   <- as.numeric(as.character(mat_event[,'periode']))
  mat_event$id <- dossier_patient
  return(mat_event)
}

# Recherche episode hypotensif par période de 90 min
# Recherche une MAP < 65mmHg pendant au moins "le drapeau".
# dans la periode de prédiction pour chaque periode d'un patient : "fichier"
## Data = numerics
## Drapeau = duree de l'hypotension
## Outcome = 65 mmHg 
## Duree phase d'observation = 60, 50, 40 min 
# periode.episode <- function(fichier,  drapeau, outcome, duree_obs){
#   
#   # fichier = subset(numerics, id == 6)
#   # drapeau = 5
#   # outcome = 65
#   # duree_obs = 60
#   
#   p = mat_event = temp = event = event_24h = observation_ahe =  periode_ahe = NULL 
#   somme = event_cum = 0
#   # Definition d'une période d'étude de 90 minutes
#   data <- periode_90min(fichier)
#   # Pour chaque période  
#   for ( i in levels(data$periode)){
#     temp <- base::subset(data, periode %in% i)
#     p <- unique(temp$periode) 
#     if(nrow(temp) == 90){
#       debut_periode <- temp$time_and_date[1]
#       # Observation window
#       fin_periode_ecoute  <- debut_periode + (60 * duree_obs)
#       observation         <- dplyr::filter(temp,(time_and_date >= debut_periode & time_and_date < fin_periode_ecoute ))
#       observation$abpmean <- as.numeric(as.character(observation$abpmean))
#       # AHE pendant l'observation
#       for ( i in 1 : length(observation$abpmean)){
#         v <- ifelse(observation$abpmean[i] < outcome & observation$abpmean[i] > 0,1,0)
#         if(v == 1 ){
#           somme <- somme + 1
#         }else {
#           somme <- 0
#         }
#         observation_ahe[i] <- somme
#       }
#       # AHE in observation window
#       eevent <- sum(observation_ahe == drapeau)
#       # Combien de temps dure l'hypotension pendant l'observation
#       temps_eevent <- length(which(observation_ahe >= 5))
#       # Prediction window
#       zone.ahe        <- debut_periode + 70 * 60
#       periode         <- dplyr::filter(temp,(time_and_date >= zone.ahe & time_and_date <= zone.ahe + 20 * 60))
#       periode$abpmean <- as.numeric(as.character(periode$abpmean))
#       somme <- 0
#       for ( i in 1 : length(periode$abpmean)){
#         v <- ifelse(periode$abpmean[i] < outcome & periode$abpmean[i] > 0,1,0)
#         if(v == 1 ){
#           somme <- somme + 1
#         }else {
#           somme <- 0
#         }
#         periode_ahe[i] <- somme
#       }
#       # AHE in prediction window
#       event <- sum(periode_ahe == drapeau) 
#       temps_event <- length(which(periode_ahe >= 5))
#       # AHE cumulé
#       event_cum = event_cum + event 
#       # Resultat
#       mat_event <- c(mat_event,
#                      unique(periode$id),
#                      eevent, temps_eevent,
#                      event, temps_event, event_cum, 
#                      p, as.character(debut_periode))
#     }
#   }
#   if(is.null(mat_event) == FALSE){
#     mat_event <- data.frame(matrix(mat_event,ncol = 8,byrow = T))
#     colnames(mat_event) <-c("id","eevent","temps_ahe_observation",
#                             "event","temps_ahe_prediction","event_cum","periode","jour")
#     mat_event$event     <- as.numeric(as.character(mat_event[,'event']))
#     mat_event$event_cum <- as.numeric(as.character(mat_event[,'event_cum']))
#     mat_event$periode   <- as.numeric(as.character(mat_event[,'periode']))
#     mat_event$jour      <- as.POSIXct(strptime(mat_event[,'jour'],"%Y-%m-%d %H:%M:%S"))
#     # Ahe dans les 24h précédentes
#     for (i in 1: length(mat_event$jour)){
#       jour1     <- mat_event[i,"jour"]
#       jour2     <-  jour1 - ( 60 * 60 * 24)
#       temp      <- dplyr::filter(mat_event, ( jour < jour1 &  jour >= jour2))
#       event_24h <- c(event_24h, sum(temp$event))
#     }
#     mat_event$event_24h <- event_24h
#   }
#   return(mat_event)
# }
# 
# Médicaments prescrits dans la période
# Recherche si des médicaments on été prescrit dans
# la période d'observation
# --- A ajouter visualisation de la période d'observation 
# --- de l'intervalle de prescription
medoc_periode <- function(df1, df2, sujet,type){
  
  # df1 <- recherche_episode
  # df2 <- amine
  # sujet = c(19208)
  # type = "amine"
  
  dt1 = dt2 = NULL
  dt1 <- subset(df1, id %in% sujet )
  dt1$debut_periode <- as.POSIXct(dt1$debut_periode, origin ="1970-01-01") 
  dt1$fin_periode <- as.POSIXct(dt1$debut_periode, origin ="1970-01-01") + (60 * 90)
  dt1 <- data.table(ID=dt1$id, start=dt1$debut_periode, end=dt1$fin_periode)
  setkeyv(dt1, c("start","end"))
  
  dt2 <- na.omit(subset(df2, subject_id %in% sujet ))
  dt2 <- data.table(ID=dt2$subject_id, start=dt2$starttime, end=dt2$endtime)
  setkeyv(dt2, c("start","end"))
  
  if(nrow(dt2) > 0){
    dt1$indx <- foverlaps(dt1, dt2, type='within', which=TRUE, mult='first')
    dt1$indx<- ifelse( is.na(dt1$indx),0,
                       ifelse(test = dt1$indx > 1, 1, dt1$indx))
    data.table::setnames(dt1, "ID","subject_id")
    data.table::setnames(dt1, "indx", type)
  }else {
    data.table::setnames(dt1, "ID","subject_id")
    dt1[,type] <- 0
  }   
  return(dt1)
}

# Ventilation durant la période
# ventilation_periode <- function(dt1, dt2){
#   
#   data = NULL
#   dt1 = base::subset(df, id %in% 10013) 
#   dt2 = base::subset(ventilation, subject_id %in% 10013)
#   
#   if(nrow(dt2)> 0){
#     for( k in 1: nrow(dt2)){
#       for( j in 1:nrow(dt1)){
#         if( dt1$start[j] <= dt2$realtime[k] & dt2$realtime[k] <= dt1$end[j] ){
#           cat(j,"...", k, "\n")
#           data$start    <- c(data$start, as.character(dt1$start[j]))
#           data$venti   <- c(data$venti ,dt2$mechvent[j])
#           data$periode <- c(data$periode,dt1$periode[j])
#         }
#       }
#     }
#     data <- data.frame(data)
#     data$id <- rep(unique(dt1$id),nrow(data))
#     return(data)
#   }
# }

ventilation_periode <- function(df, df2 = ventilation, sujet){
  
  dt1 = base::subset(df, id %in% sujet) 
  dt2 = base::subset(df2, subject_id %in% sujet)
  
  dt1$ventilation <- 0
  if(nrow(dt2)> 0){
    for( k in 1: nrow(dt1)){  
      for(j in 1: nrow(dt2)){
        if( dt1$start[k] <= dt2$realtime[j] & dt2$realtime[j] <= dt1$end[k] ){
          dt1$ventilation[k] <- 1
        }
      }
    }
  }
  return(dt1)
}


# ------------------------------------------------
# ----- Script 4 Données mimic
# ------------------------------------------------ 

# Modele linéaire :  tendance linéaire en fonction du temps
estimate_tendance <- function(x, temps_observation){
  time_series <- ts(x, start = 1 , end = temps_observation,frequency = 1)
  df_para <- lm(time_series ~ time(time_series))
  return(df_para$coefficients)
  
}

# Modele arima 
para_arima <- function(x, temps_observation){
  time_series <- ts(x, start = 1 , end = temps_observation,frequency = 1)
  fittmp <-try(arima(x, c(1,0,1)),silent=T)
  if(attr(fittmp ,"class")=="try-error")
  {
    fit1 <-list(coef=rep(NA,3))
  }
  if(attr(fittmp ,"class")!="try-error")
  {
    fit1 <- fittmp
  }
  return(fit1$coef)
}

# Discrete wavelets transform
para_haar <- function(x,prof=5){
  y <- dwt(x, filter="haar", boundary="periodic")
  coeffs <- c(y@W[[prof]],y@V[[prof]])
  return(coeffs)
}

# Fourier
para_fourier <- function(cs) {
  
  #freq_mini <- NULL
  
  # cs <- t(df[8869,9:68])
  
  aaa <- spectrum(cs,plot=F)
  df <- aaa$freq[order(aaa$spec,decreasing=T)]
  
  return(df[1:3])
}


#######################################################
# Visualisation des numérics
#######################################################

# Renvoie un graphique dynamique correspondant 
# aux numerics d'un patient : "fichier"
visualisation.dygraph <- function(fichier){
  
  data_xts <- as.xts(fichier[-1], 
                     order.by = fichier$time_and_date, "%Y-%m-%d h:m:s")[,-1]
  
  graphique <-  dygraph( data_xts ) %>%
    dyRangeSelector()%>% 
    dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"), labelsUTC = T) %>% 
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyLimit(60, color = "red")
  
  
  return(graphique)
  
}

# Represente une serie temporelle avec les traitements

# dygraph(nhtemp, main="New Haven Temperatures") %>%
#   dySeries(label="Temp (F)", color="black") %>%
#   dyShading(from="1920-1-1", to="1930-1-1", color="#FFE6E6") %>%
#   dyShading(from="1940-1-1", to="1950-1-1", color="#CCEBD6")


# Renvoie un graphique non dynamique correspondant 
# aux numerics d'un patient : "fichier"
visualisation.ggplot <- function(fichier){
  
  data <- melt(fichier,id = c("id","time_and_date"))
  data$value <- as.numeric(as.character(data$value))
  
  graphique <- ggplot(data, mapping = aes(x = time_and_date, y = value, shape = variable, colour = variable)) +
    geom_line() + theme_bw() + xlab("Temps") +
    facet_grid(facets = variable ~ .) +
    ggtitle(paste0("Numerics du patient : ", fichier$id ))
  
  return(graphique)
  
}

#######################################################
# Decalage d'une série temporelle
#######################################################
shift <- function(x, lag) {
  n <- length(x)
  xnew <- rep(NA, n)
  if (lag < 0) { xnew[1:(n-abs(lag))] <- as.character(x[(abs(lag)+1):n]) } 
  else if (lag > 0) { xnew[(lag+1):n] <- as.character(x[1:(n-lag)])} 
  else {xnew <- x}
  return(xnew)
}


#######################################################
# Détermine un episode hypotensif en continue
#######################################################

episode_continue <- function(fichier, tau){
  
  res  <- df <- NULL
  somme = 0
  
  # fichier <- subset(df_n, id %in% 10013)
  
  for ( i in 1:length(fichier$abpmean) ){
    
    v <- ifelse(fichier$abpmean[i] < 65 & fichier$abpmean[i] > 0,1,0)
    if(v == 1){somme <- somme + 1}
    else {somme <- 0}
    res = c(res, somme)
  }
  ahe <- ifelse(res >= 5, 1 , 0)
  df <- data.frame(cbind(fichier,res,ahe))
  df$ahe_shift <- shift(df$ahe,tau)
  
  return(df)
}


