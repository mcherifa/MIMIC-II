#######################################################
# Toutes les fonctions nécessaires à l'aggregation 
# des données numériques et de la base MIMIC dans psql
#
# By Menyssa CHERIFA 
# GITHUB : https://github.com/afroinshape/AHE.git
#######################################################

#######################################################
# Visualisation des numérics
#######################################################

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

visualisation.ggplot <- function(fichier){
  
  # fichier <- subset(df_long, id == 10013 )
  
  data <- melt(fichier,id = c("id","time_and_date"))
  data$value <- as.numeric(as.character(data$value))
  
  q <- ggplot(data,
              mapping = aes(x = time_and_date, y = value, shape = variable, colour = variable)) +
    geom_line() + theme_bw() + xlab("Temps") +
    facet_grid(facets = variable ~ .) +
    ggtitle(paste0("Numerics du patient : ", fichier$id ))
  
  return(q)
  
}

#######################################################
# Permet de décaler une série
#######################################################

shift <- function(x, lag) {
  
  n <- length(x)
  xnew <- rep(NA, n)
  
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- as.character(x[(abs(lag)+1):n])
  } 
  else if (lag > 0) {
    
    xnew[(lag+1):n] <- as.character(x[1:(n-lag)])
  } 
  else {
    
    xnew <- x
  }
  
  return(xnew)
}

#######################################################
# Recherche episode hypotensif par période
#######################################################

periode.episode <- function(data,  drapeau){
  
  # fichier = subset(df_long, id == 10013)
  # data = fichier
  # drapeau = 5
  # 
  p = mat_event = temp = event = event_24h =  NULL 
  somme = event_cum = 0
  
  if(nrow(data) >= 90){
    
    data$time_and_date <- as.POSIXct(strptime(data$time_and_date,
                                              "%Y-%m-%d %H:%M:%S"))
    premier.point      <- as.POSIXlt(data$time_and_date)[1]
    dernier.point      <- as.POSIXlt(data$time_and_date)[nrow(data)]
    difference         <- difftime(dernier.point, premier.point,units="hours")
    nombre.periode     <- as.numeric(difference) * 60 / 90
    timebreak          <- premier.point + (seq(0,nombre.periode) * 90 * 60)
    data$fac           <- cut(data$time_and_date, breaks = timebreak, 
                              labels = 1 : nombre.periode)
    
    for ( i in levels(data$fac)){
      
      temp <- base::subset(data, fac %in% i)
      p = unique(temp$fac) 
      
      if(nrow(temp) == 90){
        
        debut_periode <- temp$time_and_date[1]
        
        # Observation window
        fin_periode_ecoute  <- debut_periode + 60 * 60
        observation         <- dplyr::filter(temp,(time_and_date >= debut_periode & 
                                                     time_and_date < fin_periode_ecoute ))
        observation$abpmean <- as.numeric(as.character(observation$abpmean))
        
        for ( i in 1 : length(observation$abpmean)){
          
          v <- ifelse(observation$abpmean[i] < 60 &
                        observation$abpmean[i] > 0,1,0)
          if(v == 1 ){somme <- somme + 1}
          else {somme <- 0}
          
        }
        
        # AHE in observation window
        eevent <- ifelse(somme >= drapeau , 1, 0)
        
        # Prediction window
        zone.ahe        <- debut_periode + 70 * 60
        periode         <- dplyr::filter(temp,(time_and_date >= zone.ahe & 
                                                 time_and_date <= zone.ahe + 20 * 60))
        periode$abpmean <- as.numeric(as.character(periode$abpmean))
        
        for ( i in 1 : length(periode$abpmean)){
          
          v <- ifelse(periode$abpmean[i] < 65 & periode$abpmean[i] > 0,1,0)
          if(v == 1 ){somme <- somme + 1}
          else {somme <- 0}
          
        }
        # AHE in prediction window
        event <- ifelse(somme >= drapeau , 1, 0)
        
        # AHE cumulé
        event_cum = event_cum + event 
        
        # Resultat
        mat_event <- c(mat_event, unique(periode$id),
                       eevent, event, event_cum, 
                       p, as.character(debut_periode))
        
      }
    }
    
    if(is.null(mat_event)==F){
      
      mat_event <- data.frame(matrix(mat_event,ncol = 6,byrow = T))
      
      colnames(mat_event) <-c("id","eevent","event","event_cum",
                              "periode","jour")
      
      mat_event$event     <- as.numeric(as.character(mat_event[,'event']))
      mat_event$event_cum <- as.numeric(as.character(mat_event[,'event_cum']))
      mat_event$periode   <- as.numeric(as.character(mat_event[,'periode']))
      mat_event$jour      <- as.POSIXct(strptime(mat_event[,'jour'],"%Y-%m-%d %H:%M:%S"))
      
      for (i in 1: length(mat_event$jour)){
        
        jour1     <- mat_event[i,"jour"]
        jour2     <-  jour1 - ( 60 * 60 * 24)
        temp      <- dplyr::filter(mat_event, ( jour < jour1 &  jour >= jour2))
        event_24h <- c(event_24h,sum(temp$event))
        
      }
      mat_event$event_24h <- event_24h
    }
    return(mat_event)
  }
}

#######################################################
# Numerics de toute la période de 90 min
#######################################################

numerics_episode <- function(fichier){
  
  if(nrow(fichier) >= 90){
    
    fichier$time_and_date <- as.POSIXct(strptime(fichier$time_and_date,
                                                 "%Y-%m-%d %H:%M:%S"))
    
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

#######################################################
# Numerics de l'observation
# ( 60 premieres valeurs de la période ) 
#######################################################

soixante_prems <- function(fichier){
  
  data = dfset = NULL
  
  for( i in unique(fichier$periode)){
    
    dfset <- data.frame(subset(fichier, periode %in% i))
    
    if (nrow(dfset) == 90){ 
      data  <- rbind(data,dfset[1:60,]) }
  }
  return(data)
}

#######################################################
# Médicaments prescrits dans la période
#######################################################

medoc_periode <- function(df1, df2){
  
  # df1 <- recherche_episode
  # df2 <- amine
  # v_id = c(19208)
  
  
  res = dt1 = dt2 = indx = NULL
  df2$id <- as.character(df2$subject_id)
  v_id <- unique(df1$id)
  
  for ( i in v_id )
  {
    print(i)
    dt1 <- subset(df1, id %in% i )
    dt2 <- na.omit(subset(df2, id %in% i))
    
    
    dt1 <- data.table(ID=dt1$id, start=dt1$jour, end=dt1$jour)
    setkeyv(dt1, colnames(dt1))
    
    dt2 <- data.table(ID=dt2$id, start=dt2$starttime, end=dt2$endtime)
    setkeyv(dt2, colnames(dt2))
    
    if(nrow(dt2) == 0 |is.na(dt1$start)) {res = res}
    
    else
    { 
      indx <- foverlaps(dt1, dt2, type='within', which=TRUE, mult='first')
      res = rbind(res,dt1[, match:= +(!is.na(indx))][,end:=NULL])
    }
  }
  
  return(res)
}

#######################################################
# Ventilation durant la période
#######################################################

ventilation_periode <- function(dt1, dt2){
  
  data = NULL
  # dt1 = base::subset(df, id %in% 10013) 
  # dt2 = base::subset(ventilation, subject_id %in% 10013)
  # 
  if(nrow(dt2)!=0)
  {
    dt1$jour1 <- as.POSIXct(strptime(shift(dt1$jour,-1),"%Y-%m-%d %H:%M:%S"))
    dt1[is.na(dt1$jour1),"jour1"] <-  as.POSIXct(strptime(dt1$jour[nrow(dt1)] + 60 * 90,
                                                          "%Y-%m-%d %H:%M:%S"))
    
    for( k in 1: nrow(dt2)){
      for( j in 1:nrow(dt1)){
        if( dt1$jour[j] <= dt2$charttime[k] & dt2$charttime[k] <= dt1$jour1[j] )
        {
          print(dt2$charttime[k])
          data$jour    <- c(data$jour, as.character(dt1$jour[j]))
          data$venti   <- c(data$venti , 1 - dt2$extubated[k])
          data$periode <- c(data$periode,dt1$periode[j])
        }
      }
    }
    data <- data.frame(data)
    data$id <- rep(unique(dt1$id),nrow(data))
    return(data)
  }
}

#######################################################
# Résultat des lactates dans la période
#######################################################

lactate_periode <- function(dt1, dt2){
  
  data = NULL
  # dt1 = base::subset(df, id %in% i) 
  # dt2 = base::subset(lactate, subject_id %in% i)
  
  if(nrow(dt2)!=0)
  {
    dt1$jour1 <- as.POSIXct(strptime(shift(dt1$jour,-1),"%Y-%m-%d %H:%M:%S"))
    dt1[is.na(dt1$jour1),"jour1"] <-  as.POSIXct(strptime(dt1$jour[nrow(dt1)] + 60 * 90,
                                                          "%Y-%m-%d %H:%M:%S"))
    
    for( k in 1: nrow(dt2)){
      for( j in 1:nrow(dt1)){
        if( dt1$jour[j] <= dt2$charttime[k] &  dt2$charttime[k] <= dt1$jour1[j] )
        {
          data$jour    <- c(data$jour, as.character(dt1$jour[j]))
          data$lactate   <- c(data$lactate, dt2$valuenum[k])
          data$periode <- c(data$periode,dt1$periode[j])
        }
      }
    }
    data <- data.frame(data)
    data$id <- rep(unique(dt1$id),nrow(data))
    return(data)
  }
}

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







##############################################################################################################
# 
# #######################################################
# # --- NOT USE 
# #######################################################


# 
# #######################################################
# # --- Rescale continuous variables
# #######################################################
# 
# centrer_reduire <- function(x) {
#   return((x - mean(x)) / sqrt(x))
# } 
# 
# #######################################################
# # --- Train et test echantillons
# #######################################################
# 
# split_train_test <- function(donnees, var_y) {
#   
#   set.seed(20191) 
#   
#   intrain <- createDataPartition(donnees$event, p = .70, list = FALSE)
#   
#   trainDescr <- donnees[intrain, colnames(donnees)!= var_y]
#   trainY <- donnees[intrain, var_y]
#   
#   testDescr <- donnees[-intrain, colnames(donnees)!=var_y]
#   testY <- donnees[-intrain, var_y]
#   
#   # Transformations des variables 
#   
#   # a <- BoxCoxTrans(trainY) 
#   #trainY_trans <- predict(a, trainY)
#   #testY_trans <- predict(a, testY)
#   
#   train_test <- list(
#     donnees = donnees,
#     intrain = intrain,
#     
#     trainDescr = trainDescr,
#     trainY = trainY,
#     
#     testDescr = testDescr,
#     testY = testY
#     
#     #trainY_trans = trainY_trans,
#     #testY_trans = testY_trans,
#     # a = a
#   )
#   return(train_test)
# }
# 
# #######################################
# # Modele lineaire:  selection de variables 
# ####################################### 
# 
# 
# significatives <- function(x, df){
#   
#   pval <- GHQ <- NULL
#   
#   # Formula du modele
#   
#   variables_dans_model <- as.formula(
#     paste("event ~ ",x))
#   
#   # Modele
#   
#   GHQ <- glm(formula = variables_dans_model, 
#              data = df,
#              family = binomial(link = "logit")) 
#   
#   # Pvalue 
#   # print(GHQ)
#   return(summary(GHQ)$coefficients[2,4])
# }
# 
# 
# selection <- function(df){
#   
#   variables_interet  <- names(subset(df, select= c(-event, -id)))
#   res <- list()
#   
#   for (i in 1:length(variables_interet)){
#     # print(variables_interet[i])
#     res[[i]] <- significatives(variables_interet[i], df =  df)
#   }
#   
#   var_signifs <- data.frame(matrix(cbind(variables_interet,res < 0.05),
#                                    ncol = 2, nrow = length(variables_interet)))
#   
#   colnames(var_signifs) <- c("variables","significative") 
#   
#   variables_multi <- var_signifs[var_signifs$significative == T,]
#   
#   return(variables_multi$variables)
# }







