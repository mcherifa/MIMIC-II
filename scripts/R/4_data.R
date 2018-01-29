#######################################################
# Application de différentes méthodes de feature sélection
# sur les variables quanti
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Mimic/scripts/R/toolbox/les_sources.R")

#######################################
# Fonctions
#######################################

# Modele linéaire :  tendance linéaire en fonction du temps
estimate_tendance <- function(x){
  
  time_series <- ts(x, start = 1 , end = 60,frequency = 1)
  df_para <- lm(time_series ~ time(time_series))
  return(df_para$coefficients)
  
}

# Modele arima 
para_arima <- function(x){
  
  time_series <- ts(x, start = 1 , end = 60,frequency = 1)
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

#######################################
# Lecture 
#######################################

df <- readRDS(paste0(dest_data,"fichier_wide_periode.rds")) 

# limite entre numerics et les autres variables 
mini_hr = min(grep("hr",names(df)))

variable_num = c("hr","spo2","abpmean","abpdias","abpsys")

#######################################
# MOYENNE VARIANCE 
#######################################

df_resume = data.frame(row.names=1:nrow(df))
for(i in variable_num){

  mini = min(grep(i,names(df)))
  maxi = max(grep(i,names(df)))
  df_numeric <- t(df[,mini:maxi])
  df_resume <- cbind(df_resume, data.frame(m = apply(df_numeric,2,mean),
                                          v = apply(df_numeric,2,var)))
}
df_resume <- df_resume %>%
  setNames(c("m_hr","v_hr",
             "m_spo2","v_spo2",
             "m_abpm","v_abpm",
             "m_abpd","v_abpd",
             "m_abps","v_abps")) %>%
  cbind(df[,1:mini_hr-1])

#######################################
# MODELE LINEAIRE 
#######################################

df_lineaire = data.frame(row.names=1:nrow(df))
for(i in variable_num){

  mini = min(grep(i,names(df)))
  maxi = max(grep(i,names(df)))
  df_numeric <- t(df[,mini:maxi])
  df_lineaire <- cbind(df_lineaire, t(data.frame(
    m = apply(df_numeric,2,estimate_tendance))))
}
df_lineaire <- df_lineaire %>%
  setNames(c("beta_hr",  "alpha_hr",
             "beta_spo2","alpha_spo2",
             "beta_abpm","alpha_abpm",
             "beta_abpd","alpha_abpd",
             "beta_abps","alpha_abps")) %>%
  cbind(df[,1:mini_hr-1])

#######################################
#  ARIMA (1,0,1)
#######################################

# faire tourner la procédure arima automatique
# sur toutes les variables numériques
# pour savoir quel type de modele arima (0?0?0?)

# HR :
# - ma : 3306
# - ar : 1751
# mini = min(grep("hr",names(df)))
# maxi = max(grep("hr",names(df)))
# df_numeric <- t(df[,mini:maxi])
# hr <- apply(df_numeric,2,function(xx){
#   # renvoie le modele utilisé
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(hr),decreasing = T)
# 
# # SPO2 : 
# # - ma : 1958
# # - ar : 1839
# mini = min(grep("spo2",names(df)))
# maxi = max(grep("spo2",names(df)))
# df_numeric <- t(df[,mini:maxi])
# sp <- apply(df_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(sp),decreasing = T)
# 
# # ABPMEAN :
# # - ma : 2358
# # - ar : 1808
# mini = min(grep("abpm",names(df)))
# maxi = max(grep("abpm",names(df)))
# df_numeric <- t(df[,mini:maxi])
# abpm <- apply(df_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abpm),decreasing = T)
# 
# # ABP DIAS :
# # - ma : 2228
# # - ar : 1768
# mini = min(grep("abpd",names(df)))
# maxi = max(grep("abpd",names(df)))
# df_numeric <- t(df[,mini:maxi])
# abpd <- apply(df_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abpd),decreasing = T)
# 
# # ABPSYS :
# # - ma : 2303
# # - ar : 1513
# mini = min(grep("abps",names(df)))
# maxi = max(grep("abps",names(df)))
# df_numeric <- t(df[,mini:maxi])
# abps <- apply(df_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abps),decreasing = T)

df_arma = data.frame(row.names=1:nrow(df))
for(i in variable_num){

  mini = min(grep(i,names(df)))
  maxi = max(grep(i,names(df)))
  df_numeric <- t(df[,mini:maxi])
  df_arma <- cbind(df_arma, t(data.frame(
    m = apply(df_numeric,2,para_arima))))
}
df_arma <- df_arma %>%
  setNames(c("ar_hr",  "ma_hr",  "inter_hr",
             "ar_spo2","ma_spo2","inter_spo2",
             "ar_abpm","ma_abpm","inter_abpm",
             "ar_abpd","ma_abpd","inter_abpd",
             "ar_abps","ma_abps","inter_abps")) %>%
  cbind(df[,1:mini_hr-1])

#######################################
# HAAR : 5
#######################################

df_haar = data.frame(row.names=1:nrow(df))
for(i in variable_num){

  mini = min(grep(i,names(df)))
  maxi = max(grep(i,names(df)))
  df_numeric <- t(df[,mini:maxi])
  df_haar <- cbind(df_haar, t(data.frame(
    m = apply(df_numeric,2,para_haar))))
}
df_haar <- df_haar %>%
  setNames(c("W1_hr",  "V1_hr",
             "W1_spo2","V1_spo2",
             "W1_abpm","V1_abpm",
             "W1_abpd","V1_abpd",
             "W1_abps","V1_abps")) %>%
  cbind(df[,1:mini_hr-1])

# #######################################
# # FOURIER
# #######################################
# 
# df_fourier = data.frame(row.names=1:nrow(df))
# for(i in variable_num){
#   
#   mini = min(grep(i,names(df)))
#   maxi = max(grep(i,names(df)))
#   df_numeric <- t(df[,mini:maxi])
#   df_fourier <- cbind(df_fourier, t(data.frame(
#     m = apply(df_numeric,2,para_fourier))))
# }
# df_fourier <- df_fourier %>%
#   setNames(c("f1_hr",  "f2_hr",  "f3_hr",
#              "f1_spo2","f2_spo2","f3_spo2",
#              "f1_abpm","f2_abpm","f3_abpm",
#              "f1_abpd","f2_abpd","f3_abpd",
#              "f1_abps","f2_abps","f3_abps")) %>%
#   cbind(df[,1:mini_hr-1]) 

#######################################
# SAVE
#######################################

saveRDS(df_resume,   file = paste0(dest_data,"df_resume.rds"))
saveRDS(df_lineaire, file = paste0(dest_data,"df_lineaire.rds"))
saveRDS(df_arma,     file = paste0(dest_data,"df_arima.rds"))
saveRDS(df_haar,     file = paste0(dest_data,"df_haar.rds"))

