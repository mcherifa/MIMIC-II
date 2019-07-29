#######################################################
# Application de différentes méthodes de feature sélection
# sur les variables quanti
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

rm(list = ls())

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")
library(dplyr)
library(wavelets)

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"
load(file.path(chemin,"3_numerics.RData"))
load("~/Recherche/Mimic-II/data/clean/mimic2/4_data.RData")

df_information <- df_final_wide_60min[,c(1:which(names(df_final_wide_60min) == "hr.t1")-1
)]

###
# MOYENNE - VARIANCE 
###

df_final_wide_mean <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data=df_numerics_observation_60_min, mean, na.rm = T)
colnames(df_final_wide_mean) <- c("periode", "id", "mean_hr","mean_spo2","mean_abpm","mean_abpd","mean_abps")

df_final_wide_var <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                               data=df_numerics_observation_60_min, var, na.rm = T)
colnames(df_final_wide_var) <- c("periode", "id", "var_hr","var_spo2","var_abpm","var_abpd","var_abps")

###
# MODELE LINEAIRE 
###

df_final_wide_lineaire <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                    data = df_numerics_observation_60_min, estimate_tendance, temps_observation = 60) 
df_final_wide_lineaire <- do.call(data.frame, df_final_wide_lineaire)
colnames(df_final_wide_lineaire) <- c("periode", "id",
                                      "alpha_hr",  "beta_hr",
                                      "alpha_spo2","beta_spo2",
                                      "alpha_abpm","beta_abpm",
                                      "alpha_abpd","beta_abpd",
                                      "alpha_abps","beta_abps")
###
#  ARIMA (1,0,1)
###

df_final_wide_arma <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_observation_60_min, para_arima,temps_observation = 60) 
df_final_wide_arma <- do.call(data.frame, df_final_wide_arma)
colnames(df_final_wide_arma) <- c("periode","id","ar_hr",  "ma_hr",  "inter_hr",
                                  "ar_spo2","ma_spo2","inter_spo2",
                                  "ar_abpm","ma_abpm","inter_abpm",
                                  "ar_abpd","ma_abpd","inter_abpd",
                                  "ar_abps","ma_abps","inter_abps")
###
# HAAR : 5
###

df_final_wide_haar <- aggregate(cbind(hr, spo2, abpmean, abpsys, abpdias) ~ periode + id, 
                                data = df_numerics_observation_60_min, para_haar) 
df_final_wide_haar <- do.call(data.frame, df_final_wide_haar)
colnames(df_final_wide_haar) <- c("periode","id","W1_hr",  "V1_hr",
                                  "W1_spo2","V1_spo2",
                                  "W1_abpm","V1_abpm",
                                  "W1_abpd","V1_abpd",
                                  "W1_abps","V1_abps")

df_final_wide <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                        list(df_final_wide_mean, df_final_wide_var, df_final_wide_lineaire,
                             df_final_wide_arma, df_final_wide_haar))

df_final <- merge(df_information, df_final_wide, by = c("id", "periode"))
df_final <- df_final[order(df_final$id,df_final$periode),]

# #######################################
# # FOURIER
# #######################################
# 
# df_final_wide_fourier = data.frame(row.names=1:nrow(df_final_wide))
# for(i in variable_num){
#   
#   mini = min(grep(i,names(df_final_wide)))
#   maxi = max(grep(i,names(df_final_wide)))
#   df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
#   df_final_wide_fourier <- cbind(df_final_wide_fourier, t(data.frame(
#     m = apply(df_final_wide_numeric,2,para_fourier))))
# }
# df_final_wide_fourier <- df_final_wide_fourier %>%
#   setNames(c("f1_hr",  "f2_hr",  "f3_hr",
#              "f1_spo2","f2_spo2","f3_spo2",
#              "f1_abpm","f2_abpm","f3_abpm",
#              "f1_abpd","f2_abpd","f3_abpd",
#              "f1_abps","f2_abps","f3_abps")) %>%
#   cbind(df_final_wide[,1:mini_hr-1]) 

#######################################
# SAVE
#######################################

save(df_final_wide_mean,
     df_final_wide_var,
     df_final_wide_lineaire, 
     df_final_wide_arma,
     df_final_wide_haar,  
     df_final,
     file = "~/Recherche/Mimic-II/data/clean/mimic2/4_data.RData")

print("------------------------------------------------------------")
print("------------------ Transformations numerics DONE------------")
print("------------------------------------------------------------")     

# faire tourner la procédure arima automatique
# sur toutes les variables numériques
# pour savoir quel type de modele arima (0?0?0?)

# HR :
# - ma : 3306
# - ar : 1751
# mini = min(grep("hr",names(df_final_wide)))
# maxi = max(grep("hr",names(df_final_wide)))
# df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
# hr <- apply(df_final_wide_numeric,2,function(xx){
#   # renvoie le modele utilisé
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(hr),decreasing = T)
# 
# # SPO2 : 
# # - ma : 1958
# # - ar : 1839
# mini = min(grep("spo2",names(df_final_wide)))
# maxi = max(grep("spo2",names(df_final_wide)))
# df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
# sp <- apply(df_final_wide_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(sp),decreasing = T)
# 
# # ABPMEAN :
# # - ma : 2358
# # - ar : 1808
# mini = min(grep("abpm",names(df_final_wide)))
# maxi = max(grep("abpm",names(df_final_wide)))
# df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
# abpm <- apply(df_final_wide_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abpm),decreasing = T)
# 
# # ABP DIAS :
# # - ma : 2228
# # - ar : 1768
# mini = min(grep("abpd",names(df_final_wide)))
# maxi = max(grep("abpd",names(df_final_wide)))
# df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
# abpd <- apply(df_final_wide_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abpd),decreasing = T)
# 
# # ABPSYS :
# # - ma : 2303
# # - ar : 1513
# mini = min(grep("abps",names(df_final_wide)))
# maxi = max(grep("abps",names(df_final_wide)))
# df_final_wide_numeric <- t(df_final_wide[,mini:maxi])
# abps <- apply(df_final_wide_numeric,2,function(xx){
#   paste(names(coef(auto.arima(xx))),collapse=" ")
# })
# sort(table(abps),decreasing = T)








