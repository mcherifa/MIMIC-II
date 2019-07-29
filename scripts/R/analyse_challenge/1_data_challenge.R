##############################
# Générer un data.frame 
##############################

library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(gdata)
library(wavelets)
library(lubridate)
library(FactoMineR)
library(factoextra)

setwd(dir="/home/menyssa/Recherche/Mimic-II/scripts/R/analyse_challenge/")
source("~/Recherche/Mimic-II/scripts/R/toolbox/text_cleaning.R")

############################
# FONCTIONS
############################

# Combiner toutes les données
rbind_all_numeric <- function(dest, type){
  
  csv.list <- list.files(path = dest, pattern =".csv", full.names = T)
  data <- rbind.fill(lapply(csv.list, fread, na.strings ='-', header = T)) %>%
    clean_colnames() %>% 
    Filter(function(x)!all(is.na(x)),.)
  
  # Recodage des dates 
  h <- str_extract(data$time_and_date,"\\d+:\\d+:\\d+")
  d <- str_extract(data$time_and_date,"[0-9]{2}/[0-9]{2}/[0-9]{4}")
  dh <- paste0(d," ",h)
  data$time_and_date <- as.POSIXct(strptime(dh,"%d/%m/%Y %H:%M:%S"))
  # Definition de l'ID du patient 
  data$id <- as.factor(str_extract(data$id,"\\w*.(?=.csv)")) %>%  
    gsub("s0+",'' ,.) %>%
    gsub("s",'' ,.) %>%
    str_extract(., "\\d+") %>%
    as.factor()
  
  if(type == "training"){
    data <- try(base::subset(data, select = c(id, time_and_date, hr, abpsys, abpdias, 
                                              abpmean, spo2)), 
                silent = TRUE)
    data[,c('hr','abpsys','abpmean','abpdias','spo2')] <- data.frame(
      mapply(function(x){as.numeric(as.character(x))},
             data[,c('hr','abpsys','abpmean','abpdias','spo2')]))
  }else{
    data <- try(base::subset(data, select = c(id, time_and_date, hr, abp_sys, abp_dias, 
                                              abp_mean, spo2)), 
                silent = TRUE)
    
    data[,c('hr','abp_sys','abp_mean','abp_dias','spo2')] <- data.frame(
      mapply(function(x){as.numeric(as.character(x))},
             data[,c('hr','abp_sys','abp_mean','abp_dias','spo2')]))
  }
  data <- data[order(data$id,data$time_and_date),]
  return(data)
}

# fonctions Résumés
MeanVar <- function(x){
  return(c(moyenne = mean(x), variance = var(x)))
}

LinearTendance <- function(x){
  time_series <- ts(x, start = 1 , end = 60, frequency = 1)
  df_para <- lm(time_series ~ time(time_series))
  coefs <- c(intercept = df_para$coefficients[1], coef = df_para$coefficients[2])
  return(coefs)
}

ArmaModel <- function(x){
  time_series <- ts(x, start = 1 , end = 60,frequency = 1)
  fittmp <-try(arima(x, c(1,0,1)),silent=T)
  if(attr(fittmp ,"class")=="try-error") fit1 <-list(coef=rep(NA,3))
  else fit1 <- fittmp
  return(fit1$coef)
}

# Discrete wavelets transform
Wavelets <- function(x){
  y <- dwt(x, filter="haar", boundary="periodic", fast = TRUE)
  coeffs <- c( wavelets.coef = y@W[[length(names(y@W))]],
               scaling.coef = y@V[[length(names(y@V))]])
  return(coeffs)
}

# Fourier
Fourier <- function(x) {
  aaa <- spectrum(x,plot=F)
  df <- aaa$freq[order(aaa$spec,decreasing=T)]
  return(df[1:3])
}

SummaryTimesSeries <- function(df, which_signals, fun){
  dat.linear <- dat.arima <- dat.mean_var <- dat.wavelets <- dat.fourier <- data_resume <- NULL
  df <- na.omit(df)
  v_id <- sort(unique(df$id))
  for(i in v_id){
    print(i)
    tmp <- subset(obs_period, id == i ) %>%
      select(which_signals) %>%
      na.omit()
    if("mean_var" %in% fun){
      dat.mean_var <- rbind(dat.mean_var,
                            unmatrix(apply(tmp, 2 , MeanVar)))
    }
    if("linear" %in% fun){
      dat.linear <- rbind(dat.linear,
                          unmatrix(apply(tmp, 2 , LinearTendance)))
    }
    if("arima" %in% fun){
      dat.arima <- rbind(dat.arima,
                         unmatrix(apply(tmp, 2 , ArmaModel)))
    }
    
    if("wavelets" %in% fun){
      dat.wavelets <- rbind(dat.wavelets,
                            unmatrix(apply(tmp, 2 , Wavelets)))
    }
  }
  data_resume <- data.frame(cbind(v_id, dat.linear, dat.arima, dat.mean_var, dat.wavelets))
  
  colnames(data_resume) <- c("id",
                             "m_hr","v_hr","m_spo2","v_spo2", 
                             "m_abpm","v_abpm", "m_abpd","v_abpd", "m_abps","v_abps",
                             "beta_hr", "alpha_hr", "beta_spo2","alpha_spo2",
                             "beta_abpm","alpha_abpm","beta_abpd","alpha_abpd","beta_abps","alpha_abps",
                             "ar_hr","ma_hr", "inter_hr", "ar_spo2","ma_spo2","inter_spo2",
                             "ar_abpm","ma_abpm","inter_abpm", "ar_abpd","ma_abpd","inter_abpd", "ar_abps","ma_abps","inter_abps",
                             "W1_hr", "V1_hr", "W1_spo2","V1_spo2", 
                             "W1_abpm","V1_abpm","W1_abpd","V1_abpd", "W1_abps","V1_abps")
  return(list( id = v_id, data = data_resume))
}

############################
# MAIN
############################

####
#### Test
####
data_test <- rbind_all_numeric(dest = "/home/menyssa/Recherche/Mimic-II/data/testing (copie)/",
                               type = "test")

# Outcome 
outcome <- read.csv("/home/menyssa/Recherche/Mimic-II/data/testing (copie)/outcome_test.txt",
                    header = F, sep = "\t")
colnames(outcome) <- c("id", "outcome")



# Prendre la derniere heure: 3600 dernieres lignes
v_id <- unique(data_test$id)
obs_period <- NULL
for(i in v_id){
  tmp_data <- subset(data_test, id == i)
  tmp_data <- tmp_data[32401:36000,]
  obs_period <- rbind(obs_period,tmp_data)
}

# Resumer des 1h d'obs / patients
summary.data <- SummaryTimesSeries(obs_period,
                                   fun = c("linear", "arima", "mean_var","wavelets"),
                                   which_signals = c("hr", "spo2",
                                                     "abp_mean",  "abp_dias", "abp_sys"))
summary.data$data$id <- summary.data$id
data_test <- summary.data$data
data_test <- outcome %>%  merge(data_test, by = "id", all.y= T)

# Sauvegarde
save(data_test, file = "data_test.RData")

####
#### Training 
####

data <- rbind_all_numeric(dest = "/home/menyssa/Recherche/Mimic-II/data/training (copie)", 
                          type = "training")
# Extraire TO, T-70 et T-10
dates <- read.csv("/home/menyssa/Recherche/Mimic-II/data/training (copie)/dates_depart.txt",
                  header = T, sep = "\t") %>% 
  mutate(time_T0 = as.POSIXct(strptime(T0_date_time,"%d/%m/%Y %H:%M")),
         time_moins_70 = time_T0 - minutes(70),
         time_moins_10 = time_T0 - minutes(10)) %>%
  select(-T0_date_time)

# Outcome
outcome <- read.csv("/home/menyssa/Recherche/Mimic-II/data/training (copie)/outcome.txt",
                    header = T, sep = "\t")
# Recodage de l'id
dates$id <- dates$record_numerics %>%  
  gsub("a",'' ,.) %>%
  gsub("n",'' ,.) %>%
  str_extract(., "\\d+") %>%
  as.factor()

# Données avant TO
v_id <- sort(unique(data$id))
obs_period  <- NULL
for(i in v_id ){
  print(i)
  tmp_data <- subset(data, id == i)
  tmp_dates <- subset(dates, id == i)
  tmp_data <- merge(tmp_data, tmp_dates, by = "id", all.x =T) 
  obs_period <- rbind(obs_period,filter(tmp_data,(time_and_date >= time_moins_70 & time_and_date < time_moins_10)))
}

# Données d'observation résumées
summary.data <- SummaryTimesSeries(obs_period,
                                   fun = c("linear", "arima", "mean_var","wavelets"),
                                   which_signals = c("hr", "spo2", "abpmean",  "abpdias", "abpsys"))
summary.data$data$id <- summary.data$id

# Autres données et outcome
data_train <- outcome %>%
  merge(dates, by = "id", all.x = T) %>%
  select(id, sex, age, outcome) %>%
  merge(summary.data$data, by = "id", all.y = T ) %>%
  mutate(id = as.factor(as.character(id)))

# Sauvegarde
save(data_train, file = "data_train.RData")
