#######################################################
# Script combine all data transformation 
# 
# By Menyssa CHERIFA 
#######################################################

source("/home/mcherifa/Mimic/scripts/R/toolbox/les_sources.R")
#source("/run/user/1000/gvfs/smb-share:server=10.42.42.9,share=public/TRANSMISSION_FICHIERS/Menyssa/Mimic/scripts/R/toolbox/les_sources.R")

# Chargement des données 
df <- readRDS(paste0(dest_data,"fichier_wide_periode.rds"))

# Management
df <- df[which(df$eevent == 0),]
df<- subset(df, select = c( -id ,-periode, - identif,-eevent))

# Variables factors
df[,c(1:5)]  <- data.frame(lapply(df[,c(1:5)], as.factor))

# NA si 0 puis sans NA 
df[,9:428] <- data.frame(apply( df[,9:428] , 2, function(x){
	y <- ifelse(x=="0",NA,x)
	return(y)
	}))

ind <- (apply(df,1,function(xx){sum(is.na(xx))})) < 60 

df <- df[ind, ]

# Si manquant mean  
df[,9:428] <- data.frame(apply( df[,9:428] , 2, function(x){
	y <- ifelse(is.na(x),mean(x,na.rm=T),x)
	return(y)
	}))

#######################################
# RESUME MEAN VARIANCE CORRELATION 
#######################################

df_numeric_resume = temp = NULL

# HR 

df_numeric <- t(df[,9:68])
df_numeric_resume <- data.frame(m_hr = apply(df_numeric,2,mean),
                                v_hr = apply(df_numeric,2,var))
# SPO2

df_numeric <- t(df[,69:128])
temp <- data.frame(m_sp = apply(df_numeric,2,mean),
                   v_sp = apply(df_numeric,2,var))
df_numeric_resume <- cbind(df_numeric_resume,temp)

# abpsys

df_numeric <- t(df[,129:188])
temp <- data.frame(m_sys = apply(df_numeric,2,mean),
                   v_sys = apply(df_numeric,2,var))
df_numeric_resume <- cbind(df_numeric_resume,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- data.frame(m_dias = apply(df_numeric,2,mean),
                   v_dias = apply(df_numeric,2,var))
df_numeric_resume <- cbind(df_numeric_resume,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- data.frame(m_mean = apply(df_numeric,2,mean),
                   v_mean = apply(df_numeric,2,var))
df_numeric_resume <- cbind(df_numeric_resume,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- data.frame(m_resp = apply(df_numeric,2,mean),
                   v_resp = apply(df_numeric,2,var))
df_numeric_resume <- cbind(df_numeric_resume,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- data.frame(m_ratio= apply(df_numeric,2,mean),
                   v_ratio = apply(df_numeric,2,var))

df_numeric_resume <- cbind(df_numeric_resume,temp)
df_resume <- cbind(df[,1:8] , df_numeric_resume)


# Correlation 

coefficients <- NULL 

for( i in 1:dim(df)[1])
{
	df_numeric <- data.frame(cbind(t(df[i,9:68]),t(df[i,249:308])))
	colnames(df_numeric) <-  c("HR","PAM")
	coefficients <- c(coefficients,cor(df_numeric$HR,df_numeric$PAM,method = "spearman"))
}

df_resume$coefficients <- coefficients

#######################################
# RESUME LINEAIRE
#######################################

estimate_tendance <- function(x){

		time_series <- ts(x, start = 1 , end = 60,frequency = 1)
		df_para <- lm(time_series~time(time_series))
		
		return(df_para$coefficients)
}

# HR 

df_numeric <- t(df[,9:68])
df_numeric_para <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(df_numeric_para) <- c("a_hr","b_hr")

# SPO2

df_numeric <- t(df[,69:128])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_sp","b_sp")
df_numeric_para <- cbind(df_numeric_para,temp)

# abpsys


df_numeric <- t(df[,129:188])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_sys","b_sys")
df_numeric_para <- cbind(df_numeric_para,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_dias","b_dias")
df_numeric_para <- cbind(df_numeric_para,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_mean","b_mean")
df_numeric_para <- cbind(df_numeric_para,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_resp","b_resp")
df_numeric_para <- cbind(df_numeric_para,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- t(data.frame(apply(df_numeric,2,estimate_tendance)))
colnames(temp) <- c("a_ratio","b_ratio")
df_numeric_para <- cbind(df_numeric_para,temp)

df_para <- cbind(df[,1:8] , df_numeric_para)


#######################################
# RESUME ARIMA
#######################################


library(forecast)

para_arima <- function(x){
	#print(x)
		time_series <- ts(x, start = 1 , end = 60,frequency = 1)
		fittmp <-try(arima(x, c(1,0,1)),silent=T)
		if(attr(fittmp ,"class")=="try-error")
		{
		  fit1<-list(coef=rep(NA,3))
		}
		if(attr(fittmp ,"class")!="try-error")
		{
		  fit1 <- fittmp
		}
		
		return(fit1$coef)
}

# HR : ma : 1675 et ar : 953

#bbb1 <- apply(t(df[,9:68]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb1),decreasing = T) 

# SPO2 : ma : 1162 et ar : 1123h

#bbb <- apply(t(df[,69:128]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 


# ABPSYS  : ma1 : 1017 et  ar1 :1067

#bbb <- apply(t(df[,129:188]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 

# abpdias : ma1 :1351 et ar1 : 1157


#bbb <- apply(t(df[,189:248]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 

# abpmean : ma1 1187 et ar1 : 995


#bbb <- apply(t(df[,249:308]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 

# resp : ma1 1734 et ar1 1664


#bbb <- apply(t(df[,309:368]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 


# RATIO : ma1 1397 er ar1 1070

#bbb <- apply(t(df[,369:428]),2,function(xx){
#		paste(names(coef(auto.arima(xx))),collapse=" ")
#	})

#sort(table(bbb),decreasing = T) 

# HR 

df_numeric <- t(df[,9:68])
df_numeric_arma <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(df_numeric_arma) <- c("a_hr","m_hr","i_hr")

# SPO2

df_numeric <- t(df[,69:128])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_sp","b_sp")
df_numeric_arma <- cbind(df_numeric_arma,temp)

# abpsys

df_numeric <- t(df[,129:188])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_sys","b_sys")
df_numeric_arma <- cbind(df_numeric_arma,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_dias","b_dias")
df_numeric_arma <- cbind(df_numeric_arma,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_mean","b_mean")
df_numeric_arma <- cbind(df_numeric_arma,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_resp","b_resp")
df_numeric_arma <- cbind(df_numeric_arma,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- t(data.frame(apply(df_numeric,2,para_arima)))
colnames(temp) <- c("a_ratio","b_ratio")
df_numeric_arma <- cbind(df_numeric_arma,temp)

df_arma <- cbind(df[,1:8] , df_numeric_arma)



#######################################
# HAAR : 4
#######################################

library(wavelets)


para_haar <- function(x,prof=4){

        y <- dwt(x, filter="haar", boundary="periodic")
    		coeffs <- c(y@W[[prof]],y@V[[prof]])
        return(coeffs)
}

# HR

df_numeric <- t(df[,9:68])
df_numeric_haar <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(df_numeric_haar) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
# SPO2

df_numeric <- t(df[,69:128])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpsys

df_numeric <- t(df[,129:188])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_hr","W2_hr","W3_hr",
																"V1_hr","V2_hr","V3_hr") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

df_haar4 <- cbind(df[,1:8] , df_numeric_haar)


#######################################
# HAAR : 5
#######################################

library(wavelets)


para_haar <- function(x,prof=5){

        y <- dwt(x, filter="haar", boundary="periodic")
    		coeffs <- c(y@W[[prof]],y@V[[prof]])
        return(coeffs)
}


# HR

df_numeric <- t(df[,9:68])
df_numeric_haar <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(df_numeric_haar) <- c("W1_hr","V1_hr") 
# SPO2

df_numeric <- t(df[,69:128])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_s","V1_s") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpsys

df_numeric <- t(df[,129:188])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_sys","V1_sys") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_dias","V1_dias") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_mean","V1_mean") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_resp","V1_resp") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- t(data.frame(apply(df_numeric,2,para_haar)))
colnames(temp) <- c("W1_r","V1_r") 
df_numeric_haar <- cbind(df_numeric_haar,temp)

df_haar5 <- cbind(df[,1:8] , df_numeric_haar)

#######################################
# FOURIER
#######################################

para_fourier <- function(cs, sample.rate=1) {

		#freq_mini <- NULL
		
		# cs <- t(df[8869,9:68])

		
    cs <- fft(cs) / length(cs) # normalize

     distance.center <- function(c)signif( Mod(c),        4)
     angle           <- function(c)signif( 180*Arg(c)/pi, 3)

     df <- data.frame(cycle    = 0:(length(cs)-1),
                      freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                      strength = sapply(cs, distance.center),
                      delay    = sapply(cs, angle))
                      
     df<-df[-1,]
     df <- df[order(df$strength,decreasing = TRUE),]
     df <- df[1:3,]

     return(df)
}


para_fourier2 <- function(cs) {
  
  #freq_mini <- NULL
  
  # cs <- t(df[8869,9:68])
  
  aaa<-spectrum(cs,plot=F)
  df<-aaa$freq[order(aaa$spec,decreasing=T)]
  
  return(df[1:3])
}


df_numeric <- t(df[,9:68])
df_numeric_fourier <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(df_numeric_fourier) <- c("F1_hr","F2_hr","F3_hr") 

# SPO2

df_numeric <- t(df[,69:128])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("F1_s","F2_s","F3_s") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)

# abpsys

df_numeric <- t(df[,129:188])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("F1_sys","F2_sys","F3_sys") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)

# abpdias

df_numeric <- t(df[,189:248])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("W1_dias","V1_dias") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)

# abpmean

df_numeric <- t(df[,249:308])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("F1_mean","F2_mean","F3_mean") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)

# resp

df_numeric <- t(df[,309:368])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("F1_resp","F2_resp","F3_resp") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)

# ratio

df_numeric <- t(df[,369:428])
temp <- t(data.frame(apply(df_numeric,2,para_fourier)))
colnames(temp) <- c("F1_r","F2_r","F3_r") 
df_numeric_fourier <- cbind(df_numeric_fourier,temp)


df_numeric_fourier[is.na(df_numeric_fourier)] <- 0
df_fourier <- cbind(df[,1:8] , df_numeric_fourier)



#######################################
# SAVE
#######################################

saveRDS(df_resume,"/home/mcherifa/Mimic/data/clean/df_resume.rds")
saveRDS(df_para,"/home/mcherifa/Mimic/data/clean/df_para.rds")
saveRDS(df_haar4,"/home/mcherifa/Mimic/data/clean/df_haar4.rds")
saveRDS(df_haar5,"/home/mcherifa/Mimic/data/clean/df_haar5.rds")
saveRDS(df_fourier,"/home/mcherifa/Mimic/data/clean/df_fourier.rds")


saveRDS(df_arima,"/home/mcherifa/Mimic/data/clean/df_arima.rds")






