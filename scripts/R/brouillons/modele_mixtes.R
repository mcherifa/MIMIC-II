########################
#  Modelisation avec le superlearner 
########################

set.seed(1)

library(SuperLearner)
library(glmnet)
library(ggplot2)
library(nnet)
library(rpart)
library(randomForest)
library(ROCR)
library(gbm) 
library(polspline)
library(gam)
library(arm)
library(ipred)
library(xgboost)
library(pROC)
library(verification)
library(caret)
library(plyr)
library(dplyr)
library(ck37r)
library(glmmML)

require(nlme)

#  Lecture 
df_modelisation <- readRDS("/home/menyssa/Mimic/data/clean/mimic2/df_modelisation.rds") %>%
  subset(select = -c(event_cum, periode, event_24h)) %>%
  mutate(
    id      = as.numeric(as.character(id)),
    gender  = as.numeric(ifelse(gender == "M",1,0))
  )
length(unique(df_modelisation$id))

# On garde que les patients qui ont plus de 2 périodes 
# mat <- data.frame(cbind( id = unique(df_modelisation$id),
#                          nombre = table(df_modelisation$id)))
# mat_sup2 <- subset(mat, nombre > 2)
# df_modelisation <- subset(df_modelisation, id %in% mat_sup2$id)
# length(unique(df_modelisation$id))

#  Sous df_modelisation en fonction méthodes pour résumer
sample_resume 	<- df_modelisation[,c(1:10,
                                     grep("m_",names(df_modelisation)),
                                     grep("v_",names(df_modelisation)))]

sample_lineaire <- df_modelisation[,c(1:10,
                                      grep("alpha_",names(df_modelisation)),
                                      grep("beta_",names(df_modelisation)))]

sample_arma 		<- df_modelisation[,c(1:10,
                                    grep("ar_",names(df_modelisation)),
                                    grep("ma_",names(df_modelisation)),
                                    grep("inter_",names(df_modelisation)))]

sample_haar 		<- df_modelisation[,c(1:10,
                                    grep("W1_",names(df_modelisation)),
                                    grep("V1_",names(df_modelisation)))]


#  Test
df <- sample_lineaire
Y = df$event
X = subset(df, select = -c(event))
lol = paste('Y~',paste0(names(X), collapse='+'))
newX = subset(X,select = -id)

#  Meme sans les patients avec moins de 2 périodes, fit pas
# fit.glmer <- glmer(as.formula(paste(paste('Y~',paste0(names(subset(X,select = -id)),collapse='+')),'+(1|id)')),
#                    data=cbind(Y,X),
#                    glmerControl(optimizer="Nelder_Mead", optCtrl = list(maxfun = 10000)),
#                    family='binomial')


# fit.glm <- glm(as.formula(paste(paste('Y~',paste0(names(subset(X,select = -id)),collapse='+')))),
#                data = cbind(Y,X),family='binomial')

fit.gls <- gls(as.formula(paste(paste('Y~',paste0(names(subset(X,select = -id)),collapse='+')))),
              data = cbind(Y,X),  corCompSymm(0.5, form = ~ 1 | id))
prediction.gls <- data.frame(pred = predict(fit.gls, newX))


fit.glmmPQL <- glmmPQL(as.formula(paste('Y~',paste0(names(subset(X,select = -id)),collapse='+'))), 
                       random = ~ 1 | id,
                       data = cbind(Y,X), 
                       niter = 15 ,
                       family ='binomial')
prediction.PQL <- data.frame(pred=predict(fit.glmmPQL, newdata = newX, type = c("response"),level = 0))


fit.glmmML <- glmmML(as.formula(paste('Y~',paste0(names(subset(X,select = -id)),collapse='+'))),
                     family = binomial,
                     data = cbind(Y,X),
                     cluster = id)

new <- model.matrix(~ ., data = newX)[,]
predict.glmmML(fit.glmmML,new)
p2 <- fit.glmmML$coefficients %*% t(new)
prediction.glmmML <- data.frame(pred = t(plogis(p2)))

predict.glmmML(fit.glmmML,newX)

# # standardized residuals versus fitted values by id
# plot(fit.gls, resid(., type = "p") ~ fitted(.) | id, abline = 0)
# # box-plots of residuals by id
# plot(fit.gls, id ~ resid(.))
# # observed versus fitted values by id
# plot(fit.gls, Y ~ fitted(.) | id, abline = c(0,1))
