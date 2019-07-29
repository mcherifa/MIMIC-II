#######################################################
#-------- Modelisation  paramétrique 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

library(glmnet)

# library(doMC)
# registerDoMC(cores = 5)

#######################################
# Lecture
#######################################

df <- readRDS(paste0(dest_data,"df_modelisation.rds")) %>%    
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(-identif, -eevent, -care_unit)) %>%
  mutate(
    periode = as.numeric(as.character(periode)),
    id  = factor(id),
    gender  = factor(gender, levels = c("F","M"), labels = c("1","0")),
    event   = factor(event,  levels = c(0,1), labels = c("0","1")),
    amine   = factor(amine,  levels = c(0,1), labels = c("0","1")),
    curare  = factor(curare, levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate, levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,  levels = c(0,1), labels = c("0","1"))
  )

# Tirage au sort une ligne par id 

df_sample <- df %>% 
  group_by(id) %>% 
  sample_n(size = 1)  

#######################################
# 1 - Modele logistique
#######################################

# # Modele sans numerics
# 
# fit_glm_out_num <- glm (event ~  gender + age + sapsi_first +
#                           sofa_first + event_cum + event_24h +
#                           periode + amine + sedate + venti ,
#                         data = df,
#                         family = binomial)
# 
# summary(fit_glm_out_num)
# fitted.results <- predict(fit_glm_out_num, newdata = df, type = "response")
# AUC <- roc(df$event, fitted.results,ci = T)
# CI.AUC <- ci.auc(AUC)
# print(AUC)
# 
# # Modele avec numerics
# 
# fit_glm_wit_num <- glm (event ~ .,
#                         data = subset(df, select = -id),
#                         family = binomial)
# 
# summary(fit_glm_wit_num)
# fitted.results <- predict(fit_glm_wit_num,
#                           newdata = df, type = "response")
# AUC <- roc(df$event, fitted.results,ci = T)
# CI.AUC <- ci.auc(AUC)
# print(AUC)
# 
# # Sélection de modele : AIC
# 
# # fit_back  <- step(fit_glm_wit_num, direction = "backward")
# # fit_back$call

# Modele final

fit_final <- glm(formula = event ~ sapsi_first + event_cum + event_24h + periode + 
                   m_hr + m_abpm + v_abpm + v_abpd + m_abps + v_abps + beta_hr + 
                   beta_abpm + beta_abpd + beta_abps + ar_hr + ma_hr + inter_hr + 
                   inter_abpm + inter_abpd + ar_abps + ma_abps + W1_abpm + V1_abps, 
                 family = binomial, 
                 data = subset(df_sample, select = -id))

summary(fit_final)

fitted.results <- predict(fit_final,
                          newdata = df_sample,
                          type = "response")

accuracy(df_sample$event, 
         fitted.results,
         threshold = c(0.33,0.5,0.66))

#######################################
# 2 - Modele a effet aléatoire 
#######################################

fit_glmer <- glmer(event ~  event_cum + event_24h + m_hr +
                     m_abpm + v_abpm + v_abpd + m_abps + 
                     v_abps + beta_hr + beta_abpm + 
                     beta_abpd + beta_abps + ar_hr + ma_hr + 
                     inter_hr + inter_abpm + inter_abpd + 
                     ar_abps + W1_abpm + V1_abps + (1|id),
                   data = df,
                   family = binomial(link = "logit"))

summary(fit_glmer)
fitted.results <- predict(fit_glmer,
                          newdata = df, type = "response")
AUC.glmer <- roc(df$event, fitted.results,ci = T)

# Warning messages:
# 1: Some predictor variables are on very different scales: consider rescaling 
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                   Model failed to converge with max|grad| = 4.46843 (tol = 0.001, component 1)
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                   Model is nearly unidentifiable: very large eigenvalue
#                                 - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                                 - Rescale variables?

#######################################
# Exemple Elasticnet
#######################################

xfactors <- model.matrix(event ~ gender + amine + curare + sedate + venti + event_24h, 
                         data = df)[, -1]
xnum <- as.matrix(subset(df, select= c(-event, -id, -gender, -periode,
                                       -amine, -curare, -sedate, -venti, -event_24h)))

x <- xnum <- as.matrix(data.frame(xfactors, xnum))
y <- df$event

# # Note alpha=1 for lasso only and can blend with ridge penalty down to
# # alpha=0 ridge only.
# glmmod <- glmnet(x, y, alpha = 0.5, family="binomial")
# coef(glmmod)
# 
# # Plot variable coefficients vs. shrinkage parameter lambda.
# plot(glmmod, xvar="lambda")


#######################################
# Différentes Penalisations
#######################################

x.train <- x
y.train <- y 
fit.lasso <- glmnet(x.train, y.train, family="binomial", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="binomial", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="binomial", alpha=.5)


# Cross validation 
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
                          family="binomial")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
                          family="binomial")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
                          family="binomial")

best.lambda.lasso <- fit.lasso.cv$lambda.min
best.lambda.ridge <- fit.ridge.cv$lambda.min
best.lambda.elnet <- fit.elnet.cv$lambda.min

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# for (i in 0:10) {
#   assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
#                                             alpha=i/10,family="binomial"))
# }

par(mfrow=c(3,2))

# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit.lasso.cv, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit.ridge.cv, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit.elnet.cv, main="Elastic Net")

yhat_lasso <- predict(fit.lasso, s = best.lambda.lasso, newx = x, type = "response")
yhat_ridge <- predict(fit.lasso, s = best.lambda.ridge, newx = x, type = "response")
yhat_elnet <- predict(fit.lasso, s = best.lambda.elnet, newx = x, type = "response")

AUC.lasso <- roc(df$event, yhat_lasso,ci = T)
AUC.ridge <- roc(df$event, yhat_ridge,ci = T)
AUC.elnet <- roc(df$event, yhat_elnet,ci = T)

#######################################
# Resultats
#######################################

# plot des AUC 

plot(AUC.glm)
plot(AUC.glmer, col = 2, add = T)
plot(AUC.lasso, col = 3, add = T)
plot(AUC.ridge, col = 4, add = T)
plot(AUC.elnet, col = 5, add = T)
legend(0.5, 0.9, legend=c("glm", "glmer", "lasso", "ridge", "elasticnet"),
       lty=c(1,1,1,1,1),col = 1:5, title="Models")

#######################################
# Resultats
#######################################

res = data.frame(rbind (
  logistique_n = data.frame(moyenne = mean(logistique_n[[1]]$AUC),
                            ecart.type = 1.96 * sd(logistique_n[[1]]$AUC), 
                            time = mean(logistique_n[[1]]$Time_to_predict)),
  
  
  effet_aleatoire = data.frame(moyenne = mean(effet[[1]]$AUC),
                               ecart.type = 1.96 * sd(effet[[1]]$AUC), 
                               time = mean(effet[[1]]$Time_to_predict))		
  
))



# Remove columns with near zero variance 
#nzv <- nearZeroVar(df)
#df  <- df[,-nzv]


# nID <- length(unique(df$id))
# cut_data = 0.6
# inTrainID <- sample(unique(df$id), round(nID * cut_data), replace = FALSE)
# train <- subset(df, id %in% inTrainID) %>%
#   subset(select = c(-id))
# test  <- subset(df, !(id %in% inTrainID))%>%
#   subset(select = c(-id))

# Modele sans numerics

# Cross-Validation
# sous_df <- subset(df, select = c(id, event ,gender, age, sapsi_first,
#                                  sofa_first, event_cum, event_24h, 
#                                  periode, amine, sedate, venti))
#
# logistique sans numerics
# logistique_sn <- foreach(siz = 200) %dopar% 
# {
#   k_bootstrapping_logistique(sous_df, 0.6, siz)
# }
# 
# # logistique avec numerics
# logistique_n <- foreach(siz = 200) %dopar% 
# {
#   k_bootstrapping_logistique(df, 0.6, siz)
# }
# 


# Cross- Validation
#
# effet <- foreach(siz = 2) %dopar% 
# {
#   k_bootstrapping_effet(df, v = 0.6, siz)
# }
# 
# write.table(effet, file = "~/Mimic/resultats/modelisation/effet.csv", 
#             sep = "\t", row.names = T)








































