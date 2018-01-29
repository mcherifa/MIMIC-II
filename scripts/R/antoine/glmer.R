#######################################################
#-------- Modelisation  paramétrique 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

# source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

library(lme4)

#######################################
# Lecture
#######################################

df <- read.csv("/home/menyssa/Mimic/scripts/R/antoine/toute_data.csv") %>%   
  # age mal codé et supression des patients avec ahe dans la période d'observation
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(-identif, -eevent, -care_unit)) %>%
  mutate(
    periode = as.numeric(as.character(periode)),
    gender  = factor(gender,levels = c("F","M"), labels = c("1","0")),
    event   = factor(event,levels = c(0,1), labels = c("0","1")),
    amine   = factor(amine,levels = c(0,1), labels = c("0","1")),
    curare  = factor(curare,levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate,levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,levels = c(0,1), labels = c("0","1")))
# 
# df[,14: ncol(df)]<- data.frame(scale(df[,14: ncol(df)],
#                         center = TRUE, scale = TRUE))

#######################################
# Modele a effet aléatoire 
#######################################

# Un effet aléatoire sur le patient uniquement
fit_glmer <- glmer(event ~  event_cum + event_24h + m_hr + m_abpm + (1|id),
                   data = df,
                   family = binomial(link = "logit"))
                                                
summary(fit_glmer)
fitted.results <- predict(fit_glmer, newdata = df, type = "response")
AUC <- roc(df$event, fitted.results,ci = T)
CI.AUC <- ci.auc(AUC)
print(AUC)

