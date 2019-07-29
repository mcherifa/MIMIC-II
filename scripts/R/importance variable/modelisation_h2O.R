library(h2o)
library(tidyverse)

##### Training 
data <- read.csv2("/home/menyssa/Samuel/data.csv", 
                  header = T, sep = ",",dec = ".", row.names = 1)

# Initialisation
h2o.init(nthreads = -1,
         max_mem_size = "8G") 

# Recodage
data$event <- factor(data$event, levels = c(0,1), labels = c("NoAHE","AHE"))
data$gender <- factor(data$gender, levels = c(0,1), labels = c("F","M"))

# Normaliser
# variables_normales <- setdiff(names(data),
#                               c("id","event","gender", "amine", "sedate","care_unit1","care_unit4","care_unit6"))
# data[,variables_normales] <- mapply(function(x) scale(x),  data[,variables_normales])  

# Data H2O 
data <- as.h2o(data, destination_frame = "data")
tab <- h2o.tabulate(data = train, x = "delais_hospital", y = "event",
                    weights_column = NULL, nbins_x = 10, nbins_y = 10)
plot(tab)

# Split
splits <- h2o.splitFrame(data = data,
                         ratios = c(0.6, 0.2),
                         seed = 1)
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Variables
response <- "event"
predictors <- setdiff(names(data), c(response, c("C1", "id")))
predictors_bis <- setdiff(names(data), c(response, c("C1", "id","age",
                                                     "gender","sapsi_first","sofa_first","care_unit1",
                                                     "care_unit4", "care_unit6", "delais_hospital", "delais_icu")))

predictors_ter <-c("age","gender" ,"sapsi_first","sofa_first","care_unit1",
                   "care_unit4","care_unit6", "delais_hospital", "delais_icu", 
                   "amine","sedate")   

predictors_ter_bis <-c("age","gender" ,"sapsi_first","sofa_first",
                       "care_unit1","care_unit4","care_unit6",
                       "delais_hospital", "delais_icu", "amine","sedate",
                       names(data[,grep("abbm", names(data))]))   

predictors_ter_bis_ter <- setdiff(names(data), c(response, c("C1", "id",names(data[,grep("abpm", names(data))]))))


predictors_ter_bis <-c("age","gender" ,"sapsi_first","sofa_first",
                       "care_unit1","care_unit4","care_unit6",
                       "delais_hospital", "delais_icu", "amine","sedate",
                       names(data[,c(grep("W1_", names(data)),
                                     grep("V1_", names(data)))]))

# Modeles 
m1 <- h2o.deeplearning(
  model_id="dl_model_first",
  training_frame=train,
  validation_frame=valid, ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  #activation="Rectifier", ## default
  #hidden=c(200,200), ## default: 2 hidden layers with 200 neurons each
  epochs=1,
  variable_importances=T ## not enabled by default
)
summary(m1)

m2 <- h2o.glm(x = predictors, 
              y = response, 
              training_frame = train,
              validation_frame = valid,
              model_id = "glm_fit1",
              family = "binomial")  
summary(m2)
# Compute test set performance:
perf1 <- h2o.performance(m1, newdata = test)
perf2 <- h2o.performance(m2, newdata = test)
importance <- h2o.varimp(m1)
importance <- h2o.varimp(m2)

## Super Learning

library(h2oEnsemble)  

learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"

m3 <- h2o.ensemble(x = predictors_ter_bis,
                   y = response, 
                   training_frame = train,
                   validation_frame = valid,
                   family = "binomial", 
                   learner = learner, 
                   metalearner = metalearner,
                   cvControl = list(V = 5, shuffle = TRUE)) 
summary(m3)
perf <- h2o.ensemble_performance(m3, newdata = test)

# Base learner test set AUC (for comparison)
L <- length(learner)
auc <- sapply(seq(L), function(l) perf$base[[l]]@metrics$AUC)
data.frame(learner, auc)

# predictions
pp1 <- h2o.predict(m1,test)
pp2 <- h2o.predict(m2,test)
pp3 <- predict(m3,test)

predictions_glm <- as.data.frame(pp1)[,2]  
predictions_dl <- as.data.frame(pp2)[,2]  
predictions_sl <- as.data.frame(pp3$pred)[,2]  

labels <- as.data.frame(test[,"event"])[,1]
lab <- ifelse(labels =="AHE", 1, 0)

# AUC CI
library(pROC)
# intervalle de confiance 
roc3 <- roc(lab, predictions_glm, ci = T) 
roc2 <- roc(lab, predictions_dl, ci = T) 
roc1 <- roc(lab, predictions_sl, ci = T) 

ggroc(list(call_roc_name_1 = roc1, call_roc_name_2 = roc2,call_roc_name_3 = roc3)) +
  scale_x_reverse()

ggroc(list("SL" = roc1, "Deep Learning" = roc2, "GLM" = roc3),
      legacy.axes = TRUE, 
      ci = TRUE) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic() +
  ggtitle(" ROC curves")





# Plot AUC
algo <- list(m1, m2) %>% 
  map(function(x) x %>% h2o.performance(newdata = test) %>% 
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        .[c('tpr','fpr')] %>% 
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) 

algo[[3]] <-list(m3) %>% 
  map(function(x) x %>% h2o.ensemble_performance(newdata = test) %>% 
        .$ensemble %>% .@metrics %>%.$thresholds_and_metric_scores %>% 
        .[c('tpr','fpr')] %>% 
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% as.data.frame()
algo %>%
  map2(c('GLM','Deep Learning','Superlearner'),
       function(x,y) x %>% add_column(model=y)) %>% 
  reduce(rbind) %>% 
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for tree models')



