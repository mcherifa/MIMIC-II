##############################
# Modelisation
##############################

library(h2o)
library(tidyverse)

##### Training 
load("data_train.RData")

##### Test
load("data_test.RData")


# Initialisation
h2o.init(nthreads = -1,
         max_mem_size = "8G") 

# Recodage
data_train$outcome <- factor(ifelse(data_train$outcome == "H1" | 
                                      data_train$outcome == "H2",1,0), 
                             levels = c(0,1),
                             labels = c("NoAHE","AHE"))

data_test$outcome <- factor(ifelse(data_test$outcome == "H",1,0),
                            levels = c(0,1),
                            labels = c("NoAHE","AHE"))

# Data_train en h2o.frame

test <- as.h2o(data_test, destination_frame = "test")

# Définition des covariables
y <- "outcome"
x <- setdiff(names(training), c(y, c("id", "sex","age")))

# Modeles
glm_fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = training,
                    model_id = "glm_fit1",
                    family = "binomial")  

dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = training,
                            stopping_metric = "AUC",
                            model_id = "dl_fit1",
                            seed = 1)

nb_fit1 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = training,
                          model_id = "nb_fit1")

# Performance en train
glm_perf1 <- h2o.performance(model = glm_fit1, newdata = training)
h2o.auc(glm_perf1)
pred_train <- cbind(as.data.frame(h2o.predict(glm_fit1, training)), data_train$outcome)
table(pred_train$`data_train$outcome`, pred_train$predict)

dl_perf1 <- h2o.performance(model = dl_fit1, newdata = training)
h2o.auc(dl_perf1)
pred_train <- cbind(as.data.frame(h2o.predict(dl_fit1, training)), data_train$outcome)
table(pred_train$predict, pred_train$`data_train$outcome`)


# Performance en test
glm_perf2 <- h2o.performance(model = glm_fit1, newdata = test[,-1])
h2o.auc(glm_perf2)
h2o.confusionMatrix(glm_perf2)

dl_perf2 <- h2o.performance(model = dl_fit1, newdata = test[,-1])
h2o.auc(dl_perf2)
h2o.confusionMatrix(dl_perf2)

#### AUC 

list(glm_fit1, nb_fit1, dl_fit1) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(newdata = test) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('Logistic Regression','Naive Bayes','Deeplearning '),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+ 
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for tree models')

# fermeture 
h2o.shutdown()


