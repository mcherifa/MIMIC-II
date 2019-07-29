## #####################
## superLearning.R
##
## #####################

set.seed(12345)

library(tidyverse)
library(R6)
library(assertthat)
library(uuid)

## ##################
## preparing the data
## ##################

df_modelisation <- 
  readRDS("/home/menyssa/Mimic/data/clean/mimic2/df_modelisation.rds") %>%    
  mutate(
    periode = as.numeric(as.character(periode)),
    id      = as.numeric(as.character(id)),
    gender  = as.numeric(ifelse(gender == "M",1,0))
  )

# Tirage au sort une ligne par id 
df_sample <- df_modelisation %>%
  group_by(id) %>%
  sample_n(size = 1)  %>%
  data.frame()

#  Sous df_sample en fonction méthodes pour résumer
sample_resume 	<- df_sample[,c(1:12,
                               grep("m_",names(df_sample)),
                               grep("v_",names(df_sample)))]

data <- sample_resume  
covars <- subset(data, -event,-id)

## ####################################
## setting up the machine learning task
## ####################################

## -- preparing the cross-validation scheme
## --
## -- (here, 2-fold cross-validation; aim for 5-fold on whole data set)
folds <- make_folds(fold_fun = folds_vfold,
                    V = 5, 
                    cluster_ids = dat.train$id)

task <- make_sl3_Task(data = data,
                      covariates = covars,
                      outcome = "event",
                      outcome_type = "binomial",
                      id = "id",
                      folds = folds)

lrnr.lib <- 

## -- -- fourth, wrapping up the machine learning task
stack <- make_learner(Stack, lrnr.lib)

if (TRUE) {
  ## #############################
  ## working on the whole data set
  ## #############################

  ## -- training
  stack.fit <- stack$train(task)
  
  ## -- predictions made on training data
  stack.preds <- stack.fit$predict()
  head(stack.preds)
}

## ########################
## cross-validated learning
## ########################

## -- training
cv.stack <- Lrnr_cv$new(stack)

cv.fit <- cv.stack$train(task)

## -- predictions made on training data
cv.preds <- cv.fit$predict()

## -- evaluating risks
## --
## -- (should use 'loss_loglik_binomial' but problems with zeros...
## --  introduced 'tweaked_loss_loglik_binomial'...
## --  using 'loss_squared_error' instead)
## risks <- cv.fit$cv_risk(loss_loglik_binomial)
## risks <- cv.fit$cv_risk(tweaked_loss_loglik_binomial)
risks <- cv.fit$cv_risk(loss_squared_error)

## ##############
## super learning
## ##############

## -- specifying the meta-learner and making the task
metalearner <- make_learner(Lrnr_nnls)
sl <- Lrnr_sl$new(learners = stack,
                  metalearner = metalearner)

## -- carrying out the super learning task
sl.fit <- sl$train(task)

## checking the weights
sl.fit$coefficients

## -- predictions made on training data
sl.preds <- sl.fit$predict()
head(sl.preds)

## ########################
## predictions on test data
## ########################

dat.test <- prepare.data(care_unit_level = 4:6,
                         train = FALSE)$dat
task.preds <- make_sl3_Task(data = dat.test, covariates = covars,
                            outcome = "event", outcome_type = "binomial")
sl.fit$predict(task.preds)
