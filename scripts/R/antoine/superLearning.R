## #####################
## superLearning.R
##
## Antoine, pour Ményssa
## #####################

set.seed(12345)
library(tidyverse)
library(R6)
library(assertthat)
library(uuid)
source("Lrnr_glmer.R")

simpler <- TRUE

if (simpler) {
  cat("Simpler version for testing purposes...\n")
}

source("superLearning_prelim.R")

## ##################
## preparing the data
## ##################

DAT.train <- prepare.data(file = "toute_data.csv",
                          care_unit_level = 4:6)
dat.train <- DAT.train$dat
dat.train <- bind_cols(dat.train, id_for_glmer = dat.train$id)
dat.train <- dat.train %>% mutate(id_for_glmer = as.factor(id_for_glmer))
if (simpler) {
  dat.train <- dat.train[1:500, ]
  dat.train <- dat.train %>%
    mutate(id_for_glmer = as.integer(id_for_glmer)) %>%
    mutate(id_for_glmer = as.factor(id_for_glmer))
}
covars <- c(DAT.train$covars, "id_for_glmer")
screening.patterns <- DAT.train$screening.patterns
rm(DAT.train)



## ####################################
## setting up the machine learning task
## ####################################

## -- preparing the cross-validation scheme
## --
## -- (here, 2-fold cross-validation; aim for 5-fold on whole data set)
folds <- make_folds(fold_fun = folds_vfold, V = 2,
                    cluster_ids = dat.train$id)

task <- make_sl3_Task(data = dat.train, covariates = covars,
                      outcome = "event", outcome_type = "binomial",
                      id = "id", folds = folds)

## -- preparing the library of machine learning algorithms
## --
## -- -- first, the "screening" algorithms

## ## -- "screen.corP" and "screen.glmnet"
screening.lib <- list(Lrnr_pkg_SuperLearner_screener$new("screen.corP"),
                      Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"))
## ## -- (other "screening" algorithms are introduced at next step...)

## -- -- second, the "learning" algorithms per se
## --
## -- -- (here, "mean", ie, intercept model fitting, "glmer", and instanciations of "glm", "glmnet", "xgboost", "condensier")
lrnr.lib <- list(make_learner(Lrnr_mean),
                 make_learner(Lrnr_glmer, random_effects = "(1 | id_for_glmer)"))

for (pat in screening.patterns) {
  ## glm_fast
  lrnr.lib <- append(lrnr.lib, make_learner(Lrnr_glm_fast, covariates = pat))
  if (!simpler) { ## switch to 'TRUE' on whole data set; should work!
    ## xgboost
    for (nr in c(30, 40, 50)) {
      for (alpha in seq(0.3, 0.5, 0.1)) {
        lrnr.lib <- append(lrnr.lib,
                           make_learner(Lrnr_xgboost, nrounds = nr, alpha = alpha,
                                        covariates = pat))
      }
    }
    ## glmnet
    lrnr.lib <- append(lrnr.lib, make_learner(Lrnr_glmnet,
                                              covariates = pat))
    ## condensier
    lrnr.lib <- append(lrnr.lib, make_learner(Lrnr_condensier,
                                              covariates = pat))
  }
}


if (!simpler) {
  ## -- -- third, combining each "learning" algorithm with one of the "screening" algorithms
  length.lrnr.lib <- length(lrnr.lib)
  for (scrn in screening.lib) {
    for (ii  in 3:length.lrnr.lib) { ## from 3 because we exclude "Lrnr_mean" and "Lrnr_glmer"
      lrnr.lib <- append(lrnr.lib,
                         make_learner(Pipeline, scrn, lrnr.lib[[ii]]))
    }
  }
}

sprintf("Overall, %i learning algorithms are considered, each a combination of a screening algorithm and a learning algorithm",
        length(lrnr.lib))

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

stop("Stop here...\n")

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
