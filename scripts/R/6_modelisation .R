## #####################
## superLearning.R
##
## Antoine, pour Ményssa
## #####################

setwd("~/Mimic/scripts/R")

set.seed(12345)

source("superLearning_prelim.R")

## ##################
## Prepare les données
## ##################

DAT.train <- prepare.data()
dat.train <- DAT.train$dat
covars <- DAT.train$covars
screening.patterns <- DAT.train$screening.patterns
rm(DAT.train)

## ####################################
## Définition de la tache de modélisation 
## ####################################

## -- Type de cross validation
folds <- make_folds(fold_fun = folds_vfold, 
                    V = 2,
                    cluster_ids = dat.train$id)

## -- Définition de la tache 
task <- make_sl3_Task(data = dat.train, 
                      covariates = covars,
                      outcome = "event", 
                      outcome_type = "binomial",
                      id = "id", 
                      folds = folds)

## ####################################
## Sélection de variables 
## ####################################

## ## -- "screen.corP" and "screen.glmnet"
# screening.lib <- list(Lrnr_pkg_SuperLearner_screener$new("screen.corP"))
                      # Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"))

## ####################################
## Algorithme d'apprentissage 
## ####################################

# of "glm", "glmnet", "condensier")

lrnr.lib <- list(make_learner(Lrnr_mean))

for (pat in screening.patterns) {
  
  ## glm_fast
  lrnr.lib <- append(lrnr.lib, 
                     make_learner(Lrnr_glm_fast, covariates = pat))
  
  if (FALSE) { ## switch to 'TRUE' on whole data set; should work!
    
    ## glmnet
    lrnr.lib <- append(lrnr.lib, 
                       make_learner(Lrnr_glmnet,
                                    covariates = pat))
    ## condensier
    lrnr.lib <- append(lrnr.lib, 
                       make_learner(Lrnr_condensier,
                                    covariates = pat))
  }
}

## ####################################
## Combinaison de la sélection et des algos d'apprentissage
## ####################################

length.lrnr.lib <- length(lrnr.lib)
for (scrn in screening.lib) {
  for (ii  in 2:length.lrnr.lib) { ## from 2 because we exclude "Lrnr_mean"
    lrnr.lib <- append(lrnr.lib,
                       make_learner(Pipeline, scrn, lrnr.lib[[ii]]))
  }
}

## ####################################
## Nombre d'instanciations différentes 
## ####################################

sprintf("Overall, %i learning algorithms are considered, 
        each a combination of a screening algorithm and a learning algorithm",
        length(lrnr.lib))


## ####################################
## On enmpile le tout 
## ####################################
stack <- make_learner(Stack, lrnr.lib)

if (FALSE) {
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
#cv.stack <- Lrnr_cv$new(stack)

#cv.fit <- cv.stack$train(task)

## -- predictions made on training data
#cv.preds <- cv.fit$predict()

## -- evaluating risks
## --
## -- (should use 'loss_loglik_binomial' but problems with zeros...
## --  introduced 'tweaked_loss_loglik_binomial'...
## --  using 'loss_squared_error' instead)
## risks <- cv.fit$cv_risk(loss_loglik_binomial)
## risks <- cv.fit$cv_risk(tweaked_loss_loglik_binomial)

#risks <- cv.fit$cv_risk(loss_squared_error)

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
table(round(sl.fit$predict(task.preds)))

auc <- roc(dat.train$event, sl.preds)   






