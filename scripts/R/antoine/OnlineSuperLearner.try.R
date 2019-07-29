## #####################
## superLearning.R
##
## Antoine, pour Ményssa
## #####################

set.seed(12345)

source("superLearning_prelim.R")

## ##################
## preparing the data
## ##################

DAT.train <- prepare.data(file = "quelques_data.csv",
                          care_unit_level = 4:6,
                          train = TRUE)
dat.train <- DAT.train$dat
covars <- DAT.train$covars
screening.patterns <- DAT.train$screening.patterns
rm(DAT.train)


## AFFREUX!
# dat.train$event <- rbinom(nrow(dat.train), size = 1, prob = 0.5)
## (mais un seul événement sinon)

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
## -- -- (here, "mean", ie, intercept model fitting, and instanciations of "glm", "glmnet", "xgboost", "condensier")
lrnr.lib <- list(make_learner(Lrnr_mean))

for (pat in screening.patterns) {
  ## glm_fast
  lrnr.lib <- append(lrnr.lib, make_learner(Lrnr_glm_fast, covariates = pat))
  if (FALSE) { ## switch to 'TRUE' on whole data set; should work!
    ## xgboost
    for (nr in c(30)) {
      for (alpha in seq(0.3, 0.3, 0.1)) {
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

## -- -- third, combining each "learning" algorithm with one of the "screening" algorithms
length.lrnr.lib <- length(lrnr.lib)
for (scrn in screening.lib) {
  for (ii  in 2:length.lrnr.lib) { ## from 2 because we exclude "Lrnr_mean"
    lrnr.lib <- append(lrnr.lib,
                       make_learner(Pipeline, scrn, lrnr.lib[[ii]]))
  }
}

sprintf("Overall, %i learning algorithms are considered, each a combination of a screening algorithm and a learning algorithm",
        length(lrnr.lib))

## -- -- fourth, wrapping up the machine learning task
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
