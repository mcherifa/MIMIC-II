########################
# Libraries pour le superlearner 
########################

# SL.fit.gls
write.SL.template(file = 'My.SL.gls')
My.SL.gls <- function (Y, X, newX, family, obsWeights, id){
  require(nlme)
  
  if (family$family == "binomial") {
    fit.gls <- suppressWarnings(gls(as.formula(paste('Y~',paste0(names(subset(X, select = -id)), 
                                                                 collapse='+'))), 
                                    data = cbind(Y,X), 
                                  	corCompSymm(form = ~ 1 | id)))
  }
  id <- id
  newX = subset(newX, select = -id)
  pred <- predict(fit.gls, newdata = newX)
  fit <- list(object = fit.gls)
  class(fit) <- c("SL.fit.gls")
  out <- list(pred = pred, fit = fit)
  return(out)
}

# SL.fit.glmmPQL
write.SL.template(file = 'My.SL.glmmPQL')
My.SL.glmmPQL <- function (Y, X, newX, family, obsWeights,id)
{
  require(nlme); require(MASS)
  
  if (family$family == "binomial") {
    fit.glmmPQL <- suppressWarnings(glmmPQL(
      as.formula(paste('Y~', paste0(names(subset(X, select = -id)), collapse='+'))), 
      random = ~ 1 | id,
      data=cbind(Y,X), niter = 10 ,family='binomial'))
  }
  id = id
  newX = subset(newX, select = -id)
  # pred <- predict(fit.glmmPQL, newdata = newX, type = c("response"),level = 0)
  pred <- predict(fit.glmmPQL, newdata = newX)
  fit <- list(object=fit.glmmPQL)
  class(fit) <- c("SL.fit.glmmPQL")
  out <- list(pred = pred, fit = fit)
  return(out)
}

# SL.fit.glmmML
write.SL.template(file = 'My.SL.glmmML')
My.SL.glmmML <- function (Y, X, newX, family, obsWeights,id) {
  require(glmmML)
  if (family$family == "binomial") {
    fit.glmmML <- suppressWarnings(glmmML(as.formula(paste('Y~',
                                                           paste0(names(subset(X,select = -id)), collapse='+'))),
                                          family = binomial,
                                          boot = 10,
                                          data = cbind(Y,X),
                                          cluster = id))
  }
  id = id
  newX = subset(newX, select = -id)
  pred <- predict.glmmML(fit.glmmML, newdata = newX)
  fit  <- list(object=fit.glmmML)
  class(fit) <- c("SL.fit.glmmML")
  out <- list(pred = pred, fit = fit)
  return(out)
}

predict.glmmML <- function(object, newdata){
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.gls <- function(object, newdata){
  # object = fit.gls
  # newdata = subset(X, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  # p2      <-  crossprod(x = fit.gls$coefficients ,y = t(newdata))
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.glmmPQL <- function(object, newdata){
  # object = fit.glmmPQL
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients$fixed %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

######### --------------------- 

predict.SL.fit.glmmML <- function(object, newdata){
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.SL.fit.gls <- function(object, newdata){
  # object = fit.gls
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}

predict.SL.fit.glmmPQL <- function(object, newdata){
  # object = fit.glmmPQL
  # newdata = subset(X_holdout, select = -id)
  newdata <- model.matrix(~ ., data = newdata)[,]
  p2      <- object$coefficients$fixed %*% t(newdata)
  val     <- as.vector(plogis(p2))
  lab     <- "Predicted values"
  structure(val, label = lab)
}


########################
# Libraries pour le superlearner 
# effets aléatoires
########################

librairie_mixed <- c(
  "My.SL.gls",
  "My.SL.glmmPQL",
  "My.SL.glmmML")

########################
# Libraries pour le superlearner 
# sans effets aléatoires
########################

# Reseau de neurone
SL.nnet <- function(Y, X, newX, family, obsWeights, size = 6, ...){
  if(family$family=="binomial") {
    fit.nnet <- suppressWarnings(nnet::nnet(x = X, y = Y,size = size, 
                                            decay = 1,trace = FALSE, maxit = 500,
                                            linout = FALSE, weights = obsWeights))
  }
  pred <- predict(fit.nnet, newdata = newX, type = "raw")
  fit <- list(object = fit.nnet)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.nnet")
  return(out)
}

SL.bayesglm <- function(Y, X, newX, family, obsWeights, ...){
  require('arm')
  fit.glm <- arm::bayesglm(Y ~ ., data = X, family = family, weights = obsWeights)
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.bayesglm")
  return(out)
}

predict.SL.bayesglm <- function(object, newdata, ...){
  .SL.require('arm')
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  return(pred)
}

SL.gbm <- function(Y, X, newX, family, obsWeights, gbm.trees = 1000, interaction.depth = 2, shrinkage = 0.001, ...) {
  require('gbm')
  gbm.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
  if(family$family == "gaussian") {  
    fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "gaussian", 
                        n.trees = gbm.trees, interaction.depth = interaction.depth, shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, weights = obsWeights, verbose = FALSE)
  }
  if(family$family == "binomial") {
    fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "bernoulli",
                        n.trees = gbm.trees, interaction.depth = interaction.depth, shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, weights = obsWeights, verbose = FALSE)
  }
  best.iter <- gbm::gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
  pred <- predict(fit.gbm, newdata = newX, best.iter, type = "response")
  fit <- list(object = fit.gbm, n.trees = best.iter)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gbm")
  return(out)
}
predict.SL.gbm <- function(object, newdata,...){
  require('gbm')
  pred <- predict(object$object, newdata = newdata, n.trees = object$n.trees, type = "response")
  return(pred)
}


librairie <- c(
  "SL.nnet",
  "SL.glm",
  "SL.glmnet",
  "SL.rpart",
  "SL.gbm",
  "SL.bayesglm" ,
  "SL.randomForest"
)

print(" #################################")
print(" Chargement des librairies : DONE")
print(" #################################")
