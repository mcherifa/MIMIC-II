## ######################
## superLearning_prelim.R
##
## pour Ményssa
## ######################

library(tidyverse)
library(SuperLearner)
library(origami)
library(sl3)

# devtools::install_github("jeremyrcoyle/sl3")

one_hot <- function(df) {
  as_tibble(stats::model.matrix( ~.+0, data = df))
}

is_positive <- function(x) {
  all(is.numeric(x)) && all(x > 0)
}

df <- readRDS("~/Mimic/data/clean/mimic2/df_modelisation.rds") %>%    
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(-identif, -eevent, -care_unit)) %>%
  mutate(
    id = as.numeric(as.character(id)),
    periode = as.numeric(as.character(periode)),
    event   = as.numeric(as.character(event)),
    gender  = factor(gender, levels = c("F","M"), labels = c("1","0")),
    amine   = factor(amine,  levels =  c(0,1), labels = c("0","1")),
    curare  = factor(curare, levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate, levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,  levels =  c(0,1), labels = c("0","1"))
  )

prepare.data <- function(df,
                         care_unit_levels = 4:6, 
                         train = TRUE) {
  dat <- df %>%
    as.tibble()
   #  %>%
   #  select(-identif) %>%
   #  mutate(care_unit = factor(care_unit, levels = care_unit_levels))
  
  ## adding squared numerics
  dat_sq <- dat %>%
    transmute_if(is.numeric, .funs = funs(square=.^2))
  
  ## adding log and sqrt of positive numerics
  dat_log_sqrt <- dat %>% 
    transmute_if(is_positive, .funs = funs(log=log(.), sqrt = sqrt(.)))
  
  ## concatenating, "one-hot" encoding
  dat <- bind_cols(dat, dat_sq, dat_log_sqrt) %>%
    one_hot %>%
    mutate_all(as.numeric)
  
  if (!train) {
    dat %>% select(-event)
  }
  
  ## Output data 
  out <- list()
  out$dat <- dat
  
  ## all covariates:  sans outcome et id patient
  covars <- setdiff(colnames(dat), c("id", "event"))
  out$covars <- covars
  
  ## screening patterns
  exclude.log <- covars[grep("_log", covars, invert = TRUE)]
  exclude.sqrt <- covars[grep("_sqrt", covars, invert = TRUE)]
  exclude.square <- covars[grep("_square", covars, invert = TRUE)]
  exclude.log.sqrt.square <- intersect(intersect(exclude.log, exclude.sqrt),
                                       exclude.square)
  
  screening.patterns <- list(covars,
                             ## exclude.log,
                             ## exclude.sqrt,
                             ## exclude.square,
                             exclude.log.sqrt.square)
  
  exclude.m <- covars[grep("m_", covars, invert = TRUE)]
  exclude.v <- covars[grep("v_", covars, invert = TRUE)]
  exclude.mv <- intersect(exclude.m, exclude.v)
  
  screening.patterns  <-  append(screening.patterns,
                                 list(
                                   ## exclude.m,
                                   ## exclude.v,
                                   exclude.mv))
  
  exclude.inter <- covars[grep("inter_", covars, invert = TRUE)]
  
  screening.patterns  <-  append(screening.patterns,
                                 list(exclude.inter))
  
  exclude.ar <- covars[grep("ar_", covars, invert = TRUE)]
  exclude.ma <- covars[grep("ma_", covars, invert = TRUE)]
  exclude.ar.ma <- intersect(exclude.ar, exclude.ma)
  
  screening.patterns  <-  append(screening.patterns,
                                 list(
                                   ## exclude.ar,
                                   ## exclude.ma,
                                   exclude.ar.ma,
                                   intersect(exclude.log.sqrt.square, exclude.mv),
                                   intersect(exclude.log.sqrt.square, exclude.inter),
                                   Reduce(intersect, list(exclude.log.sqrt.square,
                                                          exclude.mv,
                                                          exclude.ar.ma,
                                                          exclude.inter))))
  
  out$screening.patterns <- screening.patterns
  return(out)  
}

tweaked_loss_loglik_binomial <- function (pred, truth, epsilon = 1e-4) {
  pred <- pmax(epsilon, pmin(1-epsilon, pred))
  -1 * ifelse(truth == 1, log(pred), log(1 - pred))
}

