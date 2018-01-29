

data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

# use covariates of intest and the outcome to build a task object
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
            "sexn")
task <- sl3_Task$new(cpp, covariates = covars, outcome = "haz")

# set up screeners and learners via built-in functions and pipelines
slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_and_glm <- Pipeline$new(slscreener, glm_learner)
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")

# stack learners into a model (including screeners and pipelines)
learner_stack <- Stack$new(SL.glmnet_learner, glm_learner, screen_and_glm)
stack_fit <- learner_stack$train(task)
#> Loading required package: glmnet
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following object is masked from 'package:tidyr':
#> 
#>     expand
#> Loading required package: foreach
#> 
#> Attaching package: 'foreach'
#> The following objects are masked from 'package:purrr':
#> 
#>     accumulate, when
#> Loaded glmnet 2.0-13
preds <- stack_fit$predict()
head(preds)
#>    Lrnr_pkg_SuperLearner_SL.glmnet   Lrnr_glm
#> 1:                      0.35345519 0.36298498
#> 2:                      0.35345519 0.36298498
#> 3:                      0.24554305 0.25993072
#> 4:                      0.24554305 0.25993072
#> 5:                      0.24554305 0.25993072
#> 6:                      0.02953193 0.05680264
#>    Lrnr_pkg_SuperLearner_screener_screen.glmnet___Lrnr_glm
#> 1:                                              0.36228209
#> 2:                                              0.36228209
#> 3:                                              0.25870995
#> 4:                                              0.25870995
#> 5:                                              0.25870995
#> 6:                                              0.05600958

