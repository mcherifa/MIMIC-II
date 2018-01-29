##' \code{glmer} \code{sl3} Learner.
##'
##' The                \code{glmer}                learner                uses
##' \code{\link[https://cran.r-project.org/package=lme4]{glmer}}          from
##' \code{lme4} to fit a generalized linear mixed-effects model (GLMM).
##' 
##' @docType class
##' @importFrom R6 R6Class
##' @export
##' @keywords data
##'  @return Learner  object with  methods  for training  and prediction.  See
##'   \code{\link{Lrnr_base}} for documentation on learners.
##' @format \code{\link{R6Class}} object.
##' @family Learners
##' 
##'  @section  Parameters:   \describe{  \item{\code{random_effects  =  ""}}{A
##'   \code{character}, the random effects terms of the \code{formula} as in a
##'   regular  call to  \code{glmer}.  Defaults to  \code{""},  for no  random
##'   effect whatsoever. But in that case, why would one use \code{glmer}? See
##'   the documentation of  \code{lmer} for details on the  specification of a
##'   model with  random effects using  \code{formula}s. If the  random effect
##'   terms include  the same 'id'  variable as specified  in 'make_sl3_Task',
##'   then it must  be added to the  training data set under  another name.  }
##'   \item{\code{...}}{Parameters         passed          directly         to
##'   \code{\link[https://cran.r-project.org/package=lme4]{glmer}}.   See  its
##'   documentation for details.  } }
##' 
##' @section Methods:
##' \describe{
##' \item{\code{special_function(arg_1)}}{
##'   In case the  learner would need  a special function...
##'   
##'   \itemize{
##'     \item{\code{arg_1}: A very special argument.
##'    }
##'   }
##'   }
##' }
Lrnr_glmer <- R6Class(classname = "Lrnr_glmer",
                      inherit = Lrnr_base,
                      portable = TRUE,
                      class = TRUE,
                      public = list(

                        ## You  can define  default parameter  values here  if
                        ## possible, your  learner should define  defaults for
                        ## all required parameters.
                        
                        initialize = function(random_effects = "", ...) {
                          
                          ## This  captures all  parameters to  initialize and
                          ## saves them as self$params.
                          
                          with_random_effects <- !identical(random_effects, "")
                          if (!with_random_effects) {
                            warning("Why use 'glmer' without any random effect whatsoever? Switching to 'glm' instead...\n")
                          }

                          params <- args_to_list()
                          params$with_random_effects <- with_random_effects
                          super$initialize(params = params, ...)
                        },
                        
                        ## You  can define  public functions  that allow  your
                        ## learner to  do special things (here,  for instance,
                        ## 'glm'  learner  might  return  prediction  standard
                        ## errors).
                        
                        special_function = function(arg_1){
                          
                        }
                      ),

                      private = list(
                        
                        ## List  properties your  learner  supports here;  use
                        ## sl3_list_properties() for a list of options.
                        
                        .properties = c("binomial", "weights", "offset"),
                        
                        ## List the package required for this learner here.

                        .required_packages = c("lme4"),
                        
                        ## .train  takes task  data and  returns a  fit object
                        ## that can be used to generate predictions.
                        
                        .train = function(task) {
                          
                          ## Generates  an argument  list  from the  parameters
                          ## that   were  captured   when   your  learner   was
                          ## initialized.  This allows  users to pass arguments
                          ## directly to your ml function.
                          
                          args <- self$params
                          
                          ## Get     outcome    variable     type    prefering
                          ## learner$params$outcome_type      first,      then
                          ## task$outcome_type.
                          
                          outcome_type <- self$get_outcome_type(task)
                          
                          ## Should   pass  something   on  to   your  learner
                          ## indicating outcome_type e.g. family or objective.

                          if (is.null(args$family)) {
                            args$family <- outcome_type$glm_family(return_object = TRUE)
                          }
                          family_name <- args$family$family
                          linkinv_fun <- args$family$linkinv
                          
                          ## Add  task data  to the  argument list  what these
                          ## arguments are  called depends on the  learner you
                          ## are wrapping.
                          
                          args$x <- as.matrix(task$X_intercept)
                          nms_x <- colnames(args$x)
                          
                          args$y <- outcome_type$format(task$Y)
                          nms_y <- "the_binary_outcome_of_interest"

                          ## Only add arguments on weights and offset if those
                          ## were specified when the task was generated.

                          if(task$has_node("weights")){
                            args$weights <- task$weights
                          }
                          
                          if(task$has_node("offset")){
                            args$offset <- task$offset
                          }

                          if (args$with_random_effects) {      
                            ## Testing if  the part of the  formula describing
                            ## the  random  effects  includes  the  same  'id'
                            ## variable as  specified in  'make_sl3_Task'.  If
                            ## so, then switches to 'glm'.
                            nms_re <- args$random_effects
                            pattern <- paste(c("[^a-zA-Z0-9\\.\\_]", task$nodes$id, "[^a-zA-Z0-9\\.\\_]"), collapse = "")
                            formula_makes_sense <- length(grep(pattern, nms_re)) == 0
                            if (!formula_makes_sense) {
                              warning(paste("Switching to 'glm' because the random effects component of the formula includes the same 'id' variable as specified in 'make_sl3_Task', namely ",
                                            task$nodes$id, collapse = ""))
                            }
                          }                          
                          if (args$with_random_effects & formula_makes_sense) {
                            the_formula <- sprintf("%s ~ %s",
                                                   nms_y,
                                                   paste(c(nms_x, nms_re), collapse = " + "))
                            args$formula <- as.formula(the_formula)
                            args$data <- as.data.frame(cbind(args$x, the_binary_outcome_of_interest = args$y))
                            args$x <- NULL
                            args$y <- NULL
                            args$random_effects <- NULL
                            with_random_effects <- args$with_random_effects
                            args$with_random_effects <- NULL
                            
                            ## Call a function that fits your algorithm with the
                            ## argument list you constructed.
                            browser()
                            sl3:::SuppressGivenWarnings({
                              fit_object <- sl3:::call_with_args(lme4::glmer, args)
                            }, sl3:::GetWarningsToSuppress())
                            
                            if (FALSE) {
                              ## checking why the object is so large, in order to remove what is unnecessary when time comes...
                              for (nm in slotNames(fit_object)){
                                tos <- eval(parse(text = sprintf("true_obj_size(fit_object@%s)", nm)))
                                cat(sprintf("%s: %i\n", nm, tos))
                              }
                              ## example
                              ## fit_object@frame <- data.frame(fake = NA)
                            }
                          } else {
                            ## taken from 'Lrnr_glm'
                            with_random_effects <- FALSE
                            args$ctrl <- glm.control(trace = FALSE)
                            sl3:::SuppressGivenWarnings({
                              fit_object <- sl3:::call_with_args(stats::glm.fit, args)
                            }, sl3:::GetWarningsToSuppress())
                            
                            fit_object$linear.predictors <- NULL
                            fit_object$weights <- NULL
                            fit_object$prior.weights <- NULL
                            fit_object$y <- NULL
                            fit_object$residuals <- NULL
                            fit_object$fitted.values <- NULL
                            fit_object$effects <- NULL
                            fit_object$qr <- NULL
                            fit_object$linkinv_fun <- linkinv_fun
                          }
                          
                          ## Return the fit object, which will be stored in a
                          ## learner  object and  returned  from  the call  to
                          ## learner$predict

                          attr(fit_object, "with_random_effects") <- with_random_effects
                          
                          return(fit_object)
                        },
                        
                        ## .predict takes a task and returns predictions from that task
                        .predict = function(task = NULL) {
                          verbose <- getOption("sl3.verbose")                          
                          with_random_effects <- attr(private$.fit_object, "with_random_effects")
                          if (!with_random_effects) {
                            ## taken from 'Lrnr_glm'
                            X <- task$X_intercept
                            predictions <- rep.int(NA, nrow(X))
                            if (nrow(X) > 0) {
                              coef <- private$.fit_object$coef
                              if (!all(is.na(coef))) {
                                eta <- as.matrix(X[
                                , which(!is.na(coef)), drop = FALSE,
                                  with = FALSE
                                ]) %*% coef[!is.na(coef)]
                                predictions <- as.vector(private$.fit_object$linkinv_fun(eta))
                              }
                            }
                          } else {
                            self$training_task
                            self$training_outcome_type
                            self$fit_object
                            
                            predictions <- predict(self$fit_object, task$X)
                          }
                          return(predictions)
                        }
                      ))

environment(Lrnr_glmer) <- asNamespace("sl3")


