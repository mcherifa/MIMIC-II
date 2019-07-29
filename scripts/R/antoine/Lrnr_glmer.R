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
##' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
##' @format \code{\link{R6Class}} object.
##' @family Learners
##' 
##' @section Parameters:
##' \describe{
##'   \item{\code{random_effects = ""}}{A \code{character}, the random effects terms of the \code{formula} as in a regular call to \code{glmer}. Defaults to \code{""}, for no random effect whatsoever. But in that case, why would one use \code{glmer}?
##'   }
##'   \item{\code{...}}{Parameters passed directly to \code{\link[https://cran.r-project.org/package=lme4]{glmer}}. See its documentation for details. 
##'   }
##' }
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

                          if (identical(random_effects, "")) {
                            warning("Why use 'glmer' without any random effect whatsoever?\n")
                          }
                          params <- args_to_list()
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
                          nms_y <- names(args$y)

                          nms_re <- args$random_effects
                          if (!identical(nms_re, "")) {
                            the_formula <- sprintf("%s ~ %s + %s",
                                                   nms_y, nms_x, nms_re)
                          } else {
                            the_formula <- sprintf("%s ~ %s",
                                                   nms_y, nms_x)
                          }
                          args$formula <- as.formula(the_formula)
                          args$data <- as.data.frame(cbind(args$x, args$y))
                          args$x <- NULL
                          args$y <- NULL
                                                    
                          ## Only add arguments on weights and offset if those
                          ## were specified when the task was generated.

                          if(task$has_node("weights")){
                            args$weights <- task$weights
                          }
                          
                          if(task$has_node("offset")){
                            args$offset <- task$offset
                          }
                          
                          ## Call a function that fits your algorithm with the
                          ## argument list you constructed.

                          SuppressGivenWarnings({
                            fit_object <- call_with_args(lme4::glmer, args)
                          }, GetWarningsToSuppress())

                          if (FALSE) {
                            ## checking why the object is so large, in order to remove what is unnecessary when time comes...
                            for (nm in slotNames(fit_object)){
                              tos <- eval(parse(text = sprintf("true_obj_size(fit_object@%s)", nm)))
                              cat(sprintf("%s: %i\n", nm, tos))
                            }
                            ## example
                            ## fit_object@frame <- data.frame(fake = NA)
                          }
                                                
                          ## Return the fit object, which will be stored in a
                          ## learner  object and  returned  from  the call  to
                          ## learner$predict
                          
                          return(fit_object)
                        },
                        
                        ## .predict takes a task and returns predictions from that task
                        .predict = function(task = NULL) {
                          self$training_task
                          self$training_outcome_type
                          self$fit_object
                          
                          predictions <- predict(self$fit_object, task$X)

                          return(predictions)
                        }
                      ))

