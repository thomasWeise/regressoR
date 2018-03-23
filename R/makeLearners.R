
# cache all the learners
.allLearners <- regressoR.functional::FunctionalModel.makeLearners()

#' @title Make all the Default Learners for Regression of 2-Dimensional Data
#' @description Create the list of default learners for the use in
#'   \code{\link{regressoR.applyLearners}}.
#' @return a list of regression learners that can be applied.
#' @export regressoR.makeLearners
#' @importFrom regressoR.functional FunctionalModel.makeLearners
regressoR.makeLearners <- function() .allLearners
