
# cache all the learners
#' @importFrom regressoR.functional FunctionalModel.makeLearners
.allLearners <- FunctionalModel.makeLearners()

#' @title Make all the Default Learners for Regression of 2-Dimensional Data
#' @description Create the list of default learners for the use in
#'   \code{\link{regressoR.applyLearners}}. Currently, these are the learners
#'   obtained by
#'   \code{\link[regressoR.functional]{FunctionalModel.makeLearners}()}.
#' @return a list of regression learners that can be applied.
#' @export regressoR.makeLearners
regressoR.makeLearners <- function() .allLearners
