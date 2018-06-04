
# cache the default learners
#' @importFrom regressoR.functional FunctionalModel.defaultLearners
#' @importFrom regressoR.splines regressoR.spline.default
#' @importFrom regressoR.direct regressoR.direct.default
.default <- unique(unlist(c(FunctionalModel.defaultLearners(),
                            regressoR.spline.default(),
                            regressoR.direct.default()),
                            recursive = TRUE));

# cache the monotonous learners
#' @importFrom regressoR.functional FunctionalModel.monotonousLearners
#' @importFrom regressoR.splines regressoR.spline.protected
#' @importFrom regressoR.direct regressoR.direct.default
.monotonous <- unique(unlist(c(FunctionalModel.monotonousLearners(),
                               regressoR.spline.protected(),
                               regressoR.direct.default()),
                               recursive = TRUE));

#' @title Get the Default Learners for Regression of 2-Dimensional Data
#' @description Create the list of default learners for the use in
#'   \code{\link{regressoR.applyLearners}}. These comprise some functional
#'   models, some simple multi-layer perceptrons, and splines.
#' @return a list of regression learners that can be applied.
#' @export regressoR.defaultLearners
regressoR.defaultLearners <- function() .default


#' @title Get the Default Learners for Monotonous Regression of 2-Dimensional
#'   Data
#' @description Create the list of default learners for monotonous models for
#'   the use in \code{\link{regressoR.applyLearners}}. These comprise some
#'   monotonous functional models, some simple restricted multi-layer
#'   perceptrons, and bounded splines.
#' @return a list of regression learners that can be applied.
#' @export regressoR.monotonousLearners
regressoR.monotonousLearners <- function() .monotonous
