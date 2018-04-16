#' @title A Regression Result
#' @description The regression result is a simple record which should include
#'   the output of a regression process stripped from any non-needed additional
#'   information.
#'
#' @slot time the time consumed during the regression process
#' @slot result the regression result, or \code{NULL} if regression has failed
#' @slot metric the metric used during the regression, or \code{NULL} if the
#'   metric is not to be stored
#' @slot name the name of the fitted model
#' @exportClass RegressionResult
#' @importFrom methods setClass representation prototype validObject
#' @importClassesFrom regressoR.base FittedModel
#' @seealso RegressionResult.new
RegressionResult <- setClass(
  Class = "RegressionResult",
  representation = representation(time="numeric",
                                  result="FittedModel",
                                  metric="RegressionQualityMetric",
                                  name="character"),
  prototype = prototype(result=NULL, metric=NULL),
  validity = function(object) {

    # time
    if(is.null(object@time) ||
       (!(is.numeric(object@time))) ||
       (length(object@time) != 1L) ||
       (object@time < 0)) {
      return("Consumed time must be a finite, non-negative vector.");
    }

    # check the objects
    if(!(is.null(object@metric))) {
      validObject(object@metric);
    }
    if(is.null(object@result)) {
      if(!(is.null(object@name))) {
        return("Name must be null if result is NULL.")
      }
    } else{
      validObject(object@result);
      if(is.null(object@name)) {
        return("If result is not NULL, then the name property must be non-empty string.");
      }
    }

    return(TRUE);
  }
)


#' @title Helper Method to Create an Instance of \code{\link{RegressionResult}}
#'
#' @description Always use this function to instantiate
#'   \code{\link{RegressionResult}}.
#'
#' @param time the time consumed during the regression process
#' @param result the regression result, or \code{NULL} if regression has failed
#' @param metric the metric used during the regression, or \code{NULL} if the
#'   metric is not to be stored
#' @return an instance of  \code{\link{RegressionResult}}
#' @importFrom methods new validObject
#' @importFrom regressoR.base FittedModel.new
#' @importClassesFrom regressoR.base FittedModel
#' @export RegressionResult.new
RegressionResult.new <- function(time=0L,
                                 result=NULL,
                                 metric=NULL) {

  result <- new("RegressionResult",
                time=time,
                result=(if(is.null(result)) { NULL }
                        else { if(identical(class(result), "FittedModel"))
                             { result }
                        else { FittedModel.new(f=result@f,
                                               quality=result@quality,
                                               size=result@size) } }),
                metric=metric,
                name=(if(is.null(result)) { NULL }
                      else { as.character(result) }));

  result <- force(result);
  result@time <- force(result@time);
  result@result <- force(result@result);
  result@metric <- force(result@metric);
  result@name <- force(result@name);

  validObject(result);
  return(result);
}
