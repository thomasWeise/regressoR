# @title Check if a Regression Problem has a Trivial Solution
# @description Find a trivial solution for a regression problem, if any.
# @param xx the \code{x} coordinates, i.e., the input values
# @param yy the \code{y} coordinates, i.e., the output values
# @return an instance of \code{\link{FittedModel}} which represents the
#   relationship between the \code{x} and \code{y} values
#' @importFrom regressoR.functional.models
#'   FunctionalModel.linear.from.two.points FunctionalModel.linear
#'   FunctionalModel.constant
#' @importFrom regressoR.functional FittedFunctionalModel.new
#'   FittedFunctionalModel.finalize
#' @importFrom functionComposeR function.canonicalize
.regressoR.learnTrivial <- function(xx, yy) {
  # find all unique points
  uniPoints <- unique(lapply(X=seq_along(xx), FUN=function(i) c(xx[[i]], yy[[i]])));
  # get the number of such points
  n <- length(uniPoints);
  # if there is no unique point, fail
  stopifnot(n > 0L);

  if(n <= 1L) {
    # if there is only a single point, return a constant function
    a <- uniPoints[[1L]][2L]; a <- force(a);
    if(is.finite(a)) {
      return(FittedFunctionalModel.finalize(
             FittedFunctionalModel.new(model=FunctionalModel.constant(),
                                       par=c(a),
                                       quality=0)));
    }
  }

  # there are more than three unique points (or three points that do not form a
  # quadratic function), so we cannot have a trivial solution
  return(NULL);
}
