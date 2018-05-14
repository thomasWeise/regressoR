#' @title Check if a Regression Problem has a Trivial Solution
#' @description Find a trivial solution for a regression problem, if any.
#' @param x the \code{x} coordinates, i.e., the input values
#' @param y the \code{y} coordinates, i.e., the output values
#' @return an instance of \code{\link{FittedModel}} which represents the
#'   relationship between the \code{x} and \code{y} values
#' @export regressoR.learn
#' @importFrom regressoR.functional.models FunctionalModel.linear.from.two.points FunctionalModel.quadratic.from.three.points
#' @importFrom regressoR.base FittedModel.new
.regressoR.learnTrivial <- function(x, y) {
  # find all unique points
  uniPoints <- unique(lapply(X=seq_along(x), FUN=function(i) c(x[[i]], y[[i]])));
  # get the number of such points
  n <- length(uniPoints);
  # if there is no unique point, fail
  stopifnot(n > 0L);

  if(n <= 1L) {
    # if there is only a single point, return a constant function
    y <- uniPoints[[1L]][2L];
    return(regressoR.base::FittedModel.new(f=function(x) y, q=0, size=1L));
  }

  if(n <= 2L) {
    # if there are two points, we can connect them as linear function
    v <- FunctionalModel.linear.from.two.points(uniPoints[[1L]][1L],
                                                uniPoints[[1L]][2L],
                                                uniPoints[[2L]][1L],
                                                uniPoints[[2L]][2L]);
    if(!(is.null(v))) {
      a <- v[1];
      b <- v[2];
      return(FittedModel.new(f=function(x) a+b*x, q=0, size=2L));
    }
  } else {
    # if there are three points, we can try to connect them as quadratic function
    if(n <= 3L) {
      v <- FunctionalModel.quadratic.from.three.points(uniPoints[[1L]][1L],
                                                       uniPoints[[1L]][2L],
                                                       uniPoints[[2L]][1L],
                                                       uniPoints[[2L]][2L],
                                                       uniPoints[[3L]][1L],
                                                       uniPoints[[3L]][2L]);
      if(!(is.null(v))) {
        a <- v[1];
        b <- v[2];
        c <- v[3];
        return(FittedModel.new(f=function(x) a+b*x+c*x*x, q=0, size=3L));
      }
    }
  }

  # there are more than three unique points (or three points that do not form a
  # quadratic function), so we cannot have a trivial solution
  return(NULL);
}
