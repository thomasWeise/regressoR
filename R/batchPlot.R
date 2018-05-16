#' @include RegressionResult.R

#' @title Plot all the Curves from a Data Set together
#' @description A simple utility method for visualizing the data.
#' @param results an instance or list of \code{\link{RegressionResult}}
#' @inheritDotParams plotteR::batchPlot.list -data -xfun -yfun -ffun
#' @importFrom plotteR batchPlot.list
#' @export batchPlot.RegressionResults
batchPlot.RegressionResults <- function(results, ...) {
  batchPlot.list(data=results,
                 xfun=function(x) x@metric@x,
                 yfun=function(x) x@metric@y,
                 ffun=function(x) x@result@f,
                 ...)
}
