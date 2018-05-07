#' @include RegressionResult.R

#' @title Plot all the Curves from a Data Set together
#' @description A simple utility method for visualizing the data.
#' @param results an instance or list of \code{\link{RegressionResult}}
#' @param log the logarithmic scaling info for the axes
#' @param plotPoints should the points be plotted?
#' @param plotFun should the fitted function models be plotted?
#' @importFrom plotteR batchPlot.list
#' @export batchPlot.RegressionResults
batchPlot.RegressionResults <- function(results, log="", plotPoints=TRUE, plotFun=TRUE) {
  batchPlot.list(data=results,
                 xfun=function(result) result@metric@x,
                 yfun=function(result) result@metric@y,
                 ffun=function(result, x) result@result@f(x),
                 plotXY=plotPoints,
                 plotXF=plotFun)
}
