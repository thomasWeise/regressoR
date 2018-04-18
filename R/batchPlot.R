#' @include RegressionResult.R

#' @title Plot all the Curves from a Data Set together
#' @description A simple utility method for visualizing the data.
#' @param results an instance or list of \code{\link{RegressionResult}}
#' @param log the logarithmic scaling info for the axes
#' @importFrom grDevices rgb
#' @importFrom stats runif
#' @export regressoR.batchPlot
regressoR.batchPlot <- function(results, log="") {
  x.min <- +Inf;
  y.min <- +Inf;
  x.max <- -Inf;
  y.max <- -Inf;

  for(result in results) {
    x.min <- min(x.min, min(result@metric@x));
    x.max <- max(x.max, max(result@metric@x));
    y.min <- min(y.min, min(result@metric@y));
    y.max <- max(y.max, max(result@metric@y));
  }

  plot(x=c(x.min, x.max), y=c(y.min, y.max), log=log, type="n", xlab="", ylab="");
  for(result in results) {
    col <- rgb(runif(1),runif(1), runif(1));
    points(x=result@metric@x, y=result@metric@y, col=col, lwd=0.5);
    srt <- sort(result@metric@x);
    lines(x=srt, y=result@result@f(srt), col=col, lwd=1.5);
  }
}
