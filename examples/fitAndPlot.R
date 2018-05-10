set.seed(20140208L)

library(plotteR)
library(regressoR)

# make an example
make.example <- function(f) {
  x <- sort(runif(n=21)); # generate x data
  y <- rnorm(n=length(x),
             mean=f(x),
             s=0.075);  # noisy y
  return(list(x=x, y=y, f=f));
}

# the three base functions
f1 <- function(x) 1 - 0.2*x + x*x
f2 <- function(x) 0.1 * exp(3 - x)
f3 <- function(x) 1.2 + 0.7*sin(5*x)

# create the three example data sets
f1.ex <- make.example(f1);
f2.ex <- make.example(f2);
f3.ex <- make.example(f3);

# we want to put 6 figues next to each other: the original data/function and
# fitting results at five quality levels
old.par <- par(mfrow=c(3, 2));

# plot the original data
batchPlot.list(list(f1.ex, f2.ex, f3.ex),
               xfun=function(l) l$x,
               yfun=function(l) l$y,
               ffun=function(l,x) l$f(x),
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x")

# Automatically learn models and plot the results
for(q in 0:4/4) {
  cat("Now learning f1 using quality q=", q, "\n", sep="", collapse="");
  r1 <- regressoR.learnForExport(x=f1.ex$x, y=f1.ex$y, q=q);
  cat("Now learning f2 using quality q=", q, "\n", sep="", collapse="");
  r2 <- regressoR.learnForExport(x=f2.ex$x, y=f2.ex$y, q=q);
  cat("Now learning f3 using quality q=", q, "\n", sep="", collapse="");
  r3 <- regressoR.learnForExport(x=f3.ex$x, y=f3.ex$y, q=q);
  cat("Done learning, now plotting.\n");
  X <- list(r1, r2, r3);

  # plot the regression results
  batchPlot.RegressionResults(X, plotXY=TRUE, plotXF=TRUE,
                              main=paste("q=", q, sep="", collapse=""),
                              # as names, use the fitting quality
                              names=vapply(X,
                                           FUN=function(r) round(r@result@quality),
                                           FUN.VALUE=""));
}

# restore old settings
invisible(par(old.par))
