set.seed(4577745L)

library(utilizeR)
library(plotteR)
library(regressoR)
library(parallel)

# make an example
make.example <- function(f) {
  n <- 100; # make 100 points
  x <- sort(runif(n=n, min=0, max=3)); # generate x data
  y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
  x <- rnorm(n=n, mean=x, s=0.1); # noisy x
  return(list(x=x, y=y, f=f));
}

# the three base functions
f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
       function(x) 0.1 * exp(3 - x),
       function(x) 1.2 + 0.7*sin(2*x));

# create the three example data sets
examples <- lapply(X=f, FUN=make.example);

# we want to put 6 figues next to each other: the original data/function and
# fitting results at five quality levels
old.par <- par(mfrow=c(3, 3));

log <- makeLogger(TRUE);
log("First, we plot the original data.");

# plot the original data
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               ffun = function(l, x) l$f(x),
               main="Original Data and Function Values for x",
               legend=list(x="bottom", horiz=TRUE));

# Automatically learn models and plot the results
for(q in 0:8/8) {
  qstr <- toString(round(q, 3));
  log("Now fitting the models using fitting quality spec q=", qstr);
  res <- lapply(X=examples,
                  FUN=function(ex)
                    regressoR.learnForExport(x=ex$x, y=ex$y, q=q));

  for(i in seq_along(res)) {
    log("f", i, " modeled as ",
        res[[i]]@name,
        " with quality=",
        res[[i]]@result@quality,
        " after ", res[[i]]@time,
        "s");
  }

  # plot the regression results
  batchPlot.RegressionResults(res, plotXY=TRUE, plotXF=TRUE,
                              main=paste("q=", qstr, sep="", collapse=""),
                              # as names, use the fitting quality
                              names=vapply(res,
                                           FUN=function(r) as.character(round(r@result@quality, 5)),
                                           FUN.VALUE=""),
                              legend=list(x="bottom", horiz=TRUE));
}

# restore old settings
invisible(par(old.par))
