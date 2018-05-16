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

if(!exists("n")) { n <- 3L; total <- n + 1L;}
if(!exists("arrangement")) { arrangement <- plots.arrange(total); }

# we want to put the figues next to each other: the original data/function and
# fitting results at five quality levels
old.par <- par(mfrow=arrangement);

log <- makeLogger(TRUE);
log("First, we plot the original data.");

# plot the original data
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x",
               legend=list(x="bottom", horiz=TRUE));


# create the fitting tasks to be solved
tasks <- unlist(lapply(X=seq_len(n),
                FUN=function(i) {
                  lapply(X=examples, FUN=function(d) { d$q = (i-1L)/(n-1L); d })
                }), recursive=FALSE);

# compute the results
log("Now we apply the fitting procedure in parallel using ",
    getOption("mc.cores", 2L), " cores.");
results <- mclapply(X=tasks, FUN=function(task)
                    regressoR.learnForExport(x=task$x, y=task$y, q=task$q));
log("Done with the fitting, now plotting.");

# Automatically learn models and plot the results
for(i in seq_len(n)) {
  # select the right results
  start <- (i-1L)*length(examples) + 1L;
  end   <- (i * length(examples));
  res   <- results[start:end];

  # plot the regression results, while extending the models slightly beyond the original range
  # to see how they generalize
  batchPlot.RegressionResults(res, plotXY=TRUE, plotXF=TRUE,
                              main=paste("q=", round(res[[1L]]@result@quality, 3),
                                         sep="", collapse=""),
                              # as names, use the fitting quality
                              names=vapply(res,
                                           FUN=function(r) as.character(round(r@result@quality, 3)),
                                           FUN.VALUE=""),
                              x.min.lower=-1,x.min.upper=-1,
                              x.max.lower=4, x.max.upper=4, x.add=TRUE,
                              legend=list(x="right", horiz=FALSE));
}

# restore old settings
invisible(par(old.par))
