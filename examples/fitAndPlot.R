set.seed(4577745L);

library(utilizeR);
library(plotteR);
library(regressoR);
library(parallel);
library(regressoR.functional);
library(regressoR.functional.models);

if(!exists("log")) { log <- makeLogger(TRUE); }
log("Welcome to the regression example.");
log("We will create three example data sets and then fit them using different fitting powers.");
log("We will utilize parallel computing if possible.");

# make an example
make.example <- function(f) {
  log("Now creating example for function ", function.toString(f), ".");
  n <- 225; # make 225 points
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
# get the minimum and maximal actual x coordinates
min.x    <- min(vapply(X=examples, FUN=function(z) min(z$x), FUN.VALUE=-1));
max.x    <- max(vapply(X=examples, FUN=function(z) max(z$x), FUN.VALUE=4));
log("The minimum actual x value is ", min.x, " and the maximum value is ", max.x, ".");

range.x <- max.x - min.x;
start.x <- floor(10*(min.x - 0.1*range.x))/10;
end.x   <- ceiling(10*(max.x + 0.1*range.x))/10;
log("We will draw the diagrams from x=", start.x,
    " to x=", end.x,
    " to test the generalization ability of the regression results.");

if(!exists("n")) { n <- 3L; total <- n + 1L;}
if(!exists("arrangement")) {
  arrangement <- plots.arrange(total);
}

# we want to put the figues next to each other: the original data/function and
# fitting results at five quality levels
log("Setting a grid of ", arrangement[1],
    " rows and ", arrangement[2],
    " columns for the ", total, " diagrams.")
old.par <- par(mfrow=arrangement);

log("First, we plot the original data.");

# plot the original data
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x",
               legend=list(x="bottom", horiz=TRUE),
               x.min.lower=start.x,x.min.upper=start.x,
               x.max.lower=end.x, x.max.upper=end.x, x.add=TRUE);
abline(v=min.x, col="gray");
abline(v=max.x, col="gray");

# create the fitting tasks to be solved
tasks <- unlist(lapply(X=seq_len(n),
                FUN=function(i) {
                  lapply(X=examples, FUN=function(d) {
                    d$q = round((i-1L)/(n-1L), 2); # add power
                    d # return example function + fitting power
                    })
                }), recursive=FALSE);
log("We defined ", length(tasks), " tasks to be solved in parallel.");

# get the number of cores
cores <- getOption("mc.cores");
if(is.null(cores)) {
  cores <- parallel::detectCores();
  options(mc.cores=cores);
  log("Detected ", cores, " cores for parallel computation.");
} else {
  log("Number of cores for parallel computation provided as ", cores);
}

# compute the results
log("Now we apply the fitting procedure in parallel using ",
    getOption("mc.cores", 2L), " cores.");

# learn in parallel
results <- mclapply(X=tasks, FUN=function(task)
                    regressoR.learnForExport(
                    x=task$x, y=task$y, q=task$q));
log("Done with the fitting, now plotting.");

x.dummy <- start.x + ((end.x - start.x) * ((0:100)/100));

# Automatically learn models and plot the results
for(i in seq_len(n)) {
  # select the right results
  start <- (i-1L)*length(examples) + 1L;
  end   <- (i * length(examples));
  res   <- results[start:end];

  # plot the regression results, while extending the models slightly beyond the original range
  # to see how they generalize
  batchPlot.RegressionResults(res, plotXY=TRUE, plotXF=TRUE,
    main=paste("q=", round(tasks[[start]]$q, 3), sep="", collapse=""),
    # as names, use the fitting quality
    names=vapply(res, FUN=function(r)
                 paste(r@name, ":", round(r@result@quality, 3),
                       sep="", collapse=""),
                 FUN.VALUE=""),
    x.min.lower=start.x, x.min.upper=start.x,
    x.max.lower=end.x, x.max.upper=end.x, x.add=TRUE,
    legend=list(x="bottomleft", horiz=FALSE, ncol=1L,
                cex=0.75));
  abline(v=min.x, col="gray");
  abline(v=max.x, col="gray");
  lines(x=x.dummy, y=f[[1]](x.dummy), col="gray");
  lines(x=x.dummy, y=f[[2]](x.dummy), col="gray");
  lines(x=x.dummy, y=f[[3]](x.dummy), col="gray");
}

# restore old settings
invisible(par(old.par));

log("All done.");
