# An Example for Testing the Behavior of the Standard Models on Some Example Data
#
# The example data stems from an experiment with a m-flip hill climber on a MAX-SAT instance

library(regressoR);

# first we load the data
data   <- read.csv("examples/example_maxsat_uf225-02_mFlipHCrs_01.txt", sep="\t");
data.x <- data[[1]];
data.y <- data[[3]];

cat("Data size: ", length(data.x), "\n", sep="");

# make the default plot
plot(data.x, data.y, log="x");

# translate a function to a string
function.to.string <- function(f) {
  ret <- paste(trimws(deparse(body(f))), collapse="");
  l   <- nchar(ret);
  while( (l > 2L) &&
         identical(substr(ret, 1L, 1L), "{") &&
         identical(substr(ret,  l,  l), "}")) {
    ret <- (trimws(substr(ret, 2L, l-1L)));
    l   <- nchar(ret);
  }
  return(ret);
}

# create a list of learners
learners.orig <- regressoR.makeLearners();
count         <- length(learners.orig);
indexes       <- 1L:count;


# now we assign colors
# we first make a normal list of colors and create darker versions as needed

# source: https://gist.github.com/Jfortin1/72ef064469d1703c6b30
darken <- function(color, factor=1.7){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

# the original color vector
colors.orig  <- c("red", "blue", "green", "orange",  "violet",
                  "cyan", "gold", "gray", "darkviolet");
colors  <- colors.orig;
# if we do not have enough colors, synthesize more
while(length(colors) < count) {
  colors <- unique(unlist(c(colors, sapply(colors.orig, darken))));
}
if(length(colors) > count) { colors <- colors[1:count]; }
# OK, now we have exacty the right number of colors

learners <- lapply(X=learners.orig, FUN=function(learner) {
              learner <- force(learner);
              wrapped <-  function(metric, transformation.x, transformation.y, metric.transformed) {
                time <- system.time(
                   result <- learner(metric, transformation.x, transformation.y, metric.transformed))[3];

                L <- 0L;
                for(learner2 in learners.orig) {
                  L <- 1L + L;
                  if(identical(learner, learner2)) { break; }
                }

                cat("learner ", L, ", n=", length(metric@x), ", tx=", sep="");
                if(is.null(transformation.x)) {
                  cat("null, ty=", sep="");
                } else {
                  cat(function.to.string(transformation.x@forward), ", ty=", sep="");
                }
                if(is.null(transformation.y)) {
                  cat("null, res=", sep="");
                } else {
                  cat(function.to.string(transformation.y@forward), ", res=", sep="");
                }
                if(is.null(result)) {
                  cat("null, time=", sep="");
                } else {
                  cat(result@model@name, "@", result@quality, ", time=", sep="");
                  lines(data.x, result@f(data.x), col=colors[[L]]);
                }
                cat(time, "\n", sep="");
                return(result);
              };
              wrapped <- force(wrapped);
              return(wrapped); });

totalTime <- system.time(
result <- regressoR.learn(x=data.x,
                          y=data.y,
                          learners=learners)
)[3];
cat("Total Time: ", totalTime, "s\n", sep="");


plot(data.x, data.y, log="x");
lines(data.x, result@f(data.x), col="red");
cat("    quality: ",result@quality, "\n", sep="");
cat("      model: ", result@model@name, "\n", sep="");
cat("x-transform: ", function.to.string(result@transform.x), "\n", sep="");
cat("y-transform: ", function.to.string(result@transform.y), "\n", sep="");
