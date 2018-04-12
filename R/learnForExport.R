#' @title Learn a Model for Export
#' @description A version of the \code{\link{regressoR.learn}} function which
#'   prepares the result as a list by stripping down all but the essential
#'   information (and potentially storing the original quality metric
#'   in the list). The returned list is suitable for export into a file.
#' @param x the \code{x} coordinates, i.e., the input values
#' @param y the \code{y} coordinates, i.e., the output values
#' @param learners the learners to apply
#' @param representations the list of data representations, or \code{NULL} if
#'   fitting should take place only on the raw data
#' @param metricGenerator the metric generator function
#' @param q the effort parameter: 0=minimum effort=fast/low quality, 1=maximum
#'   effort=slow=highest quality
#' @param includeMetric should be the quality function to rate models be stored
#'   in the list as well?
#' @return a list with the fields \code{f}, \code{size}, \code{quality},
#'   \code{name}, \code{time}, and \code{metric}.
#' @export regressoR.learnForExport
#' @importFrom regressoR.base regressoR.applyLearners
#' @importFrom dataTransformeR Transformation.applyDefault2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @seealso regressoR.batchLearn
regressoR.learnForExport <- function(x, y, learners = regressoR.makeLearners(),
                              representations=dataTransformeR::Transformation.applyDefault2D(x=x, y=y, addIdentity=TRUE),
                              metricGenerator=regressoR.quality::RegressionQualityMetric.default,
                              q=0.75,
                              includeMetric=TRUE) {

  result <- NULL;
  time <- max(0L, system.time(
    result <- regressoR.learn(x=x, y=y, learners=learners,
                              representations=representations,
                              metricGenerator=metricGenerator,
                              q=q)
  )[3]);

  if(is.null(result)) { return(list(time=time)); }

  f <- result@f;
  f <- force(f);
  quality <- result@quality;
  quality <- force(quality);
  size <- result@size;
  size <- force(size);
  name <- as.character(result);
  name <- force(name);


  if(includeMetric) {
    metric <- metricGenerator(x, y)
  } else {
    metric <- NULL;
  }
  metric <- force(metric);

  result <- list( f=f,
                  quality=quality,
                  size=size,
                  name=name,
                  time=time,
                  metric=metric);
  result <- force(result);
  return(result);
}
