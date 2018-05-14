#' @title Learn a Model for Export
#' @description A version of the \code{\link{regressoR.learn}} function which
#'   prepares the result as instance of \code{\link{RegressionResult}} by
#'   stripping down all but the essential information (and potentially storing
#'   the original quality metric). The returned list is suitable for export into
#'   a file and import via batch processing.
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
#' @return an instance of \code{\link{RegressionResult}}
#' @export regressoR.learnForExport
#' @importFrom regressoR.base regressoR.applyLearners
#' @importFrom dataTransformeR Transformation.applyDefault2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @seealso regressoR.batchLearn
regressoR.learnForExport <- function(x, y,
                              learners = regressoR.makeLearners(),
                              representations=dataTransformeR::Transformation.applyDefault2D(x=x, y=y, addIdentity=TRUE),
                              metricGenerator=regressoR.quality::RegressionQualityMetric.default,
                              q=0.75,
                              includeMetric=TRUE) {

  # execute the learner and measure the time
  result <- NULL;
  time <- max(0L, system.time(
    result <- regressoR.learn(x=x, y=y, learners=learners,
                              representations=representations,
                              metricGenerator=metricGenerator,
                              q=q)
  )[3]);

  # create a new instance of the regression result record
  return(RegressionResult.new(time=time,
                              result=result,
                              metric=(if(includeMetric) {
                                metricGenerator(x, y)
                              } else { NULL })));

  # do a lot of forcing to make sure that everything is evaluated
  result <- force(result);
  result@result <- force(result@result);
  result@metric <- force(result@metric);
  result@time <- force(result@time);
  result@name <- force(result@name);
  result <- force(result);
  return(result);
}
