#' @include batchLearn.R
#' @include learnForExport.R

#' @title Load a Result Stored by \code{\link{regressoR.batchLearn}}
#' @description Open a file \code{file} produced by
#'   \code{\link{regressoR.batchLearn}} and return the stored result. The result
#'   is a list generated according by the definition given in
#'   \code{\link{regressoR.learnForExport}}.
#' @param file the path of the file to read
#' @return the loaded result
#' @export regressoR.loadResult
#' @seealso regressoR.batchLearn
#' @importFrom methods validObject
regressoR.loadResult <- function(file) {
  result <- readRDS(file=file);
  result <- force(result);
  result@time <- force(result@time);
  result@result <- force(result@result);
  result@metric <- force(result@metric);
  result <- force(result);
  validObject(result);
  return(result);
}
