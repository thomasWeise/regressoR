#' @include batchLearn.R
#' @include learnForExport.R

#' @title Load a Result Stored by \code{\link{regressoR.batchLearn}}
#' @description Open a file \code{file} produced by
#'   \code{\link{regressoR.batchLearn}} and return the stored result.
#' @param file the path of the file to read
#' @return the loaded result
#' @export regressoR.loadResult
#' @seealso regressoR.batchLearn
#' @seealso regressoR.batchLearn
regressoR.loadResult <- function(file) {
  result <- readRDS(file=file);
  result <- force(result);
  return(result);
}
