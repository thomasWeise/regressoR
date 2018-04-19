#' @title Create a Combination or Selection of Results
#' @description Merge a set of results according to a given name transformation.
#' @param results the list of instances of \code{\link{RegressionResults}}
#' @param nameTransformer a transformation function to be applied to the
#'   \code{names} field of each result record
#' @return a list of \code{\link{RegressionResults}} where all names have been
#'   transformed according to \code{nameTransformer} and where the
#'   \code{results} members of instances with the same name have been merged (so
#'   that each name only occurs once in the end)
#' @export regressoR.mergeResults
regressoR.mergeResults <- function(results, nameTransformer=identity) {
  # if there are no results, we are done here
  n <- length(results);
  if(length(n) <= 0L) {
    return(list());
  }
  # if nothing else is specified, all is selected
  if(is.null(nameTransformer) || (identical(nameTransformer, identity))) {
    return(results);
  }

  # find unique names and their indexes
  names.all    <- lapply(X=results, FUN=function(result) nameTransformer(result@names));
  names.unique <- unique(names.all);
  m            <- length(names.unique);

  # if the list length is the same, we just update the names
  if(m >= n) {
    return(unname(unlist(lapply(X=1L:n, FUN=function(i) {
      r       <- results[[i]];
      r@names <- names.all[[i]];
      return(r);
    }), recursive = TRUE)));
  }

  # pick the matching results and create and return a new list of results
  result <- lapply(X=names.unique, FUN=function(name) {
    RegressionResults.new(names=name, results=unlist(lapply(
      X=which(vapply(X=names.all, FUN=identical, FUN.VALUE=FALSE, name)),
      FUN=function(i) results[[i]]@results), recursive = TRUE))
  });

  # create result list
  result <- unname(unlist(result, recursive = TRUE));
  result <- force(result);
  return(result);
}
