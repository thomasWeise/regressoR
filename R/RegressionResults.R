#' @title A List of \code{\link{RegressionResult}}s for a Specific Setup
#'   Identified by a List of Names
#' @description Each instance of this  class represents list of regression
#'   results for a specific setup identified by a list of names.
#' @slot names the vector of name components of this setup
#' @slot results the list of instances of \code{\link{RegressionResult}} for
#'   this setup
#' @importFrom methods setClass representation prototype
#' @exportClass RegressionResults
RegressionResults <- setClass(
  Class = "RegressionResults",
  representation = representation(names="character",
                                  results="list"),
  validity = function(object) {
    # check names
    if(is.null(object@names) || (length(object@names) <= 0L)) {
      return("List of names must be defined");
    }
    for(name in object@names) {
      if(is.null(name) || (nchar(name) <= 0L)) {
        return("Name cannot be NULL or empty.");
      }
    }

    # check results
    if(is.null(object@results) || (length(object@results) <= 0L)) {
      return("List of results must be defined");
    }
    for(result in object@results) {
      if(is.null(result) || (!(is(result, "RegressionResult")))) {
        return("Each result record must be an non-NULL instance of RegressionResult.");
      }
      validObject(result);
    }

    return(TRUE);
  }
)

#' @title Create an Instance of \code{\link{RegressionResults}}
#' @description Instantiate \code{\link{RegressionResults}} using this method.
#' @param names the vector of name components of this setup
#' @param results the list of instances of \code{\link{RegressionResult}} for
#'   this setup
#' @importFrom methods validObject new
#' @export RegressionResults.new
RegressionResults.new <- function(names, results) {
  result <- new("RegressionResults", names=unname(unlist(names, recursive=TRUE)),
                                     results=unname(unlist(results, recursive=TRUE)));
  result <- force(result);
  result@names <- force(result@names);
  result@results <- force(result@results);
  result <- force(result);
  validObject(result);
  return(result);
}
