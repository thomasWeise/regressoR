#' @include learnForExport.R

#' @title Apply the Regression Learning to a Directory of Data and Store the
#'   Fitted Models in another Directory
#' @description Recursively, a \code{source} directory is traversed and all
#'   files matching to a \code{selector} regular expression are picked up,
#'   loaded with a \code{loader}, and then modelled by the regressor. The
#'   resulting models are stored in a \code{destination} folder in a structure
#'   mirroring the source folder.
#'
#'   This method uses \code{\link{regressoR.learnForExport}} to learn the models
#'   and stores them into files using \code{\link{saveRDS}} to store them. They
#'   can later be read using \code{\link{regressoR.loadResult}}.
#'
#' @param source the source directory, which is recursively searched for files
#'   with data to be modeled
#' @param destination the destination folder, will be created if not existing
#' @param loader a loader function which accepts a vector of paths and is
#'   supposed to return an \code{n*2} matrix where the first column contains the
#'   \code{x} values and the second column the \code{y} values to model
#' @param selector a regular expression against which file names are matched.
#'   Only matching files are considered.
#' @param check.directory a function receiving a \code{root} folder and the
#'   \code{path} under \code{root} and decides whether this directory path
#'   should be recursively investigated (be default always \code{TRUE})
#' @param learn.single should every single file matching to the \code{selector}
#'   be modeled separately ?
#' @param learn.all should all the files in one directory combined and modeled
#'   at once?
#' @param learners the model learners to be applied
#' @param representations a function which can transform a \code{x}/\code{y}
#'   dataset into a set of transformed data instances
#' @param metricGenerator the learning quality metric generator
#' @param suffix.single the suffix to append to the files containing the single
#'   models
#' @param suffix.all the suffix to be applied to the files containing the models
#'   of all files in a folder
#' @param q the modelling quality parameter
#' @param includeMetric should the metric used for learning be stored in the
#'   files
#' @param cores the number of cores to use (\code{>1L} leads to parallel
#'   execution)
#' @param logging should progress information be printed
#' @export regressoR.batchLearn
#' @importFrom utilizeR path.batchProcessor path.batchApply path.extensionRegExp
#' @importFrom dataTransformeR Transformation.applyDefault2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @importFrom data.table rbindlist
#' @seealso regressoR.learnForExport
#' @seealso regressoR.loadResult
regressoR.batchLearn <- function(source=getwd(),
                                 destination=file.path(source, "../models"),
                                 loader=function(file) read.csv(file, sep="\t")[c(1,2)],
                                 selector=path.extensionRegExp("txt"),
                                 check.directory=NULL,
                                 learn.single=TRUE,
                                 learn.all=FALSE,
                                 learners=regressoR.makeLearners(),
                                 representations=function(x, y) dataTransformeR::Transformation.applyDefault2D(x=x, y=y, addIdentity=TRUE),
                                 metricGenerator=regressoR.quality::RegressionQualityMetric.default,
                                 suffix.single=".model",
                                 suffix.all=paste("all", suffix.single, sep="", collapse=""),
                                 q=0.75,
                                 includeMetric=TRUE,
                                 cores=1L,
                                 logging=TRUE) {

  learn.single <- force(learn.single);
  learn.all <- force(learn.all);

  if(learn.single || learn.all) {

    source <- force(source);
    source <- normalizePath(source);
    source <- force(source);

    destination <- force(destination);
    dir.create(path=destination, showWarnings = FALSE, recursive = TRUE);
    destination <- normalizePath(dest);
    destination <- force(destination);

    loader <- force(loader);
    selector <- force(selector);
    check.directory <- force(check.directory);
    learners <- force(learners);
    representations <- force(representations);
    metricGenerator <- force(metricGenerator);
    suffix.single <- force(suffix.single);
    suffix.all <- force(suffix.all);
    q <- force(q);
    includeMetric <- force(includeMetric);
    cores <- force(cores);
    logging <- force(logging);

    # Construct the modeller, a function which loads all files from "src", learns a model, and stores the result in "dst".
    modeller <- function(src, dst) {
      src <- force(src);
      dst <- force(dst);
      logging <- force(logging);

      if(logging) {
        print(paste(Sys.time(), ": beginning to regression-model ", length(src), " files to ", dst, ".", sep="", collapse=""));
      }

      loader <- force(loader);
      learners <- force(learners);
      representations <- force(representations);
      metricGenerator <- force(metricGenerator);
      q <- force(q);
      includeMetric <- force(includeMetric);

      if(length(src) > 1L) {
        data <- rbindlist(l=unname(lapply(X=src, FUN=function(file) unname(loader(file)))),
                           use.names=FALSE, idcol=NULL);
      } else {
        data <- unname(loader(src[[1L]]));
      }

      data.x <- as.vector(unname(unlist(data[,1L])));
      data.y <- as.vector(unname(unlist(data[,2])));
      result <- regressoR.learnForExport(x=data.x,
                                         y=data.y,
                                         learners=learners,
                                         representations=representations(data.x, data.y),
                                         metricGenerator=metricGenerator,
                                         q=q,
                                         includeMetric=includeMetric);

      result <- force(result);

      saveRDS(object=result, file=dst);

      # print log output
      if(logging) {
        r <- result$quality;
        if(is.null(q)) {
          r <- "failure";
        } else {
          r <- paste("a model of type ", result$name, " at quality ", result$q, sep="", collapse="");
        }
        print(paste(Sys.time(), ": finished to regression-model ", length(src), " files resulting in ",
                    r, " to ", dst, ".", sep="", collapse=""));
      }
    };
    modeller <- force(modeller);

    # create the single modeler
    if(learn.single) {
      proc.single <- path.batchProcessor(processor=modeller,
                                         dest=destination,
                                         suffix=suffix.single);
      proc.single <- force(proc.single);
      file.single <- new.env();
      assign(x=selector, value=proc.single, envir=file.single);
      file.single <- force(file.single);
    } else {
      file.single <- NULL;
    }

    # create the multi-modeller
    if(learn.all) {
      proc.all <- path.batchProcessor(processor=modeller,
                                      dest=destination,
                                      suffix=suffix.all);
      proc.all <- force(proc.all);
      file.all <- new.env();
      assign(x=selector, value=proc.all, envir=file.all);
      file.all <- force(file.all);
    } else {
      file.all <- NULL;
    }

    if(logging) {
      print(paste("Beginning regression-modelling from '", source,
                  "' to '", destination, "' using ", cores,
                  " cores.", sep="", collapse=""));
    }

    path.batchApply(path = source, file.single = file.single,
                    file.in.folder = file.all,
                    check.directory = check.directory,
                    cores = cores);

    if(logging) {
      print(paste("Finished regression-modelling from '", source,
                  "' to '", destination, "' using ", cores,
                  " cores.", sep="", collapse=""));
    }
  }
}

