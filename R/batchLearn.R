#' @include learnForExport.R

#' @title Apply the Regression Learning to a Directory of Data and Store the
#'   Fitted Models in another Directory
#' @description Recursively, a \code{source} directory is traversed and all
#' files matching to a \code{selector} regular expression are picked up,
#' loaded with a \code{loader}, and then modelled by the regressor. The
#' resulting models are stored in a \code{destination} folder in a structure
#' mirroring the source folder.
#'
#' This method uses \code{\link{regressoR.learnForExport}} to learn the models
#' and stores them into files using \code{\link{saveRDS}} to store them. They
#' can later be read using \code{\link{regressoR.loadResult}}.
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
#' @param logging should progress information be printed: either \code{TRUE} for
#'   printing to the console via \code{\link{print}}, \code{FALSE} for no
#'   logging, or a path to a file receiving logging information
#' @export regressoR.batchLearn
#' @importFrom utilizeR path.batchProcessor path.batchApply path.extensionRegExp
#' @importFrom dataTransformeR Transformation.applyDefault2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @importFrom data.table rbindlist
#' @importFrom utils read.csv
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
                                 logging=if(cores <= 1L) { TRUE } else { file.path(destination, "log.txt"); }) {

  learn.single <- force(learn.single);
  learn.all <- force(learn.all);

  if(learn.single || learn.all) {
    # we only get busy if there is something to learn

    # first canonicalize the source path
    source <- force(source);
    source <- normalizePath(source);

    # check and creat the logging destination
    logging <- force(logging);
    if(is.character(logging)) {
      # logging is a path to a file, so we first try to normalize it
      logging <- normalizePath(logging, mustWork = FALSE);
      # and make sure the hosting directory exists
      dir.create(path=dirname(logging), showWarnings = FALSE, recursive = TRUE);
      # now we can make sure that the log file exists
      file.create(logging, showWarnings = FALSE);
      # which means that now we can really normalize the path
      logging <- normalizePath(logging);
      logging <- force(logging);

      # the logger is now a function writing to the file
      logger <- function(string) {
        logging <- force(logging);
        # open the file for appending text
        con <- file(logging, "at");
        # writing the text
        writeLines(text=string, con=con);
        # closing the file
        close(con);
      }
    } else {
      if(isTRUE(logging)) {
        if(cores > 1L) {
          warning(paste("Setting logging to TRUE with cores=",
                        cores, " will may crash/fail in RStudio.",
                        sep="", collapse=""))
        }
        logger <- print;
      } else {
        logger <- NULL;
      }
    }
    logger <- force(logger);

    # wethen we also canonicalize the destination path and ensure that the
    # destination folder exists
    destination <- force(destination);
    dir.create(path=destination, showWarnings = FALSE, recursive = TRUE);
    destination <- normalizePath(destination);

    # we enforce that all parameters exist
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

    # Construct the modeler, a function which loads all files from "src",
    # learns a model, and stores the result in "dst".
    modeler <- function(src, dst) {
      src <- force(src);
      dst <- force(dst);
      logger <- force(logger);

      if(!is.null(logger)) {
        logger(paste(Sys.time(), ": beginning to regression-model ", length(src),
                    " files to ", dst, ".", sep="", collapse=""));
      }

      # enforce that all necessary parameters exist in the current scope
      loader <- force(loader);
      learners <- force(learners);
      representations <- force(representations);
      metricGenerator <- force(metricGenerator);
      q <- force(q);
      includeMetric <- force(includeMetric);

      # load the data as n*2 matrix and remove all names
      if(length(src) > 1L) {
        # if there are multiple sources, load all of them and patch them
        # together: every loaded data is supposed to be a two-column matrix and
        # we attach the matrices to each other one under the other
        data <- rbindlist(l=unname(lapply(X=src, FUN=function(file) unname(loader(file)))),
                           use.names=FALSE, idcol=NULL);
      } else {
        # we load a single file
        data <- unname(loader(src[[1L]]));
      }

      # extract the x and y data from the matrix
      data.x <- as.vector(unname(unlist(data[,1L])));
      data.y <- as.vector(unname(unlist(data[,2L])));

      # start the actual learning process
      result <- regressoR.learnForExport(x=data.x,
                                         y=data.y,
                                         learners=learners,
                                         representations=representations(data.x, data.y),
                                         metricGenerator=metricGenerator,
                                         q=q,
                                         includeMetric=includeMetric);
      result <- force(result);

      # store the result list in the output file
      saveRDS(object=result, file=dst);

      # print log output
      if(!is.null(logger)) {
        r <- result$quality;
        if(is.null(q)) {
          r <- "failure";
        } else {
          r <- paste("a model of type ", result$name, " at quality ", result$q, sep="", collapse="");
        }
        logger(paste(Sys.time(), ": finished to regression-model ", length(src), " files resulting in ",
                    r, " to ", dst, ".", sep="", collapse=""));
      }
    };
    modeler <- force(modeler);

    # create the single modeler, i.e., a wrapped batch processor which applies
    # "modeler" to each single acceptable input file
    if(learn.single) {
      # create the batch processor receiving input/output paths
      proc.single <- path.batchProcessor(processor=modeler,
                                         dest=destination,
                                         suffix=suffix.single);
      proc.single <- force(proc.single);
      # assign it to the regular expression for processors
      file.single <- new.env();
      assign(x=selector, value=proc.single, envir=file.single);
      file.single <- force(file.single);
    } else { # no single-file processors
      file.single <- NULL;
    }

    # create the single modeler, i.e., a wrapped batch processor which applies
    # "modeler" to all acceptable input files of a folder
    if(learn.all) {
      # create the batch processor receiving input/output paths
      proc.all <- path.batchProcessor(processor=modeler,
                                      dest=destination,
                                      suffix=suffix.all);
      proc.all <- force(proc.all);
      # assign it to the regular expression for processors
      file.all <- new.env();
      assign(x=selector, value=proc.all, envir=file.all);
      file.all <- force(file.all);
    } else { # no single-file processors
      file.all <- NULL;
    }

    if(!is.null(logger)) {
      # log that we start working now
      logger(paste("Beginning regression-modelling from '", source,
                  "' to '", destination, "' using ", cores,
                  " cores.", sep="", collapse=""));
    }

    # defering control to the directory walker and processor invocation engine
    # which will dispatch the matching paths to the processors, which in turn
    # will invoke the modeler
    path.batchApply(path = source, file.single = file.single,
                    file.in.folder = file.all,
                    check.directory = check.directory,
                    cores = cores);

    if(!is.null(logger)) { # yeah, we are done
      logger(paste("Finished regression-modelling from '", source,
                  "' to '", destination, "' using ", cores,
                  " cores.", sep="", collapse=""));
    }
  }
}

