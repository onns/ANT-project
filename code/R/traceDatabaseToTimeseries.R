#' traceDatabaseToTimeseries
#'
#' Converts a list of data.frames (timestamp/delay) into a list of timeseries 
#' @param traceDatabase List of files with data.frames (timestamp/delay).
#' @param P Interval duration in milliseconds. Defaults to 20 ms.
#' @param samplingFun Sampling function. Defaults to 'sum'.
#' @param output Output file for writing.
#' @return list of timeseries
#' @export
#' @importFrom foreach %dopar%
#' @examples
#' traceDatabaseToTimeseries(db, P=20, samplingFun=sum) 

traceDatabaseToTimeseries <- function(traceDatabase, P = 20, samplingFun = sum, output = NA) {

    db <- list()

    for (t in traceDatabase) {

        print(paste("Loading trace from", t))
        trace <- loadTraceDatabase(t)
        
        # Register parallel support
        doParallel::registerDoParallel(cores=parallel::detectCores())

        tmp <- foreach::foreach(tr=trace, .inorder=TRUE) %dopar% {

          # Add timeseries to db
          if (nrow(tr)>0){  
            traceToTimeseries(tr, P=P, samplingFun=samplingFun)
          }
        }

        # Unregister parallel support
        doParallel::stopImplicitCluster()

        names(tmp) <- names(trace)

        # Append lists of timeseries
        db <- c(db, tmp)

    }


    if (!is.na(output)) {
        print("Storing db...")
        saveRDS(db, file=output)
        print(paste("Saved at: '", output, "'", sep=""))
    }

    return(db)

}

#' loadTimeseriesDatabase
#'
#' Load DB from 'file'
#' @param file Path with db.dat. 
#' @return List of timeseries.
#' @export
#' @examples
#' loadTimeseriesDatabase("data/test.dat")

loadTimeseriesDatabase <- function(file) {

    return(readRDS(file=file))

}
