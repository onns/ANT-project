#' createTraceDatabase
#'
#' Lists all files in 'inPath' and generates a list of data.frames (timestamp/delay)
#' @param inPath Path with raw data.
#' @param output Output file for writing.
#' @param maxLength Maximum duration length of vectors. Defaults to 4000 milliseconds.
#' @return list with vectors of delays.
#' @export
#' @importFrom foreach %dopar%
#' @examples
#' createTraceDatabase("data/", "traces/")

createTraceDatabase <- function(inPath, outPath, maxLength = 4000) {

    dbs <- list()

    tmp <- list.files(inPath, pattern = "_[0-9]+\\.json$")[1]
    pattern <- stringr::str_replace(tmp, "_[0-9]+\\.json$", "")
    tmp <- list.files(inPath, pattern = pattern)
    sets <- stringr::str_match(tmp, "_([0-9]+)\\.json")[,2]

    # Iterate over one set of traces
    for (s in sets) {

        files <- list.files(inPath, pattern = paste("_", s, "\\.json$", sep=""))

        outname <- paste(outPath, "traces_", s, "_", maxLength, ".dat", sep="")

        dbs <- c(dbs, outname)

        # If db exists skip
        if (file.exists(outname)) next

        # Register parallel support
        doParallel::registerDoParallel(cores=parallel::detectCores())

        db <- foreach::foreach(f=files, .inorder=TRUE) %dopar% {

            print(f)

            # Read raw JSON file
            tmp <- jsonlite::fromJSON(paste(inPath,f,sep=""))

            # Relative duration and cut to maxLength
            tmp <- tmp - tmp[1]
            tmp <- tmp[tmp < maxLength]

            # Add to db
            parseTrace(tmp)

        }

        names(db) <- files

        # Unregister parallel support
        doParallel::stopImplicitCluster()

        print("Storing db...")
        saveRDS(db, file=outname)
        print(paste("Saved at: '", outname, "'", sep=""))

    }

    return(dbs)

}

#' loadTraceDatabase
#'
#' Load trace DB from path
#' @param file Path with db.dat. 
#' @return List of data.frames (timestamp/delay). 
#' @export
#' @examples
#' loadTraceDatabase("traces/traces_5_4000.dat")

loadTraceDatabase <- function(path) {

    return(readRDS(file=path))

}
