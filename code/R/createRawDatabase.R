#' createRawDatabase
#'
#' Lists all files in 'inPath' and generates a list of vectors of delays
#' @param inPath Path with raw data.
#' @param output Output file for writing.
#' @param maxLength Maximum duration length of vectors. Defaults to 4000 milliseconds.
#' @return list with vectors of delays.
#' @export
#' @importFrom foreach %dopar%
#' @examples
#' createRawDatabase("data/", "test.dat")

createRawDatabase <- function(inPath, output = NA, maxLength = 4000) {

    files <- list.files(inPath, pattern = "_[0-9]+\\.json$")

    db <- list()

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
        db[[f]] <- tmp

    }

    names(db) <- files

    # Unregister parallel support
    doParallel::stopImplicitCluster()

    if (!is.na(output)) {
        print("Storing db...")
        #longest <- max(unlist(lapply(db, length)))
        #bmatrix <- bigmemory::big.matrix(nrow=length(db), ncol=longest,
        #                                 type="double", dimnames=c(names(db),NULL))
        #for (t in names(db)) {
        #    bmatrix[t, 1:longest] <- db[[t]]
        #}
        #bigmemory::write.big.matrix(bdb, output, sep=",", row.names=TRUE)
        saveRDS(db, file=output)
        print(paste("Saved at: '", output, "'", sep=""))
    }

    return(db)

}

#' loadRawDatabase
#'
#' Load DB from 'file'
#' @param file Path with db.dat. 
#' @return List of pages with a vector of timestamps per page.
#' @export
#' @examples
#' loadRawDatabase("data/test.dat")

loadRawDatabase <- function(file) {

    return(readRDS(file=file))

}
