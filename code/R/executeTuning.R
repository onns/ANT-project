#' generateTuningDatabases
#'
#' Creates databases of timeseries for all combinations of parameters
#' @param inPath Path with databases of timeseries.
#' @param N Number of rounds with 1 dataset as ref and 1 as test.
#' @return Data frame average results.
#' @export
#' @examples
#' executeTuning("dbs/")

executeTuning <- function(inPath, logFile=NA) {

    # Copy all output into a log file
    if (!is.na(logFile)) {
        sink(logFile, split = TRUE) 
    }

    # Build table of results
    results <- NULL

    # Get dbs of timeseries
    dbs <- list.files(inPath, pattern="\\.db$")

    # Creates directory for temp cross distance matrix
    suppressWarnings(dir.create(paste(inPath, "crossMatrices", sep="")))

    # For each different type of time series
    for (dbname in dbs) {

        # Extract db parameters
        tsParameters <- parseDatabaseFilename(dbname)

        # Load db
        print(paste("Loading ", inPath, dbname, sep=""))
        db <- readRDS(paste(inPath, dbname, sep=""))

        # Set all DTW parameters for tuning
		pWindowTypes <- c("itakura", "sakoechiba")
		pStepPatterns <- c("symmetric1", "symmetric2", "asymmetric")
		pWindowSizes <- c(1, 5, 10, 30, 50, 100)
        dtwParameters <- as.data.frame(expand.grid(windowType=pWindowTypes,
                                                   stepPattern=pStepPatterns,
                                                   windowSize=pWindowSizes))
        # Remove invalid ones (itakura window sizes)
        dtwParameters <- dtwParameters[-which(dtwParameters$windowType == "itakura" 
                                              & dtwParameters$windowSize != 1),]

        # For each configuration of DTW
        for (p in 1:nrow(dtwParameters)) {

            conf <- dtwParameters[p,] # windowType, stepPattern, windowSize
            conf$P <- tsParameters$P
            conf$maxLength <- tsParameters$maxLength
            conf$sampFun <- tsParameters$sampFun

            # Print parameters 
            print(paste("Conf:", printConfigData(conf)))

            # Calculates rounds of DTW with given config
            perf <- calculatePerformance(db, conf, 3, paste(inPath, "crossMatrices/", sep=""))

            # Store results 
            for (p in names(perf)) {
                results <- rbind(results, data.frame(maxLength=conf$maxLength, P=conf$P, sampFun=conf$sampFun,
                                                 windowType=conf$windowType,stepPattern=conf$stepPattern,
                                                 windowSize=conf$windowSize, tolerance=as.numeric(p), rate=perf[p]))
            } 
            
            # Print perforamnce
            print(paste("Performance: ", paste(perf, collapse=" ")))
            print("=============")

        }

    }

    return(results)

}

#' parseDatabaseFilename
#'
#' Extract parameters from db filename
#' @param name Filename.
#' @return Named list of parameters.
#' @export
#' @examples
#' parseDatabaseFilename("ts_1000_20_sum.db")
parseDatabaseFilename <- function(name) {

    x <- stringr::str_match_all(name, "ts_([0-9]+)_([0-9]+)_([a-zA-Z]+)\\.db")
    if (length(x) != 0) {
        return(list(maxLength=x[[1]][1,2], P=x[[1]][1,3], sampFun=x[[1]][1,4]))
    }

}

#' printConfigData
#'
#' Returns formatted config data string
#' @param conf Config data.frame.
#' @return String.
#' @export
printConfigData <- function(conf) {

    return(paste("window=", conf$windowType, "|step=", conf$stepPattern,
          "|size=", conf$windowSize, "|P=", conf$P, "|length=", conf$maxLength,
          sep=""))

}

#' calculatePerformance
#'
#' Calculates aprox. matching rate for given a database and configuration
#' @param db Database of timeseries.
#' @param conf Configuration for DTW algorithm as a data.frame row.
#' @param N Number of rounds with 1 set as ref and 1 as test. Defaults to 3.
#' @param keepMatrices Save intermediate cross DTW matrices in files. Defaults to NA. 
#' @return Average matching rate after N rounds.
#' @export
#' @examples
#' calculatePerformance(db, conf, keepMatrices="dbs/crossMatrices/")

calculatePerformance <- function(db, conf, N=3, keepMatrices=NA) {

    tmp <- names(db)[[1]]
    pattern <- stringr::str_replace(tmp, "_[0-9]+\\.json$", "")
    tmp <- names(db[grepl(pattern, names(db))])
    sets <- as.numeric(stringr::str_match(tmp, "_([0-9]+)\\.json")[,2])

    N <- min(N, floor(length(sets)/2)) # use N or max number of samples

    # Use N*2 random samples for tuning
    set.seed(42) # use same samples for every calculations
    randSamples <- sample(sets, N*2, replace=FALSE)

    # Performance per round (2 times beacuse asymetry)
    performance <- matrix(ncol=5, nrow=N, dimnames=list(NULL, c("1","3","5","10","30")))

    # Do N rounds
    for (k in 1:N) {

        print(paste("Round: ", k, "/", N, sep=""))

        t <- randSamples[k]
        r <- randSamples[k+N]

        # Database timeseries names have the format 'webpage_k.json' (k in [0:14])
        test <- db[grepl(paste("_", r,"\\.json$", sep=""), names(db))]
        ref <- db[grepl(paste("_", t, "\\.json$", sep=""), names(db))]

        # Compute cross DTW matrix for each configuration and pair of test/ref
        crossDTWmatrix <- dtwDistParallel(test, ref, conf, paste(r,t,sep="-"), output=keepMatrices)

        # Calculate matching rate 
        performance[k,] <- recognitionRate(crossDTWmatrix)

    }

    return(colMeans(performance))

}

#' tracesMatch
#'
#' Returns true if trace's names are from same page
#' @param a Query's trace name
#' @param b List of names ordered by proximity.
#' @param tolerance Tolerance. Defaults to 1.
#' @return TRUE if query's name is in "tolerance" firsts sublist.
#' @export
tracesMatch <- function(query, response, tolerance=1) {
    query <- stringr::str_replace(query, "_[0-9]+\\.json$", "")
    # optional, remove TLDs with 'tldextract'
    for (j in 1:tolerance) {
        response[j] <- stringr::str_replace(response[j], "_[0-9]+\\.json$", "")
        if (query == response[j]) return(TRUE)
    }
    return (FALSE)
}

#' recognitionRate
#'
#' Given a cross distance matrix returns recognition rate
#' @param M Cross distance matrix.
#' @param tolerance Tolerance. Defaults to 1.
#' @return Recognition rate.
#' @export
recognitionRate <- function(M) {

    count <- rep(0,5)
    i <- 1

    for (t in c("1","3","5","10","30")) {

        tmp <- sapply(rownames(M), function(q) tracesMatch(q, colnames(M)[order(M[q,])], as.numeric(t)))
        count[i] <- length(tmp[tmp == TRUE])

        tmp <- sapply(colnames(M), function(q) tracesMatch(q, rownames(M)[order(M[,q])], as.numeric(t)))
        count[i] <- count[i] + length(tmp[tmp == TRUE])

        count[i] <- count[i] / (dim(M)[1] * 2) # count twice because of asymetry

        i <- i + 1

    }

    return(count)

}

#' dtwDistParallel
#'
#' Calculates cross DTW distance matrix for X and Y (lists of timeseries)
#' @param mx List of timeseries.
#' @param my List of timeseries.
#' @param conf Parameters for 'dtw' function.
#' @param output Output folder to save the cross DTW distance matrix. Defaults to NA.
#' @return Returns the cross DTW distance matrix.
#' @export
#' @examples
#' dtwDistParallel()

dtwDistParallel <- function(mx, my, conf, tag, output=NA) {

    # Try to load previously calculated matrix
    if (!is.na(output)) {
        name <- paste("crossDTWmatrix", conf$maxLength, conf$P, conf$windowType,
                      conf$stepPattern, conf$windowSize, tag, sep="_")
        name <- paste(output, name, ".dat", sep="")

        if (file.exists(name)) {
            print("Loading precalculated matrix...")
            return(readRDS(file=name))
        }

    }

    # Prepare matrix
    print("Start matrix computation (may take a long time)")
    ptm <- proc.time()

    # Register parallel support
    doParallel::registerDoParallel(cores=parallel::detectCores())

    # Calculate DTW
    res <- foreach::foreach(e=1:(length(mx)*length(my)), .inorder=TRUE) %dopar% {

        i <- ((e-1) %% length(my)) + 1
        j <- floor(e / length(my)) + 1

        tryCatch({
            dtw::dtw(unlist(mx[[i]]), unlist(my[[j]]), dist.method="Euclidean",
                distance.only=TRUE, keep.internals=FALSE,
                window.type=as.character(conf$windowType),
                step=get(as.character(conf$stepPattern),envir=getNamespace("dtw")),
                window.size=conf$windowSize)$dist
        }, error = function(e) {
            return(Inf) # return Infinite if not alignment found
        })

    }

    # Move res vector into M
    M <- matrix(unlist(res), nrow=length(mx), ncol=length(my), dimnames=list(names(mx), names(my)))

    # Unregister parallel support
    doParallel::stopImplicitCluster()
    print(paste("Execution time:", (proc.time() - ptm)[3]))

    # Save matrix into file
    if (!is.na(output)) {
        print("Storing computed matrix")
        saveRDS(M, file=name)
    }

    return(M)

}
