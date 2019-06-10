#' generateTuningDatabases
#'
#' Creates databases of timeseries for all combinations of parameters
#' @param inPath Path with raw data
#' @param outPath Path for the output dbs.
#' @param RL Range of maximum length for the traces. 
#' @param RP Range of P values.
#' @param RF Range of sampling functions.
#' @return None
#' @export
#' @examples
#' generateTuningDatabases("data/", "dbs/", RL=c(4000,2000,1000), RP=c(50,20,10,5), RF=c("sum"))

generateTuningDatabases <- function(inPath, outPath, RL, RP, RF) {

    for (maxLength in RL) {

        parameters <- as.data.frame(expand.grid(RP=RP, RF=RF))
print('a')
        tmp <- list.files(outPath, pattern = '.dat$')
        print(tmp)
        if (length(tmp) == 0){
          tmp <- createTraceDatabase(inPath, outPath, maxLength=maxLength)
        }

        print(parameters)

        for (param in 1:nrow(parameters)) {

            P <- parameters[param,]$RP
            F <- as.character(parameters[param,]$RF)
        
            name <- paste("ts",maxLength,P,F,sep="_")
            print(paste("Generating db:", name))
            output <- paste(outPath, name, ".db", sep="")

            traceDatabaseToTimeseries(tmp, P=P, samplingFun=get(F), output=output)

        }

    }

}

