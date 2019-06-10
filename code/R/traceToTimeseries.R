#' traceToTimeseries
#'
#' This function converts a raw Trace of time delays into a Timeserie sampling intervals of duration P 
#' @param trace Trace to convert.
#' @param P Interval duration in milliseconds. Defaults to 20 ms.
#' @param samplingFun Sampling function. Defaults to 'sum'.
#' @return ts() object.
#' @export
#' @examples
#' traceToTimseries(trace, P=20)

traceToTimeseries <- function(trace, P=20, samplingFun=sum) {

    trace$slot <- floor(trace[,"timestamp"]/P)
    tmp <- aggregate(trace$delay, by=list(unique.values=trace$slot), FUN=samplingFun)
    return(ts(tmp$x))

}
