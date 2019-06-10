#' paintMultiTimeseries
#'
#' Draws a line plot from several timeseries. 
#' @param v List of timeseries.
#' @return None.
#' @export
#' @examples
#' paintMultiTimseries(list(ts1, ts2, ts3))

paintMultiTimeseries <- function(tss) {

    xmax <- max(sapply(tss, function(i) length(i)))
    ymax <- max(sapply(tss, function(i) max(i)))

    colors <- rainbow(length(tss))

    plot(c(0,xmax), c(0,ymax), type="n", xlab="Interval", ylab="Delay", main="Time Series")
    for (ts in 1:length(tss)) {
            lines(tss[[ts]], type="l", col=colors[ts])
    }

}
