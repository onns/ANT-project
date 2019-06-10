#' paintMultiTraces
#'
#' Draws a line plot from several traces.
#' @param v List of traces (data.frames).
#' @return None.
#' @export
#' @examples
#' paintMultiTraces(list(tr1, tr2, tr3))

paintMultiTraces <- function(traces) {

    xmax <- max(sapply(traces, function(i) max(i$timestamp)))
    ymax <- max(sapply(traces, function(i) max(i$delay)))

    colors <- rainbow(length(traces))

    plot(c(0,xmax), c(0,ymax), type="n", xlab="Time", ylab="Delay", main="Raw traces")
    for (tr in 1:length(traces)) {
            lines(x=traces[[tr]]$timestamp, y=traces[[tr]]$diff, type="l", col=colors[tr])
    }

}
