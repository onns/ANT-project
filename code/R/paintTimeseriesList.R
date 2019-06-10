#' paintTimeseriesList
#'
#' Generates a graphic with several timeseries using grid.arrange
#' @param v List of timeseries. 
#' @return Graphic to plot.
#' @export
#' @examples
#' paintTimeseriesList(list(ts1, ts2, ts3))

paintTimeseriesList <- function(v) {

    len <- min(unlist(lapply(v, length)))

    getplot <- function(ts, col) {

        ts <- ts[1:len]

        df <- data.frame(t=1:length(ts), c=ts)

        g <- ggplot2::ggplot(df, ggplot2::aes(x=t, y=c)) +
        ggplot2::geom_line(size=0.4, color=col) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::theme(plot.title = ggplot2::element_text(size=8, face="italic")) +
        ggplot2::scale_y_continuous("time", limits=c(0,150))

        return(list(g))
    }

    glist <- mapply(getplot, v, rainbow(length(v)))

    return(do.call(gridExtra::grid.arrange, c(glist, nrow=length(v))))

}
