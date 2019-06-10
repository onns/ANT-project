#' paintTracesList
#'
#' Generates a graphic with several traces using grid.arrange
#' @param v List of traces (data.frames).
#' @return Graphic to plot.
#' @export
#' @examples
#' paintTracesList(list(tr1, tr2, tr3))

paintTracesList <- function(v) {

    len <- min(unlist(lapply(v, nrow)))
    
    getplot <- function(tr, col) {

        tr <- tr[1:len,]

        g <- ggplot2::ggplot(tr, ggplot2::aes(x=timestamp, y=diff)) +
        ggplot2::geom_line(size=0.4, color=col) +
        ggplot2::theme(plot.title = ggplot2::element_text(size=8, face="italic")) +
        ggplot2::scale_y_continuous("event duration", limits=c(0,150)) +
        ggplot2::scale_x_continuous("time")

        return(list(g))
    }

    glist <- mapply(getplot, v, rainbow(length(v)))

    return(do.call(gridExtra::grid.arrange, c(glist, nrow=length(v))))
}
