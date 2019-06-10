#' parseTrace
#'
#' Converts a vector of timestamps 'trace' into a data.frame (timestamp/delay)
#' @param trace Vector of timestamps
#' @return data.frame of delays and timestamps. 
#' @export
#' @examples
#' parseTrace("db$example_0.json")

parseTrace <- function(trace) {

    data <- data.frame(timestamp=trace, delay=NA)
    data$delay <- c(data[2:nrow(data),"timestamp"] - data[1:nrow(data)-1,"timestamp"], NA)
    data <- data[1:nrow(data)-1,] # remove last NA

    # Calc Q3, filter values smaller (reduces size)
    threshold <- summary(data$delay)[5]
    data <- data[data$delay > threshold,]

    return(data)

}
