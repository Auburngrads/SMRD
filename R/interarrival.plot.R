#' Title
#'
#' @param data.rdu 
#' @param my.title 
#' @param xlab 
#' @param ylab 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' halfbeak.rdu <- frame.to.rdu(halfbeak,
#'                              ID.column = "unit", 
#'                              time.column = "hours" ,
#'                              event.column = "event", 
#'                              data.title = "Halfbeak Data", 
#'                              time.units = "Thousands of Hours of Operation")
#' 
#' interarrival.plot(halfbeak.rdu,
#'                   xlab = "Thousands of Hours of Operation",
#'                   ylab = "Thousands of Hours Between Maintenance Actions",
#'                   my.title="")
#' 
#' }
interarrival.plot <-
function (data.rdu, my.title = NULL, xlab = paste("Age in", get.time.units(data.rdu)),
    ylab = "Time Between Failure", ...)
{
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    the.interarrival.times <- Response(interarrival.times(data.rdu))
    the.censor.codes <- events(data.rdu)
    failure.censor.names <- ClistToVec(get.failure.censor.names(data.rdu))
    is.event <- is.element(casefold(the.censor.codes), failure.censor.names)
    the.failure.times <- sort(times(data.rdu)[is.event, 1])
    plot(the.failure.times, the.interarrival.times, xlab = "",
        ylab = "", cex = 1.5, ...)
    title(xlab = xlab, ylab = ylab, cex = 1.5)
    if (is.null(my.title))
        my.title <- get.data.title(data.rdu)
    title(my.title)
}
