#' Title
#'
#' @param data.rdu 
#' @param small.for.tie 
#' @param replace.zero 
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
#' 
#' summary(halfbeak.rdu)
#' event.plot(halfbeak.rdu)
#' print(mcf(halfbeak.rdu))
#' mcf.plot(halfbeak.rdu)
#' interarrival.times(halfbeak.rdu)
#' 
#' }
interarrival.times <-
function (data.rdu, small.for.tie = 1e-05, replace.zero = T) 
{
    the.censor.codes <- casefold(events(data.rdu))
    failure.censor.names <- ClistToVec(get.failure.censor.names(data.rdu))
    is.event <- is.element(the.censor.codes, failure.censor.names)
    fail.times <- sort(times(data.rdu)[is.event, 1])
    tau <- diff(c(0, fail.times))
    if (replace.zero) 
        tau[tau <= 0] <- small.for.tie
    the.frame <- data.frame(Time = tau)
    return(frame.to.ld(the.frame, response.column = "Time"))
}
