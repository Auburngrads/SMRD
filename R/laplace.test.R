#' Title
#'
#' @param data.rdu 
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
#' laplace.test(halfbeak.rdu)
#' lewis.robinson.test(halfbeak.rdu)
#' milhbk189.test(halfbeak.rdu)
#' 
#' }
laplace.test <-
function (data.rdu)
{
    the.censor.codes <- casefold(events(data.rdu))
    failure.censor.names <- ClistToVec(get.failure.censor.names(data.rdu))
    is.event <- is.element(the.censor.codes, failure.censor.names)
    the.times <- times(data.rdu)
    the.failure.times <- sort(the.times[is.event, 1])
    censor.time <- the.times[the.censor.codes == "end", 1]
    if (length(censor.time) > 1) {
        warning("more than one End time")
        censor.time <- max(censor.time)
    }
    number.failures <- length(the.failure.times)
    zvalue <- (sum(the.failure.times) - 0.5 * number.failures *
        censor.time)/(censor.time * sqrt(number.failures/12))
    pvalue <- 1 - pchisq(zvalue^2, 1)
    return(c(zvalue = zvalue, pvalue = pvalue))
}
