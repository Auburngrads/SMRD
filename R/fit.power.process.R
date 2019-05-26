#' Title
#'
#' @param data.ld 
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
#' summary(halfbeak.rdu)
#' event.plot(halfbeak.rdu)
#' print(mcf(halfbeak.rdu))
#' mcf.plot(halfbeak.rdu)
#' interarrival.times(halfbeak.rdu)
#' mcf.plot(halfbeak.rdu,
#'          xlab = "Thousands of Hours of Operation",
#'          ylab = "Cumulative Number of Maintenance Actions")
#' 
#' fit.power.process(halfbeak.rdu)
#' fit.loglin.process(halfbeak.rdu)
#' fit.power.and.loglin.process(halfbeak.rdu)
#'                              
#' 
#' }
fit.power.process <-
function (data.ld)
{
    the.censor.codes <- censor.codes(data.ld)
    theResponse <-Response(data.ld)
    fail.times <- theResponse[the.censor.codes == 1, 1]
    censor.time <- theResponse[the.censor.codes == 2, 1]
    number.failures <- length(fail.times)
    betahat <- number.failures/sum(logb(censor.time/fail.times))
    etahat <- censor.time/number.failures^(1/betahat)
    mcf.plot(data.ld)
    time.vec <- logseq(min(fail.times), censor.time, length = 200)
    mcfhat <- (time.vec/etahat)^betahat
    lines(time.vec, mcfhat, lwd = 2)
    return(c(betahat = betahat, etahat = etahat))
}
