#' Title
#'
#' @param data.ld 
#' @param start.gamma1 
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
fit.loglin.process <-
function (data.ld, 
          start.gamma1 = 0.001)
{
    the.censor.codes <- censor.codes(data.ld)
    theResponse <- Response(data.ld)
    fail.times <- theResponse[the.censor.codes == 1, 1]
    censor.time <- theResponse[the.censor.codes == 2, 1]
    number.failures <- length(fail.times)
    sum.times <- sum(fail.times)
    
    linear.rocof.eq <- function(gamma1, 
                                number.failures,
                                sum.times,
                                censor.time) 
    {
        
        expstuff <- exp(gamma1 * censor.time)
        xvalue <- sum.times + number.failures/gamma1 - (number.failures *
            censor.time * expstuff)/(expstuff - 1)
        print(c(gamma1 = gamma1, xvalue = xvalue))
        return(xvalue)
        
    }
    
    interval <- bound.roots2(linear.rocof.eq, 
                             start.value = start.gamma1,
                             number.failures, 
                             sum.times, 
                             censor.time)
    
    uniroot.out <- uniroot(linear.rocof.eq, 
                           tol = 1e-10, 
                           interval = interval,
                           number.failures = number.failures, 
                           sum.times = sum.times,
                           censor.time = censor.time)
    
        result <- uniroot.out$root

    gamma1hat <- result
    gamma0hat <- logb((number.failures * gamma1hat) / (exp(gamma1hat * censor.time) - 1))
    mcf.plot(data.ld)
    time.vec <- logseq(min(fail.times), censor.time, length = 200)
    mcfhat <- (exp(gamma0hat) * (exp(gamma1hat * time.vec) - 1)) / gamma1hat
    lines(time.vec, mcfhat, lwd = 2)
    
    return(c(gamma0hat = gamma0hat, 
             gamma1hat = gamma1hat))
}
