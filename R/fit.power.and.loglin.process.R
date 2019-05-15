#' Title
#'
#' @param data.rdu 
#' @param start.gamma1 
#' @param xlab 
#' @param ylab 
#' @param plot.seg 
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
#' fit.power.and.loglin.process(halfbeak.rdu,
#'                              xlab = "Thousands of Hours of Operation",
#'                              ylab = "Cumulative Number of Maintenance Actions")
#' 
#' 
#' }
fit.power.and.loglin.process <-
function (data.rdu, start.gamma1 = 0.001, xlab = paste("Age in",
    get.time.units(data.rdu)), ylab = "Mean Cumulative Failures",
    plot.seg = T)
{
    event <- events(data.rdu)
    EndPoints <- is.element(casefold(event), c("end", "mend",
        "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    the.times <- times(data.rdu)
    fail.times <- the.times[CriticalEvent, 1]
    censor.time <- the.times[EndPoints, 1]
    number.failures <- length(fail.times)
    sum.times <- sum(fail.times)
    linear.rocof.eq <- function(gamma1, number.failures, sum.times,
        censor.time) {
        expstuff <- exp(gamma1 * censor.time)
        xvalue <- sum.times + number.failures/gamma1 - (number.failures *
            censor.time * expstuff)/(expstuff - 1)
        print(c(gamma1 = gamma1, xvalue = xvalue))
        return(xvalue)
    }
    interval <- bound.roots2(linear.rocof.eq, start.value = start.gamma1,
        number.failures, sum.times, censor.time)
    uniroot.out <- uniroot(linear.rocof.eq, tol = 1e-10, interval = interval,
        number.failures = number.failures, sum.times = sum.times,
        censor.time = censor.time)
    if (!is.R()) {
        result <- (uniroot.out$neg + uniroot.out$pos)/2
    }
    else {
        result <- uniroot.out$root
    }
    gamma1hat <- result
    gamma0hat <- logb((number.failures * gamma1hat)/(exp(gamma1hat *
        censor.time) - 1))
    mcf.out <- mcf(data.rdu)
    plot.mcf(mcf.out, xlab = xlab, ylab = ylab, plot.seg = plot.seg)
    time.vec <- logseq(min(fail.times), censor.time, length = 200)
    mcfhat <- (exp(gamma0hat) * (exp(gamma1hat * time.vec) -
        1))/gamma1hat
    lines(time.vec, mcfhat, lwd = 2)
    betahat <- number.failures/sum(logb(censor.time/fail.times))
    etahat <- censor.time/number.failures^(1/betahat)
    mcfhat <- (time.vec/etahat)^betahat
    lines(time.vec, mcfhat, lty = 3, lwd = 2)
    return(c(gamma0hat = gamma0hat, gamma1hat = gamma1hat, betahat = betahat,
        etahat = etahat))
}
