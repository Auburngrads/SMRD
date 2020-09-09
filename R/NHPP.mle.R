#' Title
#'
#' @param data.rdu 
#' @param form 
#' @param debug1 
#' @param theta.start 
#' @param kprint 
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
#' NHPP.mle(halfbeak.rdu, form = "power rule")
#' NHPP.mle(halfbeak.rdu, form = "log linear")
#' 
#' }
NHPP.mle <-
function (data.rdu, 
          form = "power rule",
          debug1 = 0, 
          theta.start = c(NA,NA),
          kprint = 0,...)
{
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
  
    options(digits = 5)
    func.call <- match.call()
    event <- events(data.rdu)
    EndPoints <- is.element(casefold(event), c("end", "mend", "removed"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    Times <- times(data.rdu)
    UnitID <- as.factor(get.UnitID(data.rdu))
    WindowInfo <- attr(data.rdu, "WindowInfo")
    WindowPoint <- WindowInfo$WindowPoint
    WindowU <- WindowInfo$WindowU
    WindowL <- WindowInfo$WindowL
    EndPoints <- is.element(casefold(event), c("end", "mend"))
    StartPoints <- is.element(casefold(event), c("start", "mstart"))
    CriticalEvent <- !(EndPoints | StartPoints)
    RecurrTimes <- Times[CriticalEvent, ]
    non.positive <- RecurrTimes <= 0
    if (any(non.positive)) {
      
        if (any(RecurrTimes < 0)) stop("Negative event times not allowed in NHPP model")
        warning("Events at time 0 detected. Adding a small amount to make positive.")
        unique.times <- sort(unique(RecurrTimes))
        if (length(unique.times) < 2) stop("All events times are zero")
        eps <- (unique.times[2] - unique.times[1])/10
        RecurrTimes[non.positive] <- RecurrTimes[non.positive] + eps
        Times[CriticalEvent, ] <- RecurrTimes
        times(data.rdu) <- Times
        
    }
    
    median.time <- median(unique(RecurrTimes))
    RecurrUnitID <- UnitID[CriticalEvent]
    RecurrCosts <- get.Costs(data.rdu)[CriticalEvent]
    RiskSetCounts <- RiskSet(data.rdu)$Counts
    model <- list(form = form, median.time = median.time)
    f.tranparam <- function(theta, model) {
        switch(generic.nhpp.form(model$form)[[1]], power.rule = {
            eta <- theta[1]
            beta <- theta[2]
            logMuAtMedian <- log((model$median.time/eta)^beta)
            logbeta <- log(beta)
            thetatran <- c(logMuAtMedian, logbeta)
            names(thetatran) <- c("logMuAtMedian", "logbeta")
        }, log.linear = {
            gamma0 <- theta[1]
            gamma1 <- theta[2]
            logMuAtMedian <- log(((exp(gamma1 * model$median.time) -
                1) * exp(gamma0))/gamma1)
            thetatran <- c(logMuAtMedian, gamma1)
            names(thetatran) <- c("logMuAtMedian", "gamma1")
        })
        return(thetatran)
    }
    f.origparam <- function(thetatran, model) {
        switch(generic.nhpp.form(model$form)[[1]], power.rule = {
            logMuAtMedian <- thetatran[1]
            logbeta <- thetatran[2]
            beta <- exp(logbeta)
            eta <- model$median.time/exp(logMuAtMedian)^(1/beta)
            thetaorig <- c(eta, beta)
            names(thetaorig) <- c("eta", "beta")
        }, log.linear = {
            logMuAtMedian <- thetatran[1]
            gamma1 <- thetatran[2]
            expgamma0 <- (exp(logMuAtMedian) * gamma1)/(exp(gamma1 *
                model$median.time) - 1)
            if (expgamma0 <= 0) expgamma0 <- 1e-20
            gamma0 <- log(expgamma0)
            thetaorig <- c(gamma0, gamma1)
            names(thetaorig) <- c("gamma0", "gamma1")
        })
        return(thetaorig)
    }
    start.na <- is.na(theta.start)
    if (any(start.na)) {
        mcf.out <- mcf(data.rdu, debug1 = debug1,kprint = kprint)
        time.quantiles <- quantile(mcf.out[[1]])
        mcf.quantiles <- quantile(mcf.out[[2]])
        logmcf.quantiles <- log(mcf.quantiles)
        logmuUpper <- logmcf.quantiles[5]
        logmuLower <- logmcf.quantiles[2]
        logTimeUpper <- log(time.quantiles[5])
        logTimeLower <- log(time.quantiles[2])
        switch(generic.nhpp.form(form)[[1]], power.rule = {
            beta <- (logmuUpper - logmuLower)/(logTimeUpper -
                logTimeLower)
            eta <- exp((beta * logTimeLower - logmuLower)/beta)
            auto.theta.start <- c(eta, beta)
            names(auto.theta.start) <- c("eta", "beta")
        }, log.linear = {
            ratediff <- (logmcf.quantiles[5] - logmcf.quantiles[4])/(time.quantiles[5] -
                time.quantiles[4]) - (logmcf.quantiles[3] - logmcf.quantiles[2])/(time.quantiles[3] -
                time.quantiles[2])
            gamma1 <- ratediff/((time.quantiles[5] + time.quantiles[4])/2 -
                (time.quantiles[3] + time.quantiles[2])/2)
            gamma0 <- (mcf.quantiles[[3]] * gamma1)/(exp(gamma1 *
                time.quantiles[3]) - 1)
            auto.theta.start <- c(gamma0, gamma1)
            names(auto.theta.start) <- c("gamma0", "gamma1")
        })
        theta.start[start.na] <- auto.theta.start[start.na]
    }
    theta.start <- f.tranparam(theta.start, model)
    model$t.param.names <- names(theta.start)
    theta.orig.start <- f.origparam(theta.start, model)
    model$orig.param.names <- names(theta.orig.start)
    if (map.SMRDDebugLevel() >= 4) {
        cat("Transformed starting values\n")
        print(theta.start)
        cat("Original starting values\n")
        print(theta.orig.start)
    }
    gmle.out <- gmle(log.like = f.loglikeNHPP, data.ld = data.rdu,
        theta.start = theta.orig.start, model = model, f.origparam = f.origparam,
        f.tranparam = f.tranparam, t.param.names = model$t.param.names,
        orig.param.names = model$orig.param.names,debug1= debug1)
    theta.hat <- gmle.out$est.out$x
    gmle.out$analysis.type <- paste("NHPP", form, "model")
    gmle.out$func.call <- func.call
    return(gmle.out)
}
