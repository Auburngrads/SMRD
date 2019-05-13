#' @export
plot.hazard <-
function (x, 
          xlim = c(NA, NA), 
          ylim = c(NA, NA),
          time.vec = NULL, 
          x.axis = "log", 
          y.axis = "log", 
          xlab = NULL,
          ylab = NULL, 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          col.ci = 4, 
          fits = F, 
          grids = F, 
          yaxis.line = NULL, 
          band.type = "p",
          lwd.ci = 3, 
          plotem = T,...)
{
    distribution <- x$distribution
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- x$theta.hat
    sigma <- theta.hat[2]
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(Response(x$data.ld))[xrna]
    if (!is.onlist(generic.relationship.name(x.axis), c("log",
        "linear")))
        stop("x.axis must be linear or log")
    
    if (generic.relationship.name(x.axis) == "log") {
      
        logx <- T
        
        `if`(is.null(time.vec),
             time <- logseq(xlim[1], xlim[2], length = 500),
             time <- time.vec)
  
        } else {
          
        logx <- F
        
        `if`(is.null(time.vec),
             time <- seq(xlim[1], xlim[2], length = 500),
             time <- time.vec)
    
        }
    if (!is.onlist(generic.relationship.name(y.axis), c("log",
        "linear")))
        stop("y.axis must be linear or log")
    
     `if`(generic.relationship.name(y.axis) == "log",
          logy <- T,
          logy <- F)
     
     `if`(generic.distribution(distribution) == "exponential",
          evdistribution <- "Weibull",
          evdistribution <- distribution)
    
    if (is.logdist(distribution)) {
        zero.time <- time == 0
        if (any(zero.time))
            warning("A specified time value is 0")
        the.time <- logb(time[!zero.time])
  
        } else {
          
          the.time <- time
          
        }
     
    z <- (the.time - theta.hat[1])/sigma
    log.haz.fun <- wqmf.phisl(z, evdistribution) - wqmf.phibml(z,
        evdistribution) - logb(sigma)
    if (is.logdist(distribution)) {
        log.haz.fun <- log.haz.fun - the.time
    }
    haz.fun <- exp(log.haz.fun)
    if (conf.level > 0.1) {
        omega <- wqmf.phip(z, evdistribution)/wqmf.phis(z, evdistribution) +
            exp(wqmf.phisl(z, evdistribution) - wqmf.phibml(z,
                evdistribution))
        zmat <- cbind(-omega/sigma, -(omega * z + 1)/sigma)
        varz <- diag(zmat %*% x$vcv %*% t(zmat))
        stderrq <- sqrt(varz)
        lower <- exp(log.haz.fun - zvalue * stderrq)
        upper <- exp(log.haz.fun + zvalue * stderrq)
    }
    if (fits) {
        lower <- lower * 10^9
        upper <- upper * 10^9
        haz.fun <- haz.fun * 10^9
    }
    if (is.null(xlab))
        xlab <- get.time.units(x$data.ld)
    ylab <- ifelse(fits, "Fit Rate", "Hazard Function")
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(strip.na(c(haz.fun, lower, upper)))[yrna]
    if (plotem) {
        plot.paper(xlim, 
                   ylim, 
                   ylab = ylab, 
                   xlab = xlab,
                   x.axis = x.axis, 
                   y.axis = y.axis, 
                   grids = grids,
                   mar = c(4.5, 5, 3.5, 2) + 0.1, 
                   yaxis.line = yaxis.line)
      
        lines(pp.data(time[!zero.time], logx), 
              pp.data(haz.fun, logy), lwd = 2)
        
        if (band.type == "p") {
            lines(pp.data(time[!zero.time], logx), 
                  pp.data(lower, logy), 
                  col = col.ci, 
                  lty = 3, 
                  lwd = lwd.ci)
            lines(pp.data(time[!zero.time], logx), 
                  pp.data(upper, logy), 
                  col = col.ci, 
                  lty = 3, 
                  lwd = lwd.ci)
        }
    }
    the.table <- cbind(Time = the.time, 
                       Hazard = haz.fun,
                       StdError = stderrq * haz.fun, 
                       Lower = lower, Upper = upper)
    
    conf.char <- 
      paste(floor(conf.level * 1000 + 0.01)/10, "%", sep = "")
    col.names <- dimnames(the.table)[[2]]
    col.names[c(4, 5)] <- c(paste(conf.char, "lower"), 
                            paste(conf.char, "upper"))
    
    dimnames(the.table) <- list(rep("", nrow(the.table)), col.names)
    invisible(the.table)
}
