mlhaz <-
  function (x, xlim = c(NA, NA), ylim = c(NA, NA),
            time.vec = NULL, x.axis = "log", y.axis = "log", xlab = NULL,
            ylab = NULL, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            col.ci = 4, fits = F, grids = F, yaxis.line = NULL, band.type = "p",
            lwd.ci = 3,...)
  {
    distribution <- x$distribution
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- x$theta.hat
    sigma <- theta.hat[2]
    xrna <- is.na(xlim)
    if (any(xrna))
      xlim[xrna] <- range(Response(x$data.ld))[xrna]
    
      if (is.null(time.vec)) {
        if (is.logdist(distribution))
          time <- as.numeric(logax(xlim)$ticlab)
        else time <- as.numeric(linax(xlim)$ticlab)
      } else {
        
        time = time.vec
      }
    
  if (generic.distribution(distribution) == "exponential") {
      evdistribution <- "Weibull"
    } else {
      evdistribution <- distribution
    }
    
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
    
    the.table <- cbind(the.time, Hazard = haz.fun,
                       StdError = stderrq * haz.fun, Lower = lower, Upper = upper)
    conf.char <- paste(floor(conf.level * 1000 + 0.01)/10, "%",
                       sep = "")
    col.names <- dimnames(the.table)[[2]]
    col.names[c(4, 5)] <- c(paste(conf.char, "lower"), paste(conf.char,
                                                             "upper"))
    dimnames(the.table) <- list(rep("", nrow(the.table)), col.names)
    return(the.table)
  }
