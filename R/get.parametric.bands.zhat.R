get.parametric.bands.zhat <-
function (mlest.out, 
          xlim = range(Response(mlest.out$data.ld)), 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          number.times = 50, 
          mono.tran = F, 
          shape = NULL, 
          need.list = T) 
{
    distribution <- mlest.out$distribution
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- mlest.out$theta.hat
    sigmahat <- theta.hat[2]
    
    if (need.list) {
      
        if (is.logdist(distribution)) {
          
            ltimes <- seq(logb(xlim[1]), 
                          logb(xlim[2]), 
                          length = number.times)
            times <- exp(ltimes)
            
      } else {
        
            ltimes <- seq(xlim[1], 
                          xlim[2], 
                          length = number.times)
            times <- ltimes
      }
      
  } else {
    
        if (is.logdist(distribution)) {
          
            ltimes <- logb(xlim)
            times <- exp(ltimes)
            
      } else {
        
            ltimes <- xlim
            times <- ltimes
            
      }
    
  }
    
    `if`(generic.distribution(distribution) == "exponential",
         evdistribution <- "Weibull",
         evdistribution <- distribution)
    
    zhat <- (ltimes - theta.hat[1])/sigmahat
    fhat.orig <- wqmf.phibf(zhat, evdistribution)
    zhatgood <- fhat.orig > 0 & fhat.orig < 1
    ltimes <- ltimes[zhatgood]
    times <- times[zhatgood]
    fhat <- fhat.orig[zhatgood]
    zhat <- zhat[zhatgood]
    if (length(zhat) <= 0) {
        cat("\nProbabilities out of bounds in get.parametric.bands\n", 
            fhat.orig, "\n\n")
        return(NULL)
    }
    dist.quantiles <- quant(fhat, distribution)
    zhatmat <- cbind(1, zhat)
    the.order <- order(times)
    if (conf.level > 0.1) {
        varzhat <- diag(zhatmat %*% mlest.out$vcv %*% t(zhatmat))/(sigmahat^2)
        negative.varzhat <- varzhat < 0
        if (any(negative.varzhat)) {
            warning("Negative varinaces detected in get.parametric.bands.\nCheck for lack of proper convergence, possibly caused by a poor data/model specification.\nConfidence intervals will not be plotted.")
            varzhat[negative.varzhat] <- 0
        }
        stderror.z <- sqrt(varzhat)
        stderror.f <- stderror.z * wqmf.phis(zhat, evdistribution)
        zlower <- zhat - zvalue * stderror.z
        zupper <- zhat + zvalue * stderror.z
        lower <- wqmf.phibf(zlower, evdistribution)
        upper <- wqmf.phibf(zupper, evdistribution)
        if (mono.tran) {
            lower[the.order] <- mono.lower(lower[the.order])
            upper[the.order] <- mono.upper(upper[the.order])
        }
        band.type <- "parametric pointwise"
  } else {
        lower <- NULL
        upper <- NULL
        band.type <- "none"
        stderror.f <- NULL
    }
    bands.over <- rep(T, length(fhat))
    return.list <- list(times = times, fhat = fhat, stderror.f = stderror.f, 
        lower = lower, upper = upper, bands.over = bands.over, 
        band.type = band.type)
    return(return.list)
}
