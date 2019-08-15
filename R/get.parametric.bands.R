get.parametric.bands <-
function (mlest.out,
          xlim = range(Response(mlest.out$data.ld)),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          number.times = 50,
          mono.tran = T,
          shape = NULL,
          need.list = T) 
{
    distribution <- mlest.out$distribution
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    theta.hat <- mlest.out$theta.hat
    sigma <- theta.hat[2]
    
    if (need.list) {
        
        if (is.logdist(distribution)) {
            
            ltimes <- seq(logb(xlim[1]), logb(xlim[2]), length = number.times)
            times <- exp(ltimes)
            
        } else {
            
            ltimes <- seq(xlim[1], xlim[2], length = number.times)
            times <- ltimes
            
        }
        
    } else {
        
        `if`(is.logdist(distribution),
             { ltimes <- logb(xlim) ; times <- exp(ltimes) },
             { ltimes <- xlim       ; times <- ltimes      })
        
        
    }
    
    `if`(generic.distribution(distribution) == "exponential",
         evdistribution <- "Weibull",
         evdistribution <- distribution)
    
    z <- (ltimes - theta.hat[1]) / sigma
    fhat.orig <- wqmf.phibf(z, evdistribution)
    zgood <- fhat.orig > 0 & fhat.orig < 1
    ltimes <- ltimes[zgood]
    times <- times[zgood]
    fhat <- fhat.orig[zgood]
    z <- z[zgood]
    
    if (length(z) <= 0) {
        
        cat("\nProbabilities out of bounds in get.parametric.bands\n", 
            fhat.orig, "\n\n")
        return(NULL)
        
    }
    
    dist.quantiles <- quant(fhat, distribution)
    zmat <- cbind(1, z)
    the.order <- order(times)
    if (conf.level > 0.1) {
        varz <- diag(zmat %*% mlest.out$vcv %*% t(zmat))/(sigma^2)
        negative.varz <- varz < 0
        if (any(negative.varz)) {
            warning("Negative varinaces detected in get.parametric.bands.\nCheck for lack of proper convergence, possibly caused by a poor data/model specification.\nConfidence intervals will not be plotted.")
            varz[negative.varz] <- 0
        }
        stderror <- sqrt(varz) * wqmf.phis(z, evdistribution)
        stderrq <- stderror/(fhat * (1 - fhat))
        lower <- plogis(qlogis(fhat) - zvalue * stderrq)
        upper <- plogis(qlogis(fhat) + zvalue * stderrq)
        if (mono.tran) {
            lower[the.order] <- mono.lower(lower[the.order])
            upper[the.order] <- mono.upper(upper[the.order])
        }
        band.type <- "parametric pointwise"
    }
    else {
        lower <- NULL
        upper <- NULL
        band.type <- "none"
        stderror <- NULL
    }
    bands.over <- rep(T, length(fhat))
    return.list <- list(times = times, fhat = fhat, stderror = stderror, 
        lower = lower, upper = upper, bands.over = bands.over, 
        band.type = band.type)
    return(return.list)
}
