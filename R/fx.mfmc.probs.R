fx.mfmc.probs <-
function (theta.hat, multiple.mlest.out, time.vec) 
{
    fmhat.series <- rep(1, length(time.vec))
    theta.index.low <- 1
    for (i in 1:length(multiple.mlest.out)) {
      
        theta.index.high <- theta.index.low + 
                            length(multiple.mlest.out[[i]]$theta.hat) - 1
        
        theta.hat.names <- names(multiple.mlest.out[[i]]$theta.hat)
        replace.theta.hat <- theta.hat[theta.index.low:theta.index.high]
        names(replace.theta.hat) <- theta.hat.names
        multiple.mlest.out[[i]]$theta.hat <- replace.theta.hat
        theta.index.low <- theta.index.high + 1
        
        cdfhat.out <- cdfhat(multiple.mlest.out[[i]], 
                                    time.vec = time.vec)
        
        where.na <- is.na(cdfhat.out$fhat)
        fmhat.series[!where.na] <- fmhat.series[!where.na] * 
                                   (1 - cdfhat.out$fhat[!where.na])
    }
    return(1 - fmhat.series)
}
