f.mfmc.probs <-
function (multiple.mlest.out, time.vec, do.se = T) 
{
    theta.hat <- 
      unlist(lapply(X = multiple.mlest.out, 
                    FUN = function(sublist) { sublist[["theta.hat"]] }))
    vcv.list <- 
      lapply(X = multiple.mlest.out, 
             FUN = function(sublist) { sublist[["vcv.matrix"]] })
    
    vcv.theta <- matrix(0, 
                        ncol = length(theta.hat), 
                        nrow = length(theta.hat))
    theta.index.low <- 1
    for (i in 1:length(vcv.list)) {
        theta.index.high <- theta.index.low + length(multiple.mlest.out[[i]]$theta.hat) - 
            1
        vcv.theta[theta.index.low:theta.index.high, theta.index.low:theta.index.high] <- vcv.list[[i]]
        theta.index.low <- theta.index.high + 1
    }
    if (do.se) 
        results <- f.gendeltamethod(vcv.theta = vcv.theta, 
                                    theta = theta.hat, 
                                    gfun = fx.mfmc.probs, 
                                    multiple.mlest.out, 
                                    time.vec)
    else results <- fx.mfmc.probs(theta.hat, 
                                  multiple.mlest.out, 
                                  time.vec = time.vec)
    return(results)
}
