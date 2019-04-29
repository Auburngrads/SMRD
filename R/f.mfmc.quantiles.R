f.mfmc.quantiles <-
function (multiple.mlest.out, prob.vec, time.range, do.se = T) 
{
    theta.hat <- unlist(lapply(multiple.mlest.out, function(sublist) {
        sublist[["theta.hat"]]
    }))
    vcv.list <- lapply(multiple.mlest.out, function(sublist) {
        sublist[["vcv.matrix"]]
    })
    vcv.theta <- matrix(0, ncol = length(theta.hat), nrow = length(theta.hat))
    theta.index.low <- 1
    for (i in 1:length(vcv.list)) {
        theta.index.high <- theta.index.low + length(multiple.mlest.out[[i]]$theta.hat) - 
            1
        vcv.theta[theta.index.low:theta.index.high, theta.index.low:theta.index.high] <- vcv.list[[i]]
        theta.index.low <- theta.index.high + 1
    }
    names(theta.hat) <- paste(rep(c("mu", "sigma"), length(multiple.mlest.out)), 
        names(multiple.mlest.out), sep = ";")
    dimnames(vcv.theta) <- list(names(theta.hat), names(theta.hat))
    if (do.se) 
        f.gendeltamethod(vcv.theta, theta.hat, fx.mfmc.quantiles, 
            multiple.mlest.out = multiple.mlest.out, prob.vec = prob.vec, 
            time.range = time.range)
    else fx.mfmc.quantiles(theta.hat, multiple.mlest.out, prob.vec = prob.vec, 
        time.range = time.range)
}
