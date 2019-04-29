prob.success.demo <-
function (simulation.results, qprob, qstar.range = c(qprob, 0.99999), 
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, number.plotted.points = 100) 
{
    qstar.vec <- seq(qstar.range[1], qstar.range[2], length = number.plotted.points)
    distribution <- attr(simulation.results, "distribution")
    the.kfactor <- kfactor(simulation.results, prob = 1 - qprob, 
        conf.level = conf.level)
    rvector <- sort(1 - wqmf.phibf(simulation.results$theta.hat[, 
        1] + the.kfactor * simulation.results$theta.hat[, 2], 
        distribution))
    nsim <- length(rvector)
    results <- approx(x = rvector, y = (1:nsim)/nsim, xout = qstar.vec)
    good.ones <- !is.na(results$y)
    return.list <- list(qstar.vec = results$x[good.ones], prsd = results$y[good.ones])
    the.attributes <- attributes(simulation.results)
    the.attributes[["nsim"]] <- length(rvector)
    the.attributes[["names"]] <- attr(return.list, "names")
    attributes(return.list) <- the.attributes
    return(return.list)
}
