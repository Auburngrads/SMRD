kfactor <-
function (simulation.results, prob, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100) 
{
    distribution <- attr(simulation.results, "distribution")
    rvector <- sort((simulation.results$theta.hat[, 1] - quant(prob, 
        distribution))/simulation.results$theta.hat[, 2])
    nsim <- length(rvector)
    xindex <- conf.level * nsim + 0.5
    index1 <- floor(xindex)
    index2 <- index1 + 1
    weight <- xindex - index1
    result <- (1 - weight) * rvector[index1] + weight * rvector[index2]
    return(-result)
}
