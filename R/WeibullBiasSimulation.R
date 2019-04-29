WeibullBiasSimulation <-
function (n.vec = c(10, 20, 50, 100, 200, 500, 1000), beta.vec = c(0.5, 
    1, 2, 5), censor.type = "Type 1", fail.fraction = 0.5, number.sim = 1000, 
    distribution = "Weibull") 
{
    results <- matrix(NA, nrow = length(n.vec), ncol = length(beta.vec))
    for (i in 1:length(n.vec)) {
        for (j in 1:length(beta.vec)) {
            switch(censor.type, `Type 1` = {
                sim.results <- SingleDistSim(number.sim = number.sim, 
                  distribution = distribution, theta = c(mu = 0, 
                    sigma = 1/beta.vec[j]), sample.size = n.vec[i], 
                  censor.type = "Type 1", fail.fraction = fail.fraction)
            }, `Type 2` = {
                fail.number <- floor(fail.fraction * n.vec[i])
                cat(" fail.number =", i, j, fail.number, "\n")
                sim.results <- SingleDistSim(number.sim = number.sim, 
                  distribution = distribution, theta = c(mu = 0, 
                    sigma = 1/beta.vec[j]), sample.size = n.vec[i], 
                  censor.type = "Type 2", fail.number = fail.number)
            }, {
                stop("censor.type not recognized")
            })
            results[i, j] <- mean(1/strip.na(sim.results$theta.hat[, 
                2])) - beta.vec[j]
            results[i, j] <- mean(strip.na(sim.results$theta.hat[, 
                2])) - 1/beta.vec[j]
        }
    }
    plot.paper(range(n.vec), range(results), xlab = "Sample Size", 
        ylab = "Bias")
    title(paste(distribution, "Distribution Bias with", 100 * 
        fail.fraction, "Percent Censoring"))
    for (j in 1:length(beta.vec)) {
        lines(n.vec, smooth(results[, j]), col = j)
    }
}
