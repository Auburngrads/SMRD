noise.sample.size <-
function (mu.vec = 63:70, sigma.vec = seq(2, 3, by = 0.5), n.vec = c(5,
    10, 15, 20, 25, 30), quant.to.dem = 0.015, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100)
{
    for (i in 1:length(sigma.vec)) {
        results <- matrix(NA, nrow = length(mu.vec) * length(n.vec),
            ncol = 4)
        dimnames(results) <- list(rep(" ", length = nrow(results)),
            c("sample.size", "mean", "Std.Dev", "PrSuccDemo"))
        row.now <- 0
        for (j in 1:length(n.vec)) {
            for (k in 1:length(mu.vec)) {
                row.now <- row.now + 1
                actual.reliability <- pnorm((71 - mu.vec[k])/sigma.vec[i])
                the.prob <- prsd(sample.size = n.vec[j], dem.reliability = 1 -
                  quant.to.dem, actual.reliability = actual.reliability)
                if (the.prob < 0.01)
                  the.prob <- 0
                results[row.now, 3] <- sigma.vec[i]
                results[row.now, 4] <- the.prob
                results[row.now, 1] <- n.vec[j]
                results[row.now, 2] <- mu.vec[k]
            }
        }
        cat("Probability of Successful", percent.conf.level(conf.level),
            " Demonstration  \nthat the", quant.to.dem, " quantile of the noise distribution \nis less than 71 dBa\n")
        print(results)
    }
}
