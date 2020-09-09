loop <-
function (p.fail, m, e.of.r = c(2, 5, 10, 20, 50, 100), beta = c(0.8,
    1, 1.5, 3), mu = 0, B = 2000, kprint = 0,debug1= F)
{
    results.list <- list()
    kindex <- 0
    for (i in 1:length(e.of.r)) {
        for (j in 1:length(beta)) {
            cat("\ne.of.r=", e.of.r[i], "beta=", beta[j], "\nCensor Scheme:\n")
            censor.scheme <- get.censor.scheme.all(m = m, p.fail = p.fail,
                beta[j], e.of.r[i])
            assign(envir = .frame0, inherits = !TRUE,"tmp2.censor.scheme", censor.scheme)
            tmp.censor.scheme <- censor.scheme
            efail <- censor.scheme$number.units * pweibull(censor.scheme$censor.times,
                beta[j], scale = 1)
            print(efail)
            print(sum(efail))
            print(censor.scheme)
            results <- mlesim.mmr.stagger(numsim = B, theta = c(mu,
                1/beta[j]), censor.scheme = censor.scheme, kprint = kprint,
               debug1= debug1)
            kindex <- kindex + 1
            results.name <- paste("e.of.r=", e.of.r[i], ",beta=",
                beta[j], sep = "")
            fraction.bad <- (attr(results, "numsim") - nrow(results))/attr(results,
                "numsim")
            attr(results, "fraction.bad") <- fraction.bad
            attr(results, "e.of.r") <- e.of.r
            attr(results, "beta") <- beta
            results.attributes <- attributes(results)
            results <- as.single(results)
            attributes(results) <- results.attributes
            results.list[[results.name]] <- results
        }
    }
    return(results.list)
}
