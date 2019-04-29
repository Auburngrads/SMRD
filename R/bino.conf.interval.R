bino.conf.interval <-
function (n, d, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
    printem = T) 
{
    alpha <- 1 - conf.level
    phat <- d/n
    if (d > 0) {
        bci.lower <- (1 + ((n - d + 1) * qf(1 - alpha/2, 2 * 
            n - 2 * d + 2, 2 * d))/d)^(-1)
    }
    else {
        bci.lower <- 0
        phat <- 1/(n)
    }
    if (d < n) {
        bci.upper <- (1 + (n - d)/((d + 1) * qf(1 - alpha/2, 
            2 * d + 2, 2 * n - 2 * d)))^(-1)
    }
    else {
        bci.upper <- 1
        phat <- 1/(n)
    }
    if (printem) {
        cat("\nBased on ", d, " out of ", n, " failures:\n")
        cat("\nPoint estimate = ", format(phat), sep = "")
        cat("\nA conservative ", 100 * conf.level, "% confidence interval = [", 
            format(bci.lower), ", ", format(bci.upper), "]\n\n", 
            sep = "")
    }
    return(c(bci.lower = bci.lower, bci.upper = bci.upper))
}
