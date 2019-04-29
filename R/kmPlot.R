kmPlot <-
function (data.ld)
{
    cdfest.out <- cdfest(data.ld)
    xlim <- range(cdfest.out$q[abs(cdfest.out$q) < 1e+09],
        cdfest.out$p[abs(cdfest.out$p) < 1e+09])
    plot.paper(c(0, 400), c(0, 1), xlab = "Days", ylab = "Survival",
        grids = F)
    over.interval <- !(cdfest.out$p == cdfest.out$q)
    cdfest.out$q[cdfest.out$q > 10^20] <- 1.1 * xlim[2]
    cdfest.out$p[cdfest.out$p < 0] <- 0
    print(cdfest.out)
    segments(cdfest.out$p[over.interval], 1 - cdfest.out$prob[over.interval],
        cdfest.out$q[over.interval], 1 - cdfest.out$prob[over.interval])
    the.median <- approx(x = 1 - cdfest.out$prob[over.interval],
        y = cdfest.out$p[over.interval], xout = 0.5)$y
    L90 <- approx(x = 1 - cdfest.out$prob[over.interval], y = cdfest.out$p[over.interval],
        xout = 0.1)$y
    cat("The median=", the.median, "\n")
    cat("L90=", L90, "\n")
    return(c(the.median = the.median, L90 = L90, number.cases = summary(data.ld)$number.cases))
}
