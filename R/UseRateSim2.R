UseRateSim2 <-
function (muWearUseRate, sigmaWearUseRate, muRatio, sigmaRatio,
    number.units = 500, do.ps = F, colors = rep(0, 10))
{
    WearUseRate <- muWearUseRate + sigmaWearUseRate * rnorm(number.units)
    Ratio <- muRatio + sigmaRatio * rnorm(number.units)
    CrackUseRate <- WearUseRate + Ratio
    sigmaCrackUseRate <- sqrt(sigmaWearUseRate^2 + sigmaRatio^2)
    rhoRR <- sigmaWearUseRate/sigmaCrackUseRate
    muCrackUseRate <- muWearUseRate + muRatio
    cat("muCrackUseRate=", format(muCrackUseRate), "muCrackUseRatehat=",
        format(mean(CrackUseRate)), "\n")
    cat("sigmaCrackUseRate=", format(sigmaCrackUseRate), "sigmaCrackUseRatehat=",
        format(sqrt(var(CrackUseRate))), "\n")
    cat("muWearUseRate=", format(muWearUseRate), "muWearUseRatehat=",
        format(mean(WearUseRate)), "\n")
    cat("sigmaWearUseRate=", format(sigmaWearUseRate), "sigmaWearUseRatehat=",
        format(sqrt(var(WearUseRate))), "\n")
    cat("rhoRR=", format(rhoRR), "rhoRRhat=", format(cor(WearUseRate,
        CrackUseRate)), "\n")
    the.title <- paste("log(UseRate)\n mu=", paste(format(muCrackUseRate),
        format(muWearUseRate), "sigma=", format(muCrackUseRate),
        format(sigmaWearUseRate), "rhoRR=", format(rhoRR), "\n",
        sep = ","))

    plot.paper(range(exp(WearUseRate)), range(exp(CrackUseRate)),
        x.axis = "log", y.axis = "log", grids = F, xlab = "Wear Use Rate",
        ylab = "Crack Use Rate")
    points(WearUseRate, CrackUseRate)
    if (!do.ps)
        title(the.title)
    if (do.ps)
        dev.off()
    invisible()
    return(list(WearUseRate = WearUseRate, CrackUseRate = CrackUseRate,
        muWearUseRate = muWearUseRate, sigmaWearUseRate = sigmaWearUseRate,
        muCrackUseRate = muCrackUseRate, sigmaCrackUseRate = sigmaCrackUseRate,
        rhoRR = rhoRR))
}
