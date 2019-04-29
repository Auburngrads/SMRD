UseRateSim <-
function (beta0, beta1, muWearUseRate, sigmaWearUseRate, sigma.a,
    number.units = 500, do.ps = T, colors = rep(0, 10))
{
    WearUseRate <- muWearUseRate + sigmaWearUseRate * rnorm(number.units)
    CrackUseRate <- beta0 + beta1 * WearUseRate + sigma.a * rnorm(number.units)
    sigmaCrackUseRate <- sqrt((beta1 * sigmaWearUseRate)^2 +
        sigma.a^2)
    rhoRR <- (beta1 * sigmaWearUseRate)/sqrt((beta1 * sigmaWearUseRate)^2 +
        sigma.a^2)
    muCrackUseRate <- beta0 + beta1 * muWearUseRate
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

    plot(WearUseRate, CrackUseRate)
    title(the.title)
    if (do.ps)
        dev.off()
    cat("here we are\n")
    invisible(list(WearUseRate = WearUseRate, CrackUseRate = CrackUseRate,
        muWearUseRate = muWearUseRate, sigmaWearUseRate = sigmaWearUseRate,
        muCrackUseRate = muCrackUseRate, sigmaCrackUseRate = sigmaCrackUseRate,
        rhoRR = rhoRR))
}
