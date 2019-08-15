TimeUseRateSim <-
function (muWearCycles, sigmaWearCycles, muCrackCycles, sigmaCrackCycles,
    UseRateSim.out, numberLab, do.ps = F, colors = rep(0, 10))
{
    numberField <- length(UseRateSim.out$WearUseRate)
    FieldDataSource <- rep("Field", numberField)
    WearCycles <- muWearCycles + rnorm(numberField) * sigmaWearCycles
    CrackCycles <- muCrackCycles + rnorm(numberField) * sigmaCrackCycles
    WearTime <- WearCycles - UseRateSim.out$WearUseRate
    CrackTime <- CrackCycles - UseRateSim.out$CrackUseRate
    muWearTime <- muWearCycles - UseRateSim.out$muWearUseRate
    muCrackTime <- muCrackCycles - UseRateSim.out$muCrackUseRate
    sigmaWearTime <- sqrt(sigmaWearCycles^2 + UseRateSim.out$sigmaWearUseRate^2)
    sigmaCrackTime <- sqrt(sigmaCrackCycles^2 + UseRateSim.out$sigmaCrackUseRate^2)
    rhoTT <- (UseRateSim.out$rhoRR * UseRateSim.out$sigmaWearUseRate *
        UseRateSim.out$sigmaCrackUseRate)/(sigmaWearTime * sigmaCrackTime)
    cat("muCrackTime=", format(muCrackTime), "muCrackTimehat=",
        format(mean(CrackTime)), "\n")
    cat("sigmaCrackTime=", format(sigmaCrackTime), "sigmaCrackTimehat=",
        format(sqrt(var(CrackTime))), "\n")
    cat("muWearTime=", format(muWearTime), "muWearTimehat=",
        format(mean(WearTime)), "\n")
    cat("sigmaWearTime=", format(sigmaWearTime), "sigmaWearTimehat=",
        format(sqrt(var(WearTime))), "\n")
    cat("rhoTT=", format(rhoTT), "rhoTThat=", format(cor(WearTime,
        CrackTime)), "\n")
    the.title <- paste("Failure Times\n medians=", format(exp(muCrackTime)),
        format(exp(muWearTime)), "sigma=", format(sigmaCrackTime),
        format(sigmaWearTime), "rhoTT=", format(rhoTT))

    plot.paper(x = exp(range(WearTime)), y = exp(range(CrackTime)),
        x.axis = "log", y.axis = "log", grids = F, my.title = "",
        ylab = "Crack Time", xlab = "Wear Time")
    if (!do.ps)
        title(the.title)
    CrackFail <- CrackTime < WearTime
    points(WearTime[!CrackFail], CrackTime[!CrackFail], col = 2)
    points(WearTime[CrackFail], CrackTime[CrackFail])
    abline(0, 1)
    text(x.loc(0.7), y.loc(0.3), "Crack Failures", col = 1)
    text(x.loc(0.4), y.loc(0.7), "Wear Failures", col = 2)
    if (do.ps)
        dev.off()
    LabWearCycles <- muWearCycles + rnorm(numberLab) * sigmaWearCycles
    LabCrackCycles <- muCrackCycles + rnorm(numberLab) * sigmaCrackCycles
    LabDataSource <- rep("Lab", numberLab)
    invisible(data.frame(WearTime = c(WearTime, LabWearCycles),
        CrackTime = c(CrackTime, LabCrackCycles), DataSource = c(FieldDataSource,
            LabDataSource)))
}
