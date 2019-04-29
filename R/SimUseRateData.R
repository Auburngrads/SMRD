SimUseRateData <-
function (UseRateModel, muWearUseRate, sigmaWearUseRate, beta0, 
    beta1, sigma.a, muRatio, sigmaRatio, number.units = 1000, 
    muWearCycles, sigmaWearCycles, muCrackCycles, sigmaCrackCycles, 
    numberLab = 100, censor.time, do.ps = F, colors = rep(0, 
        10)) 
{
    switch(UseRateModel, `C~beta0*W^beta1` = {
        UseRateSim.out <- UseRateSim(muWearUseRate = muWearUseRate, 
            sigmaWearUseRate = sigmaWearUseRate, beta0 = beta0, 
            beta1 = beta1, sigma.a = sigma.a, number.units = number.units, 
            do.ps = do.ps, colors = colors)
        model.string <- paste("Wear~Crack Regression Use-Rate Model rhoRR =", 
            format(UseRateSim.out$rhoRR))
    }, `W~C` = , `C~W` = {
        UseRateSim.out <- UseRateSim(muWearUseRate = muWearUseRate, 
            sigmaWearUseRate = sigmaWearUseRate, beta0 = beta0, 
            beta1 = 0, sigma.a = sigma.a, number.units = number.units, 
            do.ps = do.ps, colors = colors)
        model.string <- paste("Wear~Crack Independent Use-Rate Model rhoRR = 0")
    }, `W~(C/W)` = {
        UseRateSim.out <- UseRateSim2(muWearUseRate = muWearUseRate, 
            sigmaWearUseRate = sigmaWearUseRate, muRatio = muRatio, 
            sigmaRatio = sigmaRatio, number.units = number.units, 
            do.ps = do.ps, colors = colors)
        model.string <- paste("Wear~Ratio=Crack/Wear Independent Use-Rate Model rhoRR =", 
            format(UseRateSim.out$rhoRR))
    }, {
        stop(paste(UseRateModel, "UseRateModel not recognized", 
            "\n"))
    })
    BVNData <- TimeUseRateSim(muWearCycles = muWearCycles, sigmaWearCycles = sigmaWearCycles, 
        muCrackCycles = muCrackCycles, sigmaCrackCycles = sigmaCrackCycles, 
        UseRateSim.out = UseRateSim.out, numberLab = numberLab, 
        do.ps = do.ps, colors = colors)
    TmpSimApplianceX <- ModeCensorBVNData(BVNData, censor.time = censor.time, 
        do.ps = do.ps, colors = colors)
    TmpSimApplianceX.ld <- frame.to.ld(TmpSimApplianceX, response.column = "Time", 
        censor.column = "Status", case.weight.column = "Weights", 
        failure.mode.column = "FailureMode", x.columns = "DataSource", 
        data.title = "Simulated UseRate Data")
    attr(TmpSimApplianceX.ld, "model.string") <- model.string
    return(TmpSimApplianceX.ld)
}
