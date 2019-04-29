SimulateSimultaneous <-
function (number.sim = 20, distribution = "Weibull", sample.size, 
    censor.type = "None", fail.number = NULL, fail.fraction = NULL, 
    True.mu = 0, TrueSigma = 1) 
{
    sim.out <- SingleDistSim(number.sim = number.sim, distribution = distribution, 
        sample.size = sample.size, censor.type = censor.type, 
        fail.number = fail.number, fail.fraction = fail.fraction, 
        theta = c(True.mu, TrueSigma), var = F)
    CensorInfo <- sim.out$CensorInfo
    muhat <- sim.out$theta.hat[, "mu"]
    sigmahat <- sim.out$theta.hat[, "sigma"]
    lsinf.out <- lsinf(z = CensorInfo$zquant, censor.type = CensorInfo$censor.type, 
        distribution = distribution)
    X2Fisher <- (sample.size/TrueSigma^2) * ((muhat - True.mu)^2 * 
        lsinf.out$f11 + 2 * (muhat - True.mu) * (sigmahat - TrueSigma) * 
        lsinf.out$f12 + (sigmahat - TrueSigma)^2 * lsinf.out$f22)
    X2FisherEst <- (sample.size/sigmahat^2) * ((muhat - True.mu)^2 * 
        lsinf.out$f11 + 2 * (muhat - True.mu) * (sigmahat - TrueSigma) * 
        lsinf.out$f12 + (sigmahat - TrueSigma)^2 * lsinf.out$f22)
    X2FisherEstAlt <- ((muhat - True.mu)^2 * sim.out$infexp[, 
        "I11"] + 2 * (muhat - True.mu) * (sigmahat - TrueSigma) * 
        sim.out$infexp[, "I12"] + (sigmahat - TrueSigma)^2 * 
        sim.out$infexp[, "I22"])
    X2Local <- ((muhat - True.mu)^2 * sim.out$infobs[, "I11"] + 
        2 * (muhat - True.mu) * (sigmahat - TrueSigma) * sim.out$infobs[, 
            "I12"] + (sigmahat - TrueSigma)^2 * sim.out$infobs[, 
        "I22"])
    browser()
    return(list(X2Fisher = X2Fisher, X2FisherEst = X2FisherEst, 
        X2FisherEstAlt = X2FisherEstAlt, X2Local = X2Local))
}
