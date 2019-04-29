use.rate.fail.contrib.lognormal.density <-
function (yresp, failure.mode, use.rate.model, eps = 0.002) 
{
    TheFieldParameters <- FieldParameters(use.rate.model)
    switch(failure.mode, Wear = {
        mu.failure <- TheFieldParameters$Field.muWear
        sigma.failure <- TheFieldParameters$Field.sigmaWear
        mu.censor <- TheFieldParameters$Field.muCrack
        sigma.censor <- TheFieldParameters$Field.sigmaCrack
    }, Crack = {
        mu.failure <- TheFieldParameters$Field.muCrack
        sigma.failure <- TheFieldParameters$Field.sigmaCrack
        mu.censor <- TheFieldParameters$Field.muWear
        sigma.censor <- TheFieldParameters$Field.sigmaWear
    }, {
        stop(paste("Unrecognized failure mode = ", failure.mode))
    })
    zden <- (yresp - mu.failure)/sigma.failure
    mu.cen.given.failure <- mu.censor + TheFieldParameters$rhoTT * 
        sigma.censor * zden
    sigma.cen.given.failure <- sqrt(1 - TheFieldParameters$rhoTT^2) * 
        sigma.censor
    zsf <- (yresp - mu.cen.given.failure)/sigma.cen.given.failure
    the.answer <- -log(sigma.failure) + wqmf.phisl(zden, "normal") + 
        wqmf.phibml(zsf, "normal") + log(2 * eps)
    return(the.answer)
}
