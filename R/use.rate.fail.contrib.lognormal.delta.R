use.rate.fail.contrib.lognormal.delta <-
function (yresp, failure.mode, use.rate.model, eps = 1.00000000000001e-05) 
{
    TheFieldParameters <- FieldParameters(use.rate.model)
    switch(failure.mode, Wear = {
        lower <- bvn(mu1 = TheFieldParameters$Field.muWear, mu2 = TheFieldParameters$Field.muCrack, 
            sd1 = TheFieldParameters$Field.sigmaWear, sd2 = TheFieldParameters$Field.sigmaCrack, 
            rho = TheFieldParameters$rhoTT, ah = yresp - eps, 
            ak = yresp)
        upper <- bvn(mu1 = TheFieldParameters$Field.muWear, mu2 = TheFieldParameters$Field.muCrack, 
            sd1 = TheFieldParameters$Field.sigmaWear, sd2 = TheFieldParameters$Field.sigmaCrack, 
            rho = TheFieldParameters$rhoTT, ah = yresp + eps, 
            ak = yresp)
    }, Crack = {
        lower <- bvn(mu1 = TheFieldParameters$Field.muCrack, 
            mu2 = TheFieldParameters$Field.muWear, sd1 = TheFieldParameters$Field.sigmaCrack, 
            sd2 = TheFieldParameters$Field.sigmaWear, rho = TheFieldParameters$rhoTT, 
            ah = yresp - eps, ak = yresp)
        upper <- bvn(mu1 = TheFieldParameters$Field.muCrack, 
            mu2 = TheFieldParameters$Field.muWear, sd1 = TheFieldParameters$Field.sigmaCrack, 
            sd2 = TheFieldParameters$Field.sigmaWear, rho = TheFieldParameters$rhoTT, 
            ah = yresp + eps, ak = yresp)
    }, {
        stop(paste("Unrecognized failure mode = ", failure.mode))
    })
    return(log(lower - upper))
}
