use.rate.rcen.contrib.lognormal <-
function (yresp, use.rate.model) 
{
    TheFieldParameters <- FieldParameters(use.rate.model)
    log(bvn(mu1 = TheFieldParameters$Field.muWear, mu2 = TheFieldParameters$Field.muCrack, 
        sd1 = TheFieldParameters$Field.sigmaWear, sd2 = TheFieldParameters$Field.sigmaCrack, 
        rho = TheFieldParameters$rhoTT, ah = yresp, ak = yresp))
}
