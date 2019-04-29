get.npbands <-
function (cdfest.out, band.type, how.show.interval = "step.fun", 
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, a.limit = 0.001, 
    b.limit = 0.999) 
{
    switch(how.show.interval, sf = , step.fun = {
        bands <- get.step.bands(cdfest.out, band.type = band.type, 
            conf.level = conf.level, a.limit = a.limit, b.limit = b.limit)
    }, pwlf = , pw.linear.fun = {
        bands <- get.pw.linear.bands(cdfest.out, band.type = band.type, 
            conf.level = conf.level, a.limit = a.limit, b.limit = b.limit)
    }, stop(paste("how.show.interval variable not recognized \n", 
        "Must use step.fun (sf) or pw.linear.fun (pwlf)")))
    return(bands)
}
