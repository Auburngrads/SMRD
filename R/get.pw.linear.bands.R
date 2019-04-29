get.pw.linear.bands <-
function (cdfest.out, 
          band.type, 
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          a.limit = 0.001, 
          b.limit = 0.999, 
          mono.tran = T) 
{
    cdpoints.out <- cdpoints(cdfest.out)
    ok <- cdpoints.out$sdplot > 0
    times <- cdpoints.out$yplot[ok]
    fhat.point <- cdpoints.out$pplot
    dist.probs <- cdpoints.out$pplot[ok]
    stderrq <- cdpoints.out$sdplot[ok]/(dist.probs[ok] * (1 - 
        dist.probs[ok]))
    nux.squared <- cdpoints.out$number.observations * (cdpoints.out$sdplot[ok]/dist.probs)^2
    kx <- nux.squared/(1 + nux.squared)
    zvalue <- 0
    if (is.null(band.type) || band.type == "") 
        band.type <- "none"
    switch(casefold(band.type), none = {
        band.type <- "none"
    }, s = , simultaneous = {
        band.type <- "Simultaneous"
        bands.over <- kx > a.limit & kx < b.limit
        zvalue <- evalue(a = a.limit, b = b.limit, conf.level = conf.level)
    }, `Point-wise` = , pointwise = , p = , `point-wise` = {
        band.type <- "Pointwise"
        bands.over <- kx > 0 & kx < 1
        zvalue <- stats::qnorm(1 - (1 - conf.level)/2)
    },  {
        warning("band.type not recognized")
        band.type <- "none"
    })
    if (band.type == "none") {
        return(list(times = times, fhat = dist.probs, lower = NULL, 
            upper = NULL, bands.over = NULL, band.type = band.type))
    }
    lower <- stats::plogis(stats::qlogis(dist.probs) - zvalue * stderrq)
    upper <- stats::plogis(stats::qlogis(dist.probs) + zvalue * stderrq)
    if (mono.tran) {
        lower[!is.na(lower)] <- mono.lower(lower[!is.na(lower)])
        upper[!is.na(upper)] <- mono.upper(upper[!is.na(upper)])
    }
    return(list(times = times, fhat = fhat.point, lower = lower, 
        upper = upper, bands.over = bands.over, band.type = band.type))
}
