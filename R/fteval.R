fteval <-
function (kdmod = 1, mu1, sig1, mu2, sig2, rho, df = -0.5, d0 = 0,
    sfact = 0, tf, kprint = 0,debug1= F)
{
    max.length <- max(length(mu1), length(sig1), length(mu2),
        length(sig2), length(rho), length(df), length(d0), length(sfact),
        length(tf))
    mu1 <- expand.vec(mu1, max.length)
    sig1 <- expand.vec(sig1, max.length)
    mu2 <- expand.vec(mu2, max.length)
    sig2 <- expand.vec(sig2, max.length)
    rho <- expand.vec(rho, max.length)
    df <- expand.vec(df, max.length)
    d0 <- expand.vec(d0, max.length)
    sfact <- expand.vec(sfact, max.length)
    tf <- expand.vec(tf, max.length)
    zout <- .Fortran("sfteval", as.integer(kdmod), as.double(mu1),
        as.double(sig1), as.double(mu2), as.double(sig2), as.double(rho),
        as.double(df), as.double(d0), as.double(sfact), as.double(tf),
        as.integer(max.length), answer = double(max.length),
        ier = integer(max.length), as.integer(kprint))
    if (debug1)
        browser()
    if (mean(zout$ier) > 0) {
        warning(paste("Warning in fnprod mean(ier)=", mean(zout$ier)))
        browser()
        bad.stuff <- rbind(bad.stuff, cbind(mu1[zout$ier > 0],
            sig1[zout$ier > 0], mu2[zout$ier > 0], sig2[zout$ier >
                0], rho[zout$ier > 0], tf[zout$ier > 0], zout$ier[zout$ier >
                0]))
    }
    return(zout$answer)
}
