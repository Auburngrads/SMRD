gquant <-
function (p, distribution, shape, shape2, scale = 1)
{
    if (any(p < 0) | any(p > 1)) stop("quantiles must be between 0 and 1")
    switch(generic.distribution(distribution), exponential = {
        qvec <- qexp(p) * scale
    }, weibull = {
        qvec <- qweibull(p, shape) * scale
    }, lognormal = {
        qvec <- qlnorm(p, meanlog = logb(scale), sdlog = shape)
    }, loglogistic = {
        qvec <- qloglogis(p, locationlog = logb(scale), scalelog = shape)
    }, gamma = {
        qvec <- qgamma(p, shape) * scale
    }, igau = {
        qvec <- qigau(p, shape) * scale
    }, bisa = {
        qvec <- qbisa(p, shape) * scale
    }, goma = {
        qvec <- qgoma(p, shape[1], shape[2]) * scale
    }, gng = {
        qqgqm.now <- qgamma(p, shape)
        positive.qqgqm.now <- qqgqm.now > 0
        log.qqgqm.now <- rep(-Inf, length(qqgqm.now))
        log.qqgqm.now[positive.qqgqm.now] <- logb(qqgqm.now[positive.qqgqm.now]) * scale
        qvec <- log.qqgqm.now
    }, gnf = {
        qvec <- qf(p, shape[1], shape[2]) * scale
    }, stop(paste(distribution, "is unrecognized distribution in gquant()")))
    
    return(qvec)
}
