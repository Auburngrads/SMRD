wqmf.phibm <-
function (z, distribution) 
{
    switch(generic.distribution(distribution), weibull = , sev = {
        pvec <- exp(-exp(z))
    }, frechet = , lev = {
        pvec <- 1 - exp(-exp(-z))
    }, uniform = {
        pvec <- 1 - z
    }, normal = , lognormal = {
        pvec <- pnorm(-z)
    }, loglogistic = , logistic = {
        pvec <- plogis(-z)
    }, exponential = {
        pvec <- exp(-z)
    }, stop(paste(distribution, "is unrecognized distribution in wqmf.phibf")))
    return(pvec)
}
