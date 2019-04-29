wqmf.phibml <-
function (z, distribution) 
{
    switch(generic.distribution(distribution), weibull = , sev = {
        pvec <- -exp(z)
    }, frechet = , lev = {
        pvec <- logb(1 - exp(-exp(-z)))
    }, uniform = {
        pvec <- logb(1 - z)
    }, normal = , lognormal = {
        pvec <- logb(pnorm(-z))
    }, loglogistic = , logistic = {
        pvec <- logb(plogis(-z))
    }, exponential = {
        pvec <- -z
    }, stop(paste(distribution, "is unrecognized distribution in wqmf.phibf")))
    return(pvec)
}
