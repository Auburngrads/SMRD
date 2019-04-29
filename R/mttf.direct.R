mttf.direct <-
function (theta, distribution) 
{
    mu <- theta[1]
    sigma <- theta[2]
    switch(generic.distribution(distribution), exponential = {
        mttf <- (exp(mu))
    }, sev = {
        mttf <- (mu - 0.5772 * sigma)
    }, weibull = {
        mttf <- (exp(mu + lgamma(1 + sigma)))
    }, normal = {
        mttf <- (mu)
    }, lognormal = {
        mttf <- (exp(mu + 0.5 * sigma^2))
    }, loglogistic = {
        if (sigma < 1) mttf <- (exp(mu + lgamma(1 + sigma) + 
            lgamma(1 - sigma))) else (mttf <- (Inf))
    }, logistic = {
        mttf <- (mu)
    }, lev = {
        mttf <- (mu + 0.5772 * sigma)
    }, frechet = {
        if (sigma < 1) mttf <- (exp(mu + lgamma(1 - sigma))) else mttf <- Inf
    }, stop(paste(distribution, "is not a recognized distribution")))
    names(mttf) <- "mttf"
    return(mttf)
}
