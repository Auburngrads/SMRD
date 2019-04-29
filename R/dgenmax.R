dgenmax <-
function (tvec, distribution, theta, ...) 
{
    switch(generic.distribution(distribution), exponential = {
        pvec <- dexp(tvec, rate = 1/exp(theta[1]))
    }, weibull = {
        pvec <- dweibull(tvec, scale = exp(theta[1]), shape = 1/theta[2])
    }, lognormal = {
        pvec <- dlnorm(tvec, meanlog = theta[1], sdlog = theta[2])
    }, loglogistic = {
        pvec <- dloglogis(tvec, locationlog = theta[1], scalelog = theta[2])
    }, normal = {
        pvec <- dnorm(tvec, mean = theta[1], sd = theta[2])
    }, sev = {
        pvec <- dsev(tvec, location = theta[1], scale = theta[2])
    }, logistic = {
        pvec <- dlogis(tvec, location = theta[1], scale = theta[2])
    }, lev = {
        pvec <- dlev(tvec, location = theta[1], scale = theta[2])
    }, gamma = {
        pvec <- dgamma(tvec/theta[1], shape = theta[2])/theta[1]
    }, igau = {
        pvec <- digau(tvec, scale = theta[1], shape = theta[2])
    }, bisa = {
        pvec <- dbisa(tvec, scale = theta[1], shape = theta[2])
    }, goma = {
        pvec <- dgoma(tvec, scale = theta[1], shape = theta[2], 
            shape2 = theta[3])
    }, gng = {
        pvec <- degengl(logb(tvec), theta[1], theta[2], theta[3])/tvec
    }, gnf = {
        pvec <- df(tvec/theta[1], theta[2], theta[3])
    }, egengl = {
        pvec <- degengl(tvec, theta[1], theta[2], theta[3])
    }, egeng = {
        pvec <- degeng(tvec, theta[1], theta[2], theta[3])
    }, sevgets = {
        pvec <- dgets(tvec, theta[1], theta[2], theta[3], distribution = "sev")
    }, normalgets = {
        pvec <- dgets(tvec, theta[1], theta[2], theta[3], distribution = "normal")
    }, stop(paste(distribution, "is unrecognized distribution in dgenmax()")))
    return(pvec)
}
