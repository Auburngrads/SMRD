pgenmax <-
function (tvec, distribution, theta) 
{
    switch(generic.distribution(distribution), uniform = {
        pvec <- tvec
    }, exponential = {
        pvec <- pexp(tvec, rate = 1/theta[1])
    }, weibull = {
        pvec <- pweibull(tvec, scale = exp(theta[1]), shape = 1/theta[2])
    }, lognormal = {
        pvec <- plnorm(tvec, meanlog = theta[1], sdlog = theta[2])
    }, loglogistic = {
        pvec <- ploglogis(tvec, locationlog = theta[1], scalelog = theta[2])
    }, normal = {
        pvec <- pnorm(tvec, mean = theta[1], sd = theta[2])
    }, sev = {
        pvec <- psev(tvec, location = theta[1], scale = theta[2])
    }, logistic = {
        pvec <- plogis(tvec, location = theta[1], scale = theta[2])
    }, lev = {
        pvec <- plev(tvec, location = theta[1], scale = theta[2])
    }, gamma = {
        pvec <- pgamma(tvec/theta[1], shape = theta[2])
    }, igau = {
        pvec <- pigau(tvec, scale = theta[1], shape = theta[2])
    }, bisa = {
        pvec <- pbisa(tvec, scale = theta[1], shape = theta[2])
    }, goma = {
        pvec <- pgoma(tvec, scale = theta[1], shape = theta[2], 
            shape2 = theta[3])
    }, gng = {
        pvec <- pegengl(logb(tvec), theta[1], theta[2], theta[3])
    }, gnf = {
        pvec <- pf(tvec/theta[1], theta[2], theta[3])
    }, egengl = {
        pvec <- pegengl(tvec, theta[1], theta[2], theta[3])
    }, egeng = {
        pvec <- pegeng(tvec, theta[1], theta[2], theta[3])
    }, sevgets = {
        pvec <- pgets(tvec, theta[1], theta[2], theta[3], distribution = "sev")
    }, levgets = {
        pvec <- pgets(tvec, theta[1], theta[2], theta[3], distribution = "lev")
    }, normalgets = {
        pvec <- pgets(tvec, theta[1], theta[2], theta[3], distribution = "normal")
    }, stop(paste(distribution, "is unrecognized distribution in pgenmax()")))
    return(pvec)
}
