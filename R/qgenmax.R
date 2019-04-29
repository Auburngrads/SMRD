qgenmax <-
function (quantile, distribution, theta) 
{
    switch(generic.distribution(distribution), uniform = {
        qvec <- quantile
    }, exponential = {
        qvec <- qexp(quantile, rate = 1/exp(theta[1]))
    }, weibull = {
        qvec <- qweibull(quantile, scale = exp(theta[1]), shape = 1/theta[2])
    }, lognormal = {
        qvec <- qlnorm(quantile, meanlog = theta[1], sdlog = theta[2])
    }, loglogistic = {
        qvec <- qloglogis(quantile, locationlog = theta[1], scalelog = theta[2])
    }, normal = {
        qvec <- qnorm(quantile, mean = theta[1], sd = theta[2])
    }, sev = {
        qvec <- qsev(quantile, location = theta[1], scale = theta[2])
    }, logistic = {
        qvec <- qlogis(quantile, location = theta[1], scale = theta[2])
    }, lev = {
        qvec <- qlev(quantile, location = theta[1], scale = theta[2])
    }, gamma = {
        qvec <- qgamma(quantile/theta[1], shape = theta[2])
    }, igau = {
        qvec <- qigau(quantile, scale = theta[1], shape = theta[2])
    }, bisa = {
        qvec <- qbisa(quantile, scale = theta[1], shape = theta[2])
    }, goma = {
        qvec <- qgoma(quantile, scale = theta[1], shape = theta[2], 
            shape2 = theta[3])
    }, gng = {
        qvec <- qegengl(logb(quantile), theta[1], theta[2], theta[3])
    }, gnf = {
        qvec <- qf(quantile/theta[1], theta[2], theta[3])
    }, egengl = {
        qvec <- qegengl(quantile, theta[1], theta[2], theta[3])
    }, egeng = {
        qvec <- qegeng(quantile, theta[1], theta[2], theta[3])
    }, sevgets = {
        qvec <- qgets(quantile, theta[1], theta[2], theta[3], 
            distribution = "sev")
    }, normalgets = {
        qvec <- qgets(quantile, theta[1], theta[2], theta[3], 
            distribution = "normal")
    }, stop(paste(distribution, "is unrecognized distribution in qgenmax()")))
    return(qvec)
}
