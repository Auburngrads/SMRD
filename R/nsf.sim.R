nsf.sim <-
function (distribution = "normal", parameter = NULL, sample.size = 10) 
{
    parameter.name <- NULL
    switch(distribution, Normal = , normal = {
        sample <- rnorm(sample.size)
        cdist <- "Normal"
    }, Lognormal =, lognormal = {
        if (is.null(parameter)) parameter <- 1
        parameter.name <- "standard deviation of logs"
        sample <- rlnorm(sample.size, sdlog = parameter)
        cdist <- "Lognormal"
    }, Sev = , sev = {
        sample <- logb(-logb(runif(sample.size)))
        cdist <- "Smallest Extreme Value"
    }, Weibull = , weibull = {
        parameter.name <- "shape parametereter"
        if (is.null(parameter)) parameter <- 2
        sample <- rweibull(sample.size, shape = parameter)
        cdist <- "Weibull"
    }, Gamma = , gamma = {
        parameter.name <- "shape parametereter"
        if (is.null(parameter)) parameter <- 2
        sample <- rgamma(sample.size, shape = parameter)
        cdist <- "Gamma"
    }, t = {
        parameter.name <- "degrees of freedom"
        if (is.null(parameter)) parameter <- 5
        sample <- rt(sample.size, df = parameter)
        cdist <- "t"
    }, Uniform = , uniform = {
        sample <- runif(sample.size)
        cdist <- "Uniform"
    }, Cauchy = , cauchy = {
        sample <- rcauchy(sample.size)
        cdist <- "Cauchy"
    }, Exponential = , exponential = {
        sample <- rexp(sample.size)
        cdist <- "Exponential"
    }, Lev = , lev = {
        sample <- -logb(-logb(runif(sample.size)))
        cdist <- "Largest Extreme Value"
    }, Logistic = , logistic = {
        sample <- rlogis(sample.size)
        cdist <- "Logistic"
    }, Loglogistic = , loglogistic = {
        parameter.name <- "shape parametereter"
        if (is.null(parameter)) parameter <- 2
        sample <- exp(rlogis(sample.size))
        cdist <- "Loglogistic"
    }, {
        stop(paste("Specified distribution", distribution, "is not recognized"))
    })
    return(list(sample = sample, cdist = cdist, parameter = parameter, 
        parameter.name = parameter.name))
}
