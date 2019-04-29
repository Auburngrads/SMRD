quantile.rmd <-
function (x, rmd.fit, time.vec, number.points = 200,...)
{
    rmd.parameters <- DegSummary(rmd.fit)$parameters
    meanDt <- rmd.parameters["mu.beta0"] + rmd.parameters["mu.beta1"] *
        time.vec
    cov <- rmd.parameters["rho"] * rmd.parameters["sigma.beta0"] *
        rmd.parameters["sigma.beta1"]
    sdDt <- sqrt(rmd.parameters["sigma.beta0"]^2 + time.vec^2 *
        rmd.parameters["sigma.beta1"]^2 + 2 * time.vec * cov)
    the.quantiles <- meanDt + qnorm(x) * sdDt
    return(the.quantiles)
}
