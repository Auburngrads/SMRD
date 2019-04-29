quantile.profile.stable.parameters <-
function (x, profile.on,...)
{
    model <- get(envir = .frame0,  "model")
    distribution <- model$distribution
    theta.hat <- x
    if (!any(unlist(profile.on) > length(theta.hat)))
        return(theta.hat)
    switch(paste(profile.on, collapse = " "), `5` = {
        sigma <- exp(theta.hat[2])
        spec.quantile <- get(envir = .frame0,  "special.stuff.profile")$spec.quantile
        if (is.logdist(distribution)) {
            theta.hat[1] <- logb(x[1]) - quant(spec.quantile,
                distribution) * sigma
        } else {
            theta.hat[1] <- x[1] - quant(spec.quantile,
                distribution) * sigma
        }
    }, `6` = {
        sigma <- exp(theta.hat[2])
        spec.time <- get(envir = .frame0,  "special.stuff.profile")$spec.time
        if (is.logdist(distribution)) {
            theta.hat[1] <- logb(spec.time) - quant(x[1],
                distribution) * sigma
        } else {
            theta.hat[1] <- spec.time - quant(x[1],
                distribution) * sigma
        }
    }, `5 3` = {
        sigma <- x[2]
        spec.quantile <- get(envir = .frame0,  "special.stuff.profile")$spec.quantile
        if (is.logdist(distribution)) {
            theta.hat[1] <- logb(x[1]) - quant(spec.quantile,
                distribution) * sigma
        } else {
            theta.hat[1] <- x[1] - quant(spec.quantile,
                distribution) * sigma
        }
        theta.hat[2] <- logb(x[2])
    })
    theta.hat[1] <- theta.hat[1] + quant(model$pcensor, model$distribution) *
        sigma
    return(theta.hat)
}
