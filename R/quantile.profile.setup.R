quantile.profile.setup <-
function (x, t.param.names, profile.on,...)
{
    model <- get(envir = .frame0,  "model")
    h.x <- x
    f.origparam <- model$f.origparam
    origparam <- f.origparam(x, model)
    mu <- origparam[1]
    sigma <- origparam[2]
    distribution <- model$distribution
    if (!any(unlist(profile.on) > length(x))) {
        profile.name <- t.param.names[profile.on]
        profile.on.pos <- profile.on
        ktran <- rep(1, length(profile.on))
}   else {
        switch(paste(profile.on, collapse = " "), `5` = {
            spec.quantile <- get(envir = .frame0,  "special.stuff.profile")$spec.quantile
            profile.on.pos <- 1
            profile.name <- paste(spec.quantile, "Quantile")
            if (is.logdist(distribution)) {
                ktran <- 2
                h.x[1] <- exp(mu + sigma * quant(spec.quantile,
                  distribution))
            } else {
                ktran <- 1
                h.x[1] <- mu + sigma * quant(spec.quantile,
                  distribution)
            }
        }, `6` = {
            spec.time <- get(envir = .frame0,  "special.stuff.profile")$spec.time
            profile.on.pos <- 1
            ktran <- 3
            profile.name <- paste("Pr(T<", spec.time, ")")
            if (is.logdist(distribution)) {
                z <- (logb(spec.time) - mu)/sigma
            } else {
                z <- (spec.time - mu)/sigma
            }
            h.x[1] <- wqmf.phibf(z, distribution)
        }, `5 3` = {
            profile.on.pos <- c(1, 2)
            spec.quantile <- get(envir = .frame0,  "special.stuff.profile")$spec.quantile
            profile.name <- c(paste(spec.quantile, "Quantile"),
                "Scale")
            if (is.logdist(distribution)) {
                ktran <- c(2, 2)
                h.x[1] <- exp(mu + sigma * quant(spec.quantile,
                  distribution))
            } else {
                ktran <- c(1, 2)
                h.x[1] <- mu + sigma * quant(spec.quantile,
                  distribution)
            }
            h.x[2] <- sigma
        }, stop("Unrecognized profile specification"))
    }
    return(list(profile.name = profile.name, h.x = h.x,
        profile.on.pos = profile.on.pos, ktran = ktran))
}
