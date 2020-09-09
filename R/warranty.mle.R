warranty.mle <-
function (data.ld, distribution = "lognormal",debug1= F, theta.start = NULL)
{
    options(digits = 5)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    f.origparam <- function(thetatran, model) {
        mu.time <- thetatran[1]
        sigma.time <- exp(thetatran[2])
        mu.dist <- thetatran[3]
        sigma.dist <- exp(thetatran[4])
        ez <- exp(thetatran[5])
        rho <- (ez - 1)/(1 + ez)
        thetaorig <- c(mu.time, sigma.time, mu.dist, sigma.dist,
            rho)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    f.interpparam <- function(thetaorig) {
        thetainterp <- thetaorig
        thetainterp[1] <- exp(thetaorig[1])
        thetainterp[3] <- exp(thetaorig[3])
        interp.param.names <- names(thetaorig)
        interp.param.names[1] <- "median.time"
        interp.param.names[3] <- "median.dist"
        names(thetainterp) <- interp.param.names
        return(thetainterp)
    }
    orig.param.names <- c("mu.time", "sigma.time", "mu.dist",
        "sigma.dist", "rho")
    t.param.names <- c("mu.time", "logsigma.time", "mu.dist",
        "logsigma.dist", "zrho")
    model <- list(distribution = distribution)
    if (is.null(theta.start)) {
        miles.gmle.out <- mlest(data.ld, distribution = distribution)
        time.data.ld <- data.ld
       Response(time.data.ld) <- xmat(data.ld)[, 1, drop = F]
        time.gmle.out <- mlest(time.data.ld, distribution = distribution)
        theta.start <- c(time.gmle.out$theta.hat[1], logb(time.gmle.out$theta.hat[2]),
            miles.gmle.out$theta.hat[1], logb(miles.gmle.out$theta.hat[2]),
            0)
    }
    names(theta.start) <- t.param.names
    gmle.out <- gmle(data.ld = data.ld, log.like = warranty.log.like,
        theta.start = theta.start, model = model, f.origparam = f.origparam,
        t.param.names = t.param.names, orig.param.names = orig.param.names,
       debug1= debug1, func.call = match.call())
    thetainterp <- f.interpparam(gmle.out$origparam)
    gmle.out$thetainterp <- thetainterp
    oldClass(gmle.out) <- c("warranty.gmle.out", oldClass(gmle.out))
    MysetOldClass(attr(gmle.out, "class"))
    return(gmle.out)
}
