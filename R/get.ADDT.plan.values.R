get.ADDT.plan.values <-
function (distribution, transformation.x, transformation.response, 
    transformation.time, beta0, beta1, beta2, slope.at, sigma, 
    beta, the.slope, time.units = "Time", response.units, accelvar.units, 
    beta0.cr = NULL, beta1.cr = NULL, beta2.cr = NULL, slope.at.cr = NULL, 
    sigma.cr = NULL, beta.cr = NULL, the.slope.cr = NULL, power = NULL, 
    FailLevel = NULL, use.condition = NULL) 
{
    if (!is.null(use.condition)) {
        use.condition <- string.to.frame(use.condition)
    }
    else (stop("need to specify use condition in planning values"))
    if (is.null(beta0.cr) & is.null(beta1.cr) & is.null(beta2.cr) & 
        is.null(slope.at.cr) & is.null(sigma.cr) & is.null(beta.cr) & 
        is.null(the.slope.cr)) 
        competing.risk <- F
    else competing.risk <- T
    transformation.x <- set.relationship.power(transformation.x, 
        power)
    mod.trans.x <- fix.inverse.relationship(transformation.x)
    if (length(transformation.x) != length(beta2)) 
        stop("length(transformation.x) !=length(beta2)")
    if (missing(response.units)) 
        stop("Need to specify units for the response")
    if (missing(accelvar.units)) {
        accelvar.units <- rep(NA, length(beta2))
        for (i in 1:length(beta2)) {
            switch(generic.relationship.name(transformation.x[i]), 
                Eyring = , Arrhenius = {
                  accelvar.units[i] <- "Degrees C"
                }, humidity = {
                  accelvar.units[i] <- "RH"
                }, stop(paste("Need to specify units for accelerating variable", 
                  transformation.x[i])))
        }
    }
    else {
        if (length(accelvar.units) != length(beta2)) 
            stop("length(accelvar.units) !=length(beta2)")
    }
    if (missing(sigma)) {
        if (missing(beta)) {
            cat("Both sigma and beta missing. Assuming that beta=1 \n \n")
            beta <- 1
        }
        sigma <- 1/beta
    }
    else {
        if (!(missing(beta))) {
            stop(paste("Cannot specify both sigma and beta\nbeta=", 
                beta, "sigma=", sigma))
        }
    }
    if (missing(beta1)) {
        if (missing(slope.at) || missing(the.slope)) 
            stop("If beta1 not specified, must specify the.slope and slope.at")
        if (length(slope.at) != length(beta2)) 
            stop("length(slope.at) !=length(beta2)")
        beta2.x <- 0
        for (i in 1:length(transformation.x)) {
            beta2.x <- beta2.x + beta2[i] * f.relationship(slope.at[i], 
                subscript.relationship(mod.trans.x, i))
        }
        beta1 <- the.slope/exp(beta2.x)
    }
    beta <- 1/sigma
    theta.vec <- c(beta0, beta1, beta2, sigma)
    beta2.names <- paste("beta", seq(2, length(beta2) + 1), sep = "")
    names(theta.vec) <- c("beta0", "beta1", beta2.names, "sigma")
    if (competing.risk) {
        if (is.null(sigma.cr)) {
            if (is.null(beta.cr)) {
                cat("Both sigma.cr and beta.cr missing. Assuming that beta.cr=1 \n \n")
                beta.cr <- 1
            }
            sigma.cr <- 1/beta.cr
        }
        else {
            if (!(is.null(beta.cr))) {
                stop(paste("Cannot specify both sigma.cr and beta.cr\nbeta.cr=", 
                  beta.cr, "sigma.cr=", sigma.cr))
            }
        }
        if (is.null(beta1.cr)) {
            if (is.null(slope.at.cr) || is.null(the.slope.cr)) 
                stop("If beta1.cr not specified, must specify the.slope.cr and slope.at.cr")
            if (length(slope.at.cr) != length(beta2.cr)) 
                stop("length(slope.at.cr) !=length(beta2.cr)")
            beta2.x.cr <- 0
            for (i in 1:length(transformation.x)) {
                beta2.x.cr <- beta2.x.cr + beta2.cr[i] * f.relationship(slope.at.cr[i], 
                  subscript.relationship(mod.trans.x, i))
            }
            beta1.cr <- the.slope.cr/exp(beta2.x.cr)
        }
        beta.cr <- 1/sigma.cr
        theta.vec.cr <- c(beta0.cr, beta1.cr, beta2.cr, sigma.cr)
        beta2.names.cr <- paste(paste("beta", seq(2, length(beta2) + 
            1), sep = ""), ".cr", sep = "")
        names(theta.vec.cr) <- c("beta0.cr", "beta1.cr", beta2.names.cr, 
            "sigma.cr")
        if (ncol(use.condition) != length(beta2)) 
            stop(paste("ncol(use.condition)=", ncol(use.condition), 
                "is not equal to length(beta2)=", length(beta2), 
                " in get.ADDT.plan.values"))
        rlist <- list(distribution = distribution, transformation.x = transformation.x, 
            transformation.time = transformation.time, transformation.response = transformation.response, 
            theta.vec = theta.vec, beta = beta, sigma = sigma, 
            theta.vec.cr = theta.vec.cr, beta.cr = beta.cr, sigma.cr = sigma.cr, 
            accelvar.units = accelvar.units, response.units = response.units, 
            time.units = time.units, FailLevel = FailLevel, use.condition = use.condition)
    }
    else {
        if (ncol(use.condition) != length(beta2)) 
            stop(paste("ncol(use.condition)=", ncol(use.condition), 
                "is not equal to length(beta2)=", length(beta2), 
                " in get.ADDT.plan.values"))
        rlist <- list(distribution = distribution, transformation.x = transformation.x, 
            transformation.time = transformation.time, transformation.response = transformation.response, 
            theta.vec = theta.vec, beta = beta, sigma = sigma, 
            accelvar.units = accelvar.units, response.units = response.units, 
            time.units = time.units, FailLevel = FailLevel, use.condition = use.condition)
    }
    oldClass(rlist) <- "ADDT.plan.values"
    return(rlist)
}
