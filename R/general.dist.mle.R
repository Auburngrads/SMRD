general.dist.mle <-
function (data.ld, distribution,debug1= F, theta.start = NULL) 
{
    stop("general.dist.mle failed with lognormal; suggest generaldistmle2 as more stable")
    options(digits = 5)
    assign(envir = .frame0,  inherits = TRUE,"debug1", debug1)
    f.origparam <- function(thetatran, model) {
        tp1 <- exp(thetatran[1])
        tp2 <- exp(thetatran[2])
        zp1 <- qnorm(model$p1)
        zp2 <- qnorm(model$p2)
        theta <- (zp2 * tp1 * sqrt(tp2) - zp1 * tp2 * sqrt(tp1))/(zp2 * 
            sqrt(tp2) - zp1 * sqrt(tp1))
        beta <- (1/zp1) * (sqrt(tp1/theta) - sqrt(theta/tp1))
        thetaorig <- c(theta, beta)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    probs <- cdfest(data.ld)$prob
    p1 <- min(probs[probs > 0])/2
    p2 <- 0.9 * max(probs)
    orig.param.names <- c("theta", "beta")
    t.param.names <- c("logtp1", "logtp2")
    model <- list(p1 = p1, p2 = p2, distribution = distribution, 
        t.param.names = t.param.names, orig.param.names = orig.param.names)
    if (is.null(theta.start)) {
        ls.gmle.out <- ls.mle(data.ld, distribution = "Lognormal")
        mu <- ls.gmle.out$parameters$mu
        sigma <- ls.gmle.out$parameters$sigma
        logtp1 <- qnorm(p1, mu, sigma)
        logtp2 <- qnorm(p2, mu, sigma)
        theta.start <- c(logtp1, logtp2)
    }
    gmle.out <- gmle(log.like = general.dist.log.like, data.ld = data.ld, 
        theta.start = theta.start, model = model, f.origparam = f.origparam, 
        t.param.names = t.param.names, orig.param.names = orig.param.names, 
       debug1= debug1)
    return(gmle.out)
}
