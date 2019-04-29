PrY.GT.XSimWeibull <-
function (eta.x, beta.x, nx, eta.y, beta.y, ny, nsim = 5e+05, 
    conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, delta.factor = 0.01) 
{
    mu.x <- log(eta.x)
    sigma.x <- 1/beta.x
    mu.y <- log(eta.y)
    sigma.y <- 1/beta.y
    zquant <- sqrt(qchisq(conf.level, 1))
    thetahat <- c(mu.x, sigma.x, mu.y, sigma.y)
    delta <- delta.factor * thetahat
    f.zhat <- function(thetahat, nsim) {
        xvalues <- rsev(nsim, location = thetahat[1], scale = thetahat[2])
        yvalues <- rsev(nsim, location = thetahat[3], scale = thetahat[4])
        Prob <- sum(yvalues > xvalues)/nsim
        zhat <- qsev(Prob)
        zhat
    }
    print(thetahat)
    var.mu.x <- ((sigma.x)^2 * 1.1087)/nx
    var.sigma.x <- ((sigma.x)^2 * 0.6079)/nx
    cov.mu.x.sigma.x <- (-(sigma.x)^2 * 0.257)/nx
    var.mu.y <- ((sigma.y)^2 * 1.1087)/ny
    var.sigma.y <- ((sigma.y)^2 * 0.6079)/ny
    cov.mu.y.sigma.y <- (-(sigma.y)^2 * 0.257)/ny
    the.elements <- c(var.mu.x, cov.mu.x.sigma.x, 0, 0, cov.mu.x.sigma.x, 
        var.sigma.x, 0, 0, 0, 0, var.mu.y, cov.mu.y.sigma.y, 
        0, 0, cov.mu.y.sigma.y, var.sigma.y)
    vcv.thetahat <- matrix(the.elements, 4, 4)
    print(vcv.thetahat)
    results <- f.gendeltamethod(vcv.thetahat, thetahat, f.zhat, 
        nsim = nsim, delta = delta)
    zlower <- results$vec - zquant * results$se
    zupper <- results$vec + zquant * results$se
    Prob <- psev(c(zlower, results$vec, zupper))
    cat("LCL=", format(Prob[1]), "Prob estimate=", format(Prob[2]), 
        "UCL=", format(Prob[3]), "\n\n\n")
    invisible(Prob)
}
