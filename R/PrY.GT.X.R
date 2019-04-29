PrY.GT.X <-
function (xbar, sx, nx, ybar, sy, ny, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100) 
{
    cat("xbar=", xbar, ", sx =", sx, ", nx =", nx, "\n")
    cat("ybar=", ybar, ", sy =", sy, ", ny =", ny, "\n")
    zquant <- sqrt(qchisq(conf.level, 1))
    thetahat <- c(xbar, sx^2, ybar, sy^2)
    f.zhat <- function(thetahat) {
        xbar <- thetahat[1]
        sx2 <- thetahat[2]
        ybar <- thetahat[3]
        sy2 <- thetahat[4]
        zhat <- (ybar - xbar)/sqrt(sx2 + sy2)
        zhat
    }
    SigmaThetaHat <- c(sx^2/nx, sy^2/ny, (2 * sx^4 * (nx - 1))/nx^2, 
        (2 * sy^4 * (ny - 1))/ny^2)
    vcv.thetahat <- diag(SigmaThetaHat)
    results <- f.gendeltamethod(vcv.thetahat, thetahat, f.zhat)
    zlower <- results$vec - zquant * results$se
    zupper <- results$vec + zquant * results$se
    Prob <- pnorm(c(zlower, results$vec, zupper))
    cat("LCL=", format(Prob[1]), "Prob estimate=", format(Prob[2]), 
        "UCL=", format(Prob[3]), "\n\n\n")
    invisible(Prob)
}
