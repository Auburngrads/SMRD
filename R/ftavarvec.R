ftavarvec <-
function (distribution, std.log.censor.time) 
{
    big.ones <- std.log.censor.time > 10^10
    v11 <- rep(NA, length(std.log.censor.time))
    v12 <- rep(NA, length(std.log.censor.time))
    v22 <- rep(NA, length(std.log.censor.time))
    if (any(!big.ones)) {
        lsinf.out <- lsinf(std.log.censor.time[!big.ones], "right", 
            distribution)
        det <- lsinf.out$f11 * lsinf.out$f22 - lsinf.out$f12^2
        v11[!big.ones] <- as.vector(lsinf.out$f22/det)
        v12[!big.ones] <- as.vector(-lsinf.out$f12/det)
        v22[!big.ones] <- as.vector(lsinf.out$f11/det)
    }
    if (any(big.ones)) {
        big.log.censor.time <- 10^100
        lsinf.out <- lsinf(big.log.censor.time, "uncensored", 
            distribution)
        det <- lsinf.out$f11 * lsinf.out$f22 - lsinf.out$f12^2
        v11[big.ones] <- as.vector(lsinf.out$f22/det)
        v12[big.ones] <- as.vector(-lsinf.out$f12/det)
        v22[big.ones] <- as.vector(lsinf.out$f11/det)
    }
    return(list(v11 = v11, v12 = v12, v22 = v22))
}
