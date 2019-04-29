get.censor.scheme.all <-
function (m, p.fail, beta, e.of.r) 
{
    mu <- 0
    censor.times <- rep(0, length = m)
    number.units <- rep(0, length = m)
    cen.time <- exp(mu) * (-logb((1 - p.fail)))^(1/beta)
    j <- seq(1:m)
    cen.time <- cen.time/m
    for (k in 1:m) {
        censor.times[k] <- cen.time * j[k]
    }
    dummy <- pweibull(censor.times, beta, scale = 1)
    dummy1 <- rep(floor(e.of.r/sum(dummy)), length = m)
    total <- dummy1[1] * sum(dummy)
    if (total == e.of.r) {
        number.units <- dummy1
    }
    else if (total < e.of.r) {
        dif <- e.of.r - total
        dif1 <- floor(dif/sum(dummy))
        dummy1 <- dif1 + dummy1
        dif <- dif - dif1 * sum(dummy)
        if (dif - dummy[1] < 0) {
            number.units <- dummy1
        }
        if (dif - dummy[1] >= 0) {
            w <- 0
            while (dif >= 0) {
                w <- w + 1
                dif <- dif - dummy[w]
            }
            number.units <- dummy1 + c(rep(1, length = w - 1), 
                rep(0, length = m + 1 - w))
        }
    }
    num.units <- number.units
    return(list(number.units = number.units, censor.times = censor.times))
}
