get.cen.scheme <-
function (m, p.fail, beta, e.of.r) 
{
    m <- c(1, 3, 12, 24, 36)
    p.fail <- c(0.01, 0.05, 0.1, 0.2)
    e.of.r <- c(1, 3, 5, 10, 20, 50, 100)
    beta <- c(0.8, 1, 1.5, 3)
    mu <- 0
    censor.time <- rep(list(1:(m[3])), 480)
    number.units <- rep(list(1:(m[3])), 480)
    censor.scheme1 <- matrix(rep(0, 40), 20, 2)
    censor.scheme2 <- matrix(rep(0, 48), 24, 2)
    censor.scheme1 <- matrix(c(censor.time, number.units), 2, 
        2)
    censor.scheme2 <- c(censor.time, number.units)
    result <- matrix(rep(0, 480), 20, 24)
    for (j in 1:20) {
        for (j in 1:24) {
            result[i, ] <- censor.scheme1(m, p.fail)
            result[, j] <- censor.scheme2(beta, e.of.r)
        }
    }
    cen.time <- rep(0, length = length(p.fail) * length(beta))
    for (k in 1:(length(p.fail) * length(beta))) {
        for (i in 1:length(p.fail)) {
            for (j in 1:length(beta)) {
                cen.time[4 * (i - 1) + j] <- exp(mu) * (-logb((1 - 
                  p.fail[i])))^(1/beta[j])
            }
        }
    }
    censor.times <- matrix(rep(0, length = 80), 80, 1)
    for (g in 1:80) {
        for (l in 1:16) {
            for (n in 1:length(m)) {
                censor.times[5 * (l - 1) + n] <- (cen.time[l]/m[n])
            }
        }
    }
}
