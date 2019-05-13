#' @export
simulate.truncate.ld <-
function (object, ...)
{
    x.levels <- c(30, 40, 50)
    nreps <- c(15, 15, 15)
    trun.prop <- c(0.3, 0.4, 0.5)
    beta0 <- 3
    beta1 <- 10
    sigma <- 5
    y <- NULL
    the.truncation.codes <- NULL
    ty <- NULL
    x <- NULL
    nlevels <- length(x.levels)
    for (i in 1:nlevels) {
        mu <- beta0 + beta1 * x.levels[i]
        new.y <- mu + rnorm(nreps[i]) * sigma
        trun.level <- floor(mu + qnorm(1 - trun.prop[i]) * sigma)
        new.y <- new.y[new.y < trun.level]
        y <- c(y, new.y)
        x <- c(x, rep(x.levels[i], length(new.y)))
        the.truncation.codes <- c(the.truncation.codes, rep("right",
            length(new.y)))
        ty <- c(ty, rep(trun.level, length(new.y)))
    }
    the.ld <- make.frame.ld(y = y, the.truncation.codes = the.truncation.codes,
        ty = ty, the.xmat = as.matrix(x), data.title = paste("Simulated data from a normal dist"))
    return(the.ld)
}
