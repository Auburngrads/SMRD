scatter.plot.boot.npar.par.out <-
function (x, xlim = c(NA, NA), ylim = c(NA,
    NA), my.title = NULL, cex = 1.5, grids = F, number.points.plot = 500)
{
    mins <- function (x) { order(x)[1] }
    maxs <- function (x) { order(x)[length(x)] }
    mlest.out <- x$mlest.out
    theta.hat <- mlest.out$theta.hat
    param.names <- names(theta.hat)
    distribution <- mlest.out$distribution
    number.points.plot <- min(nrow(x$theta.hat.star),
        number.points.plot)
    mu <- mlest.out$theta.hat[1]
    if (is.null(my.title)) {
        my.title <- paste(distribution, "Distribution ML Estimate and Bootstrap Results for\n",
            get.data.title(mlest.out$data.ld))
    }
    if (generic.distribution(distribution) == "exponential") {
        sigma <- 1
  } else {
        sigma <- mlest.out$theta.hat[2]
    }
    mu.hat <- x$theta.hat.star[, 1]
    if (generic.distribution(distribution) == "exponential") {
        sigma.hat <- rep(1, length(mu.hat))
  } else {
        sigma.hat <- x$theta.hat.star[, 2]
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- range(mu.hat)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- range(sigma.hat)[yrna]
    plot.paper(xlim, ylim, my.title = my.title, xlab = parse(text = param.names[1]), 
               ylab = parse(text = param.names[2]), grids = F, cex.title = 0.8,
               cex.lab = 1.1)
    corners <- unique(c(mins(mu.hat), mins(sigma.hat), maxs(mu.hat),
        maxs(sigma.hat)))
    the.ones <- c(sample((1:length(sigma.hat))[-corners], number.points.plot -
        length(corners)), corners)
    points.default(mu, sigma, pch = 1, cex = (2.5 * GetSMRDDefault("SMRD.point.size"))/100)
    points.default(mu.hat[the.ones], sigma.hat[the.ones], pch = 16,
        cex = (0.5 * GetSMRDDefault("SMRD.point.size"))/100)
    invisible()
}
