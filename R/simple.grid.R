simple.grid <-
function (data.ld, distribution, xlim = c(NA, NA), ylim = c(NA,
    NA), size = 100, the.quantile = NA, factor = 3.5, log.quantile = F,
    restrict.expand.range = F, ...)
{
    mlest.out <- mlest(data.ld, distribution, ...)
    theta.hat <- mlest.out$theta.hat
    vcv.matrix <- mlest.out$vcv.matrix
    ls2.out <- ls2.mle(data.ld, distribution = distribution,
        theta.start = theta.hat)
    if (is.na(the.quantile)) {
        xname <- "mu"
        zquant <- 0
        quanthat <- theta.hat[1]
        ci.quant <- c(theta.hat[1] - factor * sqrt(vcv.matrix[1,
            1]), theta.hat[1] + factor * sqrt(vcv.matrix[1, 1]))
  } else {
        xname <- paste("t[",format(the.quantile, digits = 6),"]","~","Quantile","~",
            get.time.units(data.ld))
        zquant <- quant(the.quantile, distribution)
        quanthat <- theta.hat[1] + zquant * theta.hat[2]
        sequant <- sqrt(vcv.matrix[1, 1] + (zquant^2) * vcv.matrix[2,
            2] + 2 * zquant * vcv.matrix[1, 2])
        ci.quant <- c(quanthat - factor * sequant, quanthat +
            factor * sequant)
        if (is.logdist(distribution) && !log.quantile) {
            ci.quant <- exp(ci.quant)
            quanthat <- exp(quanthat)
        }
    }
    ci.spread <- c(theta.hat[2]/exp((factor * sqrt(vcv.matrix[2,
        2]))/theta.hat[2]), theta.hat[2] * exp((factor * sqrt(vcv.matrix[2,
        2]))/theta.hat[2]))
    if (generic.distribution(distribution) == "weibull") {
        spread.name <- "beta"
        spread.hat <- 1/theta.hat[2]
        ci.spread <- sort(1/ci.spread)
  } else {
        spread.name <- "sigma"
        spread.hat <- theta.hat[2]
    }
    xlim.hold <- xlim
    ylim.hold <- ylim
    if (restrict.expand.range) {
        xlim[1] <- max(xlim[1], min(ci.quant))
        xlim[2] <- min(xlim[2], max(ci.quant))
        ylim[1] <- max(ylim[1], min(ci.spread))
        ylim[2] <- min(ylim[2], max(ci.spread))
  } else {
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- ci.quant[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- ci.spread[yrna]
    }
    likelihood.grid.out <- likelihood.grid(ls2.out, the.quantile = the.quantile,
        range.list = list(xlim, ylim), relative = T, size = size,
        xname = xname, yname = spread.name, log.quantile = log.quantile)
    attr(likelihood.grid.out, "mlest.out") <- mlest.out
    attr(likelihood.grid.out, "xlim") <- xlim
    attr(likelihood.grid.out, "ylim") <- ylim
    attr(likelihood.grid.out, "distribution") <- distribution
    attr(likelihood.grid.out, "title") <- get.data.title(data.ld)
    attr(likelihood.grid.out, "quanthat") <- quanthat
    attr(likelihood.grid.out, "spread.hat") <- spread.hat
    return(likelihood.grid.out)
}
