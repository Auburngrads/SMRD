plot.use.rate.dist <-
function (x, xlab = "Cycles per Unit Time", number.sample = 50000,...)
{
    if (class(x) == "use.rate.model") {
        use.rate.model <- x
        lab.time.units <- use.rate.model$lab.time.units
        field.time.units <- use.rate.model$field.time.units
        mu.use.rate <- use.rate.model$mu.use.rate
        sigma.use.rate <- use.rate.model$sigma.use.rate
        distribution.use.rate <- use.rate.model$distribution.use.rate
        if (mu.use.rate < 0) {
            switch <- T
            mu.use.rate <- -mu.use.rate
            xlab <- paste(field.time.units, "per", lab.time.units)
        }
        else {
            switch <- F
            xlab <- paste(lab.time.units, "per", field.time.units)
        }
        sample.x <- exp(mu.use.rate + quant(runif(number.sample),
            distribution.use.rate) * sigma.use.rate)
    }
    hist(log(sample.x), xlab = xlab, xaxt = "n",
        col = 4)
    getxax.out <- logax(exp(x.loc(0)), exp(x.loc(1)))
    axis(side = 1, at = log(as.numeric(getxax.out$ticlab)), labels = getxax.out$ticlab,
        adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex = 1)
    invisible()
}
