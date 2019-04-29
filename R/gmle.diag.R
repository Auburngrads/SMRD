gmle.diag <-
function (gmle.out, ps1 = NULL, ps2 = NULL, my.title = NULL,
    xlim = NULL, ylim = NULL, fvquant = 0.5)
{
    distribution <- gmle.out$model$distribution
    theta <- gmle.out$est.out$x
    special.stuff <- gmle.out$special.stuff
    data.ld <- gmle.out$data.ld
    mu <- data.ld$muxnew %*% theta[special.stuff$mu.parameters]
    sigma <- exp(data.ld$sigmaxnew %*% theta[special.stuff$sigma.parameters])
    fitted.values <- mu + quant(fvquant, distribution) * sigma
    if  (is.even(numdist(distribution))) {
        residuals <- exp((logb(Response(data.ld)[, 1]) - fitted.values)/sigma)
        fitted.values <- exp(fitted.values)
  } else {
        residuals <- (Response(data.ld)[, 1] - fitted.values)/sigma
    }
    mlest.out <- list(distribution = distribution, fitted.values = fitted.values,
        residuals = residuals, data.ld = data.ld)
    if (!is.null(ps1))
        postscript(file = ps1, horizontal = T)
    resid.vs.fit(mlest.out, my.title = my.title, xlim = xlim,
        ylim = ylim)
    if (!is.null(ps1))
        dev.off()
    pause()
    if (!is.null(ps2))
        postscript(file = ps2, horizontal = T)
    resid.probplot(mlest.out, my.title = my.title)
    if (!is.null(ps2))
        dev.off()
    invisible(mlest.out)
}
