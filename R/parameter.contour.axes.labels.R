parameter.contour.axes.labels <-
function (specifications.for.prior, type.position = "Quantile") 
{
    distribution <- generic.distribution(attr(specifications.for.prior, 
        "distribution"))
    p <- specifications.for.prior$quantile$p
    if (is.logdist(distribution)) {
        xlab <- switch(type.position, Parameter = {
            "Scale"
        }, Quantile = {
            paste(p, "Quantile")
        })
        if (distribution == "weibull") 
            ylab <- "Beta"
        else ylab <- "Sigma"
    }
    else {
        xlab <- switch(type.position, Parameter = {
            "Location"
        }, Quantile = {
            paste(p, "Quantile")
        })
        ylab <- "Scale"
    }
    return(list(xlab = xlab, ylab = ylab))
}
