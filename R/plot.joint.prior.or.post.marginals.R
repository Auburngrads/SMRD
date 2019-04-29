plot.joint.prior.or.post.marginals <-
function (x, post.or.prior, logger = "none", type.position = "Base Quantile",
    newdata = NULL, number.plot = 500, xlim = c(NA, NA), ylim = c(NA,
        NA), include.likelihood = F, axes.range.default.post,...)
{
    distribution <- x$distribution
    old.par <- par(mfcol = c(2, 2), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    plot.joint.prior(x, post.or.prior, type.position = type.position,
        newdata = newdata, number.plot = number.plot, xlim = xlim,
        ylim = ylim, my.title = "")
    plot.marginal.prior(x, post.or.prior, type.position = type.position,
        newdata = newdata)
    plot.marginal.prior(x, post.or.prior, type.position = "Parameter",
        newdata = "sigma")
    sample.name <- single.ifelse(post.or.prior == "post", "Posterior",
        "Prior")
    mtext(paste(distribution, "Model", sample.name, "Distribution for",
        get.data.title(x$data.ld)), side = 3,
        line = -1, outer = T, cex = 1.2)
    invisible()
}
