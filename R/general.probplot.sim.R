general.probplot.sim <-
function (paper.distribution = "normal", data.distribution = "normal",
    censor.distribution = NULL, data.parameters = c(0, 1), censor.parameters = c(0,
        1), sample.size = 10, print.samp.size = F, cex = 0.7,
    xlim = c(NA, NA), ylim = c(NA, NA), xlab = NULL, censoring.max = NULL,
    ml = F, ...)
{
    data.ld <- sim.random.censoring(data.distribution = data.distribution,
        censor.distribution = censor.distribution, data.parameters = data.parameters,
        censor.parameters = censor.parameters, sample.size = sample.size,
        censoring.max = censoring.max)
    if (ml)
        mleprobplot(data.ld, paper.distribution, xlab = xlab,
            xlim = xlim, ylim = ylim, band.type = "n",
            my.title = NULL, title.option = "blank", print.parameters = F,
            ...)
    else npprobplot(data.ld, paper.distribution, xlab = xlab,
        xlim = xlim, ylim = ylim, band.type = "none",
        my.title = NULL, title.option = "blank", ...)
    if (print.samp.size)
        mtext(side = 3, line = 1, text = paste("Sample Size=",
            sample.size), cex = 1)
    invisible()
}
