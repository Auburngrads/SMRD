plot.groupm.Dest.Degrad.out <-
function (x, 
          FailLevel, 
          quant.lines.levels = c(0.1, 0.5, 0.9), 
          y.axis, 
          density.at = NULL, 
          ylim = c(NA, NA),
    xlim = c(NA, NA), my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"),
    censor.time = NULL, cex = 1, xlab = NULL, ylab = NULL, lwd = 2,
    grids = F, add = F, response.on.yaxis = T, the.censor.codes = NULL,
    plot.quant.labels = T, nxpoints = 200, focus.variable = 1,
    fixed.other.values = NULL, ...)
{
    transformation.x <- x$model$transformation.x
    if (length(transformation.x) > 1)
        dummy.x <- make.dummy.groupm.Dest.Degrad.out(x,
            focus.variable = focus.variable, fixed.other.values = fixed.other.values)
    else dummy.x <- x
    single.plot.groupm.Dest.Degrad.out(dummy.x,
        FailLevel = FailLevel, quant.lines.levels = quant.lines.levels,
        y.axis = y.axis, density.at = density.at, ylim = ylim,
        xlim = xlim, my.title = my.title, title.option = title.option,
        censor.time = censor.time, cex = cex, xlab = xlab, ylab = ylab,
        lwd = lwd, grids = grids, add = add, response.on.yaxis = response.on.yaxis,
        the.censor.codes = the.censor.codes, plot.quant.labels = plot.quant.labels,
        nxpoints = nxpoints)
    invisible()
}
