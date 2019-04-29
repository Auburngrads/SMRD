egengprobplot <-
function (data.ld, plot.dist = NULL, xlab = get.time.units(data.ld), 
    xlim = c(NA, NA), ylim = c(NA, NA), time.range = c(NA, 
        NA), conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
    interactive = T, original.par = F, mle.quantiles = T, my.title = NULL, 
    type = "y", cex = 1, cexlab = 1, sub.title = "", grids = F, 
    linear.axes = F, slope.axis = F, print.table = F, title.option = GetSMRDDefault("SMRD.TitleOption"), 
    ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), trunc.correct = T, 
    add = F, plot.censored.ticks = F, pch = 16, lty = c(3, 4, 
        5, 6, 7), lwd = c(2, 2, 2, 2, 2), length.time.vec = 100, 
    compare.dists = NULL, col.ci = 4, lwd.ci = 2, xxx.mle.out = NULL, 
    ciMethod = "normal.approx", extrapolate.ci = F, band.type = "p", 
    ...) 
{
    gmle.out <- egeng.mle(data.ld)
    distribution <- "Extended Generalized Gamma"
    basic.gmleprobplot(data.ld, distribution = distribution, 
        plot.dist = plot.dist, xlab = xlab, xlim = xlim, 
        ylim = ylim, time.range = time.range, conf.level = conf.level, 
        original.par = original.par, interactive = interactive, 
        mle.quantiles = mle.quantiles, my.title = my.title, type = type, 
        cex = cex, cexlab = cexlab, sub.title = sub.title, grids = grids, 
        linear.axes = linear.axes, slope.axis = slope.axis, print.table = F, 
        title.option = title.option, ylab = ylab, trunc.correct = trunc.correct, 
        add = add, plot.censored.ticks = plot.censored.ticks, 
        pch = pch, lty = lty, lwd = lwd, length.time.vec = length.time.vec, 
        xxx.mle.out = gmle.out, band.type = band.type)
    return(gmle.out)
}
