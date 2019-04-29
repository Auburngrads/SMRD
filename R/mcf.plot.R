mcf.plot <-
function (data.rdu, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
    plot.data = F, title.option = GetSMRDDefault("SMRD.TitleOption"), my.title = NULL, xlab = paste("Time in", 
        get.time.units(data.rdu)), ylab = "Mean Cumulative Function", 
    add = F, lty = 1, plot.seg = T, xlim = c(NA, NA), ylim = c(NA, 
        NA), band.type = "p", ci.opt = "solid") 
{
    mcf.out <- mcf(data.rdu)
    plot.mcf(mcf.out, conf.level = conf.level, title.option = title.option, 
        my.title = my.title, xlab = xlab, ylab = "Mean Cumulative Function", 
        lty = lty, plot.seg = plot.seg, xlim = xlim, ylim = ylim, 
        band.type = band.type, ci.opt = ci.opt, col.ci = 4)
    invisible(mcf.out)
}
