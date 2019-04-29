four.mleprobplot <-
function (data.ld, xlab = get.time.units(data.ld), xlim = c(NA,
    NA), ylim = c(NA, NA), conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F, ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
    trunc.correct = T, cex.points = 1.2, band.type = "p", landscape = F,
    ...)
{
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
        SMRD.NameOnPlot = "")
    if (landscape) {
        old.par <- par(mfrow = c(2, 2), oma = c(0, 0, 0, 0),
            err = -1)
  } else {
        old.par <- par(mfcol = c(2, 2), oma = c(0, 0, 0, 0),
            err = -1)
    }
    on.exit({
        par(old.par)
        par(new = F)
    })
    
        title.line.adj = 0.3
   
    distribution.list <- SMRD.FourDistributionList()
    for (i in 1:length(distribution.list)) {
        mleprobplot(data.ld = data.ld, distribution = distribution.list[i],
            ylab = "", xlab = xlab, xlim = xlim, ylim = ylim,
            conf.level = conf.level, my.title = "", grids = grids,
            linear.axes = F, title.option = "only.dist", trunc.correct = T,
            cex.points = cex.points, band.type = band.type, print.parameters = F,
            title.line.adj = title.line.adj)
    }
    mtext(text = ylab, side = 2, line = 2, outer = T, cex = 1.2)
    if (conf.level > 0.1) {
        conf.int.title <- paste("and Pointwise", percent.conf.level(conf.level),
            "Confidence Intervals")
  } else {
        conf.int.title <- ""
    }
    if (is.null(my.title)) {
        my.title <- paste(get.data.title(data.ld), "\nProbability Plots with ML Estimates ",
            conf.int.title)
    }
    if (title.option == "full") {
       
        title.line <- 0
        mtext(side = 3, line = title.line, outer = T, text = my.title, cex = 1)
    }
    SMRDOptions(save.SMRD.options)
    invisible()
}
