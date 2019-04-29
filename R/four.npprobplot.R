four.npprobplot <-
  function (data.ld, xlab = get.time.units(data.ld), xlim = c(NA,NA),
            ylim = c(NA, NA), time.vec = NULL, distribution.list = c('weibull', 'sev', 'lognormal', 'normal'),
            conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            my.title = NULL, title.option = GetSMRDDefault("SMRD.TitleOption"), grids = F,
            ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
            trunc.correct = T, cex.points = 2, band.type = "s", landscape = F,
            ...)
  {
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F,
                                         SMRD.NameOnPlot = "")
    if (landscape) {
      old.par <- par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), err = -1)
    } else {
      old.par <- par(mfcol = c(2, 2), oma = c(0, 0, 0, 0), err = -1)
    }
    on.exit({
      par(mfrow = c(1,1))
      par(new = F)
      SMRDOptions(save.SMRD.options)
    })
    title.line.adj = 0.3

    for (i in 1:length(distribution.list)) {

      npprobplot(data.ld, distribution = distribution.list[i],
                 xlab = xlab, xlim = xlim, ylim = ylim,
                 time.vec = time.vec, conf.level = conf.level, my.title = "",
                 grids = grids, linear.axes = F, title.option = "only.dist",
                 ylab = paste("F(",xlab,")", sep = ""), trunc.correct = T,
                 cex.points = cex.points,
                 band.type = band.type, title.line.adj = title.line.adj)
      mtext(text = ylab, side = 2, line = 2, outer = T, cex = 1.1)
      mtext(text = xlab, side = 1, line = 2, outer = T, cex = 1.1)
    }


    if (conf.level > 0.1) {
      conf.int.title <- paste("and", generic.band.type(band.type),
                              percent.conf.level(conf.level), "Confidence Intervals")
    } else {
      conf.int.title <- ""
    }
    if (is.null(my.title)) {
      my.title <- paste(get.data.title(data.ld), "\nProbability Plots",
                        conf.int.title)
    }
    if (title.option == "full") {
      title.line <- 0

      mtext(side = 3, line = 0.5, outer = T, text = my.title,
            cex = 1.3)
    }
    invisible()
  }
