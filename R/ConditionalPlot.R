ConditionalPlot <-
function (y.var, x.var, cond.var, data, my.title = NULL, xlim = c(NA, 
    NA), ylim = c(NA, NA), xlab = NULL, ylab = NULL, lock.range = T) 
{
    unique.cond.var <- unique(data[, cond.var])
    old.par <- par(mfrow = get.mfcol.vec(length(unique.cond.var)), 
        oma = c(0, 4, 4, 0), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    save.SMRD.options <- SMRDOptions(SMRD.DateOnPlot = F, 
        SMRD.NameOnPlot = "")
    if (is.null(xlab)) 
        xlab <- x.var
    if (is.null(ylab)) 
        ylab <- y.var
    if (lock.range) {
        xrna <- is.na(xlim)
        if (any(xrna)) 
            xlim[xrna] <- range(data[, x.var])[xrna]
        yrna <- is.na(ylim)
        if (any(yrna)) 
            ylim[yrna] <- range(data[, y.var])[yrna]
    }
    for (i in 1:length(unique.cond.var)) {
        the.ones <- unique.cond.var[i] == data[, cond.var]
        sub.data <- data[the.ones, , drop = F]
        if (!lock.range) {
            xrna <- is.na(xlim)
            if (any(xrna)) 
                xlim[xrna] <- range(sub.data[, x.var])[xrna]
            yrna <- is.na(ylim)
            if (any(yrna)) 
                ylim[yrna] <- range(sub.data[, y.var])[yrna]
        }
        plot(xlim, ylim, type = "n", xlab = "", ylab = "")
        points.default(sub.data[, x.var], sub.data[, y.var])
        title(xlab = xlab, ylab = ylab, cex = 1.1)
        mtext(text = paste(cond.var, "=", format(unique.cond.var[i])), 
            side = 3, outer = F, line = 2, cex = 1)
    }
    SMRDOptions(save.SMRD.options)
    if (is.null(my.title)) 
        my.title <- paste("")
    mtext(text = my.title, side = 3, outer = T, line = 2, cex = 1.2)
    invisible()
}
