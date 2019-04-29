plot.prob.succ.demo.type2 <-
function (x, qprob, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    the.n.r.values = 1:length(names(x)), do.legend = "On plot",
    xlim = c(NA, NA), my.title = NULL, ylim = c(NA, NA),
    grids = T, bw = F,...)
{
    prob.success.demo.out <- list()
    names.all.x <- names(x)
    data.xlim <- NULL
    data.ylim <- NULL
    number.lines <- length(names.all.x)
    if (bw) {
        color.map <- 1:number.lines
        lty.map <- rep(1, number.lines)
}   else {
        color.map <- rep(1, number.lines)
        lty.map <- 1:number.lines
    }
    distribution <- attr(x[[1]], "distribution")
    if (is.null(my.title))
        my.title <- paste("Demonstration that", distribution,
            "Distribution Reliability \nExceeds", qprob, "with",
            percent.conf.level(conf.level), "Confidence")
    text.vec <- rep(NA, length(names(x)))
    for (i in the.n.r.values) {
        the.x <- prob.success.demo(x[[i]], qprob = qprob,
            conf.level = conf.level)
        prob.success.demo.out[[names.all.x[i]]] <- the.x
        data.xlim <- range(data.xlim, the.x$qstar.vec)
        data.ylim <- range(data.ylim, the.x$prsd)
        text.vec[i] <- paste(paste("n=", attr(the.x, "sample.size"),
            sep = ""), paste("r=", attr(the.x, "number.fail"),
            sep = ""), sep = ", ")
    }
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- data.xlim[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- data.ylim[yrna]
    plot.paper(xlim, ylim, xlab = "Actual Reliability",
        ylab = "Pr(Successful Demonstration)", grids = grids,
        my.title = "")
    title(main = my.title)
    for (i in the.n.r.values) {
        the.x <- prob.success.demo.out[[names.all.x[i]]]
        dvec <- the.x$qstar.vec
        answer <- the.x$prsd
        lines(dvec, answer, lty = i, col = i,
            lwd = 2)
        #if (lty.map[i] == 2)
        #   lines(dvec, answer, lty = lty.map[i], col = color.map[i],
        #      lwd = 3)
    }
    lwd.fix <- rep(1, number.lines)
    bty = "o"
    bg0 = "white"
    switch(do.legend, `On plot` = , `Top Left` = {
        if (number.lines > 1) lwd.fix[2] <- 3
        legend(x.loc(0.01), y.loc(0.95), bty = bty, bg = bg0,
            text.vec[rev(the.n.r.values)], cex = 1.1, col = 1:number.lines,
            lty = 1:number.lines, lwd = lwd.fix, y.intersp = 0.675)
    }, `Bottom Right` = {
        if (number.lines > 1) lwd.fix[2] <- 3
        legend(x.loc(0.75), y.loc(0.25), bty = bty, bg = bg0,
            text.vec[rev(the.n.r.values)], cex = 1.1, col = 1:number.lines,
            lty = 1:number.lines, lwd = lwd.fix, y.intersp = 0.675)
    }, `New page` = {
        plot(c(0, 0), c(1, 1), xlab = "", ylab = "", type = "n",
            xaxt = "n", yaxt = "n")
        legend(x.loc(0.01), y.loc(0.95), bty = bty, bg = bg0,
            text.vec[rev(the.n.r.values)], cex = 1.1, col = 1:number.lines,
            lty = 1:number.lines, lwd = lwd.fix, y.intersp = 0.675)
    })
    invisible()
}
