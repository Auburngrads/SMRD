plotsym <-
function (x, y, symbols, x.axis = "linear", y.axis = "linear",
    special = T, the.sub.title = NULL, add = F, do.rev = T, do.legend = F,
    new.page = T, ylim = c(NA, NA), xlim = c(NA, NA), xlab = "",
    ylab = "", cex.points = 1.2, square.plot = F, ideal.line = F,
    do.old.par = T, pch.vec = 1:number.symbols, col.vec = (1:(number.symbols +
        1))[-5], ...)
{
    symbols <- as.factor(symbols)
    if (square.plot) {
        old.par <- par("pty")
        par(pty = "s")
        if (do.old.par)
            on.exit(par(pty = old.par))
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- range(x)[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- range(y)[yrna]
        xlim <- range(xlim, ylim)
        ylim <- xlim
}   else {
        xrna <- is.na(xlim)
        if (any(xrna))
            xlim[xrna] <- range(x)[xrna]
        yrna <- is.na(ylim)
        if (any(yrna))
            ylim[yrna] <- range(y)[yrna]
    }
    usymbols <- unique(levels(symbols))
    cat("xlim=c(", format(xlim), "),ylim=c(", format(ylim),
        ")\n")
    if (!add) {
        plot.paper(xlim, ylim, x.axis = x.axis, y.axis = y.axis,
            grids = F, xlab = xlab, ylab = ylab)
        if (!is.null(the.sub.title))
            mtext(text = the.sub.title, side = 3, line = 0.5,
                cex = 0.8)
    }
    number.symbols <- length(usymbols)
    for (i in 1:number.symbols) {
        the.ones <- usymbols[i] == symbols & special
        if (length(x[the.ones]) > 0) {
            points.default(x[the.ones], y[the.ones], pch = pch.vec[i],
                col = col.vec[i], xpd = T, cex = cex.points,
                ...)
        }
        the.ones <- usymbols[i] == symbols & !special
        if (length(x[the.ones]) > 0) {
            points.default(x[the.ones], y[the.ones], pch = 16,
                col = col.vec[i], cex = cex.points, xpd = T,
                ...)
        }
    }
    if (ideal.line) {
        abline(0, 1)
    }
    if (is.numeric(do.legend)) {
        the.xloc <- do.legend[1]
        the.yloc <- do.legend[2]
        do.legend <- T
        new.page <- F
}   else {
        the.xloc <- 0.8
        the.yloc <- 0.8
    }
    if (do.legend) {
        if (new.page)
            plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
        legend(x.loc(the.xloc), y.loc(the.yloc), as.character(usymbols),
            col = 1:length(usymbols), pch = 1:length(usymbols),y.intersp = 0.675)
    }
    return(list(symbols = as.character(usymbols), col = col.vec[1:length(usymbols)],
        marks = pch.vec[1:length(usymbols)]))
}
