quantile.var.fact <-
function (x, number.points = 200, my.title = x,...)
{
    proportion.failing <- c(".005", ".01", ".015", ".02", ".03",
        ".04", ".05", ".07", ".1", ".15", ".2", ".3", ".4", ".5",
        ".6", ".7", ".8", ".9", ".99999")
    std.log.censor.time <- quant(as.numeric(proportion.failing),
        x)
    std.quant.of.int <- seq(quant(5e-04, x), quant(0.9995,
        x), length = number.points)
    varplot.setup(x, my.title = my.title)
    var.out <- ftavarvec(x, std.log.censor.time)
    ymatrix <- matrix(0, ncol = length(proportion.failing), nrow = number.points)
    for (i in 1:length(proportion.failing)) {
        ymatrix[, i] <- var.out$v11[i] + 2 * std.quant.of.int *
            var.out$v12[i] + (std.quant.of.int)^2 * var.out$v22[i]
    }
    matlines(std.quant.of.int, logb(ymatrix), col = 1, lty = 1,
        lwd = 2)
    switch(as.character(numdist(x)), `1` = {
        mtext(side = 3, at = log(1-exp(-exp(-2.8))), parse(text = "p[c]==.005"), cex = 0.8, adj = .5)
        mtext(side = 3, at = log(1-exp(-exp(-1))), ".01", cex = 0.8, padj = -.25)
        mtext(side = 3, at = log(1-exp(-exp(1.28))), ".02", cex = 0.8, padj = -.25)
        mtext(side = 4, at = 6.18, ".05", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 5.72, ".07", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 5.23, ".10", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 4.12, ".20", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 2.93, ".40", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 1.76, ".70", cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 0.8, "1",cex = 0.8, adj = -1.25, las = 1)
    }, `3` = {
        mtext(side = 3, at = 0.280443, parse(text = "p[c]==.005"),cex = 0.8, adj = 0.5)
        mtext(side = 3, at = 1.5965, ".01",cex = 0.8, padj = -.25)
        mtext(side = 4, at = 6.77, ".02",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 5.66, ".05",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 5.2, ".07",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 4.77, ".10",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 3.85, ".20",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 2.85, ".40",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 2.02, ".70",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 1.38, "1",cex = 0.8, adj = -1.25, las = 1)
    }, `5` = {
        mtext(side = 3, at = -2.82, parse(text = "p[c]==.005"),cex = 0.8, adj = 0.5)
        mtext(side = 3,at = -0.99, ".01", cex = 0.8,padj = -.25)
        mtext(side = 3, at = 1.25, ".02",cex = 0.8, padj = -.25)
        mtext(side = 4, at = 7.2, ".05",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 6.71, ".07",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 6.24, ".10",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 5.29228, ".20",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 4.2693, ".40",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 3.4094, ".70",cex = 0.8, adj = -.25, las = 1)
        mtext(side = 4, at = 2.91, "1",cex = 0.8, adj = -1.25, las = 1)
    })
}
