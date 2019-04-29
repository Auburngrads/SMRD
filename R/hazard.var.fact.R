hazard.var.fact <-
function (distribution, number.points = 200, my.title = distribution) 
{
    proportion.failing <- c(".005", ".01", ".015", ".02", ".03", 
        ".04", ".05", ".07", ".1", ".15", ".2", ".3", ".4", ".5", 
        ".6", ".7", ".8", ".9", ".999")
    std.log.censor.time <- quant(as.numeric(proportion.failing), 
        distribution)
    std.quant.of.int <- seq(quant(5e-04, distribution), quant(0.9995, 
        distribution), length = number.points)
    haz.varplot.setup(distribution, my.title = my.title)
    var.out <- ftavarvec(distribution, std.log.censor.time)
    dg.dmu <- -(wqmf.phip(std.quant.of.int, distribution)/wqmf.phis(std.quant.of.int, 
        distribution) + wqmf.phis(std.quant.of.int, distribution)/exp(wqmf.phibml(std.quant.of.int, 
        distribution)))
    dg.dsigma <- -((wqmf.phip(std.quant.of.int, distribution)/wqmf.phis(std.quant.of.int, 
        distribution) + wqmf.phis(std.quant.of.int, distribution)/exp(wqmf.phibml(std.quant.of.int, 
        distribution))) * std.quant.of.int + 1)
    ymatrix <- matrix(0, ncol = length(proportion.failing), nrow = number.points)
    for (i in 1:length(proportion.failing)) {
        ymatrix[, i] <- dg.dmu^2 * var.out$v11[i] + 2 * dg.dmu * 
            dg.dsigma * var.out$v12[i] + dg.dsigma^2 * var.out$v22[i]
    }
    matlines(std.quant.of.int, logb(ymatrix), col = 1, lty = 1, 
        lwd = 2)
    switch(as.character(numdist(distribution)), `1` = {
        mtext(side = 3, at = log(1-exp(-exp(-1.98432))), parse(text = "p[c]==.005"), adj = 0.25, cex = 0.8)
        mtext(side = 3, at = log(1-exp(-exp(0.892837))), ".01", adj = -0.95, cex = 0.8, padj = -.25 )
        mtext(side = 4, at = 7.74566, ".02", las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 6.6, ".05", las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 6.18, ".07",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 5.75, ".10",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 4.6, ".20",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 3.5, ".40",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 2.5, ".70",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 1.51, "1",las = 1, cex = 0.8, adj = -1.25)
    }, `3` = {
        mtext(side = 4, at = 8.2, parse(text = "p[c]"), las = 1, cex = 0.9, adj = -.75)
        mtext(side = 4, at = 7.2, ".005",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 6.28928, ".01",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 5.5, ".02",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 4.55, ".05",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 4.1, ".07",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 3.6, ".10",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 2.76, ".20",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 1.9, ".40",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 1.12, ".70",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at = 0.52, "1",las = 1, cex = 0.8, adj = -1.25)
    }, `5` = {
        mtext(side = 4, at = 8.2, parse(text = "p[c]"), las = 1, cex = 0.9, adj = -.75)
        mtext(side = 4, at =  5.5, ".005",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  4.7, ".01",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  3.97, ".02",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  3.04, ".05",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  2.7, ".07",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  2.33, ".10",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  1.56, ".20",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  0.78, ".40",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  0.09, ".70",las = 1, cex = 0.8, adj = -.25)
        mtext(side = 4, at =  -0.41, "1",las = 1, cex = 0.8, adj = -1.25)
    })
}
