haz.varplot.setup <-
function (distribution, my.title = distribution) 
{
    sev.tick.labels <- c(".001", ".005", ".01", ".02", ".05", 
        ".1", ".2", ".3", ".5", ".7", ".9", ".99")
    sev.tick.locations <- c(".001", ".002", ".003", ".004", ".005", 
        ".006", ".007", ".008", ".009", ".01", ".012", ".014", 
        ".016", ".018", ".02", ".03", ".04", ".045", ".05", ".06", 
        ".07", ".08", ".09", ".1", ".12", ".14", ".16", ".18", 
        ".2", ".22", ".24", ".26", ".28", ".3", ".35", ".4", 
        ".45", ".5", ".55", ".6", ".65", ".7", ".75", ".8", ".85", 
        ".9", ".92", ".94", ".96", ".98", ".99")
    normal.tick.labels <- c(".001", ".005", ".01", ".02", ".05", 
        ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", 
        ".95", ".99")
    normal.tick.locations <- c(".001", ".002", ".003", ".004", 
        ".005", ".006", ".007", ".008", ".009", ".01", ".015", 
        ".02", ".03", ".04", ".05", ".06", ".07", ".08", ".09", 
        ".1", ".12", ".14", ".16", ".18", ".2", ".25", ".3", 
        ".35", ".4", ".45", ".5", ".55", ".6", ".65", ".7", ".75", 
        ".8", ".82", ".84", ".86", ".88", ".9", ".91", ".92", 
        ".93", ".94", ".95", ".96", ".97", ".98", ".99")
    variance.tick.labels <- c(".5", "1", "5", "10", "50", "100", 
        "500", "1000", "3000")
    variance.tick.locations <- c(".5", ".6", ".7", ".8", ".9", 
        "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "20", 
        "30", "40", "50", "60", "70", "80", "90", "100", "200", 
        "300", "400", "500", "600", "700", "800", "900", "1000", 
        "2000", "3000")
    switch(distribution, `Smallest Extreme Value` = , sev = {
        tick.locations <- as.numeric(sev.tick.locations)
        tick.labels <- sev.tick.labels
    }, normal = , Normal = {
        tick.locations <- as.numeric(normal.tick.locations)
        tick.labels <- normal.tick.labels
        logger <- ""
    }, logistic = , Logisstic = {
        tick.locations <- as.numeric(normal.tick.locations)
        tick.labels <- normal.tick.labels
        logger <- "x"
    })
    variance.tick.locations <- as.numeric(variance.tick.locations)
    tick.label.loc <- as.numeric(tick.labels)
    old.par <- par(mar = c(4.5, 5, 3.5, 2) + 0.1, err = -1)
    xlim <- range(quant(tick.locations, distribution))
    ylim <- range(logb(variance.tick.locations))
    plot(xlim, ylim, xlab = "", ylab = "", xaxt = "n", 
        yaxt = "n", type = "n", cex = 1.1)
    axis(side = 1, at = quant(tick.locations, distribution), 
        labels = F, tck = -0.01, mgp = c(5, 2.1, 0), cex = 1.1)
    axis(side = 1, at = quant(tick.label.loc, distribution), 
        labels = tick.labels, adj = 0.5, tck = -0.02, mgp = c(5, 
            1.1, 0), cex.axis = 1.1)
    qtick.locations <- quant(tick.locations, distribution)
    yvec.low <- rep(logb(0.1), length(qtick.locations))
    yvec.high <- rep(logb(5000), length(qtick.locations))
    matlines(rbind(qtick.locations, qtick.locations), rbind(yvec.low, 
        yvec.high), col = "steelblue", lty = 3, lwd = 0.85)
    axis(side = 2, at = logb(variance.tick.locations), labels = F, 
        tck = -0.01, mgp = c(5, 2.1, 0), cex.axis = 1.1)
    axis(side = 2, at = logb(as.numeric(variance.tick.labels)), 
        labels = variance.tick.labels, adj = 1, tck = -0.02, 
        mgp = c(5, 1.1, 0), cex.axis = 1.1, las = 1)
    ltick.locations <- logb(variance.tick.locations)
    xvec.low <- rep(quant(1e-04, distribution), length(ltick.locations))
    xvec.high <- rep(quant(0.9999, distribution), length(ltick.locations))
    matlines(rbind(xvec.low, xvec.high), rbind(ltick.locations, 
        ltick.locations), col = "steelblue", lty = 3, lwd = 0.85)
    mtext(side = 1, line = 3, expression("Quantile "*p[e]*" Corresponding to Time of Interest"), 
        cex = 1.1)
    mtext(side = 2, line = 3, parse(text = "V[log(widehat(h))]"), 
        cex = 1.1)
    #mtext(side = 3, line = 3, outer = F, text = my.title, cex = 1.5)
}
