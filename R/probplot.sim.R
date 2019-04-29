probplot.sim <-
function (distribution = "normal", parameter = NULL, sample.size = 10, 
    axis.probs = c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 
        0.99), print.samp.size = F, cex = 0.7, xlim = c(NA, 
        NA), ylim = c(NA, NA)) 
{
    result <- nsf.sim(distribution, parameter, sample.size)
    xdata <- sort(result$sample)
    ppoint <- (1:length(xdata) - 0.5)/length(xdata)
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(xdata)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(qnorm(ppoint))[yrna]
    plot(xlim, ylim, yaxt = "n", type = "n", ylab = "", 
        xlab = "data", cex = 0.7)
    points.default(xdata, qnorm(ppoint), cex = (cex * (GetSMRDDefault("SMRD.point.size")))/100, 
        pch = 16)
    axis(side = 2, at = qnorm(as.numeric(axis.probs)), labels = axis.probs, 
        adj = 1, tck = -0.02, mgp = c(5, 1.1, 0), cex = 0.7)
    if (print.samp.size) 
        mtext(side = 3, line = 1, text = paste("Sample Size=", 
            sample.size), cex = 1)
    invisible(list(sim = result$cdist, parameter = result$parameter, 
        parameter.name = result$parameter.name))
}
