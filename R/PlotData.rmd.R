PlotData.rmd <-
function (data.rmd, xlab = attr(data.rmd, "time.units"), ylab = attr(data.rmd, 
    "degradation.units"), my.title = get.data.title(data.rmd), 
    xlim = c(NA, NA), ylim = c(NA, NA)) 
{
    Time <- data.rmd$Time
    Degradation <- data.rmd$Degradation
    Unit <- data.rmd$Unit
    unique.units <- unique(Unit)
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- range(Time)[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- range(Degradation)[yrna]
    plot.paper(xlim, ylim, grids = F, xlab = xlab, ylab = ylab)
    title(my.title)
    for (i in 1:length(unique.units)) {
        the.ones <- unique.units[i] == Unit
        sub.times <- Time[the.ones]
        sub.degrad <- Degradation[the.ones]
        lines(sub.times, sub.degrad, type = "b", col = length(sub.degrad)/2)
    }
}
