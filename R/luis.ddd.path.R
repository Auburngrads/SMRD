luis.ddd.path <-
function (xuse, beta0, beta1, beta2, sigma, transformation, critdeg,
    TimeUnits,ResponseUnits, legendx = 0.4, legendy = 0.9)
{
    temp <- xuse[1]
    x <- luis.arrhenius(temp)
    Time <- seq(0, 60, length = 400)
    ymax <- 100
    N <- exp(beta0 + beta1 * sqrt(Time) * exp(-beta2 * x))
    if (transformation == "yes") {
        plot.paper(range(Time), c(10, ymax), grids = F, xlab = TimeUnits,
            ylab =ResponseUnits, x.axis = "sqrt", y.axis = "log")
}   else {
        plot.paper(range(Time), c(10, ymax), grids = F, xlab = TimeUnits,
            ylab =ResponseUnits, x.axis = "linear", y.axis = "linear")
    }
    abline(h = critdeg)
    number.lines <- length(xuse)
    j <- 2
    for (i in 1:number.lines) {
        temp <- xuse[i]
        x <- luis.arrhenius(temp)
        N <- exp(beta0 + beta1 * sqrt(Time) * exp(-beta2 * x))
        if (transformation == "yes")
            lines(sqrt(Time), log(N), lty = i)
        else lines(Time, N, lty = i)
    }
    labels <- paste(xuse, "DegreesC")
    cat(x.loc(legendx), y.loc(legendy), legendx, legendy, "\n")
    legend(x.loc(legendx), y.loc(legendy), labels,
           lty = 1:number.lines, y.intersp = 0.675)
}
