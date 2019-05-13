#' @export
plot.prior.pdf <-
function (x, s2, distribution = "triangle", parname = "sigma",
    smallprob = pnorm(-3), fudge = 1.1,...)
{
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow = c(1, 2))
    par(mar = par("mar") + c(0, 1.5, 0, 0))
    triang.pdf <- function(x, low, upper, smallprob) {
        w <- (upper - low)/2
        ind <- (x <= (low + w))
        (ind * (x - low))/(w^2) + ((1 - ind) * (upper - x))/(w^2)
    }
    unif.pdf <- function(x, low, upper, smallprob) {
        return((1 + x * 0)/(upper - low))
    }
    normal.pdf <- function(x, low, upper, smallprob) {
        mu <- (low + upper)/2
        sigma <- (upper - mu)/qnorm(1 - smallprob)
        return(dnorm((x - mu)/sigma))
    }
    switch(distribution, triangle = {
        FUN <- triang.pdf
    }, normal = {
        FUN <- normal.pdf
    }, uniform = {
        FUN <- unif.pdf
    }, {
        stop(paste(distribution, "is unrecognized distribution"))
    })
    slope.sign <- sign(1 - 0.5 * logb(s2/x))
    sval <- seq(x, s2, length = 10000)
    pdf.log <- FUN(logb(sval), logb(x), logb(s2), smallprob)
    yrange <- range(c(pdf.log, 0))
    yrange[2] <- fudge * yrange[2]
    plot(range(sval), yrange, type = "n", log = "x", xlab = paste(parname,
        "[log axis]"), yaxt = "n", ylab = "", cex = 1.2, yaxs = "i")
    lines(sval, pdf.log, lwd = 2)
    if (distribution == "uniform") {
        lines(c(x, x), c(0, pdf.log[1]))
        lines(c(s2, s2), c(0, pdf.log[1]))
    }
    pdf.orig <- (1/sval) * FUN(logb(sval), logb(x), logb(s2),
        smallprob)
    yrange <- range(c(pdf.orig, 0))
    yrange[2] <- 1.1 * yrange[2]
    plot(range(sval), yrange, type = "n", xlab = parname, yaxt = "n",
        ylab = "", cex = 1.2, yaxs = "i")
    lines(sval, pdf.orig, lwd = 2)
    if (distribution == "uniform") {
    }
}
