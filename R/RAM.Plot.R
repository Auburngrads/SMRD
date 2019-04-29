RAM.Plot <-
function (grids = F, axsi = T, cex.point = 1.5, ...)
{
    if (axsi)
        old.par <- par(yaxs = "i", xaxs = "i")
    else old.par <- par()
    on.exit(par(old.par))
    lambda <- (-log(0.5))/300
    eta <- 300/((-log(0.5))^0.5)
    probplot.setup(xlim = c(101, 999), ylim = c(0.011,
        0.989), distribution = "Weibull", xlab = "Time", ylab = "Unreliability F(t)",
        grids = grids, ...)
    xvec <- logseq(100, 1000, length = 100)
    normal <- pnorm(xvec, mean = 300, sd = 30)
    Weibull <- pweibull(xvec, scale = eta, shape = 2)
    Exponential <- pweibull(xvec, scale = 1/lambda, shape = 1)
    lines(log(xvec), quant(normal, "sev"), lty = 1, lwd = 2)
    lines(log(xvec), quant(Weibull, "sev"), lty = 3, lwd = 2)
    lines(log(xvec), quant(Exponential, "sev"), lty = 5, lwd = 2)
    points(log(300), quant(0.5, "sev"), cex = cex.point, pch = 15)
    points(log(1/lambda), quant(pweibull(1/lambda, scale = 1/lambda,
        shape = 1), "sev"), cex = cex.point, pch = 15)
    points(log(266), quant(pweibull(266, scale = eta, shape = 2),
        "sev"), cex = cex.point, pch = 15)
}
