contour.my.surface <-
function (x, x1.range, x2.range, size = 50,...)
{
    tran.x1.range <- f.relationship(x1.range, "arrhenius3")
    tran.x2.range <- f.relationship(x2.range, "log")
    x1.vec <- (seq(tran.x1.range[1], tran.x1.range[2], length = size))
    x2.vec <- seq(tran.x2.range[1], tran.x2.range[2], length = size)
    the.fun <- function(x1, x2, x) {
        return(x[1] - x1 * x[2] + x2 * x[3] - x1 * x2 *
            x[4])
    }
    plot.paper(x1.range, x2.range, grids = F, x.axis = "arrhenius3",
        y.axis = "log", xlab = parse(text = "C**o"), ylab = "Current")
    if (!is.na(x)) {
        result <- exp(outer(x1.vec, x2.vec, the.fun, x = x))
        contour(x1.vec, x2.vec, result, add = T)
    }
}
