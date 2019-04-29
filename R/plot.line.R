plot.line <-
function (x, point2, xrange = NULL, col = 1, lty = 1, lwd = 1,...)
{
    if (x[1] - point2[1] != 0) {
        slope <- (x[2] - point2[2])/(x[1] - point2[1])
        intercept <- x[2] - slope * x[1]
        if (is.null(xrange))
            xrange <- c(x[1], point2[1])
        yrange <- intercept + slope * xrange
        lines(xrange, yrange, col = col, lty = lty, lwd = lwd)
}   else {
        lines(c(x[1], point2[1]), c(x[2], point2[2]),
            col = col, lty = lty, lwd = lwd)
    }
}
