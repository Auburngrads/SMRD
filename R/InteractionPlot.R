InteractionPlot <-
function (interp.out, yindices = c(1, 4, 7, 10))
{
    zmat <- interp.out$z
    xvec <- interp.out$x
    yvec <- interp.out$y
    number.x <- length(xvec)
    plot.paper(range(interp.out$x), range(zmat), grids = F, xlab = parse(text = "C**o"),
        ylab = "Hours")
    for (i in 1:length(yindices)) {
        yval <- yvec[yindices[i]]
        lines(xvec, zmat[yindices[i], ])
        text(xvec[number.x] + 4, zmat[yindices[i], number.x],
            paste(as.character(yval), "mA"))
    }
}
