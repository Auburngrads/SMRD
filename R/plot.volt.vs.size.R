plot.volt.vs.size <-
function (x = c(100, 200, 300), size.vec = c(1, 2, 3),...)
{
    old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), pty = "s",
        mar = c(5.1, 5.1, 6.1, 4.1))
    on.exit({
        par(old.par)
        par(new = F)
    })
    char.list <- expand.grid(volts = x, size = size.vec)
    volts <- as.numeric(char.list[[1]])
    size <- as.numeric(char.list[[2]])
    vpm <- volts/size
    plot(c(30, 300), c(1, 3), type = "n", xlab = "Volts", ylab = "Size (mm)",
        cex = 1.2)
    points.default(volts, size)
    points.default(50, 1, pch = 0, cex = 2)
    plot(c(30, 400), c(1, 3), type = "n", xlab = "Voltage Stress\n(Volts/mm)",
        ylab = "Size (mm)", cex = 1.2)
    points.default(vpm, size)
    points.default(50, 1, pch = 0, cex = 2)
    invisible(cbind(size = size, volts = volts, vpm = vpm))
}
