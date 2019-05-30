#' @export
plot.vpm.vs.size <-
function (x = c(100, 150, 200), 
          size.vec = c(1, 2, 3),
          use.conditions = c(50.1),
          debug1 = F,...)
{
    old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), pty = "s",
        mar = c(5.1, 5.1, 6.1, 4.1))
    on.exit({
        par(old.par)
        par(new = F)
    })
    char.list <- expand.grid(vpm = x, size = size.vec)
    if(debug1) browser()
    vpm <- char.list[, 1]
    size <- char.list[, 2]
    volts <- vpm * size
    plot(c(50, 200), c(1, 3), type = "n", xlab = "Voltage Stress (Volts/mm)",
        ylab = "Size (mm)", cex = 1.2)
    points.default(vpm, size)
    points.default(50, 1, pch = 0, cex = 2)
    plot(c(50, 600), c(1, 3), type = "n", xlab = "Volts", ylab = "Size (mm)",
        cex = 1.2)
    points.default(volts, size)
    points.default(50, 1, pch = 0, cex = 2)
    out.matrix <- cbind(size = size, vpm = vpm, volts = volts)
    out.matrix
}
