CirclePlot <-
function (cex = 1, do.old.par = T, cir.contours = c(0.1, 0.2,
    0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), radius.lines = seq(0,
    350, by = 10), angle.markers = seq(0, 330, by = 30), background.lwd = 0.5)
{
    par(pty = "s")
    plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", xaxt = "n",
        yaxt = "n", xlab = "", ylab = "", cex = cex)
    for (i in 1:length(cir.contours)) {
        DrawCircle(cir.contours[i], background.lwd = background.lwd)
    }
    for (i in 1:length(radius.lines)) {
        DrawRadiusLine(angle = radius.lines[i], background.lwd = background.lwd)
    }
    for (i in 1:length(angle.markers)) {
        DrawLine(angle.markers[i])
    }
}
