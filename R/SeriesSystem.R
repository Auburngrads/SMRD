SeriesSystem <-
function (n, RL = 1, RY = 1.3, border = 0.1, ycenter = 0.5, ...) 
{
    plot(c(0, 1), c(1, 0), type = "n", xaxt = "n", yaxt = "n", 
        xlab = "", ylab = "", bty = "n")
    dx <- (1 - 2 * border)/(n * RL + n + RL)
    dy <- RY * dx
    dL <- RL * dx
    DrawLine(c(border, ycenter), c(border + dL, ycenter), ...)
    StartPoint <- border + dL
    for (i in 1:n) {
        DrawBox(c(StartPoint + dx/2, ycenter), dx, dy, ...)
        DrawLine(c(StartPoint + dx, ycenter), c(StartPoint + 
            dx + dL, ycenter), ...)
        text(StartPoint + dx/2, ycenter, as.character(i))
        StartPoint <- StartPoint + dx + dL
    }
}
