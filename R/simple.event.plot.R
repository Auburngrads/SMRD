simple.event.plot <-
function (data.ld, my.title = NULL) 
{
    nprepair.out <- nprepair(data.ld)
    iusys <- nprepair.out$iusys
    ctime <- nprepair.out$ctime
    idelta <- as.matrix(nprepair.out$idelta)
    nsys <- length(iusys)
    minx <- 0 - max(nprepair.out$utime)/20
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    plot(c(minx, max(nprepair.out$utime, nprepair.out$ctime)), 
        c(0, 1), type = "n", xlab = "", ylab = "", yaxt = "n", 
        cex = 1.5, bty = "n")
    title(xlab = paste("Age in", data.ld$time.units), cex = 1.5)
    if (is.null(my.title)) 
        my.title <- data.ld$data.title
    title(my.title)
    textx <- par("usr")[1]
    ydelta <- 1/(nsys + 1)
    vindic <- min(ydelta/3, 1/(11 * 3))
    for (is in 1:length(iusys)) {
        ypos <- 0.1
        lines(c(0, ctime[is]), c(ypos, ypos), lwd = 2)
        if (ctime[is] > 0) 
            lines(c(ctime[is], ctime[is]), c(ypos - vindic, ypos + 
                vindic), lwd = 2)
        markers <- idelta[, is]
        event.times <- nprepair.out$utime[markers > 0]
        points.default(event.times, rep(ypos, length = length(event.times)), 
            pch = 4)
    }
    invisible(nprepair.out)
}
