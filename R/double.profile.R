double.profile <-
function (struct, levels = c(0.01, 2, 0.3, 0.4, 0.6, 0.8, 0.9), 
    theta = 0, phi = 15, r = sqrt(3)) 
{
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow = c(1, 2), oma = c(0, 0, 6, 0))
    contour(x = struct$x, y = struct$y, z = struct$z, levels = levels, 
        xlab = struct$ylab, ylab = struct$xlab)
    title(main = "Profile Likelihood Contour Plot")
    wqm.persp(x = struct$x, y = struct$y, z = struct$z, theta = theta, 
        phi = phi, r = r, xlab = struct$xlab, ylab = struct$ylab, 
        zlab = "Profile")
    title(main = "Profile Likelihood Perspective Plot")
    if (!is.null(struct$subtitle)) 
        text <- struct$subtitle
    else text <- "Profile Likelihood Plots"
    mtext(side = 3, line = 2, cex = 2, outer = TRUE, text = text)
}
