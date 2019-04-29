show.use.rate.likelihood <-
function (failure.mode, number.points = 100) 
{
    xvec <- seq(-3, 3, length = number.points)
    yvec <- seq(-3, 3, length = number.points)
    quadfun <- function(x, y, rho) (x^2 + y^2 - 2 * rho * x * 
        y)
    zmat <- outer(xvec, yvec, FUN = quadfun, rho = 0.7)
    old.par <- par(pty = "s")
    on.exit(par(old.par))
    contour(zmat, xvec, yvec, xlab = "Time Wear", ylab = "Time Crack", 
        xaxt = "n", yaxt = "n", levels = c(0.2, 1, 3, 5), labex = 0)
    abline(0, 1)
    delta = 2
    time.point <- 38
    switch(failure.mode, mode2 = {
        DrawLine(c(time.point, time.point - delta), c(time.point, 
            time.point + delta))
        DrawLine(c(time.point, time.point - delta), c(200, time.point - 
            delta))
        DrawLine(c(time.point, time.point + delta), c(200, time.point + 
            delta))
        DrawLine(c(200, time.point - delta), c(200, time.point + 
            delta))
        polyx <- c(time.point, 200, 200, time.point)
        polyy <- c(time.point - delta, time.point - delta, time.point + 
            delta, time.point + delta)
        polygon(polyx, polyy, density = 20, angle = -45, border = F, 
            col = 6)
    }, mode1 = {
        DrawLine(c(time.point - delta, time.point), c(time.point + 
            delta, time.point))
        DrawLine(c(time.point - delta, time.point), c(time.point - 
            delta, 200))
        DrawLine(c(time.point + delta, time.point), c(time.point + 
            delta, 200))
        DrawLine(c(time.point - delta, 200), c(time.point + delta, 
            200))
        polyy <- c(time.point, 200, 200, time.point)
        polyx <- c(time.point - delta, time.point - delta, time.point + 
            delta, time.point + delta)
        polygon(polyx, polyy, density = 20, angle = -45, border = F, 
            col = 6)
    }, censored = {
        DrawLine(c(time.point, time.point), c(time.point, 200))
        DrawLine(c(time.point, time.point), c(200, time.point))
        polyx <- c(time.point, 200, 200, time.point)
        polyy <- c(time.point, time.point, 200, 200)
        polygon(polyx, polyy, density = 20, angle = -45, border = F, 
            col = 6)
    })
}
