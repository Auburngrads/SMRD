DrawCircle <-
function (radius = 1, angle = seq(1, 360, by = 1), background.lwd = 1) 
{
    radians <- function(x) {
        (x * 2 * pi)/360
    }
    xvec <- radius * sin(radians(angle))
    yvec <- radius * cos(radians(angle))
    lines(xvec, yvec, lwd = background.lwd)
}
