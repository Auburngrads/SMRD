DrawRadiusLine <-
function (angle, radius = 1, origin.radius = 0.1, background.lwd = 1) 
{
    radians <- function(x) {
        (x * 2 * pi)/360
    }
    x <- radius * sin(radians(angle))
    y <- radius * cos(radians(angle))
    origin <- c(origin.radius * sin(radians(angle)), origin.radius * 
        cos(radians(angle)))
    DrawLine(origin, c(x, y), lwd = background.lwd)
}
