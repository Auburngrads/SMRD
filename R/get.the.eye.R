get.the.eye <-
function (direction, elevation, distance) 
{
    radians <- function(degrees) {
        (pi * degrees)/180
    }
    direction <- radians(direction)
    elevation <- radians(elevation)
    zzz <- cos(elevation) * distance
    rrr <- sin(elevation) * distance
    eye <- cbind(sin(direction) * rrr, cos(direction) * rrr, 
        zzz)
    return(eye)
}
