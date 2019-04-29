DrawBox <-
function (center, dx, dy, ...) 
{
    NW <- c(center[1] - dx/2, center[2] + dy/2)
    NE <- c(center[1] + dx/2, center[2] + dy/2)
    SW <- c(center[1] - dx/2, center[2] - dy/2)
    SE <- c(center[1] + dx/2, center[2] - dy/2)
    DrawLine(NW, NE, ...)
    DrawLine(NE, SE, ...)
    DrawLine(SE, SW, ...)
    DrawLine(SW, NW, ...)
}
