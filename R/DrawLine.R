DrawLine <-
function (start, end, arrow = F, ...) 
{
    if (arrow) 
        arrows(start[1], start[2], end[1], end[2], ...)
    else lines(c(start[1], end[1]), c(start[2], end[2]), ...)
}
