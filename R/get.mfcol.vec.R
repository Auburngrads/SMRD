get.mfcol.vec <-
function (number.plots) 
{
    if (number.plots > 25) 
        return(c(5, 5))
    start <- floor(sqrt(number.plots))
    if (start * start >= number.plots) 
        return(c(start, start))
    if (start * (start + 1) >= number.plots) 
        return(c(start, start + 1))
    return(c(start + 1, start + 1))
}
