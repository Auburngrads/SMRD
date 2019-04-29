get.time.units <-
function (data.d) 
{
    time.units <- attr(data.d, "time.units")
    if (is.null(time.units)) 
        time.units <- data.d$time.units
    if (is.null(time.units)) 
        time.units <- ""
    return(time.units)
}
